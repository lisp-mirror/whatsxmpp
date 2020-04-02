(defpackage :whatsxmpp
  (:use :cl :usocket :event-emitter :blackbird :blackbird-base))
(in-package :whatsxmpp)

(defvar *last-stanza*)
(defparameter +streams-ns+ "urn:ietf:params:xml:ns:xmpp-streams")
(defparameter +stanzas-ns+ "urn:ietf:params:xml:ns:xmpp-stanzas")
(defparameter +component-ns+ "jabber:component:accept")
(defparameter +disco-info-ns+ "http://jabber.org/protocol/disco#info")
(defparameter +disco-items-ns+ "http://jabber.org/protocol/disco#items")

(defclass xmpp-component (event-emitter)
  ((socket
    :initarg :socket
    :accessor component-socket)
   (socket-lock
    :initform (bt:make-recursive-lock)
    :accessor component-socket-lock)
   (data-lock
    :initform (bt:make-recursive-lock)
    :accessor component-data-lock)
   (sink
    :initarg :sink
    :accessor component-sink)
   (name
    :initarg :name
    :reader component-name)
   (stream-id
    :initform nil
    :accessor component-stream-id)
   (shared-secret
    :initarg :shared-secret
    :reader component-shared-secret)
   (promises
    :initform (make-hash-table :test 'equal)
    :accessor component-promises)))

(defmacro with-component-data-lock ((comp) &body body)
  `(bt:with-recursive-lock-held ((component-data-lock ,comp))
     ,@body))

(defclass xmpp-source (cxml:broadcast-handler)
  ((component
    :initarg :component
    :accessor source-component)
   (depth
    :initform 0
    :accessor source-depth)))

(defun make-xmpp-source (comp)
  (let ((ret (cxml:make-broadcast-handler)))
    (change-class ret 'xmpp-source
                  :component comp)
    ret))

(defmethod sax:start-document ((s xmpp-source))
  (declare (ignore s))
  (format *debug-io* "~&XMPP --> [document started]~%"))

(defmethod sax:start-element ((s xmpp-source) namespace-uri local-name qname attributes)
  (with-accessors ((depth source-depth) (comp source-component) (handlers cxml:broadcast-handler-handlers)) s
    (incf depth)
    (when (and (eql depth 1) (equal qname "stream:stream"))
      (flet ((local-name-is-id (attr)
               (equal (sax:attribute-local-name attr) "id")))
        (let ((stream-id-attr (find-if #'local-name-is-id attributes)))
          (when (not stream-id-attr)
            (error "Server didn't send a stream ID"))
          (format *debug-io* "~&XMPP --> [stream started, ID ~A]~%" (sax:attribute-value stream-id-attr))
          (with-component-data-lock (comp)
            (setf (component-stream-id comp) (sax:attribute-value stream-id-attr))
            (emit :stream-started comp))
          (return-from sax:start-element))))
    (when (eql depth 2)
      (let ((dom-builder (cxml-dom:make-dom-builder)))
        (format *debug-io* "~&XMPP --> ")
        (setf handlers (list (cxml:make-character-stream-sink *debug-io*) dom-builder))
        (sax:start-document dom-builder)))
    (call-next-method s namespace-uri local-name qname attributes)))

(defmethod sax:end-element :before ((s xmpp-source) namespace-uri local-name qname)
  (when (equal qname "stream:stream")
    (error "Server closed the stream")))

(defmethod sax:end-element :after ((s xmpp-source) namespace-uri local-name qname)
  (with-accessors ((depth source-depth) (comp source-component) (handlers cxml:broadcast-handler-handlers)) s
    (decf depth)
    (when (eql depth 1)
      (let* ((debug-sink (first handlers))
             (dom-builder (second handlers))
             (stanza (sax:end-document dom-builder)))
        (sax:end-document debug-sink)
        (terpri *debug-io*)
        (setf handlers nil)
        (emit :raw-stanza comp stanza)))))

(defclass xmpp-sink (cxml:broadcast-handler)
  ((sink-open
    :initform t
    :accessor sink-open)))

(defmethod sax:start-document ((s xmpp-sink))
  (declare (ignore s))
  (format *debug-io* "~&XMPP <-- "))

(defmethod sax:end-element ((s xmpp-sink) namespace-uri local-name qname)
  (if (and (sink-open s) (equal local-name "stream"))
      ;; The <stream:stream> element gets opened at the start of the connection
      ;; and closing it represents ending the connection. We therefore don't
      ;; want to close it...
      ;; Instead, send some empty characters to get the sinks to write the last ">"
      ;; bit of the opening tag.
      (sax:characters s "")
      (call-next-method s namespace-uri local-name qname))
  (terpri *debug-io*))

(defun close-xmpp-component (comp)
  (bt:with-recursive-lock-held ((component-socket-lock comp))
    (setf (sink-open (component-sink comp)) nil)
    (write-sequence (babel:string-to-octets "</stream:stream>"
                                            :encoding :utf-8)
                    (component-socket comp))
    (force-output (component-socket comp))
    (close (component-socket comp))))

(defun make-xmpp-sink (socket)
  (let ((ret (cxml:make-broadcast-handler
              (cxml:make-character-stream-sink *debug-io*)
              (cxml:make-octet-stream-sink socket))))
    (change-class ret 'xmpp-sink)
    ret))

(defmacro with-dom-xml-output (&body body)
  `(cxml:with-xml-output (cxml-dom:make-dom-builder)
     ,@body))

(defun component-listen-thread (comp)
  "Listening thread for an XMPP component: constantly reads from the socket and emits new stanzas."
  (format *debug-io* "Starting component listening thread~%")
      ;; ### Story time! ###
      ;; So I spent an hour debugging why this wasn't working.
      ;; And, long story short, if you just call CXML:PARSE with a stream
      ;; it gets converted into an 'xstream' inside CXML, which has a :SPEED
      ;; property. This :SPEED property controls how many bytes it tries to buffer
      ;; before actually doing the parsing and the goddamn default is 8192 (!!).
      ;; This obviously ain't gonna fly for our TCP socket, because the initial stream
      ;; start element is less than 8192 bytes. So we make our own stupid xstream
      ;; and specify the speed manually, and then it works.
      ;;
      ;; Wouldn't it be nice if people documented this sort of thing?
      (let ((source (make-xmpp-source comp))
            (fucking-stream (cxml:make-xstream (component-socket comp)
                                               :speed 1 ; FFFFFFFFUUUUUUUU
                                               :initial-speed 1)))
        (cxml:parse fucking-stream source
                    :recode t)))

(defmacro with-component-xml-output ((comp) &body body)
  (let ((ret-sym (gensym)))
    `(with-accessors ((lock component-socket-lock) (socket component-socket) (sink component-sink))
         ,comp
       (with-component-data-lock (,comp)
         (bt:with-recursive-lock-held (lock)
           (let ((,ret-sym nil))
             (cxml:with-xml-output sink
               (setf ,ret-sym ,@body))
             (force-output socket)
             ,ret-sym))))))

(defun write-stream-header (comp)
  (with-component-xml-output (comp)
    (cxml:with-namespace ("stream" "http://etherx.jabber.org/streams")
      (cxml:with-element "stream:stream"
        (cxml:attribute "xmlns" +component-ns+)
        (cxml:attribute "to" (component-name comp))))))

(defun component-stream-started (comp)
  (with-component-xml-output (comp)
    (cxml:with-element "handshake"
      (cxml:attribute "xmlns" +component-ns+)
      (cxml:text (string-downcase (sha1:sha1-hex (concatenate 'string (component-stream-id comp) (component-shared-secret comp))))))))

(defun make-message-uuid (comp)
  (with-accessors ((promises component-promises)) comp
    (let ((uuid (string-downcase (write-to-string (uuid:make-v4-uuid))))
          (promise (make-promise)))
      (setf (gethash uuid promises) promise)
      (values uuid promise))))

(defmacro with-stanza ((comp stanza-name &key type from to) &body body)
  (alexandria:with-gensyms (uuid ret from-sym)
    `(with-component-xml-output (,comp)
       (let ((,from-sym (or ,from (component-name ,comp))))
         (multiple-value-bind (,uuid ,ret)
             (make-message-uuid ,comp)
           (cxml:with-element ,stanza-name
             (cxml:attribute "from" ,from-sym)
             (cxml:attribute "id" ,uuid)
             ,(when to
                `(cxml:attribute "to" ,to))
             ,(when type
                `(cxml:attribute "type" ,type))
             ,@body)
           ,ret)))))

(defmacro with-iq ((comp to &key (type "get") from) &body body)
  "Send an IQ stanza (of type TYPE) on the COMP component, from the JID FROM (default: component name) to the JID TO, with BODY specifying further CXML commands to make up the body of the stanza. Returns a promise."
  `(with-stanza (,comp "iq"
                 :type ,type
                 :to ,to
                 :from ,from)
     ,@body))

(defmacro with-message ((comp to &key (type "chat") from) &body body)
  "Send a message stanza (of type TYPE) on the COMP component. Semantics the same as WITH-IQ, except for the fact that message stanzas don't normally prompt a response."
  `(with-stanza (,comp "message"
                 :type ,type
                 :to ,to
                 :from ,from)
     ,@body))

(defmacro with-presence ((comp to &key type from) &body body)
  "Send a presence stanza (of type TYPE) on the COMP component. Semantics the same as WITH-IQ, except for the fact that presence stanzas don't normally prompt a response."
  `(with-stanza (,comp "presence"
                 :type ,type
                 :to ,to
                 :from ,from)
     ,@body))

(defun get-node-named (nodes name)
  "Finds the node with tag name NAME in NODES, returning NIL if none was found."
  (flet ((is-the-node (node) (equal (dom:tag-name node) name)))
    (find-if #'is-the-node nodes)))

(defun get-disco-info (comp to &optional from)
  "Send an XEP-0030 disco#info request. Returns a promise that resolves with a list of supported features."
  (attach
   (with-iq (comp to :from from)
     (cxml:with-element "query"
       (cxml:attribute "xmlns" +disco-info-ns+)))
   (lambda (results)
     (let ((query-node (get-node-named results "query"))
           (features '()))
       (unless query-node
         (error "Malformed disco#info response: no <query/>"))
       (loop
         for node across (dom:child-nodes query-node)
         do (let ((name (dom:tag-name node)))
              (when (equal name "feature")
                (setf features (cons (dom:get-attribute node "var") features)))))
       features))))

(defun get-disco-items (comp to &optional from)
  "Send an XEP-0030 disco#items request. Returns a promise that resolves with an alist, mapping JIDs to names."
  (attach
   (with-iq (comp to :from from)
     (cxml:with-element "query"
       (cxml:attribute "xmlns" +disco-items-ns+)))
   (lambda (results)
     (let ((query-node (get-node-named results "query"))
           (items '()))
       (unless query-node
         (error "Malformed disco#items response: no <query/>"))
       (loop
         for node across (dom:child-nodes query-node)
         do (let ((name (dom:tag-name node)))
              (when (equal name "item")
                (setf items (cons
                             (cons (dom:get-attribute node "jid") (dom:get-attribute node "name"))
                             items)))))
       items))))

(defun send-text-message (comp to-jid text &optional from)
  "Send a simple text message to TO-JID, containing TEXT."
  (with-message (comp to-jid :from from)
    (cxml:with-element "body"
      (cxml:text text))))

(defun handle-stream-error (comp stanza)
  (flet ((is-error-node (node)
           (equal (dom:namespace-uri node) +streams-ns+))
         (is-text-node (node)
           (equal (dom:tag-name node) "text")))
    (let* ((children (dom:child-nodes stanza))
           (error-node (find-if #'is-error-node children))
           (error-text-node (find-if #'is-text-node children))
           (error-name (dom:tag-name error-node))
           (error-text (when error-text-node
                         (dom:node-value (elt (dom:child-nodes error-text-node) 0)))))
      (warn "Stream error of type ~A encountered: ~A" error-name error-text)
      (emit :stream-error comp error-name error-text stanza))))

(define-condition stanza-error (error)
  ((defined-condition
       :initarg :defined-condition
       :accessor stanza-error-condition)
   (type
    :initarg :type
    :accessor stanza-error-type)
   (text
    :initarg :text
    :initform nil
    :accessor stanza-error-text)
   (raw
    :initarg :raw
    :accessor stanza-error-raw))
  (:report (lambda (err stream)
             (with-slots (defined-condition type text) err
               (format stream "~A (type ~A): ~A" defined-condition type text)))))

(defun extract-stanza-error (stanza)
  "Extracts a STANZA-ERROR from the given STANZA, which must contain an <error/> element conforming to RFC 6120 § 8.3."
  (flet ((is-error-condition-node (node)
           (equal (dom:namespace-uri node) +stanzas-ns+))
         (is-error-node (node)
           (equal (dom:tag-name node) "error"))
         (is-text-node (node)
           (and (equal (dom:namespace-uri node) +stanzas-ns+) (equal (dom:tag-name node) "text"))))
    (let* ((error-node (find-if #'is-error-node (dom:child-nodes stanza)))
           (error-children (dom:child-nodes error-node))
           (type (dom:get-attribute error-node "type"))
           (condition-node (find-if #'is-error-condition-node error-children))
           (condition-name (dom:tag-name condition-node))
           (text-node (find-if #'is-text-node error-children))
           (text (when text-node
                   (dom:node-value (elt (dom:child-nodes text-node) 0)))))
      (make-condition 'stanza-error
                      :raw error-node
                      :defined-condition condition-name
                      :type type
                      :text text))))

(defun handle-connection-complete (comp)
  (declare (ignore comp))
  (format *debug-io* "Connection complete! \o/"))

(defun handle-iq-response (comp stanza)
  "Handles an IQ response STANZA for component COMP."
  (with-component-data-lock (comp)
    (let ((type (dom:get-attribute stanza "type"))
          (id (dom:get-attribute stanza "id"))
          (from (dom:get-attribute stanza "from")))
      (if (equal type "get")
          (emit :iq-get comp id from stanza)
          (symbol-macrolet
              ((promise (gethash id (component-promises comp))))
            (if promise
                (progn
                  (format t "~&IQ ~A from ~A for ~A~%" type from id)
                  (cond
                    ((equal type "result") (finish promise (dom:child-nodes stanza)))
                    ((equal type "error") (signal-error promise (extract-stanza-error stanza)))
                    (t (warn "Invalid IQ stanza type: ~A" type)))
                  (setf promise nil))
                (warn "Unsolicited IQ stanza from ~A of type ~A, ID ~A" from type id)))))))

(defun component-stanza (comp stanza)
  (setf *last-stanza* stanza)
  (let* ((stanza (dom:document-element stanza))
         (tag-name (dom:tag-name stanza)))
    (cond
      ((equal tag-name "stream:error") (handle-stream-error comp stanza))
      ((equal tag-name "handshake") (handle-connection-complete comp))
      ((equal tag-name "iq") (handle-iq-response comp stanza))
      (t (emit :stanza comp stanza)))))

(defun make-component (server port shared-secret name)
  "Make a new XMPP component, connecting to SERVER on PORT with SHARED-SECRET."
  (let* ((socket (socket-stream
                  (socket-connect server port
                                  :element-type '(unsigned-byte 8))))
         (component (make-instance 'xmpp-component
                                   :socket socket
                                   :sink (make-xmpp-sink socket)
                                   :name name
                                   :shared-secret shared-secret)))
    (bt:make-thread (lambda ()
                      (component-listen-thread component))
                    :name "XMPP component listen thread")
    (on :stream-started component (lambda ()
                                    (component-stream-started component)))
    (on :raw-stanza component (lambda (stanza)
                            (component-stanza component stanza)))
    (write-stream-header component)
    component))
