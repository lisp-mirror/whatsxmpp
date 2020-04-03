(in-package :whatsxmpp)

(defvar *last-stanza*)
(defparameter +streams-ns+ "urn:ietf:params:xml:ns:xmpp-streams")
(defparameter +stanzas-ns+ "urn:ietf:params:xml:ns:xmpp-stanzas")
(defparameter +component-ns+ "jabber:component:accept")
(defparameter +disco-info-ns+ "http://jabber.org/protocol/disco#info")
(defparameter +disco-items-ns+ "http://jabber.org/protocol/disco#items")
(defparameter +muc-ns+ "http://jabber.org/protocol/muc")
(defparameter +file-upload-ns+ "urn:xmpp:http:upload:0")
(defparameter +vcard-temp-ns+ "vcard-temp")

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
   (handlers
    :initform (make-hash-table)
    :accessor component-handlers)
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

(defun request-http-upload-slot (comp service-jid filename size mime-type)
  "Requests an XEP-0363 HTTP Upload slot from the service at SERVICE-JID, aiming to upload the file with FILENAME, SIZE (in bytes) and MIME-TYPE. Returns a promise that resolves with a list of the form ((PUT-URL . ((HEADER-NAME . HEADER-VALUE) ...)) GET-URL)."
  (declare (type xmpp-component comp) (type string service-jid filename mime-type) (type integer size))
  (attach
   (with-iq (comp service-jid)
     (cxml:with-element "request"
       (cxml:attribute "xmlns" +file-upload-ns+)
       (cxml:attribute "filename" filename)
       (cxml:attribute "size" (write-to-string size))
       (cxml:attribute "content-type" mime-type)))
   (lambda (results)
     (let ((slot-node (get-node-named results "slot")))
       (unless slot-node
         (error "Malformed XEP-0363 response: no <slot/>"))
       (let* ((children (dom:child-nodes slot-node))
              (put-node (get-node-named children "put"))
              (get-node (get-node-named children "get"))
              (headers '()))
         (unless (and put-node get-node)
           (error "Malformed XEP-0363 response: PUT or GET nodes missing"))
         (loop
           for node across (dom:child-nodes put-node)
           do (let ((name (dom:tag-name node)))
                (when (equal name "header")
                  (setf headers (cons
                                 (cons (dom:get-attribute node "name")
                                       (dom:node-value (elt (dom:child-nodes node) 0)))
                                 headers)))))
         `((,(dom:get-attribute put-node "url") . ,headers) ,(dom:get-attribute get-node "url")))))))

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
    :initform nil
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

(defun send-iq-error (comp id to from e)
  "Send E (a STANZA-ERROR) as an IQ error response."
  (with-component-xml-output (comp)
    (cxml:with-element "iq"
      (cxml:attribute "type" "error")
      (cxml:attribute "id" id)
      (cxml:attribute "from" to)
      (cxml:attribute "to" from)
      (cxml:with-element "error"
        (cxml:attribute "type" (stanza-error-type e))
        (cxml:with-element (stanza-error-condition e)
          (cxml:attribute "xmlns" +stanzas-ns+))
        (when (stanza-error-text e)
          (cxml:with-element "text"
            (cxml:text (stanza-error-text e))))))))

(defmacro disco-identity (name type category)
  `(cxml:with-element "identity"
     (cxml:attribute "name" ,name)
     (cxml:attribute "type" ,type)
     (cxml:attribute "category" ,category)))

(defmacro disco-feature (feature)
  `(cxml:with-element "feature"
     (cxml:attribute "var" ,feature)))

(defun disco-info-handler (comp &key to &allow-other-keys)
  "Handles XEP-0030 disco#info requests."
  (format *debug-io* "~&disco#info: ~A~%" to)
  (with-component-data-lock (comp)
    `((cxml:with-element "query"
        (cxml:attribute "xmlns" ,+disco-info-ns+)
        (disco-feature +disco-info-ns+)
        ,@(cond
            ((equal to (component-name comp))
             `((disco-identity "whatsxmpp bridge" "xmpp" "gateway")
               (disco-feature ,+muc-ns+)))
            (t nil))))))

(defun disco-items-handler (comp &key to &allow-other-keys)
  "Handles XEP-0030 disco#items requests."
  (format *debug-io* "~&disco#items: ~A~%" to)
  (with-component-data-lock (comp)
    `((cxml:with-element "query"
        (cxml:attribute "xmlns" ,+disco-info-ns+)))))

(defun register-component-iq-handler (comp handler-name func)
  "Register FUNC to be called for the HANDLER-NAME IQ handler on COMP."
  (with-component-data-lock (comp)
    (setf (gethash handler-name (component-handlers comp)) func)))

(defun call-component-iq-handler (comp handler &rest args)
  "Calls the IQ handler identified by the symbol HANDLER on COMP, with the provided ARGS."
  (destructuring-bind (&key id to from &allow-other-keys) args
    (with-component-data-lock (comp)
      (catcher
       (attach
        (let ((func (gethash handler (component-handlers comp))))
          (unless func
            (error 'stanza-error
                   :defined-condition "feature-not-implemented"
                   :text (format nil "No handler for ~A registered" handler)
                   :type "cancel"))
          (let ((result (apply func comp args)))
            result))
        (lambda (result-forms)
          (eval `(with-component-xml-output (,comp)
                   (cxml:with-element "iq"
                     (cxml:attribute "type" "result")
                     (cxml:attribute "id" ,id)
                     (cxml:attribute "from" ,to)
                     (cxml:attribute "to" ,from)
                     ,@result-forms)))))
       (stanza-error (e) (send-iq-error comp id to from e))
       (t (e)
          (send-iq-error comp id to from
                         (make-condition 'stanza-error
                                         :defined-condition "internal-server-error"
                                         :text (write-to-string e)
                                         :type "cancel"))
          (with-simple-restart
              (continue "Continue execution.")
            (invoke-debugger e)))))))

(defun handle-iq-get (comp id from stanza)
  "Handles an IQ-get STANZA for component COMP."
  (let* ((first-child (elt (dom:child-nodes stanza) 0))
         (tag-name (dom:tag-name first-child))
         (to (dom:get-attribute stanza "to"))
         (xmlns (dom:get-attribute first-child "xmlns"))
         (handler-type
           (cond
             ((and (equal xmlns +disco-info-ns+) (equal tag-name "query"))
              :disco-info)
             ((and (equal xmlns +disco-items-ns+) (equal tag-name "query"))
              :disco-items)
             ((and (equal xmlns +vcard-temp-ns+) (equal tag-name "vCard"))
              :vcard-temp-get)
             (t
              :generic-iq))))
    (call-component-iq-handler comp handler-type
                               :to to
                               :id id
                               :from from
                               :stanza stanza)))

(defun handle-iq-response (comp stanza)
  "Handles an IQ response STANZA for component COMP."
  (with-component-data-lock (comp)
    (let ((type (dom:get-attribute stanza "type"))
          (id (dom:get-attribute stanza "id"))
          (from (dom:get-attribute stanza "from")))
      (if (equal type "get")
          (handle-iq-get comp id from stanza)
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

(defun handle-presence (comp stanza)
  "Handles a presence STANZA for component COMP."
  (let* ((type (dom:get-attribute stanza "type"))
         (from (dom:get-attribute stanza "from"))
         (to (dom:get-attribute stanza "to"))
         (event-name
           (cond
             ((equal type "subscribe") :presence-subscribe)
             ((equal type "probe") :presence-probe)
             ((equal type "unavailable") :presence-unavailable)
             (t :presence))))
    (emit event-name comp :from from :to to :stanza stanza)))

(defun handle-message (comp stanza)
  "Handles a message STANZA for component COMP."
  (let* ((from (dom:get-attribute stanza "from"))
         (to (dom:get-attribute stanza "to"))
         (body (get-node-named (dom:child-nodes stanza) "body")))
    (if body
        (let* ((child-nodes (dom:child-nodes body))
               (text (if (> (length child-nodes) 0)
                         (dom:node-value (elt child-nodes 0))
                         "")))
          (emit :text-message comp :from from :to to :body text :stanza stanza))
        (emit :message comp :from from :to to :stanza stanza))))

(defun component-stanza (comp stanza)
  "Handles a STANZA received by component COMP."
  (setf *last-stanza* stanza)
  (let* ((stanza (dom:document-element stanza))
         (tag-name (dom:tag-name stanza)))
    (cond
      ((equal tag-name "stream:error") (handle-stream-error comp stanza))
      ((equal tag-name "handshake") (handle-connection-complete comp))
      ((equal tag-name "iq") (handle-iq-response comp stanza))
      ((equal tag-name "presence") (handle-presence comp stanza))
      ((equal tag-name "message") (handle-message comp stanza))
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
