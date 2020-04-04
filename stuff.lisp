(in-package :whatsxmpp)

(defvar *last-stanza*)
(defparameter +streams-ns+ "urn:ietf:params:xml:ns:xmpp-streams")
(defparameter +stanzas-ns+ "urn:ietf:params:xml:ns:xmpp-stanzas")
(defparameter +component-ns+ "jabber:component:accept")
(defparameter +disco-info-ns+ "http://jabber.org/protocol/disco#info")
(defparameter +disco-items-ns+ "http://jabber.org/protocol/disco#items")
(defparameter +muc-ns+ "http://jabber.org/protocol/muc")
(defparameter +file-upload-ns+ "urn:xmpp:http:upload:0")
(defparameter +oob-ns+ "jabber:x:oob")
(defparameter +chat-markers-ns+ "urn:xmpp:chat-markers:0")
(defparameter +delivery-delay-ns+ "urn:xmpp:delay")
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

(defclass whatsxmpp-component (xmpp-component)
  ((whatsapps
    :initform (make-hash-table :test 'equal)
    :accessor component-whatsapps)
   (reconnect-timer
    :initform nil
    :accessor component-reconnect-timer)
   (upload-component-name
    :initarg :upload-component-name
    :accessor component-upload-component-name)))

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
    (handler-case
        (cxml:parse fucking-stream source
                    :recode t)
      (error (e)
        (with-simple-restart
            (continue "Continue execution.")
          (invoke-debugger e))
        (format *debug-io* "~&Component listen thread failed: ~A~%" e)
        (emit :error comp e)))))

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

(defun sha1-hex (str)
  "Returns the SHA1 of STR, a string, in lowercase hex."
  (format nil "~(~{~2,'0X~}~)"
          (coerce
           (ironclad:digest-sequence :sha1
                                     (babel:string-to-octets str))
           'list)))

(defun component-stream-started (comp)
  (with-component-xml-output (comp)
    (cxml:with-element "handshake"
      (cxml:attribute "xmlns" +component-ns+)
      (cxml:text (string-downcase (sha1-hex (concatenate 'string (component-stream-id comp) (component-shared-secret comp))))))))

(defun make-message-uuid (comp)
  (with-accessors ((promises component-promises)) comp
    (let ((uuid (string-downcase (write-to-string (uuid:make-v4-uuid))))
          (promise (make-promise)))
      (setf (gethash uuid promises) promise)
      (values uuid promise))))

(defmacro with-stanza ((comp stanza-name &key type from to id) &body body)
  (alexandria:with-gensyms (uuid ret from-sym id-sym)
    `(with-component-xml-output (,comp)
       (let ((,from-sym (or ,from (component-name ,comp)))
             (,id-sym ,id))
         (multiple-value-bind (,uuid ,ret)
             (if ,id-sym
                 (values ,id-sym ,id-sym)
                 (make-message-uuid ,comp))
           (cxml:with-element ,stanza-name
             (cxml:attribute "from" ,from-sym)
             (cxml:attribute "id" ,uuid)
             ,(when to
                `(cxml:attribute "to" ,to))
             ,(when type
                `(cxml:attribute "type" ,type))
             ,@body)
           ,ret)))))

(defmacro with-iq ((comp to &key (type "get") from id) &body body)
  "Send an IQ stanza (of type TYPE) on the COMP component, from the JID FROM (default: component name) to the JID TO, with BODY specifying further CXML commands to make up the body of the stanza. Returns a promise."
  `(with-stanza (,comp "iq"
                 :type ,type
                 :to ,to
                 :from ,from
                 :id ,id)
     ,@body))

(defmacro with-message ((comp to &key (type "chat") from id) &body body)
  "Send a message stanza (of type TYPE) on the COMP component. Semantics the same as WITH-IQ, except for the fact that message stanzas don't normally prompt a response."
  `(with-stanza (,comp "message"
                 :type ,type
                 :to ,to
                 :from ,from
                 :id ,id)
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
  (format *debug-io* "Connection complete! \o/")
  (emit :connected comp))

(defun send-stanza-error (comp &key id to from e stanza-type)
  "Send E (a STANZA-ERROR) as an error response to a stanza of type STANZA."
  (with-component-xml-output (comp)
    (cxml:with-element stanza-type
      (cxml:attribute "type" "error")
      (cxml:attribute "id" id)
      (cxml:attribute "from" from)
      (cxml:attribute "to" to)
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
       (stanza-error (e)
                     (send-stanza-error comp
                                        :stanza-type "iq"
                                        :id id :to from :from to :e e))
       (t (e)
          (send-stanza-error comp
                             :stanza-type "iq"
                             :id id
                             :to from
                             :from to
                             :e (make-condition 'stanza-error
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
         (id (dom:get-attribute stanza "id"))
         (body (get-node-named (dom:child-nodes stanza) "body")))
    (if body
        (let* ((child-nodes (dom:child-nodes body))
               (text (if (> (length child-nodes) 0)
                         (dom:node-value (elt child-nodes 0))
                         "")))
          (emit :text-message comp :from from :to to :body text :id id :stanza stanza))
        (emit :message comp :from from :to to :id id :stanza stanza))))

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

(defun register-whatsxmpp-handlers (comp)
  (register-component-iq-handler comp :disco-info #'disco-info-handler)
  (register-component-iq-handler comp :disco-items #'disco-items-handler))

(defun parse-jid (jid)
  "Parse JID, returning the multiple values HOSTNAME, LOCALPART and RESOURCE."
  (declare (type string jid))
  (let ((at-pos (position #\@ jid))
        (slash-pos (position #\/ jid)))
    (cond
      ((and (not slash-pos) (not at-pos))
       (values jid nil nil))
      ((and slash-pos (not at-pos))
       (multiple-value-bind (hostname resource)
           (whatscl::split-at jid slash-pos)
         (values hostname nil resource)))
      ((and (not slash-pos) at-pos)
       (multiple-value-bind (localpart hostname)
           (whatscl::split-at jid at-pos)
         (values hostname localpart nil)))
      (t
       (multiple-value-bind (rest resource)
           (whatscl::split-at jid slash-pos)
         (multiple-value-bind (localpart hostname)
             (whatscl::split-at rest at-pos)
           (values hostname localpart resource)))))))

(defun strip-resource (jid)
  "Strips a resource from JID, if there is one, returning the bare JID."
  (let ((slash-pos (position #\/ jid)))
    (if slash-pos
        (whatscl::split-at jid slash-pos)
        jid)))

(defun admin-jid (comp)
  "Get the admin JID for COMP. You need the lock to be taken out for this one."
  (concatenate 'string "admin@" (component-name comp) "/adminbot"))

(defparameter *admin-help-text*
  "This is a very beta WhatsApp to XMPP bridge!
Commands:
- register: set up the bridge
- connect: manually connect to WhatsApp
- stop: disconnect from WhatsApp, and disable automatic reconnections
- status: get your current status
- help: view this help text")

(defparameter *reconnect-every-secs* 5
  "Interval between calls to WA-RESETUP-USERS.")

(defun admin-msg (comp jid text)
  "Send an admin message from the admin on COMP to JID."
  (send-text-message comp jid text (admin-jid comp)))

(defun wa-resetup-users (comp)
  "Go through the list of WhatsApp users and reconnect those whose connections have dropped."
  (with-component-data-lock (comp)
    (let* ((users-to-reconnect
             (loop
               for jid being the hash-keys in (component-whatsapps comp)
                 using (hash-value conn)
               append (unless conn
                        (list jid))))
           (num-users (length users-to-reconnect)))
      (when (> num-users 0)
        (format *debug-io* "~&resetup-users: ~A users to reconnect~%" num-users))
      (loop
        for user in users-to-reconnect
        do (handle-setup-user comp user))
      (trivial-timers:schedule-timer (component-reconnect-timer comp) *reconnect-every-secs*))))

(defun send-qrcode (comp jid text)
  "Send a QR code containing TEXT to JID."
  (with-component-data-lock (comp)
    (uiop:with-temporary-file (:stream stream
                               :pathname path
                               :keep t) ; Needed because async
      (format *debug-io* "~&using path ~A~%" path)
      (cl-qrencode:encode-png-stream text stream)
      (catcher
       (attach
        (request-http-upload-slot comp (component-upload-component-name comp)
                                  "qrcode.png"
                                  (file-length stream)
                                  "image/png")
        (lambda (slot)
          (destructuring-bind ((put-url . headers) get-url) slot
            (format *debug-io* "~&got put-url: ~A~%   get-url: ~A~%" put-url get-url)
            (multiple-value-bind (body status-code)
                (drakma:http-request put-url
                                     :additional-headers headers
                                     :content-type "image/png"
                                     :method :put
                                     :content path)
              (unless (and (>= status-code 200) (< status-code 300))
                (format *debug-io* "~&upload failed! status ~A, body ~A~%" status-code body)
                (error "HTTP upload failed with status ~A" status-code))
              (with-component-data-lock (comp)
                (let ((ajid (admin-jid comp)))
                  (admin-msg comp jid "WhatsApp Web registration: Scan the following QR code with your device! (Menu -> WhatsApp Web)")
                  (with-message (comp jid :from ajid)
                    (cxml:with-element "body"
                      (cxml:text get-url))
                    (cxml:with-element "x"
                      (cxml:attribute "xmlns" +oob-ns+)
                      (cxml:with-element "url"
                        (cxml:text get-url))))
                  (admin-msg comp jid "(Code expired? Be faster next time. Get a new one with `connect`.)")))))))
       (t (e)
          (admin-msg comp jid (format nil "Failed to upload QR code!~%Report the following error to the bridge admin: `~A`" e))
          (invoke-debugger e))))))

(defparameter *user-jid-scanner*
  (cl-ppcre:create-scanner "u([0-9]+)"))

(defparameter *group-jid-scanner*
  (cl-ppcre:create-scanner "g([0-9]+)-([0-9]+)"))

(defun wa-jid-to-whatsxmpp-localpart (waj)
  "Convert a whatscl JID object to a WhatsXMPP localpart."
  (with-accessors ((localpart whatscl::jid-localpart) (hostname whatscl::jid-hostname)) waj
    (cond
      ((or (equal hostname "s.whatsapp.net") (equal hostname "c.us"))
       (concatenate 'string "u" localpart))
      ((equal hostname "g.us")
       (concatenate 'string "g" localpart))
      (t
       (concatenate 'string "other-" localpart "-" hostname)))))

(defun whatsxmpp-localpart-to-wa-jid (localpart)
  "Parses a WhatsXMPP localpart, returning a whatscl JID object if parsing is successful.
WhatsXMPP represents users as u440123456789 and groups as g1234-5678."
  (cl-ppcre:register-groups-bind (digits)
      (*user-jid-scanner* localpart)
    (return-from whatsxmpp-localpart-to-wa-jid
      (whatscl::make-jid digits "s.whatsapp.net")))
  (cl-ppcre:register-groups-bind (creator ts)
      (*group-jid-scanner* localpart)
    (return-from whatsxmpp-localpart-to-wa-jid
      (whatscl::make-jid (concatenate 'string creator "-" ts) "g.us"))))

(defun wa-conn-recent-p (comp conn jid)
  (let ((current (gethash jid (component-whatsapps comp))))
    (eql current conn)))

(defmacro with-wa-handler-context ((comp conn jid) &body body)
  "Takes the component data lock, checks that CONN is the most up-to-date connection for JID, and then executes BODY."
  `(with-component-data-lock (,comp)
     (if (wa-conn-recent-p ,comp ,conn ,jid)
         (progn ,@body)
         (warn "WA handler called with out of date connection, ignoring"))))

(defun wa-handle-ws-error (comp conn jid err)
  (with-wa-handler-context (comp conn jid)
    (format *debug-io* "~&ws-error ~A: ~A~%" jid err)
    (admin-msg comp jid
               (format nil "WhatsApp websocket error: ~A" err))
    (setf (gethash jid (component-whatsapps comp)) nil)))

(defun wa-handle-ws-close (comp conn jid)
  (with-wa-handler-context (comp conn jid)
    (format *debug-io* "~&ws-close: ~A~%" jid)
    (admin-msg comp jid
               "WhatsApp websocket closed (will reconnect soon).")
    (setf (gethash jid (component-whatsapps comp)) nil)))

(defun wa-handle-ws-open (comp conn jid)
  (with-wa-handler-context (comp conn jid)
    (format *debug-io* "~&ws-open: ~A~%" jid)
    (admin-msg comp jid
               "WhatsApp websocket connected.")))

(defun wa-handle-ws-qrcode (comp conn jid qrcode)
  (with-wa-handler-context (comp conn jid)
    (format *debug-io* "~&qrcode: ~A~%" jid)
    (send-qrcode comp jid qrcode)))

(defun update-session-data (jid sessdata)
  (with-prepared-statement
      (update-sessdata-stmt "UPDATE users SET session_data = ? WHERE jid = ?")
    (format *debug-io* "~&update sessdata for ~A~%" jid)
    (bind-parameters update-sessdata-stmt sessdata jid)
    (sqlite:step-statement update-sessdata-stmt)))

(defun wa-handle-ws-connected (comp conn jid wa-jid)
  (with-wa-handler-context (comp conn jid)
    (let ((sessdata (whatscl::serialize-persistent-session (whatscl::wac-session conn))))
      (update-session-data jid sessdata))
    (admin-msg comp jid
               (format nil "Logged in to WhatsApp as ~A." wa-jid))
    (format *debug-io* "~&ws-connected: ~A~%" jid)))

(defun wa-handle-error-status-code (comp conn jid err)
  (with-wa-handler-context (comp conn jid)
    (format *debug-io* "~&error-status-code for ~A: ~A~%" jid err)
    (let ((status-code (whatscl::scerror-status-code err)))
      (cond
        ((equal status-code 401)
         (progn
           (admin-msg comp jid "Error: The WhatsApp Web connection was removed from your device! You'll need to scan the QR code again.")
           (update-session-data jid "")))
        ((equal status-code 403)
         (progn
           (admin-msg comp jid "Error: WhatsApp Web denied access. You may have violated the Terms of Service.")
           (update-session-data jid "")))
        (t
         (admin-msg comp jid (format nil "Login failure: ~A" err)))))
    (admin-msg comp jid "(Disabling automatic reconnections.)")
    (remhash jid (component-whatsapps comp))))

(defun wa-handle-error (comp conn jid err)
  (with-wa-handler-context (comp conn jid)
    (format *debug-io* "~&whatscl error for ~A: ~A~%" jid err)
    (admin-msg comp jid
               (format nil "A programming error has been detected and your connection has been aborted unexpectedly.~%Report the following error to the bridge admin: ~A" err))
    (admin-msg comp jid "(Disabling automatic reconnections.)")
    (remhash jid (component-whatsapps comp))))

(defun wa-handle-message (comp conn jid msg delivery-type)
  (with-wa-handler-context (comp conn jid)
    (let* ((key (whatscl::message-key msg))
           (wa-id (whatscl::message-id msg))
           (contents (whatscl::message-contents msg))
           (wa-ts (whatscl::message-ts msg))
           (xmpp-id (concatenate 'string
                                 "wa-" wa-id "-" (write-to-string wa-ts)))
           (local-time:*default-timezone* local-time:+utc-zone+)
           (ts (local-time:unix-to-timestamp wa-ts)))
      (format *debug-io* "~&message ~A for ~A with key ~A (type ~A)~%"
              wa-id jid key delivery-type)
      (when (eql delivery-type :relay) ; i.e. realtime
        (when (typep key 'whatscl::message-key-receiving) ; ignore group and self messages
          (when (typep contents 'whatscl::message-contents-text)
            (let ((text (whatscl::contents-text contents))
                  (from (concatenate 'string
                                     (wa-jid-to-whatsxmpp-localpart (whatscl::key-jid key))
                                     "@"
                                     (component-name comp)
                                     "/whatsapp")))
              (with-message (comp jid :from from :id xmpp-id)
                (cxml:with-element "body"
                  (cxml:text text))
                (cxml:with-element "delay"
                  (cxml:attribute "xmlns" +delivery-delay-ns+)
                  (cxml:attribute "stamp" (local-time:format-timestring nil ts)))
                (cxml:with-element "markable"
                  (cxml:attribute "xmlns" +chat-markers-ns+))))))))))

(defun bind-wa-handlers (comp conn jid)
  (on :ws-open conn (lambda () (wa-handle-ws-open comp conn jid)))
  (on :ws-close conn (lambda (&rest args)
                       (declare (ignore args))
                       (wa-handle-ws-close comp conn jid)))
  (on :ws-error conn (lambda (e) (wa-handle-ws-error comp conn jid e)))
  (on :error conn (lambda (e) (wa-handle-error comp conn jid e)))
  (on :error-status-code conn (lambda (e) (wa-handle-error-status-code comp conn jid e)))
  (on :qrcode conn (lambda (text) (wa-handle-ws-qrcode comp conn jid text)))
  (on :message conn (lambda (msg dt) (wa-handle-message comp conn jid msg dt)))
  (on :connected conn (lambda (waj) (wa-handle-ws-connected comp conn jid waj))))

(defun handle-setup-user (comp jid)
  "Set up a WhatsApp connection for JID on COMP."
  (with-component-data-lock (comp)
    (format *debug-io* "~&setup user: ~A~%" jid)
    (with-prepared-statement
        (get-session-data-stmt "SELECT session_data FROM users WHERE jid = ?")
        (bind-parameters get-session-data-stmt jid)
      (assert (sqlite:step-statement get-session-data-stmt) ()
              "HANDLE-SETUP-USER called for invalid JID ~A" jid)
      (let* ((sessdata (sqlite:statement-column-value get-session-data-stmt 0))
             (sess (when (and sessdata (> (length sessdata) 0))
                     (format *debug-io* "~&reusing old session data for ~A~%" jid)
                     (whatscl::deserialize-persistent-session sessdata)))
             (conn (whatscl::make-connection sess)))
        (admin-msg comp jid "Connecting to WhatsApp...")
        (symbol-macrolet
            ((stored-conn (gethash jid (component-whatsapps comp))))
          (let ((old-conn))
            (when stored-conn
              (setf old-conn stored-conn))
            (setf stored-conn conn)
            (bind-wa-handlers comp conn jid)
            (when old-conn
              (admin-msg comp jid "(destroying your old connection)")
              (whatscl::close-connection old-conn))
            (whatscl::start-connection conn)))))))

(defun start-user-registration (comp jid)
  "Register the JID as wanting to use the bridge COMP."
  (with-component-data-lock (comp)
    (let ((stripped (strip-resource jid)))
      (admin-msg comp jid "Starting registration!")
      (format *debug-io* "~&register: ~A~%" stripped)
      (with-prepared-statement
          (insert-stmt "INSERT INTO users (jid) VALUES (?) ON CONFLICT (jid) DO UPDATE SET session_data = ''")
        (bind-parameters insert-stmt stripped)
        (sqlite:step-statement insert-stmt))
      (admin-msg comp jid "WhatsApp connection should begin shortly...")
      (handle-setup-user comp stripped))))

(defun handle-admin-command (comp from body uid)
  "Handles an admin command sent to COMP."
  (labels ((reply (text)
             (send-text-message comp from text (admin-jid comp))))
    (let ((body (string-downcase body))
          (stripped (strip-resource from)))
      (cond
        ((and uid (equal body "register"))
         (reply (format nil "You're already registered!~%Try `connect`. If you really want to re-register, use the `register -force` command.")))
        ((or
          (and (not uid) (equal body "register"))
          (and uid (equal body "register -force")))
         (start-user-registration comp stripped))
        ((equal body "help")
         (reply *admin-help-text*))
        ((not uid)
         (reply "You're not registered with this bridge. Try `register` or `help`."))
        ((equal body "status")
         (multiple-value-bind (conn exists-p)
             (gethash stripped (component-whatsapps comp))
           (reply
            (cond
              ((and conn (whatscl::wac-jid conn))
               (format nil "Connected and logged in as ~A."
                       (whatscl::wac-jid conn)))
              (conn "Connected, but not logged in.")
              (exists-p "Temporarily disconnected.")
              (t "Disconnected (automatic reconnections disabled).")))))
        ((equal body "connect")
         (handle-setup-user comp stripped))
        ((equal body "stop")
         (let ((conn (gethash stripped (component-whatsapps comp))))
           (when (remhash stripped (component-whatsapps comp))
             (reply "WhatsApp connections disabled."))
           (when conn
             (whatscl::close-connection conn))))
        (t
         (reply "Unknown command. Try `help` for a list of supported commands."))))))

(defun whatsxmpp-message-handler (comp &key from to body id &allow-other-keys)
  "Handles a message sent to the whatsxmpp bridge."
  (with-component-data-lock (comp)
    (multiple-value-bind (to-hostname to-localpart)
        (parse-jid to)
      (unless (equal to-hostname (component-name comp))
        (warn "Got message addressed to ~A!" to)
        (return-from whatsxmpp-message-handler))
      (format *debug-io* "~&message from: ~A~%" from)
      (with-prepared-statement
          (get-user "SELECT id FROM users WHERE jid = ?")
        (let ((stripped (strip-resource from)))
          (bind-parameters get-user stripped)
          (let ((uid (when (sqlite:step-statement get-user)
                       (first (column-values get-user))))
                (conn (gethash stripped (component-whatsapps comp)))
                (wa-jid (whatsxmpp-localpart-to-wa-jid to-localpart)))
            (labels
                ((send-error (e)
                   (send-stanza-error comp
                                      :stanza-type "message"
                                      :id id :to from :from to
                                      :e e)))
              (cond
                ((equal to-localpart "admin")
                 (handle-admin-command comp from body uid))
                ((not uid)
                 (send-error (make-condition 'stanza-error
                                             :defined-condition "registration-required"
                                             :text "You must register to use this bridge."
                                             :type "auth")))
                ((not wa-jid)
                 (send-error (make-condition 'stanza-error
                                             :defined-condition "item-not-found"
                                             :text "That user's JID isn't in a recognizable format."
                                             :type "modify")))
                ((not conn)
                 (send-error (make-condition 'stanza-error
                                             :defined-condition "recipient-unavailable"
                                             :text "You're currently not connected to WhatsApp."
                                             :type "wait")))
                (t
                 (whatscl::send-simple-text-message conn wa-jid body))))))))))

(defun whatsxmpp-load-users (comp)
  (with-component-data-lock (comp)
    (with-prepared-statement
        (stmt "SELECT jid FROM users;")
        (loop
          while (sqlite:step-statement stmt)
          do (with-bound-columns (jid) stmt
               (setf (gethash jid (component-whatsapps comp)) nil))))))

(defun whatsxmpp-init ()
  "Initialise the whatsxmpp bridge."
  (connect-database)
  (with-prepared-statement
      (config "SELECT server, port, component_name, shared_secret, upload_component_name FROM configuration WHERE rev = 1")
    (assert (sqlite:step-statement config) () "No configuration in database!")
    (destructuring-bind (server port component-name shared-secret upload-name)
        (column-values config)
      (let* ((comp (make-component server port shared-secret component-name))
             (ret (change-class comp 'whatsxmpp-component
                                :upload-component-name upload-name)))
        (on :text-message ret (lambda (&rest args)
                                (apply #'whatsxmpp-message-handler ret args)))
        (register-whatsxmpp-handlers ret)
        (whatsxmpp-load-users ret)
        (setf (component-reconnect-timer ret) (trivial-timers:make-timer
                                               (lambda () (wa-resetup-users ret))
                                               :name "reconnection timer"))
        (on :connected ret (lambda () (wa-resetup-users ret)))
        ret))))
