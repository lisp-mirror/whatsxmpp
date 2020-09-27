(in-package :whatsxmpp)

(defvar *xmpp-debug-io* (make-broadcast-stream))
(defvar *xmpp-debug-out* (make-synonym-stream '*xmpp-debug-io*))

(defclass xmpp-component (event-emitter)
  ((socket
    :initarg :socket
    :accessor component-socket)
   (socket-lock
    :initform (bt:make-recursive-lock "component socket lock")
    :accessor component-socket-lock)
   (data-lock
    :initform (bt:make-recursive-lock "component data lock")
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
  (format *xmpp-debug-out* "~&XMPP --> [document started]~%"))

(defmethod sax:start-element ((s xmpp-source) namespace-uri local-name qname attributes)
  (with-accessors ((depth source-depth) (comp source-component) (handlers cxml:broadcast-handler-handlers)) s
    (incf depth)
    (when (and (eql depth 1) (equal qname "stream:stream"))
      (flet ((local-name-is-id (attr)
               (equal (sax:attribute-local-name attr) "id")))
        (let ((stream-id-attr (find-if #'local-name-is-id attributes)))
          (when (not stream-id-attr)
            (error "Server didn't send a stream ID"))
          (format *xmpp-debug-out* "~&XMPP --> [stream started, ID ~A]~%" (sax:attribute-value stream-id-attr))
          (with-component-data-lock (comp)
            (setf (component-stream-id comp) (sax:attribute-value stream-id-attr))
            (emit :stream-started comp))
          (return-from sax:start-element))))
    (when (eql depth 2)
      (let ((dom-builder (cxml-dom:make-dom-builder)))
        (format *xmpp-debug-out* "~&XMPP --> ")
        (setf handlers (list (cxml:make-character-stream-sink *xmpp-debug-out*) dom-builder))
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
        (terpri *xmpp-debug-out*)
        (setf handlers nil)
        (emit :raw-stanza comp stanza)))))

(defclass xmpp-sink (cxml:broadcast-handler)
  ((sink-open
    :initform t
    :accessor sink-open)))

(defmethod sax:start-document ((s xmpp-sink))
  (declare (ignore s))
  (format *xmpp-debug-out* "~&XMPP <-- "))

(defmethod sax:end-element ((s xmpp-sink) namespace-uri local-name qname)
  (if (and (sink-open s) (equal local-name "stream"))
      ;; The <stream:stream> element gets opened at the start of the connection
      ;; and closing it represents ending the connection. We therefore don't
      ;; want to close it...
      ;; Instead, send some empty characters to get the sinks to write the last ">"
      ;; bit of the opening tag.
      (sax:characters s "")
      (call-next-method s namespace-uri local-name qname))
  (terpri *xmpp-debug-out*))

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
              (cxml:make-character-stream-sink *xmpp-debug-out*)
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
  ;;
  ;; ### Part II: The Fucking Stream Strikes Back ###
  ;; ...and, after another hour of debugging, I found out you have to specify the `name'
  ;; arg, otherwise it breaks -- but ONLY randomly and once you decide to deploy it
  ;; in production, of course.
  (let ((source (make-xmpp-source comp))
        (fucking-stream (cxml:make-xstream (component-socket comp)
                                           :speed 1 ; FFFFFFFFUUUUUUUU
                                           :name (cxml::make-stream-name ; AAAARGH
                                                  :entity-name "main document"
                                                  :entity-kind :main
                                                  :uri nil)
                                           :name "XMPP server stream"
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
      (cxml:text (string-downcase (sha1-hex (concatenate 'string (component-stream-id comp) (component-shared-secret comp))))))))

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
                                                :text (format nil "~A" e)
                                                :type "cancel"))
          (warn "IQ handler for ~A failed: ~A" handler e))))))

(defun handle-iq-get (comp id from stanza)
  "Handles an IQ-get STANZA for component COMP."
  (let* ((children (child-elements stanza))
         (first-child (if (> (length children) 0)
                          (elt children 0)
                          (return-from handle-iq-get)))
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
             ((and (equal xmlns +mam-ns+) (equal tag-name "query"))
              :mam-query)
             ((and (equal xmlns +ping-ns+) (equal tag-name "ping"))
              :ping)
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
      (if (or (equal type "get") (equal type "set"))
          (handle-iq-get comp id from stanza)
          (symbol-macrolet
              ((promise (gethash id (component-promises comp))))
            (if promise
                (progn
                  (format t "~&IQ ~A from ~A for ~A~%" type from id)
                  (cond
                    ((equal type "result") (finish promise (child-elements stanza)))
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
    (emit event-name comp :from from :to to :type type :stanza stanza)))

(defun handle-message (comp stanza)
  "Handles a message STANZA for component COMP."
  (let* ((from (dom:get-attribute stanza "from"))
         (to (dom:get-attribute stanza "to"))
         (id (dom:get-attribute stanza "id"))
         (children (child-elements stanza))
         (body (get-node-named children "body"))
         (marker (get-node-with-xmlns children +chat-markers-ns+))
         (oob-element (get-node-with-xmlns children +oob-ns+))
         (oob-url-element (when oob-element
                            (get-node-named (child-elements oob-element) "url")))
         (chat-state (get-node-with-xmlns children +chat-states-ns+)))
    (cond
      (body
       (let* ((text (get-node-text body))
              (oob-url (when oob-url-element
                         (get-node-text oob-url-element))))
         (emit :text-message comp :from from :to to :body text :id id :stanza stanza
               :oob-url oob-url)))
      (marker
       (let ((marker-type (dom:tag-name marker))
             (msgid (dom:get-attribute marker "id")))
         (emit :message-marker comp :from from :to to :type marker-type :marker-id msgid :id id :stanza stanza)))
      (chat-state
       (let ((state-type (dom:tag-name chat-state)))
         (emit :chat-state comp :from from :to to :type state-type :id id :stanza stanza)))
      (t
       (emit :message comp :from from :to to :id id :stanza stanza)))))

(defun component-stanza (comp stanza)
  "Handles a STANZA received by component COMP."
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
