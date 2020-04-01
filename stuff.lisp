(defpackage :whatsxmpp
  (:use :cl :usocket :event-emitter))
(in-package :whatsxmpp)

(defvar *last-stanza*)
(defparameter +streams-ns+ "urn:ietf:params:xml:ns:xmpp-streams")
(defparameter +component-ns+ "jabber:component:accept")

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
    :reader component-shared-secret)))

(defmacro with-component-data-lock (comp &body body)
  `(bt:with-recursive-lock-held ((component-data-lock ,comp))
     ,@body))

(defmethod sax:start-document ((s start-ignoring-sink))
  (declare (ignore s)))

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
          (with-component-data-lock comp
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

(defmacro with-component-xml-output (comp &rest body)
  `(with-accessors ((lock component-socket-lock) (socket component-socket) (sink component-sink))
       ,comp
     (bt:with-recursive-lock-held (lock)
       (cxml:with-xml-output sink
         ,@body)
       (force-output socket))))

(defun write-stream-header (comp)
  (with-component-data-lock comp
    (with-component-xml-output comp
      (cxml:with-namespace ("stream" "http://etherx.jabber.org/streams")
        (cxml:with-element "stream:stream"
          (cxml:attribute "xmlns" +component-ns+)
          (cxml:attribute "to" (component-name comp)))))))

(defun component-stream-started (comp)
  (with-component-data-lock comp
    (with-component-xml-output comp
      (cxml:with-element "handshake"
        (cxml:attribute "xmlns" +component-ns+)
        (cxml:text (string-downcase (sha1:sha1-hex (concatenate 'string (component-stream-id comp) (component-shared-secret comp)))))))))

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

(defun handle-connection-complete (comp)
  (declare (ignore comp))
  (format *debug-io* "Connection complete! \o/"))

(defun component-stanza (comp stanza)
  (setf *last-stanza* stanza)
  (let* ((stanza (dom:document-element stanza))
         (tag-name (dom:tag-name stanza)))
    (cond
      ((equal tag-name "stream:error") (handle-stream-error comp stanza))
      ((equal tag-name "handshake") (handle-connection-complete comp))
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
