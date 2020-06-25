(in-package :whatsxmpp)

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

(defmacro with-presence ((comp to &key type from id) &body body)
  "Send a presence stanza (of type TYPE) on the COMP component. Semantics the same as WITH-IQ, except for the fact that presence stanzas don't normally prompt a response."
  `(with-stanza (,comp "presence"
                 :type ,type
                 :to ,to
                 :from ,from
                 :id ,id)
     ,@body))

(defun get-node-named (nodes name)
  "Finds the node with tag name NAME in NODES, returning NIL if none was found."
  (flet ((is-the-node (node) (equal (dom:tag-name node) name)))
    (find-if #'is-the-node nodes)))

(defun get-node-with-xmlns (nodes xmlns)
  "Finds the node with XML namespace XMLNS in NODES, returning NIL if none was found."
  (flet ((is-the-node (node) (equal (dom:get-attribute node "xmlns") xmlns)))
    (find-if #'is-the-node nodes)))

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
