(in-package :whatsxmpp)

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
       (let* ((children (child-elements slot-node))
              (put-node (get-node-named children "put"))
              (get-node (get-node-named children "get"))
              (headers '()))
         (unless (and put-node get-node)
           (error "Malformed XEP-0363 response: PUT or GET nodes missing"))
         (loop
           for node across (child-elements put-node)
           do (let ((name (dom:tag-name node)))
                (when (equal name "header")
                  (setf headers (cons
                                 (cons (dom:get-attribute node "name")
                                       (dom:node-value (elt (child-elements node) 0)))
                                 headers)))))
         `((,(dom:get-attribute put-node "url") . ,headers) ,(dom:get-attribute get-node "url")))))))
