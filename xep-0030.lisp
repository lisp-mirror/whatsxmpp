(in-package :whatsxmpp)


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

(defmacro disco-identity (name type category)
  `(cxml:with-element "identity"
     ,@(when name
         `((cxml:attribute "name" ,name)))
     (cxml:attribute "type" ,type)
     (cxml:attribute "category" ,category)))

(defmacro disco-feature (feature)
  `(cxml:with-element "feature"
     (cxml:attribute "var" ,feature)))
