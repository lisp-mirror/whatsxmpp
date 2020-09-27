;;;; XEP-0313: Message Archive Management

(in-package :whatsxmpp)

(defun whitelisted-mam-keywordize (thing)
  "Interns THING, but only after making sure it's a string from XEP-0313."
  (if (member thing '("start" "end" "with" "first" "last" "count" "max" "FORM_TYPE" "after" "before")
              :test #'string=)
      (intern (string-upcase thing) :keyword)
      thing))

(defun alist-from-mam-query (query-elt)
  "Parses the QUERY-ELT, a MAM <query> element, and returns an alist."
  (labels ((consify-df (field-elt)
             (cons (whitelisted-mam-keywordize
                    (dom:get-attribute field-elt "var"))
                   (nil-empty
                    (get-node-text
                     (get-node-named (child-elements field-elt) "value")))))
           (consify-rsm (rsm-elt)
             (cons (whitelisted-mam-keywordize
                    (dom:node-name rsm-elt))
                   (nil-empty (get-node-text rsm-elt)))))
    (let* ((x-elt (get-node-with-xmlns (child-elements query-elt) +data-forms-ns+))
           (rsm-elt (get-node-with-xmlns (child-elements query-elt) +rsm-ns+))
           (query-id (dom:get-attribute query-elt "queryid"))
           (form-fields (map 'list #'consify-df (child-elements x-elt)))
           (rsm-fields (when rsm-elt
                         (map 'list #'consify-rsm (child-elements rsm-elt)))))
      (append form-fields rsm-fields (when query-id
                                       `((:query-id . ,query-id)))))))
