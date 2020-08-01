;;;; XEP-0115: Entity Capabilities

(in-package :whatsxmpp)

(defun format-disco-identity (name type category &optional (lang ""))
  "Formats a disco#info identity into a verification string part."
  (format nil "~A/~A/~A/~A" category type lang name))

(defun generate-entity-caps (disco-info-list)
  "Using DISCO-INFO-LIST, a quoted list of calls to DISCO-IDENTITY and DISCO-FEATURE, generate and return an XEP-0115 verification string.
WARNING: You must pre-sort DISCO-INFO-LIST according to the rules in XEP-0115 § 5.1."
  (let (identities features)
    (loop
      for call in disco-info-list
      do (ecase (car call)
           (disco-identity (push (cdr call) identities))
           (disco-feature (push (cdr call) features))))
    (qbase64:encode-bytes
     (ironclad:digest-sequence :sha1
      (babel:string-to-octets
       (format nil "~{~A<~}~{~A<~}"
              (mapcar (lambda (call)
                        ;; Because DISCO-IDENTITY and FORMAT-DISCO-IDENTITY
                        ;; intentionally take the same lambda lists,
                        ;; we can just do this.
                        (apply #'format-disco-identity call))
                      ;; NREVERSE because pushing things does it the wrong
                      ;; way round (FIXME, inefficient)
                      (nreverse identities))
              ;; DISCO-FEATURE takes one argument (the feature name)
              (mapcar #'car (nreverse features))))))))
