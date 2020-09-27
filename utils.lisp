(in-package :whatsxmpp)

(defun octets-to-lowercase-hex (buf)
  "Formats BUF, a vector of octets, as a lowercase hex string and returns it."
  (declare (type (vector (unsigned-byte 8)) buf))
  (format nil "~(~{~2,'0X~}~)" (coerce buf 'list)))

(defun sha1-octets (buf)
  "Returns the SHA1 of BUF, a vector of octets, in lowercase hex."
  (octets-to-lowercase-hex (ironclad:digest-sequence :sha1 buf)))

(defun sha1-hex (str)
  "Returns the SHA1 of STR, a string, in lowercase hex."
  (sha1-octets (babel:string-to-octets str)))

(defun child-elements (node)
  "Returns the child elements (excluding text nodes) of the CXML DOM node NODE."
  (remove-if-not #'dom:element-p (dom:child-nodes node)))

(defun nil-empty (seq)
  "If SEQ (a sequence) is empty, returns NIL; otherwise, returns SEQ."
  (unless (eql (length seq) 0) seq))

(defmacro with-promise-from-thread (() &body forms)
  "Return a promise that executes FORMS in a new thread, resolving the promise with the return value of (PROGN ,@FORMS) or rejecting it if an ERROR condition is thrown (with said condition)."
  (let ((resolve (gensym))
        (reject (gensym)))
    `(with-promise (,resolve ,reject)
       (bt:make-thread
        (lambda ()
          (handler-case
              (,resolve (progn ,@forms))
            (error (e) (,reject e))))))))
