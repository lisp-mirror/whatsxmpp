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
