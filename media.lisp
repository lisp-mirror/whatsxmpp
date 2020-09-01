;;;; Handling media uploading / downloading

(in-package :whatsxmpp)

(defun squonch-image-to-jpeg-thumbnail (opticl-image)
  "Resize the provided OPTICL-IMAGE to a small 640x480 thumbnail and return an octet vector of JPEG data for this thumbnail."
  (check-type opticl-image opticl:image)
  (let* ((image-out-stream (flexi-streams:make-in-memory-output-stream))
         (resized-image (opticl:resize-image opticl-image 480 640))
         (useless (opticl:write-jpeg-stream image-out-stream resized-image)) ; squonch
         (image-thumbnail (flexi-streams:get-output-stream-sequence image-out-stream)))
    (declare (ignore useless))
    (concatenate '(vector (unsigned-byte 8)) image-thumbnail)))

(defun maybe-upload-whatsapp-media (conn media-url)
  "If the media at MEDIA-URL can be sent natively as a WhatsApp upload, download it and re-host it on WhatsApp.
Returns a promise that resolves with either a STRING (if the media could not be rehosted or is ineligible) or WHATSCL:MESSAGE-CONTENTS-IMAGE (if the media pointed to is an image and it's been successfully re-uploaded)."
  (check-type media-url string)
  (let ((opticl-function (opticl::get-image-stream-reader (pathname-type media-url))))
    (if opticl-function
        (attach
         (download-remote-media media-url)
         (lambda (media-data)
           (let* ((image-stream (flexi-streams:make-in-memory-input-stream media-data))
                  (image-mime (or (trivial-mimes:mime-lookup media-url)
                                  (error "Couldn't guess image MIME type for ~A" media-url)))
                  (parsed-image (funcall opticl-function image-stream))
                  (squonched-image (squonch-image-to-jpeg-thumbnail parsed-image)))
             (opticl:with-image-bounds (image-y image-x) parsed-image
               (attach
                (put-whatsapp-media-file conn media-data :image image-mime)
                (lambda (file-info)
                  (make-instance 'whatscl::message-contents-image
                                 :file-info file-info
                                 :width-px image-x
                                 :height-px image-y
                                 :jpeg-thumbnail squonched-image)))))))
        (promisify media-url))))

(defun download-remote-media (media-url)
  "Returns a promise that downloads the remote MEDIA-URL and resolves with an octet vector of the downloaded data."
  ;; FIXME FIXME FIXME: this function is a trivial DoS vector, if you provide an infinite file like time.gif,
  ;; or a file that's like 1GB.
  (check-type media-url string)
  (with-promise-from-thread ()
    (format *debug-io* "~&downloading remote media: ~A~%" media-url)
    (multiple-value-bind (response status-code)
        (drakma:http-request media-url
                             :force-binary t)
      (unless (eql status-code 200)
        (format *error-output* "~&downloading failed! status ~A~%" status-code)
        (error "Remote media download failed with status code ~A~~%%" status-code))
      (check-type response (simple-array (unsigned-byte 8)))
      (format *debug-io* "~&downloaded ~A (length: ~A)~%" media-url (length response))
      response)))

(defun put-whatsapp-media-file (conn file-data media-type mime-type)
  "Encrypts and uploads FILE-DATA (an octet vector), a WhatsApp media file of type MEDIA-TYPE (one of :IMAGE, :VIDEO, :AUDIO, or :DOCUMENT) to WhatsApp, returning a promise that resolves with a WHATSCL:FILE-INFO when done."
  (check-type file-data (simple-array (unsigned-byte 8)))
  (check-type media-type (member :image :video :audio :document))
  (attach
   (with-promise (resolve reject)
     (format *debug-io* "~&requesting WhatsApp upload slot~%")
     (whatscl::start-media-upload
      conn
      (lambda (conn auth-token ttl hosts)
        (declare (ignore conn))
        (if auth-token
            (resolve auth-token ttl hosts)
            (reject (make-condition 'error
                                    "WhatsApp upload slot request rejected"))))))
   (lambda (auth-token ttl hosts)
     (declare (ignore ttl))
     (with-promise-from-thread ()
       (multiple-value-bind (encrypted-blob media-key file-sha256 file-enc-sha256)
           (whatscl::encrypt-media-data file-data media-type)
         (let* ((token (qbase64:encode-bytes file-enc-sha256 :scheme :uri))
                (url-to-use (format nil "https://~A/mms/~(~A~)/~A"
                                    (first hosts) (symbol-name media-type) token))
                (headers `(("Origin" . "https://web.whatsapp.com")
                           ("Referer" . "https://web.whatsapp.com")))
                (qs-params `(("auth" . ,auth-token) ("token" . ,token))))
           (format *debug-io* "~&uploading encrypted media file (length ~A) to ~A"
                   (length encrypted-blob) url-to-use)
           (multiple-value-bind (response status-code)
               (drakma:http-request url-to-use
                                    :method :post
                                    :content encrypted-blob
                                    :content-type "application/octet-stream"
                                    :parameters qs-params
                                    :additional-headers headers)
             (let ((response (babel:octets-to-string response)))
               (unless (eql status-code 200)
                 (format *debug-io* "~&whatsapp upload failed! status ~A / ~A" status-code response)
                 (error "Downloading media failed with status ~A / ~A" status-code response))
               (let* ((json-response (cl-json:decode-json-from-string response))
                      (url (or (whatscl::cassoc :url json-response)
                               (error "No :URL field in upload response ~A" json-response))))
                 (format *debug-io* "~&got whatsapp uploaded media url ~A~%" url)
                 (make-instance 'whatscl::file-info
                                :media-key media-key
                                :url (pb:string-field url)
                                :sha256 file-sha256
                                :enc-sha256 file-enc-sha256
                                :length-bytes (length encrypted-blob)
                                :mime-type (pb:string-field mime-type)))))))))))

(defun upload-whatsapp-media-file (comp file-info media-type &optional filename)
  "Downloads the WhatsApp media file specified by FILE-INFO, uploads it via COMP, and returns a promise which resolves to the URL of the uploaded media.
MEDIA-TYPE is one of (:image :video :audio :document)."
  (declare (type (member :image :video :audio :document) media-type))
  (with-component-data-lock (comp)
    (with-accessors ((url whatscl::file-info-url)
                     (mime-type whatscl::file-info-mime-type)
                     (sha256 whatscl::file-info-sha256)
                     (enc-sha256 whatscl::file-info-enc-sha256)
                     (length-bytes whatscl::file-info-length-bytes)
                     (media-key whatscl::file-info-media-key))
        file-info
      (let* ((mime-type (first (uiop:split-string mime-type :separator ";")))
             (extension (or (mimes:mime-file-type mime-type) "what"))
             (filename (or filename
                           (concatenate 'string (octets-to-lowercase-hex sha256) "." extension))))
        (format *debug-io* "~&requesting an upload slot for whatsapp media (type ~A, length ~A): ~A~%" mime-type length-bytes filename)
        (attach
         (request-http-upload-slot comp (component-upload-component-name comp)
                                   filename length-bytes mime-type)
         (lambda (slot)
           (destructuring-bind ((put-url . headers) get-url) slot
             (format *debug-io* "~&got put-url: ~A~%    get-url: ~A~%" put-url get-url)
             (with-promise-from-thread ()
               (format *debug-io* "~&fetching whatsapp media url: ~A~%" url)
               (multiple-value-bind (file-data status-code)
                   (drakma:http-request url)
                 (unless (eql status-code 200)
                   (format *debug-io* "~&couldn't fetch whatsapp media! status ~A, body ~A~%" status-code file-data)
                   (error "Downloading media failed with status ~A" status-code))
                 (format *debug-io* "~&got ~A bytes, decrypting~%" (length file-data))
                 (let ((sha256-expected (ironclad:digest-sequence :sha256 file-data))
                       (decrypted-file (whatscl::decrypt-media-data media-key file-data media-type)))
                   (unless (equalp enc-sha256 sha256-expected)
                     (error "Encrypted SHA256 mismatch"))
                   (multiple-value-bind (body status-code)
                       (drakma:http-request put-url
                                            :additional-headers headers
                                            :content-length (length decrypted-file)
                                            :content-type mime-type
                                            :method :put
                                            :content decrypted-file)
                     (unless (and (>= status-code 200) (< status-code 300))
                       (format *debug-io* "~&upload failed! status ~A, body ~A~%" status-code body)
                       (error "HTTP upload failed with status ~A" status-code))
                     get-url)))))))))))

(defun send-qrcode (comp jid text)
  "Send a QR code containing TEXT to JID."
  (with-component-data-lock (comp)
    (uiop:with-temporary-file (:stream stream
                               :pathname path
                               :keep t) ; Needed because async
      (format *debug-io* "~&using path ~A~%" path)
      (cl-qrencode:encode-png-stream text stream)
      (force-output stream) ; otherwise the QR codes get chopped off?
      (catcher
       (let ((content-length (file-length stream)))
         (attach
          (request-http-upload-slot comp (component-upload-component-name comp)
                                    "qrcode.png"
                                    (file-length stream)
                                    "image/png")
          (lambda (slot)
            (with-promise-from-thread ()
              (destructuring-bind ((put-url . headers) get-url) slot
                (format *debug-io* "~&got put-url: ~A~%   get-url: ~A~%" put-url get-url)
                (multiple-value-bind (body status-code)
                    (drakma:http-request put-url
                                         :additional-headers headers
                                         :content-type "image/png"
                                         :content-length content-length
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
                      (admin-msg comp jid "(Code expired? Be faster next time. Get a new one with `connect`.)")))))))))
       (t (e)
          (admin-msg comp jid (format nil "Failed to upload QR code!~%Report the following error to the bridge admin: `~A`" e)))))))
