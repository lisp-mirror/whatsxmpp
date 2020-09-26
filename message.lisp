;; Message processing

(in-package :whatsxmpp)

(defclass xmpp-message ()
  ((conversation
    :initarg :conversation
    :reader conversation
    :documentation "The localpart of the conversation this message is in (either a user or a group)")
   (uid
    :initarg :uid
    :reader uid
    :documentation "The user ID this message is associated with.")
   (from
    :initarg :from
    :reader from
    :documentation "The sender of the message. In a 1-to-1, this is the same as CONVERSATION if the other party sent it (and not if not); in a group, this is the group nickname / resource of the sender.")
   (timestamp
    :initarg :timestamp
    :reader timestamp
    :documentation "A LOCAL-TIME timestamp of when the message was sent.")
   (xmpp-id
    :initarg :xmpp-id
    :reader xmpp-id
    :documentation "The XMPP-side ID of the message (given in the 'id' header, and as the MUC <stanza-id> element)")
   (orig-id
    :initarg :orig-id
    :initform nil
    :reader orig-id
    :documentation "The WhatsApp-side ID of the message, if any.")
   (body
    :initarg :body
    :reader body
    :documentation "The message text.")
   (oob-url
    :initarg :oob-url
    :initform nil
    :reader oob-url
    :documentation "The URL of uploaded media contained in this message, if any.")))

(defun wa-message-key-to-conversation-and-from (comp jid key &optional conn)
  "Takes KEY, a WHATSCL::MESSAGE-KEY for a message for bridge user JID, and returns (VALUES CONVERSATION FROM).
If a CONN is provided, it's used to create a new chat if that's required; otherwise, an error is signaled.
FIXME: the above behaviour is a bit meh."
  (let* ((wx-localpart (wa-jid-to-whatsxmpp-localpart (whatscl::key-jid key)))
         (uid (get-user-id jid)))
    (typecase key
      (whatscl::message-key-receiving
       ;; Received in a 1-to-1: conversation same as from
       (values wx-localpart wx-localpart))
      (whatscl::message-key-sending
       (if (uiop:string-prefix-p "g" wx-localpart)
           (alexandria:if-let ((user-resource (get-user-chat-resource uid wx-localpart)))
             (values wx-localpart user-resource)
             ;; If we don't have a user chat resource, just use their localpart.
             ;; This shouldn't really happen that frequently.
             (progn
               (values wx-localpart (first (split-sequence:split-sequence #\@ jid)))
               (warn "Using fallback localpart for sent message in group ~A; that's rather interesting." wx-localpart)))
           ;; Put the user's jid as "from". This is okay, since we pretty much only
           ;; want to determine "was it us or them" in a 1-to-1 conversation, which
           ;; is done by comparing from to conversation.
           (values wx-localpart jid)))
      (whatscl::message-key-group-receiving
       (let* ((chat-id (or
                        (get-user-chat-id uid wx-localpart)
                        (when conn
                          (add-wa-chat comp conn jid (whatscl::key-jid key))
                          (get-user-chat-id uid wx-localpart))))
              (participant-localpart (wa-jid-to-whatsxmpp-localpart (whatscl::key-participant key))))
         (if chat-id
             (let ((from-resource (or
                                   (get-participant-resource chat-id participant-localpart)
                                   ;; whee fallback go brrr
                                   participant-localpart)))
               (values wx-localpart from-resource))
             (error "Couldn't find or create group chat for ~A" chat-id)))))))

(defmacro with-new-xmpp-message-context ((comp jid msg &optional conn) &body body)
  "Evaluate FORMS, binding NEW-XMPP-MESSAGE (lambda-list (BODY &KEY OOB-URL SYSTEM-GENERATED)) to a function that returns an instance of the XMPP-MESSAGE class, using information contained in the message MSG received for the bridge user JID."
  (alexandria:with-gensyms (key wa-id wa-ts uid ts conversation from xmpp-id orig-id)
    `(let* ((,key (whatscl::message-key ,msg))
            (,wa-id (whatscl::message-id ,msg))
            (,wa-ts (whatscl::message-ts ,msg))
            (,uid (get-user-id ,jid))
            (local-time:*default-timezone* local-time:+utc-zone+)
            (,ts (local-time:unix-to-timestamp ,wa-ts)))
       (multiple-value-bind (,conversation ,from)
           (wa-message-key-to-conversation-and-from ,comp ,jid ,key ,conn)
         (labels ((new-xmpp-message (body &key oob-url system-generated)
                    (let ((,xmpp-id (if system-generated
                                        (princ-to-string (uuid:make-v4-uuid))
                                        (concatenate 'string "wa-" ,wa-id "-" (princ-to-string ,wa-ts))))
                          (,orig-id (unless system-generated ,wa-id)))
                      (make-instance 'xmpp-message
                                     :conversation ,conversation
                                     :from ,from
                                     :uid ,uid
                                     :timestamp ,ts
                                     :oob-url oob-url
                                     :xmpp-id ,xmpp-id
                                     :orig-id ,orig-id
                                     :body body
                                     :oob-url oob-url))))
           ,@body)))))

(defun quote-content (content)
  "Prepends '> ' to each line of CONTENT."
  (let ((oss (make-string-output-stream)))
    (loop
      for item in (split-sequence:split-sequence #\Linefeed content)
      do (format oss "> ~A~%" item))
    (get-output-stream-string oss)))


(defun deliver-xmpp-message (comp msg)
  "Deliver MSG, an XMPP-MESSAGE, to the intended destinations on COMP."
  (let* ((jid (get-user-jid (uid msg)))
         (one-to-one-p (uiop:string-prefix-p "u" (conversation msg)))
         (component-host (component-name comp))
         (destinations (if one-to-one-p
                           ;; We can't send a message the user sent in a 1:1.
                           (when (string= (conversation msg) (from msg))
                             (list jid))
                           (get-user-chat-joined (uid msg) (conversation msg))))
         (from (if one-to-one-p
                   (concatenate 'string (from msg) "@" component-host "/whatsapp")
                   (concatenate 'string (conversation msg) "@" component-host "/" (from msg)))))
    (loop
      for to in destinations
      do (with-message (comp to
                        :from from
                        :id (xmpp-id msg)
                        :type (if one-to-one-p "chat" "groupchat"))
           (cxml:with-element "body"
             (cxml:text (body msg)))
           (when (oob-url msg)
             (cxml:with-element "x"
               (cxml:attribute "xmlns" +oob-ns+)
               (cxml:with-element "url"
                 (cxml:text (oob-url msg)))))
           (cxml:with-element "delay"
             (cxml:attribute "xmlns" +delivery-delay-ns+)
             (cxml:attribute "stamp" (local-time:format-timestring nil (timestamp msg))))
           (cxml:with-element "active"
             (cxml:attribute "xmlns" +chat-states-ns+))
           (unless one-to-one-p
             (cxml:with-element "stanza-id"
               (cxml:attribute "xmlns" +unique-stanzas-ns+)
               (cxml:attribute "id" (xmpp-id msg))
               (cxml:attribute "by" (concatenate 'string (conversation msg) "@" component-host)))
             (when (orig-id msg)
               (cxml:with-element "origin-id"
                 (cxml:attribute "xmlns" +unique-stanzas-ns+)
                 (cxml:attribute "id" (orig-id msg)))))
           (when (orig-id msg)
             ;; Messages without a WhatsApp ID aren't markable for hopefully
             ;; obvious reasons.
             (cxml:with-element "markable"
               (cxml:attribute "xmlns" +chat-markers-ns+)))))))

(defun make-xmpp-messages-for-wa-message (comp conn jid msg)
  "Returns a promise that is resolved with a list of XMPP-MESSAGE objects generated from the WhatsApp message object MSG.
If something like file uploading fails, the promise can also be rejected."
  (promisify
   (with-new-xmpp-message-context (comp jid msg conn)
     (let ((contents (whatscl::message-contents msg))
           (qc (alexandria:when-let
                   ((summary (whatscl::message-quoted-contents-summary msg)))
                 (quote-content summary))))
       (typecase contents
         (whatscl::message-contents-text
          (let* ((contents-text (whatscl::contents-text contents))
                 (text (format nil "~@[~A~]~A" qc contents-text)))
            (list (new-xmpp-message text))))
         (whatscl::message-contents-file
          (let* ((file-info (whatscl::contents-file-info contents))
                 (media-type (whatscl::get-contents-media-type contents))
                 (filename (when (typep contents 'whatscl::message-contents-document)
                             (whatscl::contents-filename contents)))
                 (caption (whatscl::contents-caption contents))
                 (upload-promise (upload-whatsapp-media-file comp file-info media-type filename)))
            (attach upload-promise
                    (lambda (get-url)
                      (append
                       (when (or caption qc)
                         (let ((text (format nil "~@[~A~]~@[~A~]" qc caption)))
                           (list (new-xmpp-message text
                                                   :system-generated t))))
                       (list (new-xmpp-message get-url
                                               :oob-url get-url)))))))
         ;; FIXME: handle location messages, stub messages, etc.
         (t nil))))))
