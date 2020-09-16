(in-package :whatsxmpp)

(defparameter +version+ "0.0.1")

(defclass whatsxmpp-component (xmpp-component)
  ((whatsapps
    :initform (make-hash-table :test 'equal)
    :accessor component-whatsapps)
   (reconnect-timer
    :initform nil
    :accessor component-reconnect-timer)
   (upload-component-name
    :initarg :upload-component-name
    :accessor component-upload-component-name)))

(defun send-text-message (comp to-jid text &optional from)
  "Send a simple text message to TO-JID, containing TEXT."
  (with-message (comp to-jid :from from)
    (cxml:with-element "body"
      (cxml:text text))))

(defun handle-connection-complete (comp)
  (format *debug-io* "Connection complete! \\o/")
  (emit :connected comp))

(defparameter +whatsapp-user-disco-info-list+
  `((disco-identity "whatsxmpp" "phone" "client")
    ;; FIXME: The features here must be lexicographically sorted!
    (disco-feature ,+entity-caps-ns+)
    (disco-feature ,+chat-states-ns+)
    (disco-feature ,+disco-info-ns+))
  "List of calls to DISCO-IDENTITY and DISCO-FEATURE for WhatsApp users bridged through to XMPP.")
(defparameter +whatsapp-user-entity-caps+
  (generate-entity-caps +whatsapp-user-disco-info-list+)
  "Entity caps string for a bridged WhatsApp user.")

(defun disco-info-handler (comp &key to from &allow-other-keys)
  "Handles XEP-0030 disco#info requests."
  (format *debug-io* "~&disco#info: ~A~%" to)
  (with-component-data-lock (comp)
    `((cxml:with-element "query"
        (cxml:attribute "xmlns" ,+disco-info-ns+)
        ,@(multiple-value-bind
                (to-hostname to-localpart to-resource)
              (parse-jid to)
            (declare (ignore to-hostname to-resource))
            (let* ((uid (get-user-id from))
                   (user-name (get-contact-name uid to-localpart))
                   (chat-subject (get-user-chat-subject uid to-localpart)))
            (cond
              ((equal to (component-name comp))
               `((disco-identity "whatsxmpp bridge" "xmpp" "gateway")
                 (disco-feature ,+disco-info-ns+)
                 (disco-feature ,+disco-items-ns+)
                 (disco-feature ,+muc-ns+)))
              (user-name
               +whatsapp-user-disco-info-list+)
              (chat-subject
               `((disco-identity ,chat-subject "text" "conference")
                 (disco-feature ,+disco-info-ns+)
                 (disco-feature ,+muc-ns+)
                 (disco-feature ,+muc-stable-id-ns+)
                 (disco-feature ,+unique-stanzas-ns+)
                 (disco-feature "muc_hidden")
                 (disco-feature "muc_persistent")
                 (disco-feature "muc_membersonly")
                 (disco-feature "muc_nonanonymous")))
              (t nil))))))))

(defun disco-items-handler (comp &key to from &allow-other-keys)
  "Handles XEP-0030 disco#items requests."
  (format *debug-io* "~&disco#items: ~A from ~A~%" to from)
  (with-component-data-lock (comp)
    `((cxml:with-element "query"
        (cxml:attribute "xmlns" ,+disco-items-ns+)
        ,@(when (equal to (component-name comp))
            (let ((uid (get-user-id (strip-resource from))))
              (format *debug-io* "~&muc list disco#items for ~A~%" (strip-resource from))
              (loop
                for (localpart . subject) in (get-user-groupchats uid)
                collect `(cxml:with-element "item"
                           (cxml:attribute "jid" ,(concatenate 'string
                                                               localpart "@"
                                                               (component-name comp)))
                           (cxml:attribute "name" ,subject)))))))))

(defun admin-jid (comp)
  "Get the admin JID for COMP. You need the lock to be taken out for this one."
  (concatenate 'string "admin@" (component-name comp) "/adminbot"))

(defparameter *admin-help-text*
  (format nil
  "** whatsxmpp, version ~A, a theta.eu.org project **
Commands:
- register: set up the bridge
- connect: manually connect to WhatsApp
- stop: disconnect from WhatsApp, and disable automatic reconnections
- status: get your current status
- getroster: trigger an XEP-0144 roster item exchange (in some clients, this'll pop up a window asking to add contacts to your roster)
- help: view this help text
- refresh-chats: force the bridge to update member lists + subject for all of your group chats"
  +version+))

(defparameter *reconnect-every-secs* 5
  "Interval between calls to WA-RESETUP-USERS.")

(defun admin-msg (comp jid text)
  "Send an admin message from the admin on COMP to JID."
  (send-text-message comp jid text (admin-jid comp)))

(defun admin-presence (comp jid status &optional show)
  "Send presence from the admin on COMP to JID."
  (with-presence (comp jid
                  :from (admin-jid comp))
    (when show
      (cxml:with-element "show"
        (cxml:text show)))
    (cxml:with-element "status"
      (cxml:text status))))

(defun wa-resetup-users (comp)
  "Go through the list of WhatsApp users and reconnect those whose connections have dropped."
  (with-component-data-lock (comp)
    (let* ((users-to-reconnect
             (loop
               for jid being the hash-keys in (component-whatsapps comp)
                 using (hash-value conn)
               append (unless conn
                        (list jid))))
           (num-users (length users-to-reconnect)))
      (when (> num-users 0)
        (format *debug-io* "~&resetup-users: ~A users to reconnect~%" num-users))
      (loop
        for user in users-to-reconnect
        do (handle-setup-user comp user))
      (trivial-timers:schedule-timer (component-reconnect-timer comp) *reconnect-every-secs*))))

(defparameter *user-jid-scanner*
  (cl-ppcre:create-scanner "u([0-9]+)"))

(defparameter *group-jid-scanner*
  (cl-ppcre:create-scanner "g([0-9]+)-([0-9]+)"))

(defun wa-jid-to-whatsxmpp-localpart (waj)
  "Convert a whatscl JID object to a WhatsXMPP localpart."
  (unless waj
    (format *error-output* "WA-JID-TO-WHATSXMPP-LOCALPART called with NIL!")
    (return-from wa-jid-to-whatsxmpp-localpart "unknown"))
  (with-accessors ((localpart whatscl::jid-localpart) (hostname whatscl::jid-hostname)) waj
    (cond
      ((or (equal hostname "s.whatsapp.net") (equal hostname "c.us"))
       (concatenate 'string "u" localpart))
      ((equal hostname "g.us")
       (concatenate 'string "g" localpart))
      (t
       (concatenate 'string "other-" localpart "-" hostname)))))

(defun whatsxmpp-localpart-to-wa-jid (localpart)
  "Parses a WhatsXMPP localpart, returning a whatscl JID object if parsing is successful.
WhatsXMPP represents users as u440123456789 and groups as g1234-5678."
  (cl-ppcre:register-groups-bind (digits)
      (*user-jid-scanner* localpart)
    (return-from whatsxmpp-localpart-to-wa-jid
      (whatscl::make-jid digits "s.whatsapp.net")))
  (cl-ppcre:register-groups-bind (creator ts)
      (*group-jid-scanner* localpart)
    (return-from whatsxmpp-localpart-to-wa-jid
      (whatscl::make-jid (concatenate 'string creator "-" ts) "g.us"))))

(defun wa-conn-recent-p (comp conn jid)
  (let ((current (gethash jid (component-whatsapps comp))))
    (eql current conn)))

(defmacro with-wa-handler-context ((comp conn jid) &body body)
  "Takes the component data lock, checks that CONN is the most up-to-date connection for JID, and then executes BODY."
  `(with-component-data-lock (,comp)
     (if (wa-conn-recent-p ,comp ,conn ,jid)
         (progn ,@body)
         (warn "WA handler called with out of date connection, ignoring"))))

(defun wa-handle-ws-error (comp conn jid err)
  (with-wa-handler-context (comp conn jid)
    (format *debug-io* "~&ws-error ~A: ~A~%" jid err)
    (admin-msg comp jid
               (format nil "WhatsApp websocket error: ~A~%Will automatically reconnect, but if issues persist, try a re-connect or re-register." err))
    (admin-presence comp jid "WebSocket error" "away")
    (setf (gethash jid (component-whatsapps comp)) nil)))

(defun wa-handle-ws-close (comp conn jid)
  (with-wa-handler-context (comp conn jid)
    (format *debug-io* "~&ws-close: ~A~%" jid)
    (when (nth-value 1 (gethash jid (component-whatsapps comp)))
      ;; If true, we're still doing automatic reconnections.
      ;; Otherwise, we will have already yelled at the user for
      ;; whatever caused them to disconnect, so don't do anything here.
      (admin-msg comp jid
                 "WhatsApp websocket closed (will reconnect soon).")
      (admin-presence comp jid "WebSocket closed" "away")
      (setf (gethash jid (component-whatsapps comp)) nil))))

(defun wa-handle-ws-qrcode (comp conn jid qrcode)
  (with-wa-handler-context (comp conn jid)
    (format *debug-io* "~&qrcode: ~A~%" jid)
    (admin-presence comp jid "Waiting for QR code" "away")
    (send-qrcode comp jid qrcode)))

(defun update-session-data (jid sessdata)
  (with-prepared-statement
      (update-sessdata-stmt "UPDATE users SET session_data = ? WHERE jid = ?")
    (format *debug-io* "~&update sessdata for ~A~%" jid)
    (bind-parameters update-sessdata-stmt sessdata jid)
    (sqlite:step-statement update-sessdata-stmt)))

(defun wa-handle-ws-connected (comp conn jid wa-jid)
  (with-wa-handler-context (comp conn jid)
    (let ((sessdata (whatscl::serialize-persistent-session (whatscl::wac-session conn)))
          (status (format nil "Logged in to WhatsApp as ~A." wa-jid)))
      (update-session-data jid sessdata)
      (admin-msg comp jid status)
      (admin-presence comp jid status)
      (whatscl::send-presence conn :available)
      (format *debug-io* "~&ws-connected: ~A (as ~A)~%" jid wa-jid))))

(defun wa-handle-disconnect (comp conn jid kind)
  (with-wa-handler-context (comp conn jid)
    (format *debug-io* "~&disconnect for ~A: ~A" jid kind)
    (let ((reason
            (case kind
              (:replaced "Connection replaced by other WhatsApp Web session")
              (:removed "Connection removed in mobile app"))))
      (admin-msg comp jid (format nil "Error: ~A." reason))
      (admin-presence comp jid reason "xa"))
    (admin-msg comp jid "(Disabling automatic reconnections.)")
    (remhash jid (component-whatsapps comp))))

(defun wa-handle-error-status-code (comp conn jid err)
  (with-wa-handler-context (comp conn jid)
    (format *debug-io* "~&error-status-code for ~A: ~A~%" jid err)
    (if (typep err 'whatscl::login-error)
        (progn
          (let ((status-code (whatscl::scerror-status-code err)))
            (cond
              ((equal status-code 401)
               (progn
                 (admin-msg comp jid "Error: The WhatsApp Web connection was removed from your device! You'll need to scan the QR code again.")
                 (admin-presence comp jid "Connection removed" "xa")
                 (update-session-data jid "")))
              ((equal status-code 403)
               (progn
                 (admin-msg comp jid "Error: WhatsApp Web denied access. You may have violated the Terms of Service.")
                 (admin-presence comp jid "Access denied" "xa")
                 (update-session-data jid "")))
              ((equal status-code 419)
               (progn
                 (admin-msg comp jid "Error: WhatsApp Web have invalidated this connection for some reason. You'll need to scan the QR code again. (It's unclear why this happens.")
                 (admin-presence comp jid "Connection invalidated" "xa")
                 (update-session-data jid "")))
              (t
               (progn
                 (admin-presence comp jid "Login failure" "xa")
                 (admin-msg comp jid (format nil "Login failure: ~A" err))))))
          (admin-msg comp jid "(Disabling automatic reconnections.)")
          (remhash jid (component-whatsapps comp)))
        (admin-msg comp jid (format nil "Warning: A non-fatal WhatsApp error has occurred.~%You should be fine to continue, but if problems persist, consider re-connecting or re-registering.~%Details: ~A" err)))))

(defun wa-handle-error (comp conn jid err bt)
  (with-wa-handler-context (comp conn jid)
    (format *debug-io* "~&whatscl error for ~A: ~A~%Backtrace:~A~%" jid err bt)
    (admin-msg comp jid
               (format nil "A programming error has been detected and your connection has been aborted unexpectedly.~%Report the following error to the bridge admin: ~A" err))
    (admin-msg comp jid "(Disabling automatic reconnections.)")
    (admin-presence comp jid "Programming error" "xa")
    (remhash jid (component-whatsapps comp))))

(defun wa-message-key-to-stanza-headers (comp conn jid msg-id msg-ts key)
  "Takes KEY, a WHATSCL::MESSAGE-KEY, and returns (VALUES FROM TOS ID TYPE GROUP-LOCALPART) [i.e. the values of the 'from', 'to', 'id' and 'type' stanza headers, where TOS is a list of recipients], or NIL if no action should be taken to deliver the message."
  (let* ((xmpp-id (concatenate 'string
                               "wa-" msg-id "-" (write-to-string msg-ts)))
         (group-localpart (wa-jid-to-whatsxmpp-localpart (whatscl::key-jid key)))
         (uid (get-user-id jid))
         (previous-xmpp-id (lookup-wa-msgid uid msg-id)))
    (unless previous-xmpp-id
      (typecase key
        (whatscl::message-key-receiving
         (progn
           (format *debug-io* "~&direct message ~A for ~A~%" msg-id jid)
           (values (concatenate 'string
                                group-localpart
                                "@"
                                (component-name comp)
                                "/whatsapp")
                   (list jid) xmpp-id "chat" nil)))
        (whatscl::message-key-group-receiving
         (let* ((chat-id (get-user-chat-id uid group-localpart))
                (participant-localpart (wa-jid-to-whatsxmpp-localpart (whatscl::key-participant key))))
           (format *debug-io* "~&group message ~A in ~A for ~A~%" msg-id group-localpart jid)
           (if chat-id
               (let ((from-resource (or (get-participant-resource chat-id participant-localpart)
                                        participant-localpart))
                     (recipients (get-user-chat-joined uid group-localpart)))
                 (if recipients
                     (values (concatenate 'string
                                          group-localpart "@" (component-name comp)
                                          "/" from-resource)
                             recipients xmpp-id "groupchat" group-localpart)
                     (warn "None of ~A's resources were joined to group ~A to receive message ~A!" jid group-localpart msg-id)))
               (progn
                 (warn "No chat in database for group ~A for ~A -- creating" group-localpart jid)
                 (admin-msg comp jid (format nil "Received message in unknown new WhatsApp group chat ~A; you should receive an invitation soon..." (whatscl::key-jid key)))
                 (add-wa-chat comp conn jid (whatscl::key-jid key))
                 (return-from wa-message-key-to-stanza-headers)))))
        (t nil)))))

(defun wa-handle-message (comp conn jid msg delivery-type)
  (declare (ignore delivery-type))
  (with-wa-handler-context (comp conn jid)
    (let* ((key (whatscl::message-key msg))
           (wa-id (whatscl::message-id msg))
           (contents (whatscl::message-contents msg))
           (wa-ts (whatscl::message-ts msg))
           (uid (get-user-id jid))
           (local-time:*default-timezone* local-time:+utc-zone+)
           (ts (local-time:unix-to-timestamp wa-ts)))
      (multiple-value-bind
            (from recipients xmpp-id xmpp-type group-localpart)
          (wa-message-key-to-stanza-headers comp conn jid wa-id wa-ts key)
        (when from
          (macrolet
              ((send-message ((&key suppress-insert) &body contents)
                 (let ((to-sym (gensym)))
                   `(progn
                      ;; Referencing lexical variables in a MACROLET! How hacky.
                      (unless ,suppress-insert
                        (insert-user-message uid xmpp-id wa-id))
                      (loop
                        for ,to-sym in recipients
                        do (with-message (comp ,to-sym
                                               :from from
                                               :id xmpp-id
                                               :type xmpp-type)
                             ,@contents
                             (cxml:with-element "delay"
                               (cxml:attribute "xmlns" +delivery-delay-ns+)
                               (cxml:attribute "stamp" (local-time:format-timestring nil ts)))
                             (cxml:with-element "active"
                               (cxml:attribute "xmlns" +chat-states-ns+))
                             (when (and group-localpart (not ,suppress-insert))
                               (cxml:with-element "stanza-id"
                                 (cxml:attribute "xmlns" +unique-stanzas-ns+)
                                 (cxml:attribute "id" xmpp-id)
                                 (cxml:attribute "by" (concatenate 'string
                                                                   group-localpart
                                                                   "@"
                                                                   (component-name comp))))
                               (cxml:with-element "origin-id"
                                 (cxml:attribute "xmlns" +unique-stanzas-ns+)
                                 (cxml:attribute "id" wa-id)))
                             (cxml:with-element "markable"
                               (cxml:attribute "xmlns" +chat-markers-ns+))))))))
            (let* ((qc (whatscl::message-quoted-contents-summary msg)))
              (typecase contents
                (whatscl::message-contents-text
                 (let* ((contents-text (whatscl::contents-text contents))
                        (text (format nil "~@[> ~A~%~]~A" qc contents-text)))
                   (send-message ()
                    (cxml:with-element "body"
                      (cxml:text text)))))
                (whatscl::message-contents-file
                 (let* ((file-info (whatscl::contents-file-info contents))
                        (media-type (whatscl::get-contents-media-type contents))
                        (filename (when (typep contents 'whatscl::message-contents-document)
                                    (whatscl::contents-filename contents)))
                        (caption (whatscl::contents-caption contents))
                        (upload-promise (upload-whatsapp-media-file comp file-info media-type filename)))
                   (catcher
                    (attach upload-promise
                            (lambda (get-url)
                              (with-component-data-lock (comp)
                                (when (or caption qc)
                                  (let ((text (format nil "~@[> ~A~%~]~@[~A~]" qc caption)))
                                    (send-message (:suppress-insert t)
                                     (cxml:with-element "body"
                                       (cxml:text text)))))
                                (send-message ()
                                 (cxml:with-element "body"
                                   (cxml:text get-url))
                                 (cxml:with-element "x"
                                   (cxml:attribute "xmlns" +oob-ns+)
                                   (cxml:with-element "url"
                                     (cxml:text get-url)))))))
                    (error (e)
                           (with-component-data-lock (comp)
                             ;; Insert the thing into the database, so this message
                             ;; doesn't repeat.
                             (insert-user-message uid xmpp-id wa-id)
                             (format *debug-io* "~&whatsapp media message ~A from ~A failed! error: ~A~%"
                                     wa-id from e)
                             (admin-msg comp jid
                                        (format nil "Warning: Failed to process a media message sent to you by ~A:~%    ~A"
                                                from e)))))))))))))))

(defun get-avatar-data (avatar-url)
  "Fetches AVATAR-URL, using the database as a cache. Returns the SHA1 hash (lowercase) of the avatar data as first argument, and the actual octets as second."
  (with-prepared-statements
      ((get-stmt "SELECT sha1, image FROM avatar_data WHERE avatar_url = ?")
       (insert-stmt "INSERT INTO avatar_data (avatar_url, sha1, image) VALUES (?, ?, ?)"))
    (bind-parameters get-stmt avatar-url)
    (if (sqlite:step-statement get-stmt)
        (with-bound-columns (sha1 image) get-stmt
          (values sha1 image))
        (progn
          (format *debug-io* "~&fetching avatar url: ~A~%" avatar-url)
          (multiple-value-bind (data status-code)
              (drakma:http-request avatar-url)
            (format *debug-io* "~&fetch resulted in status ~A~%" status-code)
            (when (eql status-code 200)
              (let ((sha1 (sha1-octets data)))
                (bind-parameters insert-stmt avatar-url sha1 data)
                (sqlite:step-statement insert-stmt)
                (values sha1 data))))))))

(defun get-contact-avatar-data (uid localpart)
  "Get a set of avatar data (returned by GET-AVATAR-DATA) for LOCALPART, a possible contact for the user with ID UID.
Returns three values: avatar data (as two values), and a generalized boolean specifying whether the user had an avatar (i.e. for no avatar users, returns (VALUES NIL NIL T))"
  (with-prepared-statements
      ((get-stmt "SELECT avatar_url FROM user_contacts WHERE user_id = ? AND wa_jid = ?"))
    (bind-parameters get-stmt uid localpart)
    (when (sqlite:step-statement get-stmt)
      (with-bound-columns (avatar-url) get-stmt
        (values-list (append
                      (if (and avatar-url (> (length avatar-url) 0) (not (equal avatar-url "NO-AVATAR")))
                          (multiple-value-list (get-avatar-data avatar-url))
                          `(nil nil))
                      (cons (> (length avatar-url) 0) nil)))))))

(defun wa-request-avatar (comp conn jid wa-jid localpart)
  (format *debug-io* "~&requesting avatar for ~A from ~A~%" localpart jid)
  (whatscl::get-profile-picture conn wa-jid
                                (lambda (conn result)
                                  (wa-handle-avatar-result comp conn jid localpart result))))

(defun handle-wa-contact-presence (comp conn jid localpart &key noretry destination)
  "Send out a presence stanza for LOCALPART to JID, or queue requests for that user's status or avatar if they're lacking."
  (unless (uiop:string-prefix-p "u" localpart)
    (return-from handle-wa-contact-presence))
  (let* ((uid (get-user-id jid))
         (status (get-contact-status uid localpart))
         (wa-jid (whatsxmpp-localpart-to-wa-jid localpart)))
    (multiple-value-bind (avatar-sha1 avatar-data has-avatar-p)
        (get-contact-avatar-data uid localpart)
      (declare (ignore avatar-data))
      (if (and has-avatar-p status)
          (with-presence (comp (or destination jid)
                               :from (concatenate 'string
                                                  localpart
                                                  "@"
                                                  (component-name comp)))
            (cxml:with-element "status"
              (cxml:text status))
            (cxml:with-element "c"
              (cxml:attribute "xmlns" +entity-caps-ns+)
              (cxml:attribute "hash" "sha-1")
              (cxml:attribute "node" "https://git.theta.eu.org/eta/whatsxmpp")
              (cxml:attribute "ver" +whatsapp-user-entity-caps+))
            (cxml:with-element "x"
              (cxml:attribute "xmlns" +vcard-avatar-ns+)
              (if avatar-sha1
                  (cxml:with-element "photo"
                    (cxml:text avatar-sha1))
                  (cxml:with-element "photo"))))
          (progn
            (unless noretry
              (unless avatar-sha1
                (wa-request-avatar comp conn jid wa-jid localpart))
              (unless status
                (format *debug-io* "~&requesting status for ~A from ~A~%" localpart jid)
                (whatscl::get-profile-status conn wa-jid
                                             (lambda (conn result)
                                               (wa-handle-status-result comp conn jid localpart result))))))))))

(defun handle-wa-contact-presence-subscriptions (comp jid localpart)
  "Check if we need to send out presence subscriptions for LOCALPART."
  (unless (uiop:string-prefix-p "u" localpart)
    (return-from handle-wa-contact-presence-subscriptions))
  (let ((uid (get-user-id jid)))
    (assert uid () "No user ID for ~A!" jid)
    (with-prepared-statements
        ((get-stmt "SELECT subscription_state, name, notify, id FROM user_contacts WHERE user_id = ? AND wa_jid = ?")
         (update-stmt "UPDATE user_contacts SET subscription_state = ? WHERE id = ?"))
      (bind-parameters get-stmt uid localpart)
      (unless (sqlite:step-statement get-stmt)
        (error "No contact with localpart ~A exists!" localpart))
      (with-bound-columns (subscription-state name notify ctid) get-stmt
        (let ((name-to-use (or name
                               (when notify (concatenate 'string "~" notify))))
              (from (concatenate 'string localpart "@" (component-name comp))))
          (when (and (equal subscription-state "none") name-to-use)
            (with-presence (comp jid
                            :type "subscribe"
                            :from from)
              (cxml:with-element "nick"
                (cxml:attribute "xmlns" +nick-ns+)
                (cxml:text name-to-use)))
            (bind-parameters update-stmt "asked" ctid)
            (sqlite:step-statement update-stmt)))))))

(defun add-wa-contact (comp conn jid contact)
  "Adds the WHATSCL:CONTACT to the list of JID's contacts, or updates it if it already exists. Returns the contact's localpart."
  (with-accessors ((ct-jid whatscl::contact-jid)
                   (ct-notify whatscl::contact-notify)
                   (ct-name whatscl::contact-name))
      contact
    (let ((uid (get-user-id jid))
          (wx-localpart (wa-jid-to-whatsxmpp-localpart ct-jid)))
      (unless (uiop:string-prefix-p "u" wx-localpart)
        (return-from add-wa-contact))
      (assert uid () "No user ID for ~A!" jid)
      (with-prepared-statements
          ((get-stmt "SELECT id, name, notify FROM user_contacts WHERE user_id = ? AND wa_jid = ?")
           (update-stmt "UPDATE user_contacts SET name = ?, notify = ? WHERE id = ?")
           (update-chat-names "UPDATE user_chat_members SET resource = ? WHERE wa_jid = ?")
           (insert-stmt "INSERT INTO user_contacts (user_id, wa_jid, name, notify) VALUES (?, ?, ?, ?)"))
        (bind-parameters get-stmt uid wx-localpart)
        (if (sqlite:step-statement get-stmt)
            (with-bound-columns (id name notify) get-stmt
              (let ((notify (or ct-notify notify))
                    (name (or ct-name name)))
                (bind-parameters update-stmt name notify id)
                (sqlite:step-statement update-stmt)))
            (progn
              (bind-parameters insert-stmt uid wx-localpart ct-name ct-notify)
              (sqlite:step-statement insert-stmt)))
        ;; Update the resource in all chats the user is joined to, so we can use the user's actual name instead of a phone number if possible
        (let ((newname (get-contact-name uid wx-localpart)))
          (bind-parameters update-chat-names newname wx-localpart))
        wx-localpart))))

(defun wa-handle-contacts (comp conn jid contacts)
  (with-wa-handler-context (comp conn jid)
    (format *debug-io* "~&got ~A contacts for ~A~%" (length contacts) jid)
    (loop
      for contact in contacts
      do (add-wa-contact comp conn jid contact))))

(defun wa-handle-contact (comp conn jid contact)
  (with-wa-handler-context (comp conn jid)
    (format *debug-io* "~&got contact ~A for ~A~%" contact jid)
    (add-wa-contact comp conn jid contact)))

(defun request-wa-chat-metadata (comp conn jid wx-localpart)
  "Request chat metadata for WX-LOCALPART (a MUC localpart) for the user with JID."
  (format *debug-io* "~&requesting chat metadata for ~A from ~A~%" wx-localpart jid)
  (whatscl::get-group-metadata conn (whatsxmpp-localpart-to-wa-jid wx-localpart)
                               (lambda (conn meta)
                                 (wa-handle-group-metadata comp conn jid wx-localpart meta))))

(defun handle-wa-chat-invitation (comp conn jid uid localpart &key noretry)
  "Checks to see whether the group chat LOCALPART has any metadata; if not, requests some. If it does, and the user hasn't been invited to that group chat yet, send them an invitation."
  (unless (uiop:string-prefix-p "g" localpart)
    (return-from handle-wa-chat-invitation))
  (with-prepared-statements
      ((get-stmt "SELECT id, invitation_state FROM user_chats WHERE user_id = ? AND wa_jid = ?")
       (count-stmt "SELECT COUNT(*) FROM user_chat_members WHERE chat_id = ?")
       (update-stmt "UPDATE user_chats SET invitation_state = ? WHERE id = ?"))
    (bind-parameters get-stmt uid localpart)
    (assert (sqlite:step-statement get-stmt) ()
            "Chat ~A doesn't exist in database!" localpart)
    (with-bound-columns (chat-id invitation-state) get-stmt
      (bind-parameters count-stmt chat-id)
      (assert (sqlite:step-statement count-stmt))
      (with-bound-columns (n-members) count-stmt
        (if (> n-members 0)
            (when (equal invitation-state "none")
              (with-message (comp jid)
                (cxml:with-element "x"
                  (cxml:attribute "xmlns" +muc-invite-ns+)
                  (cxml:attribute "jid" (concatenate 'string
                                                     localpart
                                                     "@"
                                                     (component-name comp)))))
              (bind-parameters update-stmt "invited" chat-id)
              (sqlite:step-statement update-stmt))
            (unless noretry
              (request-wa-chat-metadata comp conn jid localpart)))))))

(defun add-wa-chat (comp conn jid ct-jid)
  "Adds the JID CT-JID to the list of the user's groupchats, if it is a groupchat. If it's a user JID, sends a presence subscription request if necessary."
  (let ((uid (get-user-id jid))
        (wx-localpart (wa-jid-to-whatsxmpp-localpart ct-jid)))
    (when (uiop:string-prefix-p "u" wx-localpart)
      ;; The user has an open chat with this other user, so they probably want a presence subscription.
      (handle-wa-contact-presence-subscriptions comp jid wx-localpart)
      (return-from add-wa-chat))
    (unless (uiop:string-prefix-p "g" wx-localpart)
      (warn "Interesting localpart pased to ADD-WA-CHAT: ~A" wx-localpart)
      (return-from add-wa-chat))
    (assert uid () "No user ID for ~A!" jid)
    (unless (get-user-chat-id uid wx-localpart)
      (insert-user-chat uid wx-localpart))
    (handle-wa-chat-invitation comp conn jid uid wx-localpart)))

(defun wa-handle-chats (comp conn jid chats)
  (with-wa-handler-context (comp conn jid)
    (format *debug-io* "~&got ~A chats for ~A~%" (length chats) jid)
    (loop
      for chat in chats
      do (add-wa-chat comp conn jid (whatscl::chat-jid chat)))))

(defun wa-handle-message-ack (comp conn jid &key id ack from to participant &allow-other-keys)
  (with-wa-handler-context (comp conn jid)
    (format *debug-io* "~&message ack: ~A is ~A (from ~A, to ~A, participant ~A)~%" id ack from to participant)
    (when (equal (whatscl::jid-to-string from) (whatscl::wac-jid conn))
      ;; (someone else acked this message)
      (let* ((uid (get-user-id jid))
             (xmpp-id (lookup-wa-msgid uid id)))
        (if xmpp-id
            (let ((marker-name
                    (cond
                      ((eql ack :received) "received")
                      ((eql ack :read) "displayed")
                      ((eql ack :played) "displayed")
                      (t (return-from wa-handle-message-ack)))))
              (if participant
                  (let* ((participant-localpart (wa-jid-to-whatsxmpp-localpart participant))
                         (group-localpart (wa-jid-to-whatsxmpp-localpart to))
                         (chat-id (get-user-chat-id uid group-localpart)))
                    (if chat-id
                        (let ((from-resource (or (get-participant-resource chat-id participant-localpart)
                                                 participant-localpart))
                              (recipients (get-user-chat-joined uid group-localpart)))
                          (loop
                            for recip in recipients
                            do (with-message (comp recip
                                                   :from (concatenate 'string
                                                                      group-localpart
                                                                      "@"
                                                                      (component-name comp)
                                                                      "/"
                                                                      from-resource)
                                                   :type "groupchat")
                                 (cxml:with-element marker-name
                                   (cxml:attribute "xmlns" +chat-markers-ns+)
                                   (cxml:attribute "id" xmpp-id)))))
                        (warn "Ack for message ID ~A: couldn't find chat id?" id)))
                  (let ((from-jid (concatenate 'string
                                               (wa-jid-to-whatsxmpp-localpart to)
                                               "@"
                                               (component-name comp))))
                    (with-message (comp jid
                                        :from from-jid)
                      (cxml:with-element marker-name
                        (cxml:attribute "xmlns" +chat-markers-ns+)
                        (cxml:attribute "id" xmpp-id))))))
            (warn "Got ack for unknown message id ~A" id))))))

(defun wa-handle-message-send-result (comp conn jid &key orig-from orig-to orig-id orig-body result muc-resource)
  (with-wa-handler-context (comp conn jid)
    (format *debug-io* "~&message send result for ~A from ~A: ~A~%" orig-id orig-from result)
    (handler-case
        (let ((status (cdr (assoc :status result))))
          (unless status
            (error "No status response provided by WhatsApp"))
          (unless (eql status 200)
            (error "Message sending failed with code ~A" status))
          (if muc-resource
              ;; Do a MUC echo
              (let* ((new-from (concatenate 'string orig-to "/" muc-resource))
                     (group-localpart (nth-value 1 (parse-jid orig-to)))
                     (recipients (get-user-chat-joined (get-user-id jid) group-localpart)))
                (loop
                  for recip in recipients
                  do (with-message (comp recip :from new-from :id orig-id :type "groupchat")
                       (cxml:with-element "body"
                         (cxml:text orig-body))
                       (cxml:with-element "stanza-id"
                         (cxml:attribute "xmlns" +unique-stanzas-ns+)
                         ;; XXX: This isn't actually compliant; we're supposed to generate
                         ;; our own IDs here. However, the worst you can do is confuse
                         ;; your own clients...
                         (cxml:attribute "id" orig-id)
                         (cxml:attribute "by" orig-to))
                       (cxml:with-element "markable"
                         (cxml:attribute "xmlns" +chat-markers-ns+)))))
              (with-message (comp orig-from :from orig-to)
                (cxml:with-element "received"
                  (cxml:attribute "xmlns" +delivery-receipts-ns+)
                  (cxml:attribute "id" orig-id)))))
      (error (e)
        (send-stanza-error comp
                           :id orig-id :to orig-from :from orig-to
                           :stanza-type "message"
                           :e (make-condition 'stanza-error
                                              :defined-condition "recipient-unavailable"
                                              :type "modify"
                                              :text (format nil "~A" e)))))))

(defun wa-handle-avatar-result (comp conn jid for-localpart result)
  (with-wa-handler-context (comp conn jid)
    (format *debug-io* "~&avatar result for ~A from ~A: ~A~%" for-localpart jid result)
    (let ((avatar-url (or result "NO-AVATAR"))
          (uid (get-user-id jid)))
      (with-prepared-statements
          ((update-stmt "UPDATE user_contacts SET avatar_url = ? WHERE user_id = ? AND wa_jid = ?"))
        (bind-parameters update-stmt avatar-url uid for-localpart)
        (sqlite:step-statement update-stmt)
        (handle-wa-contact-presence comp conn jid for-localpart :noretry t)))))

(defun wa-handle-status-result (comp conn jid for-localpart result)
  (with-wa-handler-context (comp conn jid)
    (format *debug-io* "~&status result for ~A from ~A: ~A~%" for-localpart jid result)
    (let ((avatar-url (or result "Status unknown or hidden"))
          (uid (get-user-id jid)))
      (with-prepared-statements
          ((update-stmt "UPDATE user_contacts SET status = ? WHERE user_id = ? AND wa_jid = ?"))
        (bind-parameters update-stmt avatar-url uid for-localpart)
        (sqlite:step-statement update-stmt)
        (handle-wa-contact-presence comp conn jid for-localpart :noretry t)))))

(defun wa-handle-picture-change (comp conn jid for-jid was-removed)
  (with-wa-handler-context (comp conn jid)
    (let* ((localpart (wa-jid-to-whatsxmpp-localpart for-jid))
           (uid (get-user-id jid))
           (contact-name (get-contact-name uid localpart)))
      (when contact-name
        (format *debug-io* "~&picture change notification for ~A from ~A (removed: ~A)~%" localpart jid was-removed)
        (if was-removed
            (with-prepared-statements
                ((update-stmt "UPDATE user_contacts SET avatar_url = ? WHERE user_id = ? AND wa_jid = ?"))
              (bind-parameters update-stmt "NO-AVATAR" uid localpart)
              (sqlite:step-statement update-stmt)
              (handle-wa-contact-presence comp conn jid localpart :noretry t))
            (wa-request-avatar comp conn jid for-jid localpart))))))

(defun wa-handle-status-change (comp conn jid for-jid status)
  (with-wa-handler-context (comp conn jid)
    (let* ((localpart (wa-jid-to-whatsxmpp-localpart for-jid))
           (uid (get-user-id jid))
           (contact-name (get-contact-name uid localpart)))
      (when contact-name
        (format *debug-io* "~&status change notification for ~A from ~A~%" localpart jid)
        (with-prepared-statements
            ((update-stmt "UPDATE user_contacts SET status = ? WHERE user_id = ? AND wa_jid = ?"))
          (bind-parameters update-stmt status uid localpart)
          (sqlite:step-statement update-stmt)
          (handle-wa-contact-presence comp conn jid localpart :noretry t))))))

(defun wa-handle-group-metadata (comp conn jid localpart data)
  (with-wa-handler-context (comp conn jid)
    (let* ((uid (get-user-id jid))
           (cid (get-user-chat-id uid localpart))
           (subject (whatscl::cassoc :subject data)))
      (unless cid
        (setf cid (insert-user-chat uid localpart)))
      (format *debug-io* "~&got group metadata for ~A from ~A~%" localpart jid)
      (unless subject
        (admin-msg comp jid (format nil "Warning: Failed to update group ~A: received ~A~%This warning usually appears when trying to get information for a group you're no longer in, and can be safely ignored." localpart data))
        (warn "Received incomplete group metadata for ~A from ~A: ~A" localpart jid data)
        (return-from wa-handle-group-metadata))
      (when cid
        (with-prepared-statements
            ((update-subject-stmt "UPDATE user_chats SET subject = ? WHERE id = ?")
             (delete-members-stmt "DELETE FROM user_chat_members WHERE chat_id = ?")
             (insert-member-stmt "INSERT INTO user_chat_members (chat_id, wa_jid, resource, affiliation) VALUES (?, ?, ?, ?)"))
          (with-transaction
            (bind-parameters update-subject-stmt subject cid)
            (bind-parameters delete-members-stmt cid)
            (sqlite:step-statement update-subject-stmt)
            (sqlite:step-statement delete-members-stmt)
            (loop
              for part in (whatscl::aval :participants data)
              do (let ((localpart (wa-jid-to-whatsxmpp-localpart
                                   (whatscl::parse-jid
                                    (whatscl::aval :id part)))))
                   (bind-parameters insert-member-stmt
                                    cid localpart ; chat_id, wa_jid
                                    (3 ; resource
                                     (or
                                      (get-contact-name uid localpart)
                                      (substitute #\+ #\u localpart)))
                                    (4 ; affiliation
                                     (if (whatscl::cassoc :is-admin part)
                                         "admin"
                                         "member"))))
                   (sqlite:step-statement insert-member-stmt)
                   (sqlite:reset-statement insert-member-stmt)))
          (handle-wa-chat-invitation comp conn jid uid localpart :noretry t))))))

(defun wa-handle-presence (comp conn jid &key for-jid type participant &allow-other-keys)
  (with-wa-handler-context (comp conn jid)
    (let* ((localpart (wa-jid-to-whatsxmpp-localpart for-jid))
           (chat-state
             (cond
               ((eql type :composing) "composing")
               ((eql type :paused) "paused")
               ((eql type :available) "active")
               ((eql type :unavailable) "gone")
               (t (return-from wa-handle-presence)))))
      (unless participant ; Groups hard
        (let ((from-jid (concatenate 'string
                                     localpart
                                     "@"
                                     (component-name comp))))
          (with-message (comp jid
                              :from from-jid)
            (cxml:with-element chat-state
              (cxml:attribute "xmlns" +chat-states-ns+))))))))

(defun wa-handle-chat-modified (comp conn jid chat-jid)
  (with-wa-handler-context (comp conn jid)
    (let ((wx-localpart (wa-jid-to-whatsxmpp-localpart chat-jid)))
      (format *debug-io* "~&chat-modified: ~A for ~A~&" wx-localpart jid)
      (request-wa-chat-metadata comp conn jid wx-localpart))))

(defun bind-wa-handlers (comp conn jid)
  (on :ws-close conn (lambda (&rest args)
                       (declare (ignore args))
                       (wa-handle-ws-close comp conn jid)))
  (on :ws-error conn (lambda (e) (wa-handle-ws-error comp conn jid e)))
  (on :disconnect conn (lambda (k) (wa-handle-disconnect comp conn jid k)))
  (on :chat-modified conn (lambda (k) (wa-handle-chat-modified comp conn jid k)))
  (on :error conn (lambda (e backtrace) (wa-handle-error comp conn jid e backtrace)))
  (on :error-status-code conn (lambda (e) (wa-handle-error-status-code comp conn jid e)))
  (on :qrcode conn (lambda (text) (wa-handle-ws-qrcode comp conn jid text)))
  (on :message conn (lambda (msg dt) (wa-handle-message comp conn jid msg dt)))
  (on :contacts conn (lambda (contacts) (wa-handle-contacts comp conn jid contacts)))
  (on :chats conn (lambda (chats) (wa-handle-chats comp conn jid chats)))
  (on :contact conn (lambda (contact) (wa-handle-contact comp conn jid contact)))
  (on :message-ack conn (lambda (&key id ack from to participant &allow-other-keys)
                          (wa-handle-message-ack comp conn jid
                                                 :id id :ack ack :from from :to to
                                                 :participant participant)))
  (on :picture-change conn (lambda (for-jid removed)
                             (wa-handle-picture-change comp conn jid for-jid removed)))

  (on :status-change conn (lambda (for-jid status)
                             (wa-handle-status-change comp conn jid for-jid status)))
  (on :presence conn (lambda (&key of type participant &allow-other-keys)
                       (wa-handle-presence comp conn jid
                                           :for-jid of :type type
                                           :participant participant)))
  (on :connected conn (lambda (waj) (wa-handle-ws-connected comp conn jid waj))))

(defun handle-setup-user (comp jid)
  "Set up a WhatsApp connection for JID on COMP."
  (with-component-data-lock (comp)
    (format *debug-io* "~&setup user: ~A~%" jid)
    (with-prepared-statement
        (get-session-data-stmt "SELECT session_data FROM users WHERE jid = ?")
        (bind-parameters get-session-data-stmt jid)
      (assert (sqlite:step-statement get-session-data-stmt) ()
              "HANDLE-SETUP-USER called for invalid JID ~A" jid)
      (let* ((sessdata (sqlite:statement-column-value get-session-data-stmt 0))
             (sess (when (and sessdata (> (length sessdata) 0))
                     (format *debug-io* "~&reusing old session data for ~A~%" jid)
                     (whatscl::deserialize-persistent-session sessdata)))
             (conn (whatscl::make-connection sess)))
        (admin-msg comp jid "Connecting to WhatsApp...")
        (admin-presence comp jid "Connection in progress..." "away")
        (symbol-macrolet
            ((stored-conn (gethash jid (component-whatsapps comp))))
          (let ((old-conn))
            (when stored-conn
              (setf old-conn stored-conn))
            (setf stored-conn conn)
            (bind-wa-handlers comp conn jid)
            (when old-conn
              (admin-msg comp jid "(destroying your old connection)")
              (whatscl::close-connection old-conn))
            (handler-case
                (whatscl::start-connection conn)
              (error (e)
                (admin-msg comp jid (format nil "Connection failed:~% ~A" e))
                (admin-msg comp jid "(will retry)")
                (setf stored-conn nil)))))))))

(defun start-user-registration (comp jid)
  "Register the JID as wanting to use the bridge COMP."
  (with-component-data-lock (comp)
    (let ((stripped (strip-resource jid)))
      (admin-msg comp jid "Starting registration!")
      (format *debug-io* "~&register: ~A~%" stripped)
      (with-prepared-statement
          (insert-stmt "INSERT INTO users (jid) VALUES (?) ON CONFLICT (jid) DO UPDATE SET session_data = ''")
        (bind-parameters insert-stmt stripped)
        (sqlite:step-statement insert-stmt))
      (with-presence (comp stripped
                      :type "subscribe"
                      :from (admin-jid comp))
        (cxml:with-element "status"
          (cxml:text "Please add the whatsxmpp admin user to your roster; if you don't, things will probably break in various fun ways."))
        (cxml:with-element "nick"
          (cxml:attribute "xmlns" +nick-ns+)
          (cxml:text "whatsxmpp admin")))
      (admin-msg comp jid "WhatsApp connection should begin shortly...")
      (handle-setup-user comp stripped))))

(defun get-admin-status (comp jid)
  "Get the status text of the admin user for the user with ID JID. Returns a <show/> value as second value."
  (multiple-value-bind (conn exists-p)
      (gethash jid (component-whatsapps comp))
    (cond
      ((and conn (whatscl::wac-jid conn))
       (format nil "Connected and logged in as ~A."
               (whatscl::wac-jid conn)))
      (conn (values "Connected, but not logged in." "away"))
      (exists-p (values "Temporarily disconnected." "away"))
      (t (values "Disconnected (automatic reconnections disabled)." "xa")))))

(defun do-roster-exchange (comp jid uid)
  "Initiate an XEP-0144 Roster Item Exchange for JID (with user ID UID)."
  (let ((localparts (get-user-contact-localparts uid)))
    (with-message (comp jid)
      (cxml:with-element "x"
        (cxml:attribute "xmlns" +roster-exchange-ns+)
        (loop
          for ct-localpart in localparts
          do (when ct-localpart
               (let* ((ct-jid (concatenate 'string
                                           ct-localpart
                                           "@"
                                           (component-name comp)))
                      (ct-name (get-contact-name uid ct-localpart
                                                 :no-phone-number t)))
                 (when ct-name
                   (cxml:with-element "item"
                     (cxml:attribute "action" "add")
                     (cxml:attribute "jid" ct-jid)
                     (cxml:attribute "name" ct-name)
                     (cxml:with-element "group"
                       (cxml:text "WhatsApp")))))))))))

(defun handle-admin-command (comp from body uid)
  "Handles an admin command sent to COMP."
  (labels ((reply (text)
             (send-text-message comp from text (admin-jid comp))))
    (let ((body (string-downcase body))
          (stripped (strip-resource from)))
      (cond
        ((and uid (equal body "register"))
         (reply (format nil "You're already registered!~%Try `connect`. If you really want to re-register, use the `register -force` command.")))
        ((or
          (and (not uid) (equal body "register"))
          (and uid (equal body "register -force")))
         (start-user-registration comp stripped))
        ((equal body "help")
         (reply *admin-help-text*))
        ((not uid)
         (reply "You're not registered with this bridge. Try `register` or `help`."))
        ((equal body "getroster")
         (progn
           (do-roster-exchange comp stripped uid)
           (reply "Roster exchange request sent.")))
        ((equal body "status")
         (reply (get-admin-status comp stripped)))
        ((equal body "connect")
         (handle-setup-user comp stripped))
        ((equal body "stop")
         (let ((conn (gethash stripped (component-whatsapps comp))))
           (when (remhash stripped (component-whatsapps comp))
             (reply "WhatsApp connections disabled."))
           (when conn
             (whatscl::close-connection conn))))
        ((equal body "refresh-chats")
         (let ((conn (gethash stripped (component-whatsapps comp))))
           (if conn
               (let ((chats (get-user-groupchats uid)))
                 (reply (format nil "Refreshing metadata for ~A groupchats...~%When the metadata refresh is complete, you'll need to rejoin all of your groupchats (most easily accomplished by reconnecting yourself to XMPP)."
                                (length chats)))
                 (loop
                   for (localpart . subject) in chats
                   do (request-wa-chat-metadata comp conn stripped localpart)))
               (reply "You're not connected to WhatsApp."))))
        ((uiop:string-prefix-p "refresh-chat " body)
         (let ((conn (gethash stripped (component-whatsapps comp)))
               (localpart-to-use (subseq body #.(length "refresh-chat "))))
           (if conn
               (progn
                 (reply (format nil "Refreshing metadata for ~A..." localpart-to-use))
                 (request-wa-chat-metadata comp conn stripped localpart-to-use))
               (reply "You're not connected to WhatsApp."))))
        (t
         (reply "Unknown command. Try `help` for a list of supported commands."))))))

(defun whatsxmpp-vcard-temp-handler (comp &key to from &allow-other-keys)
  "Handles a vcard-temp IQ request."
  (format *debug-io* "~&vcard-temp: ~A (from ~A)~%" to from)
  (with-component-data-lock (comp)
    (let* ((uid (get-user-id from))
           (to-localpart (nth-value 1 (parse-jid to))))
      (multiple-value-bind (name avatar-data)
             (cond
               ((equal to-localpart "admin")
                "whatsxmpp admin")
               ((not uid)
                (error 'stanza-error
                       :defined-condition "registration-required"
                       :text "You must register with the bridge admin to view contact details."
                       :type "auth"))
               (t
                (let ((name (get-contact-name uid to-localpart)))
                  (unless name
                    (error 'stanza-error
                           :defined-condition "item-not-found"
                           :text "No vCard for that JID is available at this time."
                           :type "modify"))
                  (values name
                          (nth-value 1 (get-contact-avatar-data uid to-localpart))))))
        `((cxml:with-element "vCard"
            (cxml:attribute "xmlns" +vcard-temp-ns+)
            (cxml:with-element "FN"
              (cxml:text ,name))
            (cxml:with-element "NICKNAME"
              (cxml:text ,name))
            ,(when avatar-data
               `(cxml:with-element "PHOTO"
                  (cxml:with-element "TYPE"
                    (cxml:text "image/jpeg"))
                  (cxml:with-element "BINVAL"
                    (cxml:text ,(qbase64:encode-bytes avatar-data)))))))))))

(defun handle-muc-join (comp jid muc-localpart muc-chatid roomnick)
  "Handles JID joining MUC-LOCALPART."
  (format *debug-io* "~&~A joining MUC ~A with roomnick ~A~%" jid muc-localpart roomnick)
  (let ((muc-jid (concatenate 'string muc-localpart "@" (component-name comp))))
    (with-prepared-statements
        ((get-subject-stmt "SELECT subject FROM user_chats WHERE id = ?")
         (get-resource-stmt "SELECT user_resource FROM user_chats WHERE id = ?")
         (update-resource-stmt "UPDATE user_chats SET user_resource = ? WHERE id = ?")
         (get-members-stmt "SELECT id, wa_jid, resource, affiliation FROM user_chat_members WHERE chat_id = ?")
         (update-member-resource-stmt "UPDATE user_chat_members SET resource = ? WHERE id = ?")
         (insert-joined-stmt "INSERT INTO user_chat_joined (chat_id, jid) VALUES (?, ?) ON CONFLICT DO NOTHING"))
      (labels
          ((send-presence (from-resource from-jid affiliation role &rest stati)
             (with-presence (comp jid :from (concatenate 'string muc-jid "/" from-resource))
               (cxml:with-element "x"
                 (cxml:attribute "xmlns" +muc-user-ns+)
                 (cxml:with-element "item"
                   (cxml:attribute "jid" from-jid)
                   (cxml:attribute "affiliation" affiliation)
                   (cxml:attribute "role" role))
                 (loop
                   for status in stati
                   do (cxml:with-element "status"
                        (cxml:attribute "code" status)))))))
        ;; step 0: if there's already a roomnick set, we gotta use that one to avoid confusion
        ;; (this restriction will be relaxed in later versions)
        (bind-parameters get-resource-stmt muc-chatid)
        (assert (sqlite:step-statement get-resource-stmt))
        (let* ((old-roomnick (first (column-values get-resource-stmt)))
               (new-roomnick roomnick)
               (roomnick (if (and old-roomnick (> (length old-roomnick) 0))
                             old-roomnick new-roomnick)))
          ;; step 1: send in-room presence from other occupants
          (bind-parameters get-members-stmt muc-chatid)
          (loop
            while (sqlite:step-statement get-members-stmt)
            do (with-bound-columns (memid mem-localpart resource affiliation) get-members-stmt
                 (let ((resource-to-use (if (equal resource roomnick)
                                            (concatenate 'string resource " [WA]")
                                            resource)))
                   (unless (equal resource-to-use resource)
                     ;; prevent conflicts with the user's chosen roomnick
                     (bind-parameters update-member-resource-stmt resource-to-use memid)
                     (sqlite:step-statement update-member-resource-stmt)
                     (sqlite:reset-statement update-member-resource-stmt))
                   (send-presence resource-to-use
                                  (concatenate 'string
                                               mem-localpart
                                               "@"
                                               (component-name comp))
                                  affiliation
                                  (if (equal affiliation "member") "participant" "moderator")))))
          ;; step 2: send self-presence
          (if (equal old-roomnick new-roomnick)
              (send-presence roomnick jid "member" "participant" "100" "110")
              ;; 210 means "we forced what your nick was for you"
              (send-presence roomnick jid "member" "participant" "100" "110" "210"))
          ;; step 3: send subject
          (bind-parameters get-subject-stmt muc-chatid)
          (assert (sqlite:step-statement get-subject-stmt))
          (with-bound-columns (subject) get-subject-stmt
            (with-message (comp jid
                                :from muc-jid
                                :type "groupchat")
              (cxml:with-element "subject"
                (cxml:text subject))))
          ;; step 4: update resource & joined information if required
          (bind-parameters update-resource-stmt roomnick muc-chatid)
          (sqlite:step-statement update-resource-stmt)
          (bind-parameters insert-joined-stmt muc-chatid jid)
          (sqlite:step-statement insert-joined-stmt))))))

(defun whatsxmpp-presence-unavailable-handler (comp &key from to &allow-other-keys)
  "Handles a presence unavailable broadcast."
  (with-component-data-lock (comp)
    (multiple-value-bind (to-hostname to-localpart)
        (parse-jid to)
      (declare (ignore to-hostname))
      (let* ((stripped (strip-resource from))
             (uid (get-user-id stripped))
             (chat-id (get-user-chat-id uid to-localpart)))
        (when (and uid (uiop:string-prefix-p "g" to-localpart))
          (format *debug-io* "~&~A muc-presence-unavailable: ~A~%" from to)
          (when chat-id
            (with-prepared-statements
                ((remove-joined-stmt "DELETE FROM user_chat_joined WHERE chat_id = ? AND jid = ?"))
              (bind-parameters remove-joined-stmt chat-id from)
              (sqlite:step-statement remove-joined-stmt))))))))

(defun whatsxmpp-presence-handler (comp &key from to type id stanza &allow-other-keys)
  "Handles a presence broadcast."
  (unless (or (not type) (eql (length type) 0))
    (return-from whatsxmpp-presence-handler))
  (with-component-data-lock (comp)
    (multiple-value-bind (to-hostname to-localpart to-resource)
        (parse-jid to)
      (declare (ignore to-hostname))
      (let* ((stripped (strip-resource from))
             (uid (get-user-id stripped))
             (x-element (get-node-with-xmlns (child-elements stanza) +muc-ns+)))
        (cond
          (x-element
           (handler-case
               (progn
                 (format *debug-io* "~&~A muc-presence: ~A~%" from to)
                 (unless uid
                   (error 'stanza-error
                          :defined-condition "registration-required"
                          :text "You must register to join MUCs via this bridge."
                          :type "auth"))
                 (let ((chat-id (get-user-chat-id uid to-localpart)))
                   (unless chat-id
                     (error 'stanza-error
                            :defined-condition "item-not-found"
                            :text "Couldn't find a WhatsApp chat with that JID."
                            :type "modify"))
                   (unless to-resource
                     (error 'stanza-error
                            :defined-condition "jid-malformed"
                            :text "Please specify a room nickname."
                            :type "modify"))
                   (handle-muc-join comp from to-localpart chat-id to-resource)))
             (stanza-error (e)
               (send-stanza-error comp
                                  :stanza-type "presence"
                                  :id id :to from :from to
                                  :e e))))
          (t nil))))))

(defun whatsxmpp-presence-probe-handler (comp &key from to id &allow-other-keys)
  "Handles presence probe requests."
  (with-component-data-lock (comp)
    (multiple-value-bind (to-hostname to-localpart)
        (parse-jid to)
      (declare (ignore to-hostname))
      (format *debug-io* "~&presence probe to: ~A from: ~A~%" to from)
      (let* ((stripped (strip-resource from))
             (uid (get-user-id stripped))
             (conn (gethash stripped (component-whatsapps comp))))
        (flet ((respond-with-unavailable ()
                 (with-presence (comp from
                                 :from to
                                 :type "unavailable"
                                 :id id))))
          (cond
            ((equal to-localpart "admin")
             (multiple-value-bind (admin-status admin-show)
                 (get-admin-status comp stripped)
               (admin-presence comp from admin-status admin-show)))
            ((or (not uid) (not conn)) (respond-with-unavailable))
            ((get-contact-name uid to-localpart)
             (handle-wa-contact-presence comp conn stripped to-localpart
                                         :destination from))
            (t (respond-with-unavailable))))))))

(defun whatsxmpp-presence-subscribe-handler (comp &key from to id &allow-other-keys)
  "Handles a presence subscription request."
  (with-component-data-lock (comp)
    (multiple-value-bind (to-hostname to-localpart)
        (parse-jid to)
      (declare (ignore to-hostname))
      (format *debug-io* "~&presence subscribe from: ~A~%" from)
      (if (or (equal to-localpart "admin") (whatsxmpp-localpart-to-wa-jid to-localpart))
          (with-presence (comp (strip-resource from)
                          :from to
                          :type "subscribed"))
          (send-stanza-error comp
                             :stanza-type "presence"
                             :id id :to from :from to
                             :e (make-condition 'stanza-error
                                                :defined-condition "item-not-found"
                                                :text "That user's JID isn't in a recognizable format."
                                                :type "modify"))))))

(defun whatsxmpp-chat-state-handler (comp &key from to type &allow-other-keys)
  "Handles a chat state sent to the whatsxmpp bridge."
  (with-component-data-lock (comp)
    (multiple-value-bind (to-hostname to-localpart)
        (parse-jid to)
      (declare (ignore to-hostname))
      (format *debug-io* "~&chat state: ~A is ~A to ~A~%" from type to)
      (let* ((stripped (strip-resource from))
             (uid (get-user-id stripped))
             (conn (gethash stripped (component-whatsapps comp)))
             (wa-jid (whatsxmpp-localpart-to-wa-jid to-localpart))
             (presence-type
               (cond
                 ((string= type "composing") :composing)
                 ((string= type "paused") :paused)
                 ((string= type "active") :available)
                 ((string= type "gone") :unavailable)
                 (t (return-from whatsxmpp-chat-state-handler)))))
        (unless uid
          (warn "Got chat state for user that isn't registered")
          (return-from whatsxmpp-chat-state-handler))
        (unless wa-jid
          (return-from whatsxmpp-chat-state-handler))
        (unless conn
          (warn "Can't send chat state, since user connection is offline")
          (return-from whatsxmpp-chat-state-handler))
        (whatscl::send-presence conn presence-type
                                (unless (or (eql presence-type :available)
                                            (eql presence-type :unavailable))
                                  wa-jid))))))

(defun whatsxmpp-marker-handler (comp &key from to type marker-id id &allow-other-keys)
  "Handles a message marker sent to the whatsxmpp bridge."
  (with-component-data-lock (comp)
    (multiple-value-bind (to-hostname to-localpart)
        (parse-jid to)
      (declare (ignore to-hostname))
      (format *debug-io* "~&marker: ~A on ~A from ~A~%" type marker-id from)
      (unless (equal type "displayed")
        (return-from whatsxmpp-marker-handler))
      (let* ((stripped (strip-resource from))
             (uid (get-user-id stripped))
             (conn (gethash stripped (component-whatsapps comp)))
             (wa-jid (whatsxmpp-localpart-to-wa-jid to-localpart)))
        (unless uid
          (warn "Got marker for user that isn't registered")
          (return-from whatsxmpp-marker-handler))
        (unless wa-jid
          (return-from whatsxmpp-marker-handler))
        (unless conn
          (warn "Can't send marker, since user connection is offline")
          (send-stanza-error comp
                             :id id :from to :to from
                             :stanza-type "message"
                             :e (make-condition 'stanza-error
                                                :defined-condition "recipient-unavailable"
                                                :text "Can't process chat marker: you're currently not connected to WhatsApp."
                                                :type "wait"))
          (return-from whatsxmpp-marker-handler))
        (let ((wa-msgid (lookup-xmpp-msgid uid marker-id)))
          (if wa-msgid
              (progn
                (format *debug-io* "~&marking read for ~A: ~A from ~A~%" stripped wa-msgid wa-jid)
                (whatscl::send-message-read conn wa-jid wa-msgid))
              (warn "Got marker for unknown XMPP message ID ~A" marker-id)))))))

(defun whatsxmpp-message-handler (comp &key from to body id oob-url &allow-other-keys)
  "Handles a message sent to the whatsxmpp bridge."
  (with-component-data-lock (comp)
    (multiple-value-bind (to-hostname to-localpart to-resource)
        (parse-jid to)
      (declare (ignore to-hostname))
      (format *debug-io* "~&message from: ~A~%" from)
      (let* ((stripped (strip-resource from))
             (uid (get-user-id stripped))
             (conn (gethash stripped (component-whatsapps comp)))
             (wa-jid (whatsxmpp-localpart-to-wa-jid to-localpart))
             (user-resource (get-user-chat-resource uid to-localpart)))
        (labels
            ((send-error (e)
               (send-stanza-error comp
                                  :stanza-type "message"
                                  :id id :to from :from to
                                  :e e)))
          (cond
            ((equal to-localpart "admin")
             (handle-admin-command comp from body uid))
            ((not uid)
             (send-error (make-condition 'stanza-error
                                         :defined-condition "registration-required"
                                         :text "You must register to use this bridge."
                                         :type "auth")))
            ((not wa-jid)
             (send-error (make-condition 'stanza-error
                                         :defined-condition "item-not-found"
                                         :text "That user's JID isn't in a recognizable format."
                                         :type "modify")))
            ((or (not conn) (not (whatscl::wac-jid conn)))
             (send-error (make-condition 'stanza-error
                                         :defined-condition "recipient-unavailable"
                                         :text "You're currently not connected to WhatsApp."
                                         :type "wait")))
            ((and to-resource (uiop:string-prefix-p "g" to-localpart))
             (send-error (make-condition 'stanza-error
                                         :defined-condition "feature-not-implemented"
                                         :text "MUC PMs are (deliberately) not implemented. Message the user directly instead."
                                         :type "cancel")))
            (t
             (let* ((content-to-send (if oob-url
                                        (maybe-upload-whatsapp-media conn oob-url)
                                        (promisify body)))
                    (callback (lambda (conn result)
                                (wa-handle-message-send-result comp conn stripped
                                                               :orig-from from
                                                               :orig-to to
                                                               :orig-id id
                                                               :orig-body body
                                                               :muc-resource user-resource
                                                               :result result))))
               (catcher
                (attach
                 content-to-send
                 (lambda (content)
                   (let ((msgid
                           (etypecase content
                             (whatscl::message-contents-image (whatscl::send-simple-image-message conn wa-jid content callback))
                             (string (whatscl::send-simple-text-message conn wa-jid content callback)))))
                     (whatscl::send-presence conn :available)
                     (insert-user-message uid id msgid))))
                (t (e)
                   (format *error-output* "~&failed to send message! ~A~%" e)
                   (send-error (make-condition 'stanza-error
                                               :defined-condition "internal-server-error"
                                               :text (princ-to-string e)
                                               :type "wait"))))))))))))

(defun whatsxmpp-load-users (comp)
  (with-component-data-lock (comp)
    (with-prepared-statement
        (stmt "SELECT jid FROM users;")
        (loop
          while (sqlite:step-statement stmt)
          do (with-bound-columns (jid) stmt
               (setf (gethash jid (component-whatsapps comp)) nil))))))

(defun register-whatsxmpp-handlers (comp)
  (register-component-iq-handler comp :disco-info #'disco-info-handler)
  (register-component-iq-handler comp :vcard-temp-get #'whatsxmpp-vcard-temp-handler)
  (register-component-iq-handler comp :disco-items #'disco-items-handler))

(defun whatsxmpp-init ()
  "Initialise the whatsxmpp bridge."
  (connect-database)
  (with-prepared-statement
      (config "SELECT server, port, component_name, shared_secret, upload_component_name FROM configuration WHERE rev = 1")
    (assert (sqlite:step-statement config) () "No configuration in database!")
    (destructuring-bind (server port component-name shared-secret upload-name)
        (column-values config)
      (let* ((comp (make-component server port shared-secret component-name))
             (ret (change-class comp 'whatsxmpp-component
                                :upload-component-name upload-name)))
        (on :text-message ret (lambda (&rest args)
                                (apply #'whatsxmpp-message-handler ret args)))
        (on :message-marker ret (lambda (&rest args)
                                  (apply #'whatsxmpp-marker-handler ret args)))
        (on :presence-subscribe ret (lambda (&rest args)
                                      (apply #'whatsxmpp-presence-subscribe-handler ret args)))
        (on :presence-probe ret (lambda (&rest args)
                                  (apply #'whatsxmpp-presence-probe-handler ret args)))
        (on :presence-unavailable ret (lambda (&rest args)
                                  (apply #'whatsxmpp-presence-unavailable-handler ret args)))
        (on :presence ret (lambda (&rest args)
                            (apply #'whatsxmpp-presence-handler ret args)))
        (on :chat-state ret (lambda (&rest args)
                            (apply #'whatsxmpp-chat-state-handler ret args)))
        (register-whatsxmpp-handlers ret)
        (whatsxmpp-load-users ret)
        (setf (component-reconnect-timer ret) (trivial-timers:make-timer
                                               (lambda () (wa-resetup-users ret))
                                               :name "reconnection timer"))
        (on :connected ret (lambda () (wa-resetup-users ret)))
        ret))))

#+sbcl
(defparameter *comp* nil)

#+sbcl
(defun report-error-and-die (err)
  (format *error-output* "[!] Fatal error, bridge aborting!~%")
  (trivial-backtrace:print-backtrace err
                                     :output *error-output*)
  (loop
    for thr in (bt:all-threads)
    do (progn
         (format *error-output* "[!] State of thread ~A:~%" thr)
         (sb-thread:interrupt-thread thr (lambda ()
                                           (sb-debug:print-backtrace
                                            :stream *error-output*)))))
  (sb-ext:exit :code 1 :abort t))

#+sbcl
(defun main ()
  "Hacky main() function for running this in 'the real world' (outside emacs)"
  (setf *debugger-hook* (lambda (condition hook)
                          (declare (ignore hook))
                          (report-error-and-die condition)))
  (when (< (length sb-ext:*posix-argv*) 2)
    (format *error-output* "fatal: a path to the database must be provided~%")
    (format *error-output* "usage: ~A DATABASE_PATH~%" (elt sb-ext:*posix-argv* 0))
    (sb-ext:exit :code 2 :abort t))
  (let ((*default-database-path* (elt sb-ext:*posix-argv* 1)))
    (format t "[*] whatsxmpp version ~A / an eta project <https://theta.eu.org>~%" +version+)
    (format t "[+] Using database at ~A~%" *default-database-path*)
    #+use-swank
    (block nil
      (format t "[+] Starting SWANK server~%")
      (setf swank:*configure-emacs-indentation* nil)
      (swank:create-server :dont-close t))
    (format t "[+] Initializing bridge~%")
    (setf *comp* (whatsxmpp-init))
    (on :error *comp* (lambda (e)
                        (report-error-and-die e)))
    ;; don't pretty-print stuff with newlines
    (setf *print-right-margin* most-positive-fixnum)
    ;; We don't have anything better to do, so let's wait on a condition
    ;; variable that'll never wake up.
    (let ((lock (bt:make-lock))
          (condvar (bt:make-condition-variable)))
      (loop
        (bt:with-lock-held (lock)
          (bt:condition-wait condvar lock))))))

#+use-swank
(uiop:register-image-dump-hook
 (lambda ()
   (swank-loader:init
    :load-contribs t
    :reload t)
   (swank:swank-require '("SWANK-FANCY-INSPECTOR" "SWANK-FUZZY" "SWANK-MACROSTEP" "SWANK-PRESENTATIONS"
                          "SWANK-REPL" "SWANK-PACKAGE-FU" "SWANK-TRACE-DIALOG" "SWANK-INDENTATION"
                          "SWANK-SBCL-EXTS" "SWANK-ARGLISTS" "SWANK-C-P-C" "SWANK-UTIL" "SB-CLTL2"
                          "SB-INTROSPECT" "SB-BSD-SOCKETS" "SB-POSIX" "ASDF" "asdf" "UIOP" "uiop"))))
