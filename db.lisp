(in-package :whatsxmpp)

(defun get-user-id (jid)
  "Get the user ID of JID, or NIL if none exists."
  (with-prepared-statement
      (get-user "SELECT id FROM users WHERE jid = ?")
    (let ((stripped (strip-resource jid)))
      (bind-parameters get-user stripped)
      (when (sqlite:step-statement get-user)
        (first (column-values get-user))))))

(defun get-user-jid (id)
  "Get the user JID for the ID, or NIL if none exists."
  (with-prepared-statement
      (get-user "SELECT jid FROM users WHERE id = ?")
      (bind-parameters get-user id)
    (when (sqlite:step-statement get-user)
      (first (column-values get-user)))))

(defun get-user-contact-localparts (uid)
  "Returns a list of all contact localparts for UID."
  (with-prepared-statements
      ((get-stmt "SELECT wa_jid FROM user_contacts WHERE user_id = ?"))
    (bind-parameters get-stmt uid)
    (loop
      while (sqlite:step-statement get-stmt)
      collect (sqlite:statement-column-value get-stmt 0))))

(defun get-user-chat-id (uid localpart)
  "Get the user chat ID of LOCALPART for UID, or NIL if none exists."
  (with-prepared-statements
      ((get-stmt "SELECT id FROM user_chats WHERE user_id = ? AND wa_jid = ?"))
    (bind-parameters get-stmt uid localpart)
    (when (sqlite:step-statement get-stmt)
      (with-bound-columns (id) get-stmt
        id))))

(defun get-user-chat-localpart (chat-id)
  "Get the user chat localpart for CHAT-ID, or NIL if none exists."
  (with-prepared-statements
      ((get-stmt "SELECT wa_jid FROM user_chats WHERE id = ?"))
    (bind-parameters get-stmt chat-id)
    (when (sqlite:step-statement get-stmt)
      (with-bound-columns (localpart) get-stmt
        localpart))))

(defun get-user-chat-subject (uid localpart)
  "Get the user chat subject of LOCALPART for UID, or NIL if none exists."
  (with-prepared-statements
      ((get-stmt "SELECT subject FROM user_chats WHERE user_id = ? AND wa_jid = ?"))
    (bind-parameters get-stmt uid localpart)
    (when (sqlite:step-statement get-stmt)
      (with-bound-columns (subject) get-stmt
        subject))))

(defun get-user-chat-resource (uid localpart)
  "Get the user chat resource of LOCALPART for UID, or NIL if none exists."
  (with-prepared-statements
      ((get-stmt "SELECT user_resource FROM user_chats WHERE user_id = ? AND wa_jid = ?"))
    (bind-parameters get-stmt uid localpart)
    (when (sqlite:step-statement get-stmt)
      (with-bound-columns (resource) get-stmt
        (when (and resource (> (length resource) 0))
          resource)))))

(defun get-participant-resource (chat-id localpart)
  "Get the participant resource for LOCALPART in CHAT-ID, or NIL if none exists."
  (with-prepared-statements
      ((get-stmt "SELECT resource FROM user_chat_members WHERE chat_id = ? AND wa_jid = ?"))
    (bind-parameters get-stmt chat-id localpart)
    (when (sqlite:step-statement get-stmt)
      (with-bound-columns (resource) get-stmt
        (when (and resource (> (length resource) 0))
          resource)))))

(defun get-user-chat-joined (uid localpart)
  "Get the user chat resource of LOCALPART for UID, or NIL if none exists."
  (with-prepared-statements
      ((get-stmt "SELECT ucj.jid FROM user_chats AS uc, user_chat_joined AS ucj WHERE uc.user_id = ? AND uc.wa_jid = ? AND uc.id = ucj.chat_id"))
    (bind-parameters get-stmt uid localpart)
    (loop
      while (sqlite:step-statement get-stmt)
      append (column-values get-stmt))))

(defun get-contact-name (uid localpart &key no-phone-number)
  "Get a name for LOCALPART, a possible contact for the user with ID UID."
  (with-prepared-statements
      ((get-stmt "SELECT name, notify FROM user_contacts WHERE user_id = ? AND wa_jid = ?"))
    (bind-parameters get-stmt uid localpart)
    (when (sqlite:step-statement get-stmt)
      (with-bound-columns (name notify) get-stmt
        (or name notify (unless no-phone-number (substitute #\+ #\u localpart)))))))

(defun get-contact-status (uid localpart)
  "Get the contact status text for LOCALPART, a possible contact for the user with ID UID."
  (declare (type integer uid) (type string localpart))
  (with-prepared-statements
      ((get-stmt "SELECT status FROM user_contacts WHERE user_id = ? AND wa_jid = ?"))
    (bind-parameters get-stmt uid localpart)
    (when (sqlite:step-statement get-stmt)
      (with-bound-columns (status) get-stmt
        status))))

(defun insert-user-message (uid xmpp-id wa-id)
  "Inserts a mapping between the message IDs XMPP-ID and WA-ID for the user UID."
  (with-prepared-statements
      ((insert-stmt "INSERT INTO user_messages (user_id, xmpp_id, wa_id) VALUES (?, ?, ?)"))
    (bind-parameters insert-stmt uid xmpp-id wa-id)
    (sqlite:step-statement insert-stmt)))

(defun insert-user-chat (uid wa-id)
  "Inserts a user chat with localpart WA-ID into the database for the user with UID."
  (with-prepared-statements
      ((insert-stmt "INSERT INTO user_chats (user_id, wa_jid) VALUES (?, ?) ON CONFLICT DO NOTHING"))
    (bind-parameters insert-stmt uid wa-id)
    (sqlite:step-statement insert-stmt)))

(defun lookup-wa-msgid (uid wa-msgid)
  "Look up the XMPP ID for the WhatsApp message ID WA-MSGID, when received for the user UID."
  (with-prepared-statements
      ((get-stmt "SELECT xmpp_id FROM user_messages WHERE user_id = ? AND wa_id = ?"))
    (bind-parameters get-stmt uid wa-msgid)
    (when (sqlite:step-statement get-stmt)
      (with-bound-columns (xid) get-stmt
        xid))))

(defun lookup-xmpp-msgid (uid xmpp-msgid)
  "Look up the WhatsApp message ID for the XMPP message ID XMPP-MSGID, when received for the user UID."
  (with-prepared-statements
      ((get-stmt "SELECT wa_id FROM user_messages WHERE user_id = ? AND xmpp_id = ?"))
    (bind-parameters get-stmt uid xmpp-msgid)
    (when (sqlite:step-statement get-stmt)
      (with-bound-columns (wid) get-stmt
        wid))))

(defun get-contact-localparts (uid)
  "Get a list of contact localparts for the user with ID UID."
  (with-prepared-statements
      ((get-stmt "SELECT wa_jid FROM user_contacts WHERE user_id = ?"))
    (bind-parameters get-stmt uid)
    (loop
      while (sqlite:step-statement get-stmt)
      collect (with-bound-columns (localpart) get-stmt localpart))))

(defun get-user-groupchats (uid)
  "Get a list of groupchat info (cons pairs of LOCALPART . SUBJECT) for the user with ID UID."
  (with-prepared-statements
      ((get-stmt "SELECT wa_jid, subject FROM user_chats WHERE user_id = ?"))
    (bind-parameters get-stmt uid)
    (loop
      while (sqlite:step-statement get-stmt)
      collect (with-bound-columns (localpart subject) get-stmt (cons localpart subject)))))

(defun get-contact-chat-subjects (uid wa-jid)
  "Get a list of chat subjects for each chat the remote contact identifed by WA-JID is in, for the user with ID UID."
  (with-prepared-statements
      ((get-stmt "SELECT DISTINCT user_chats.subject FROM user_chats, user_chat_members WHERE user_chats.id = user_chat_members.chat_id AND user_chat_members.wa_jid = ? AND user_chats.user_id = ?"))
    (bind-parameters get-stmt wa-jid uid)
    (loop
      while (sqlite:step-statement get-stmt)
      collect (with-bound-columns (subject) get-stmt subject))))

(defun insert-xmpp-message (xm)
  "Inserts XM, a groupchat XMPP-MESSAGE, into the database."
  (assert (uiop:string-prefix-p "g" (conversation xm)) () "Tried to insert XMPP message for non-groupchat conversation ~A" (conversation xm))
  (let* ((chat-id (or
                   (get-user-chat-id (uid xm) (conversation xm))
                   (error "Couldn't find chat id for conversation ~A / uid ~A"
                          (conversation xm) (uid xm))))

         (local-time:*default-timezone* local-time:+utc-zone+)
         (ts-unix (local-time:timestamp-to-unix (timestamp xm))))
    (with-prepared-statements
        ((insert-stmt "INSERT INTO user_chat_history (user_id, chat_id, user_from, ts_unix, xmpp_id, orig_id, body, oob_url) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"))
      (bind-parameters insert-stmt (1 (uid xm)) (2 chat-id) (3 (from xm)) (4 ts-unix) (5 (xmpp-id xm)) (6 (orig-id xm)) (7 (body xm)) (8 (oob-url xm)))
      (sqlite:step-statement insert-stmt))))

(defun lookup-wa-msgid-in-history (uid wa-msgid)
  "Look up the XMPP ID for the WhatsApp message ID WA-MSGID, when received for the user UID."
  (with-prepared-statements
      ((get-stmt "SELECT xmpp_id FROM user_chat_history WHERE user_id = ? AND orig_id = ?"))
    (bind-parameters get-stmt uid wa-msgid)
    (when (sqlite:step-statement get-stmt)
      (with-bound-columns (xid) get-stmt
        xid))))

(defun user-archiving-enabled-p (uid)
  "Returns a generalized boolean for whether the user with ID UID has archiving enabled or not."
  (with-prepared-statements
      ((get-stmt "SELECT enable_archiving FROM users WHERE id = ?"))
    (bind-parameters get-stmt uid)
    (when (sqlite:step-statement get-stmt)
      (with-bound-columns (ena) get-stmt
        (not (eql ena 0))))))

(defun user-set-archiving-state (uid enabled)
  "Set the user's archiving state for the user with ID UID to ENABLED (either T or NIL)."
  (let ((ena (if enabled 1 0)))
    (with-prepared-statements
        ((set-stmt "UPDATE users SET enable_archiving = ? WHERE id = ?"))
      (bind-parameters set-stmt ena uid)
      (sqlite:step-statement set-stmt))))

(defun jid-admin-p (jid)
  "Returns a generalized boolean for whether the JID is a bridge administrator."
  (with-prepared-statements
      ((get-stmt "SELECT id FROM administrators WHERE jid = ?"))
    (bind-parameters get-stmt jid)
    (when (sqlite:step-statement get-stmt)
      t)))

(defun db-unregister-user (uid)
  "Unregister the user with ID UID."
  (with-prepared-statements
      ((remove-user-stmt "DELETE FROM users WHERE id = ?")
       (remove-contacts-stmt "DELETE FROM user_contacts WHERE user_id = ?")
       (remove-messages-stmt "DELETE FROM user_messages WHERE user_id = ?")
       (remove-chats-stmt "DELETE FROM user_chats WHERE user_id = ?")
       (get-chats-stmt "SELECT id FROM user_chats WHERE user_id = ?")
       (remove-chat-members-stmt "DELETE FROM user_chat_members WHERE chat_id = ?")
       (remove-chat-joined-stmt "DELETE FROM user_chat_joined WHERE chat_id = ?")
       (remove-chat-history-stmt "DELETE FROM user_chat_history WHERE user_id = ?"))
    (with-transaction ()
      (bind-parameters get-chats-stmt uid)
      (loop
        while (sqlite:step-statement get-chats-stmt)
        do (with-bound-columns (chatid) get-chats-stmt
             (loop
               for stmt in (list remove-chat-members-stmt remove-chat-joined-stmt remove-chat-history-stmt)
               do (progn
                    (sqlite:reset-statement stmt)
                    (bind-parameters stmt chatid)
                    (sqlite:step-statement stmt)))))
      (loop
        for stmt in (list remove-chats-stmt remove-messages-stmt remove-contacts-stmt remove-user-stmt)
        do (progn
             (sqlite:reset-statement stmt)
             (bind-parameters stmt uid)
             (sqlite:step-statement stmt))))))

(defun get-chat-history-ts (uid chat-id xmpp-id)
  "Look up the UNIX timestamp for the given UID, CHAT-ID and XMPP-ID."
  (with-prepared-statements
      ((get-stmt "SELECT ts_unix FROM user_chat_history WHERE user_id = ? AND chat_id = ? AND xmpp_id = ?"))
    (bind-parameters get-stmt uid chat-id xmpp-id)
    (when (sqlite:step-statement get-stmt)
      (with-bound-columns (tsu) get-stmt
        tsu))))

(defun query-archive (uid chat-id &key start end (limit 100) reference-stanza-id forward-page)
  "Query the chat history archive for the chat identified by CHAT-ID and UID. Optionally narrow the query using START and END (UNIX timestamps), returning at most LIMIT items (which is clamped to 100).
If an RSM REFERENCE-STANZA-ID is provided, narrow the query to be either after (T) or before (NIL) the history entry with that stanza ID, depending on the value of FORWARD-PAGE (see brackets)."
  (let ((statement (make-string-output-stream))
        (localpart (get-user-chat-localpart chat-id))
        (local-time:*default-timezone* local-time:+utc-zone+)
        (args (list chat-id uid)) ; WARNING this list is nreverse'd later!
        (items-returned 0)
        (sqlite-stmt))
    (format statement "SELECT user_from, ts_unix, xmpp_id, orig_id, body, oob_url FROM user_chat_history WHERE user_id = ? AND chat_id = ?")
    (when reference-stanza-id
      (let ((reference-ts (or
                           (get-chat-history-ts uid chat-id reference-stanza-id)
                           (error "Couldn't locate reference stanza ID ~A" reference-stanza-id))))
        (if forward-page
            (setf start reference-ts)
            (setf end reference-ts))))
    (when start
      (format statement " AND ts_unix > ?")
      (push start args))
    (when end
      (format statement " AND ts_unix < ?")
      (push end args))
    (unless limit
      (setf limit 100))
    (when (> limit 100)
      (setf limit 100)) ; clamp me owo
    ;; We copy a trick from biboumi: in order to figure out whether there are
    ;; more results if not for the limit existing, simply increment the limit
    ;; by 1 and see if you get the extra element.
    (format statement " ORDER BY ts_unix ~A LIMIT ~A" (if forward-page "ASC" "DESC") (1+ limit))
    (setf args (nreverse args))
    (bt:with-recursive-lock-held (*db-lock*)
      (let ((stmt-text (get-output-stream-string statement)))
        (setf sqlite-stmt (sqlite:prepare-statement *db* stmt-text)))
      (loop
        for param in args
        for n from 1
        do (sqlite:bind-parameter sqlite-stmt n param))
      (values
       (funcall
        (if forward-page #'identity #'nreverse)
        (loop
          while (sqlite:step-statement sqlite-stmt)
          do (incf items-returned)
          while (<= items-returned limit)
          collect (with-bound-columns (from ts-unix xmpp-id orig-id body oob-url) sqlite-stmt
                    (make-instance 'xmpp-message
                                   :uid uid
                                   :conversation localpart
                                   :from from
                                   :timestamp (local-time:unix-to-timestamp ts-unix)
                                   :xmpp-id xmpp-id
                                   :orig-id orig-id
                                   :body body
                                   :oob-url oob-url))))
       (<= items-returned limit)))))
