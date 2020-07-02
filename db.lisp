(in-package :whatsxmpp)

(defun get-user-id (jid)
  "Get the user ID of JID, or NIL if none exists."
  (with-prepared-statement
      (get-user "SELECT id FROM users WHERE jid = ?")
    (let ((stripped (strip-resource jid)))
      (bind-parameters get-user stripped)
      (when (sqlite:step-statement get-user)
        (first (column-values get-user))))))

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

(defun get-contact-name (uid localpart)
  "Get a name for LOCALPART, a possible contact for the user with ID UID."
  (with-prepared-statements
      ((get-stmt "SELECT name, notify FROM user_contacts WHERE user_id = ? AND wa_jid = ?"))
    (bind-parameters get-stmt uid localpart)
    (when (sqlite:step-statement get-stmt)
      (with-bound-columns (name notify) get-stmt
        (or name notify (substitute #\+ #\u localpart))))))

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
      ((insert-stmt "INSERT INTO user_chats (user_id, wa_jid) VALUES (?, ?)"))
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