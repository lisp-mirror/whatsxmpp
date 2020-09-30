BEGIN;
ALTER TABLE configuration ADD COLUMN allow_archiving BOOL NOT NULL DEFAULT false;
ALTER TABLE configuration ADD COLUMN allow_history_fetches BOOL NOT NULL DEFAULT false;
ALTER TABLE users ADD COLUMN enable_archiving BOOL NOT NULL DEFAULT false;
CREATE TABLE administrators (
	id INTEGER PRIMARY KEY,
	jid VARCHAR UNIQUE NOT NULL
);
CREATE TABLE user_chat_history (
  id INTEGER PRIMARY KEY,
  user_id INT NOT NULL REFERENCES users,
  chat_id INT NOT NULL REFERENCES user_chats,
  user_from VARCHAR NOT NULL,
  ts_unix INT NOT NULL,
  xmpp_id VARCHAR NOT NULL,
  orig_id VARCHAR,
  body VARCHAR NOT NULL,
  oob_url VARCHAR
);
CREATE UNIQUE INDEX user_chat_history_unique ON user_chat_history (user_id, chat_id, xmpp_id);
COMMIT;
