BEGIN;
CREATE TABLE new_user_contacts (
       id INTEGER PRIMARY KEY,
       user_id INT NOT NULL REFERENCES users,
       wa_jid VARCHAR NOT NULL,
       subscription_state VARCHAR NOT NULL DEFAULT 'none',
       avatar_url VARCHAR,
       name VARCHAR,
       notify VARCHAR,
       status VARCHAR,
       UNIQUE(user_id, wa_jid)
);

INSERT INTO new_user_contacts SELECT * FROM user_contacts;
DROP TABLE user_contacts;
ALTER TABLE new_user_contacts RENAME TO user_contacts;
CREATE INDEX user_contacts_idx ON user_contacts (user_id, wa_jid);
COMMIT;
