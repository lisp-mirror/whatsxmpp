CREATE TABLE configuration (
       rev INT PRIMARY KEY,
       server VARCHAR NOT NULL,
       port INT NOT NULL,
       component_name VARCHAR NOT NULL,
       shared_secret VARCHAR NOT NULL,
       upload_component_name VARCHAR NOT NULL
);

CREATE TABLE users (
       id INTEGER PRIMARY KEY,
       jid VARCHAR UNIQUE NOT NULL,
       session_data VARCHAR
);

CREATE TABLE user_contacts (
       id INTEGER PRIMARY KEY,
       user_id INT NOT NULL REFERENCES users,
       wa_jid VARCHAR UNIQUE NOT NULL,
       subscription_state VARCHAR NOT NULL DEFAULT 'none',
       avatar_url VARCHAR,
       name VARCHAR,
       notify VARCHAR,
       status VARCHAR
);

CREATE TABLE user_messages (
       id INTEGER PRIMARY KEY,
       user_id INT NOT NULL REFERENCES users,
       xmpp_id VARCHAR NOT NULL,
       wa_id VARCHAR NOT NULL,
       UNIQUE(user_id, wa_id)
);

CREATE TABLE avatar_data (
       avatar_url VARCHAR NOT NULL PRIMARY KEY,
       sha1 VARCHAR NOT NULL,
       image BLOB NOT NULL
);

CREATE TABLE user_chats (
       id INTEGER PRIMARY KEY,
       user_id INT NOT NULL REFERENCES users,
       wa_jid VARCHAR NOT NULL,
       user_resource VARCHAR,
       invitation_state VARCHAR NOT NULL DEFAULT 'none',
       subject VARCHAR
);

CREATE TABLE user_chat_members (
       id INTEGER PRIMARY KEY,
       chat_id INT NOT NULL REFERENCES user_chats,
       wa_jid VARCHAR NOT NULL,
       resource VARCHAR NOT NULL,
       affiliation VARCHAR NOT NULL DEFAULT 'member'
);

CREATE TABLE user_chat_joined (
       id INTEGER PRIMARY KEY,
       chat_id INT NOT NULL REFERENCES user_chats,
       jid VARCHAR NOT NULL
);
