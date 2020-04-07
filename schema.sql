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
