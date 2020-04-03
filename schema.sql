CREATE TABLE users (
       id SERIAL PRIMARY KEY,
       jid VARCHAR UNIQUE NOT NULL,
       session_data VARCHAR
);

CREATE TABLE user_contacts (
       id SERIAL PRIMARY KEY,
       user_id INT NOT NULL REFERENCES users,
       wa_jid VARCHAR UNIQUE NOT NULL,
       subscription_state VARCHAR NOT NULL DEFAULT 'none',
       avatar_url VARCHAR,
       name VARCHAR,
       notify VARCHAR
);
