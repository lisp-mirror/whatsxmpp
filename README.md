# whatsxmpp

[![GNU AGPLv3 licensed](https://www.gnu.org/graphics/agplv3-155x51.png)](https://www.gnu.org/licenses/agpl-3.0.en.html)
[![XMPP chatroom: whatsxmpp@conf.theta.eu.org](https://inverse.chat/badge.svg?room=whatsxmpp@conf.theta.eu.org)](xmpp:whatsxmpp@conf.theta.eu.org?join)
![Maintenance](https://img.shields.io/maintenance/yes/2020.svg)

![Lisp warning](http://www.lisperati.com/lisplogo_warning_256.png)

A WhatsApp Web transport for the [Extensible Messaging and Presence Protocol (XMPP)](https://xmpp.org/), otherwise known as Jabber. (alpha!)

*This is the spiritual successor of [sms-irc](https://git.theta.eu.org/eta/sms-irc), a similar project that works with IRC instead of XMPP.*

## What is this?

This is a multi-user transport for WhatsApp, using the [whatscl](https://git.theta.eu.org/eta/whatscl)
library for Common Lisp. By scanning a QR code generated by the bridge with the WhatsApp
app on your phone, you can send and receive messages and media with your Jabber ID.

**Note:** You currently need an XMPP server of your own to try this. It's only been tested
with [prosody 0.11](https://prosody.im/) as of the time of writing - and there are some
additional caveats: take a look at the requirements list.

## What works?

- Sending private messages/DMs both ways
- *Basic* support for MUCs
- Magically populating your roster using [XEP-0144: Roster Item Exchange](https://xmpp.org/extensions/xep-0144.html)
- Downloading/decrypting media from WhatsApp and uploading it to your XEP-0363 server
- Avatars
- Read receipts
- Status text
- Typing notifications / chat state

## What doesn't yet?

- [XEP-0313: Message Archive Management](https://xmpp.org/extensions/xep-0313.html) in MUCs (DMs should be done by your server)
- Support for users joining and leaving MUCs
- Support for the topic changing in MUCs
- Uploading media to WhatsApp (currently, it just comes through as a link)
- Probably other stuff

## What you'll need

- An XMPP server (we recommend [prosody](https://prosody.im/), but it might also work with ejabberd; let us know!)
  - You need to set up a new *external component* for the bridge ([see prosody doc](https://prosody.im/doc/components)).
  - In addition, you **must** configure an [XEP-0363 (HTTP File Upload)](https://xmpp.org/extensions/xep-0363.html) component. ([see prosody doc](https://modules.prosody.im/mod_http_upload.html))
    - **WARNING:** Prosody's `mod_http_upload` does not allow the bridge to use it, as of the time of writing (2020-05-28). You will need to replace `mod_http_upload.lua` in your community modules directory with `doc/mod_http_upload.lua` from this repository for it to work.
- An installation of [Docker](https://www.docker.com/)
  - You *can* try and run the bridge without Docker. However, we really don't recommend it, especially if you aren't familiar with Common Lisp.
  - Ask in the support MUC (link at the top of this file) if you want to do this.
- [SQLite](https://www.sqlite.org/) installed (specifically the `sqlite3` command).

## Instructions

### Step 1: configure your XMPP server

Make sure you've followed the links above to set up XEP-0363 and an external component for the bridge. With prosody, your config might look something like:

```
Component "upload.capulet.lit" "http_upload"
        http_upload_file_size_limit = 104857600

Component "whatsapp.capulet.lit"
        component_secret = "juliet"
```

**WARNING**: Unless you want to run a public bridge (not recommended at this time), limit
access to the external component to only people on your server. (On prosody, add
`modules_disabled = { "s2s" }` to the component configuration.)

### Step 2: set up the database and storage for the bridge

I'm going to assume you want to store bridge data at the path `/wx`. Replace this path
with wherever you actually want to put the data in the commands below.

Set up the database schema:

```
$ sqlite3 /wx/data.sqlite3 < ./schema.sql
```

Then, configure the bridge -- replacing the values below with the actual values:

```
$ sqlite3 /wx/data.sqlite3
SQLite version 3.31.1 2020-01-27 19:55:54
Enter ".help" for usage hints.
sqlite> INSERT INTO configuration (rev, server, port, component_name, shared_secret, upload_component_name) VALUES (1, "capulet.lit", 5347, "whatsapp.capulet.lit", "juliet", "upload.capulet.lit");
sqlite> .quit
```

A few things to note here:

- The `port` is whatever port your XMPP server accepts incoming component connections on (*not* client-to-server or server-to-server connections!). For prosody, the default is 5347.
- The `component_name` is whatever you specified in the prosody config for this component.
- The `shared_secret` is the same as the `component_secret`.
- The `upload_component_name` is the name of the XEP-0363 HTTP Upload component.

### Step 3: run the bridge

You can build the Docker image yourself from the `Dockerfile` in the repo, or you can just
use my hosted copy. (Don't download it a ton you guys, I pay for this stuff.)

You'll want to pass through the directory (`/wx`, or whatever it is) where you created
the database as a Docker volume, so the container can access it. Then, supply the path
to the database as the first argument to the container.

The hosted image is called `eu.gcr.io/etainfra/whatsxmpp`. Using the `docker` CLI,
you might run the bridge like so:

```
docker run \
    --name whatsxmpp \
    --restart always \
    -v /wx:/data \
    eu.gcr.io/etainfra/whatsxmpp \
    /data/data.sqlite3
```

Consult the [Docker reference](https://docs.docker.com/engine/reference/run/) for more details
on this step, including how to run the image in the background and more!

If you see `Connection complete! \o/` in the logs, it's working!

### Step 4: set up the WhatsApp Web part

You'll interact with the bridge by talking to `admin@whatsapp.capulet.lit` (well,
the last part of that JID will be different depending on your setup). Send this user
`help` to check that the bridge is working (you should get some help text).

To set it up, have your phone at the ready to scan the QR code (Menu -> WhatsApp Web).
Then, send `register` to the admin user, and scan the QR code you're given.

(Did it break at this part and give you a nasty error? Check your XEP-0363 HTTP Upload
service is working correctly, and allows the bridge to use it as described above!)

You should then receive a crapton of presence subscription requests and MUC invites
for everyone you are remotely related to on WhatsApp, and for all the WhatsApp groups
you're in, and the bridge is done!

**Tip:** If your client supports [XEP-0144: Roster Item Exchange](https://xmpp.org/extensions/xep-0144.html) (Gajim on desktop is good for this), send `getroster` to the admin user
to pop up a window where you can insert all your WhatsApp contacts in one go!

## Support

[![XMPP chatroom: whatsxmpp@conf.theta.eu.org](https://inverse.chat/badge.svg?room=whatsxmpp@conf.theta.eu.org)](xmpp:whatsxmpp@conf.theta.eu.org?join)

Come join us in [whatsxmpp@conf.theta.eu.org](xmpp:whatsxmpp@conf.theta.eu.org?join) if you
have questions or issues using the bridge.