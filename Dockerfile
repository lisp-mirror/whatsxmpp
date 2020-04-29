FROM rigetti/lisp AS whatscl-compiled

RUN apt update -y && apt install -y git
RUN git clone https://github.com/eeeeeta/websocket-driver /src/websocket-driver
RUN git clone https://git.theta.eu.org/whatscl.git /src/whatscl
ADD ./ /src/whatsxmpp
WORKDIR /src/whatsxmpp
RUN make

FROM debian:stable-slim AS whatsxmpp
WORKDIR /wx
RUN apt-get update && apt-get install -y libz-dev libsqlite3-dev libssl-dev ca-certificates
RUN apt-get clean && rm -rf /var/lib/apt/lists/*
RUN mkdir /wx-data
COPY --from=whatscl-compiled /src/whatsxmpp/whatsxmpp /wx/whatsxmpp
ENTRYPOINT "/wx/whatsxmpp"
