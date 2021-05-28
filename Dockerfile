FROM clfoundation/sbcl:alpine3.13 AS builder

RUN apk add --no-cache git

RUN git clone https://github.com/daewok/sbcl.git /usr/local/src/sbcl/

WORKDIR /usr/local/src/sbcl

RUN set -x && git checkout static-executable-v2-2.1.3

# Install build prereq and build SBCL with sb-linkable-runtime and
# sb-prelink-linkage-table
RUN set -x \
    # I frequently build arm executables on an arm64 computer. Need to add this
    # otherwise SBCL can get confused
    && case "$(cat /etc/apk/arch)" in \
         armv7) SBCL_ARCH=arm;; \
         aarch64) SBCL_ARCH=arm64;; \
         x86_64) SBCL_ARCH=x86-64;; \
         *) echo "Unknown arch" >&2; exit 1;; \
       esac \
    && export SBCL_ARCH \
    && apk add --no-cache ca-certificates curl openssl make gcc musl-dev linux-headers gnupg patch zlib-dev zlib-static \
    # Remove the hardcoding of armv5 as target arch. Use the default provided
    # by the base image. Required when building for ARM on Alpine 3.12.
    && sed -i -e "s/CFLAGS += -marm -march=armv5/CFLAGS += -marm/" src/runtime/Config.arm-linux \
    && sh make.sh --fancy --with-sb-linkable-runtime --with-sb-prelink-linkage-table \
    && sh install.sh

RUN mkdir -vp /srcs/common-lisp/

ENV HOME /srcs

# Install Quicklisp

ADD https://beta.quicklisp.org/quicklisp.lisp /tmp/quicklisp.lisp

RUN set -x \
    && sbcl --non-interactive \
            --eval '(load "/tmp/quicklisp.lisp")' \
            --eval '(quicklisp-quickstart:install :path "/tmp/quicklisp")' \
            --eval '(ql::without-prompting (ql:add-to-init-file))'

RUN apk add --no-cache sqlite-static openssl-libs-static

# Clone sources

RUN set -x && git clone https://github.com/eeeeeta/websocket-driver /srcs/common-lisp/websocket-driver
RUN set -x && git clone https://git.theta.eu.org/eta/whatscl.git /srcs/common-lisp/whatscl
COPY . /srcs/common-lisp/whatsxmpp

# Load whatsxmpp into an image, save the foreign symbols it requires, and dump the
# core.

RUN set -x \
    && sbcl --non-interactive \
            --eval '(sb-ext:restrict-compiler-policy (quote safety) 3 3)' \
            --eval '(ql:quickload :whatsxmpp)' \
            --load tools-for-build/dump-linkage-info.lisp \
            --eval '(sb-dump-linkage-info:dump-to-file "/tmp/linkage-info.sexp")' \
            --eval '(sb-ext:unlock-package (quote sb-sys))' \
            --eval '(defun sb-sys::reopen-shared-objects (&rest args) (declare (ignore args)))' \
            --eval '(sb-ext:save-lisp-and-die "/tmp/whatsxmpp.core")'

# Build a static runtime, with libsqlite3 linked and the required symbols in the
# linkage table.

RUN ls /usr/lib/

RUN set -x \
    && cat /tmp/linkage-info.sexp \
    && sbcl --script tools-for-build/create-linkage-table-prelink-info-override.lisp \
            /tmp/linkage-info.sexp \
            /tmp/linkage-table-prelink-info-override.c \
    && cat /tmp/linkage-table-prelink-info-override.c \
    && while read l; do \
         eval "${l%%=*}=\"${l#*=}\""; \
       done < /usr/local/lib/sbcl/sbcl.mk \
    && $CC $CFLAGS -Wno-builtin-declaration-mismatch -o /tmp/linkage-table-prelink-info-override.o -c /tmp/linkage-table-prelink-info-override.c \
    && $CC -no-pie -static $LINKFLAGS -o /tmp/static-sbcl /usr/local/lib/sbcl/$LIBSBCL /tmp/linkage-table-prelink-info-override.o -lsqlite3 -lssl -lcrypto $LIBS

# Use the new static runtime to load the previous core and then dump a
# compressed executable with the toplevel function set to run the sb-gmp test
# suite.
RUN set -x \
    && /tmp/static-sbcl \
        --core /tmp/whatsxmpp.core \
        --disable-ldb \
        --lose-on-corruption \
        --non-interactive \
        --eval '(push :cl+ssl-foreign-libs-already-loaded *features*)' \
        --eval '(cl+ssl:reload)' \
        --eval '(sb-ext:save-lisp-and-die "/tmp/whatsxmpp" :executable t :toplevel (function whatsxmpp::main) :compression t)'

FROM alpine

RUN apk add --no-cache ca-certificates

COPY --from=builder /tmp/whatsxmpp /whatsxmpp

ENTRYPOINT /whatsxmpp
