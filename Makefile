LISP ?= sbcl

all:
	$(LISP) \
		--eval '(ql:quickload :whatsxmpp)' \
		--eval '(asdf:make :whatsxmpp)' \
		--eval '(quit)'
