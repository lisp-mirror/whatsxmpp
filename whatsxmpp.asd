(defsystem "whatsxmpp"
  :depends-on ("usocket" "bordeaux-threads" "event-emitter" "blackbird" "cxml" "ironclad" "uuid" "sqlite" "whatscl" "drakma" "local-time" "trivial-timers" "swank" "trivial-backtrace" "trivial-mimes")
  :serial t
  :build-operation "program-op"
  :build-pathname "whatsxmpp"
  :entry-point "whatsxmpp::main"
  :components
  ((:file "packages")
   (:file "sqlite")
   (:file "stuff")))
