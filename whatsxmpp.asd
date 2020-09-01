(defsystem "whatsxmpp"
  :depends-on ("usocket" "bordeaux-threads" "event-emitter" "blackbird" "cxml" "ironclad" "uuid" "sqlite" "whatscl" "drakma" "local-time" "trivial-timers" "trivial-backtrace" "trivial-mimes" "opticl")
  :serial t
  :build-operation "program-op"
  :build-pathname "whatsxmpp"
  :entry-point "whatsxmpp::main"
  :components
  ((:file "packages")
   (:file "utils")
   (:file "namespaces")
   (:file "component")
   (:file "xmpp")
   (:file "xep-0030")
   (:file "xep-0363")
   (:file "xep-0115")
   (:file "sqlite")
   (:file "db")
   (:file "media")
   (:file "stuff")))
