(defsystem "whatsxmpp"
  :depends-on ("usocket" "bordeaux-threads" "event-emitter" "blackbird" "cxml" "ironclad" "uuid" "sqlite" "whatscl" "drakma")
  :serial t
  :components
  ((:file "packages")
   (:file "sqlite")
   (:file "stuff")))
