(defsystem "whatsxmpp"
  :depends-on ("usocket" "bordeaux-threads" "event-emitter" "blackbird" "cxml" "sha1" "uuid" "sqlite")
  :serial t
  :components
  ((:file "packages")
   (:file "sqlite")
   (:file "stuff")))
