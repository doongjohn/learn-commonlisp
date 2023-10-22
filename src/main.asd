(asdf:defsystem "main"
  :depends-on ("uiop")
  :components ((:file "ansi-esc")
               (:file "main" :depends-on ("ansi-esc"))))
