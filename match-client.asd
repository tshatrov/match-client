;;;; match-client.asd

(asdf:defsystem #:match-client
  :description "Common Lisp client for https://github.com/pavlovai/match"
  :author "Timofei Shatrov <timofei.shatrov@gmail.com>"
  :license "MIT"
  :depends-on (#:dexador
               #:jsown)
  :serial t
  :components ((:file "package")
               (:file "match-client")))

