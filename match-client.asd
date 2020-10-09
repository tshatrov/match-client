;;;; match-client.asd

(asdf:defsystem #:match-client
  :description "Common Lisp client for https://github.com/pavlovai/match"
  :author "Timofei Shatrov <timofei.shatrov@gmail.com>"
  :license "MIT"
  :depends-on (#:dexador
               #:drakma
               #:alexandria
               #:jsown
               #:cl-store
               #:lparallel
               #:cl-ppcre
               #:cl-graphicsmagick
               )
  :serial t
  :components ((:file "package")
               (:file "image")
               (:file "tag")
               (:file "cache")
               (:file "match-client")
               (:file "viewer")
               ))

(asdf:defsystem #:match-client/twigaten
  :description "Twigaten search addon"
  :author "Timofei Shatrov <timofei.shatrov@gmail.com>"
  :license "MIT"
  :depends-on (#:match-client
               #:cxml
               #:closure-html
               #:css-selectors
               #:xpath
               )
  :serial t
  :components ((:file "twigaten"))
  )


(asdf:defsystem #:match-client/saucenao
  :description "Saucenao search addon"
  :author "Timofei Shatrov <timofei.shatrov@gmail.com>"
  :license "MIT"
  :depends-on (#:match-client
               #:cxml
               #:closure-html
               #:css-selectors
               #:parse-float
               )
  :serial t
  :components ((:file "saucenao"))
  )


(asdf:defsystem #:match-client/all
  :description "All match-client addons"
  :author "Timofei Shatrov <timofei.shatrov@gmail.com>"
  :license "MIT"
  :depends-on (#:match-client/twigaten
               #:match-client/saucenao
               )
  )
