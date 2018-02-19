;;;; package.lisp

(defpackage #:match-client
  (:use #:cl)
  (:export
   #:add-local
   #:add-url
   #:add-resized
   #:delete-path
   #:match
   #:match-resized
   #:update
   #:get-path
   #:call-on-url-or-path
   ))

