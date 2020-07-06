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
   #:match-dir
   #:update
   #:get-path
   #:call-on-url-or-path
   #:view-file
   #:view-file-native
   #:view-file-slime
   #:sort-dir
   #:view-matches
   #:make-tag
   #:get-tag
   #:set-tag
   #:with-tag
   #:tag-not-found
   #:get-local-path
   ))
