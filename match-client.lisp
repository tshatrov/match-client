(in-package #:match-client)

(defvar *base-url* "")
(defvar *local-tag* "")
(defvar *cache-file* "")
(defvar *root-dirs* '())

(load (asdf:system-relative-pathname :match-client "settings.lisp") :if-does-not-exist nil)

(defun api-url (path)
  (concatenate 'string *base-url* path))

(defun get-path (path &key (use-tag t))
  (format nil "~@[[~a] ~]~a" (and use-tag *local-tag*) path))

(defun parse-request (&rest args)
  (multiple-value-bind (content return-code)
      (handler-bind ((dex:http-request-failed #'dex:ignore-and-continue))
        (apply 'dex:request args))
    (cond
      ((<= 400 return-code 499)
       (jsown:new-js
         ("status" "fail")
         ("error" content)
         ("code" return-code)))
      (t (let ((obj (jsown:parse content)))
           (jsown:extend-js obj ("code" return-code)))))))

(defun add-local (file &key path (use-tag t) (metadata "{}"))
  "Add local image to Match server"
  (parse-request
   (api-url "/add")
   :method :post
   :content `(("image" . ,(pathname file))
              ("filepath" . ,(get-path (or path file) :use-tag use-tag))
              ("metadata" . ,metadata))))

(defun delete-path (path &key (use-tag t))
  (parse-request
   (api-url "/delete")
   :method :delete
   :content `(("filepath" . ,(get-path path :use-tag use-tag)))
   ))
