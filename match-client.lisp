(in-package #:match-client)

(defvar *base-url* "")
(defvar *local-tag* "")

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


(defvar *cache*)
(defvar *cache-lock* (bordeaux-threads:make-lock "match-cache-lock"))
(defvar *msg-lock* (bordeaux-threads:make-lock "match-msg-lock"))

(defun format-msg (str &rest args)
  (terpri)
  (apply 'format t str args)
  (force-output))

(defun worker (file)
  (lambda ()
    (let ((filepath (get-path (namestring (path file)))))
      (bordeaux-threads:with-lock-held (*msg-lock*)
        (format-msg "Action ~a for ~a" (status file) filepath))
      (bordeaux-threads:with-lock-held (*cache-lock*)
        (setf (status file) :ok)))))

(defun update (&key (threads 10))
  (format-msg "Updating cache...")
  (force-output)
  (setf *cache* (update-cache))
  (format-msg "Stat: ~a" (stat-cache *cache*))
  (pcall:with-local-thread-pool (:size threads)
    (loop for value in (alexandria:hash-table-values *cache*)
       do (pcall:pcall (worker value)))))
