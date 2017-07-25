(in-package #:match-client)

(defvar *base-url* "")
(defvar *local-tag* "")
(defvar *auth* nil)

(defvar *needs-referer*
  '(("^http(s)?://i.pximg.net/" . "https://www.pixiv.net")))

(load (asdf:system-relative-pathname :match-client "settings.lisp") :if-does-not-exist nil)

(defun api-url (path)
  (concatenate 'string *base-url* path))

(defun get-path (path &key (use-tag t))
  (format nil "~@[[~a] ~]~a" (and use-tag *local-tag*) path))

(defun parse-request (&rest args)
  (multiple-value-bind (content return-code)
      (handler-bind ((dex:http-request-failed #'dex:ignore-and-continue))
        (when *auth*
          (setf args `(,@args :basic-auth ,*auth*)))
        (apply 'dex:request args #-(and)(append args '(:verbose t :keep-alive nil :use-connection-pool nil))))
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

(defun add-url (url &key path (metadata "{}"))
  "Add remote image to Match server"
  (parse-request
   (api-url "/add")
   :method :post
   :content `(("url" . ,url)
              ("filepath" . ,(or path url))
              ("metadata" . ,metadata))))

(defun delete-path (path &key (use-tag t))
  (parse-request
   (api-url "/delete")
   :method :delete
   :content `(("filepath" . ,(get-path path :use-tag use-tag)))
   ))

(defun download-with-ref (out url referer)
  (multiple-value-bind (content code)
      (dex:request url :headers `(("Referer" . ,referer))
                   :force-binary t :want-stream t)
    (when (= code 200)
      (with-open-file (stream out :direction :output :if-exists :supersede
                              :element-type :default)
        (uiop:copy-stream-to-stream content stream :element-type '(unsigned-byte 8)))
      out)))

(defun match* (content)
  (let ((result (parse-request
                 (api-url "/search")
                 :method :post
                 :content content)))
    (cond ((string= (or (jsown::val-safe result "status") "fail") "ok")
           (mapcar (lambda (match &aux (score (jsown:val match "score")))
                     (unless (integerp score) (setf (jsown:val match "score") (float score)))
                     match)
                   (jsown:val result "result")))
          (t (error "Error: ~a" result)))))

(defun match (url-or-path)
  (typecase url-or-path
    (pathname (match* `(("image" . ,url-or-path))))
    (t (loop for (regex . referer) in *needs-referer*
          if (ppcre:scan regex url-or-path)
          do (return (uiop:with-temporary-file (:pathname tmp :prefix "match-dl" :type (pathname-type url-or-path))
                       (download-with-ref tmp url-or-path referer)
                       (match* `(("image" . ,tmp)))))
          finally (return (match* `(("url" . ,url-or-path))))))))

(defvar *cache*)
(defvar *cache-lock* (bordeaux-threads:make-lock "match-cache-lock"))
(defvar *msg-lock* (bordeaux-threads:make-lock "match-msg-lock"))

(defun format-msg (str &rest args)
  (bordeaux-threads:with-lock-held (*msg-lock*)
    (terpri)
    (apply 'format t str args)
    (force-output)))

(defun add-resized (path &rest resize-args)
  (uiop:with-temporary-file (:pathname tmp :prefix "match-thumb" :type (pathname-type path))
    (multiple-value-bind (out w h rw rh) (apply 'resize-image (namestring path) (namestring tmp) resize-args)
      (let ((metadata (jsown:new-js ("w" w) ("h" h))))
        (cond
          (out
           (jsown:extend-js metadata ("rw" rw) ("rh" rh))
           (add-local tmp :path path :metadata (jsown:to-json* metadata)))
          (t
           (add-local path :metadata (jsown:to-json* metadata))))))))

(defun match-resized (path &rest resize-args)
  (uiop:with-temporary-file (:pathname tmp :prefix "match-thumb" :type (pathname-type path))
    (let ((out (apply 'resize-image (namestring path) (namestring tmp) resize-args)))
      (if out (match tmp) (match path)))))

(defun worker (file)
  (lparallel:future
    (let ((filepath (get-path (namestring (path file)))))
      (case (status file)
        ((:new :update :error)
         (format-msg "Action ~a for ~a" (status file) filepath)
         (let ((result (add-resized (path file))))
           (cond ((string= (or (jsown::val-safe result "status") "fail") "ok")
                  (setf (status file) :ok)
                  (format-msg "Finished updating ~a" filepath))
                 (t
                  (setf (status file) :error (message file) result)
                  (format-msg "Error updating ~a: ~a" filepath result)))))
        (:delete
         (format-msg "Action ~a for ~a" (status file) filepath)
         (let ((result (delete-path filepath))
               (key (namestring (path file))))
           (cond ((string= (or (jsown::val-safe result "status") "fail") "ok")
                  (bordeaux-threads:with-lock-held (*cache-lock*)
                    (remhash key *cache*))
                  (format-msg "Finished deleting ~a" filepath))
                 (t
                  (setf (status file) :error (message file) result)
                  (format-msg "Error deleting ~a: ~a" filepath result)))))))))

(defun update (&key (threads 4))
  (format-msg "Updating cache...")
  (force-output)
  (setf *cache* (update-cache))
  (format-msg "Stat: ~a" (stat-cache *cache*))
  (let ((lparallel:*kernel* (lparallel:make-kernel threads)))
    (unwind-protect
         (loop for value in (alexandria:hash-table-values *cache*)
            collect (worker value) into futures
            finally (map nil 'lparallel:force futures))
      (lparallel:end-kernel)))
  (format-msg "Final stat: ~a" (stat-cache *cache*))
  (save-cache *cache*))
