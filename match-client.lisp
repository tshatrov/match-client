(in-package #:match-client)

(defvar *base-url* "")
(defvar *local-tag* "")
(defvar *auth* nil)
(defvar *debug* nil)

(defvar *browser-user-agent* "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:56.0) Gecko/20100101 Firefox/56.0")
(defvar *needs-headers* nil)

(load (asdf:system-relative-pathname :match-client "settings.lisp") :if-does-not-exist nil)

(defvar *last-match* nil)

(defun api-url (path)
  (concatenate 'string *base-url* path))

(defun get-path (path &key (use-tag t))
  (format nil "~@[[~a] ~]~a" (and use-tag *local-tag*) path))

(defun parse-request (&rest args)
  (when *auth*
    (setf args `(,@args :basic-auth ,*auth*)))
  (when *debug*
    (setf args (append args '(:verbose t)))) ;; :keep-alive nil :use-connection-pool nil

  (multiple-value-bind (content return-code)
      (prog ((retries 0))
       retry
       (handler-bind
           ((dex:http-request-failed #'dex:ignore-and-continue)
            (stream-error (lambda (e) (declare (ignore e)) (when (< retries 3) (incf retries) (go retry)))))
         (return (apply 'dex:request args))))
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

(defun download-with-headers (out url &optional headers)
  (let ((dex:*use-connection-pool* nil))
    (multiple-value-bind (content code)
        (drakma:http-request url :additional-headers headers :force-binary t :want-stream t)
        #-(and)(dex:request url :headers headers :force-binary t :want-stream t)
      (unwind-protect
           (when (= code 200)
             (with-open-file (stream out :direction :output :if-exists :supersede
                                     :element-type :default)
               (uiop:copy-stream-to-stream content stream :element-type '(unsigned-byte 8)))
             out)
        (ignore-errors (close content))))))

(defun match* (content)
  (setf content (cons (cons "all_orientations" nil) content))
  (let ((result (parse-request
                 (api-url "/search")
                 :method :post
                 :content content)))
    (cond ((string= (or (jsown:val-safe result "status") "fail") "ok")
           (setf *last-match*
                 (mapcar (lambda (match &aux (score (jsown:val match "score")))
                           (unless (integerp score) (setf (jsown:val match "score") (float score)))
                           match)
                         (jsown:val result "result"))))
          (t (error "Error: ~a" result)))))

(defun get-url-filetype (url &key (default "jpg"))
  (let ((path (quri:uri-path (quri:uri url))))
    (or (and path (pathname-type path)) default)))

(defun call-on-download (fn url &optional headers)
  (uiop:with-temporary-file (:pathname tmp :prefix "match-dl" :type (get-url-filetype url))
    (download-with-headers tmp url headers)
    (funcall fn tmp)))

(defun pathify (url-or-path)
  (typecase url-or-path
    (pathname url-or-path)
    (t (handler-case (multiple-value-bind (schema ui hostname) (quri:parse-uri url-or-path)
                       (declare (ignore ui))
                       (if (and schema hostname)
                           url-or-path
                           (pathname url-or-path)))
         (quri:uri-error () (pathname url-or-path))))))

(defun call-on-url-or-path (url-or-path fn-path &optional fn-url
                            &aux (url-or-path (pathify url-or-path)))
  (typecase url-or-path
    (pathname (funcall fn-path url-or-path))
    (t (loop for (regex . headers) in *needs-headers*
          if (ppcre:scan regex url-or-path)
          do (return (call-on-download fn-path url-or-path headers))
          finally (return (if fn-url
                              (funcall fn-url url-or-path)
                              (call-on-download fn-path url-or-path)))))))

(defun match-local (path)
  (match* `(("image" . ,path))))

(defun match-resized (path &rest resize-args)
  (uiop:with-temporary-file (:pathname tmp :prefix "match-thumb" :type (pathname-type path))
    (multiple-value-bind (out w h) (apply 'resize-image (namestring path) (namestring tmp) resize-args)
      (values (match-local (if out tmp path))
              (list w h)))))

(defun match (url-or-path &optional download-p)
  (call-on-url-or-path
   url-or-path
   'match-resized
   (unless download-p (lambda (f) (match* `(("url" . ,f)))))))

(defvar *cache*)
(defvar *cache-lock* (bordeaux-threads:make-lock "match-cache-lock"))
(defvar *msg-lock* (bordeaux-threads:make-lock "match-msg-lock"))

(defun format-msg (str &rest args)
  (bordeaux-threads:with-lock-held (*msg-lock*)
    (terpri)
    (apply 'format t str args)
    (force-output)))

(defun add-resized (path &rest resize-args &aux (lpath (translate-canonical-path path)))
  (uiop:with-temporary-file (:pathname tmp :prefix "match-thumb" :type (pathname-type path))
    (multiple-value-bind (out w h rw rh) (apply 'resize-image (namestring lpath) (namestring tmp) resize-args)
      (let ((metadata (jsown:new-js ("w" w) ("h" h))))
        (cond
          (out
           (jsown:extend-js metadata ("rw" rw) ("rh" rh))
           (add-local tmp :path path :metadata (jsown:to-json* metadata)))
          (t
           (add-local lpath :path path :metadata (jsown:to-json* metadata))))))))

(defun match-dir (path)
  (let ((images (find-images path)))
    (loop for img-file in images
       for img-path = (path img-file)
       for name = (namestring img-path)
       for match = (match-resized img-path)
       for result = (or match :not-found)
       collect (cons name result) into results
       append match into last-match
       finally (setf *last-match* last-match) (return results))))

(defun worker (file)
  (lparallel:future
    (let ((filepath (get-path (namestring (path file)))))
      (case (status file)
        ((:new :update :error)
         (format-msg "Action ~a for ~a" (status file) filepath)
         (let ((result (handler-case (add-resized (path file))
                         (invalid-image () :invalid)
                         (error (c) c))))
           (cond ((eql result :invalid)
                  (setf (status file) :invalid)
                  (format-msg "Invalid image file: ~a" filepath))
                 ((string= (or (jsown:val-safe result "status") "fail") "ok")
                  (setf (status file) :ok)
                  (format-msg "Finished updating ~a" filepath))
                 (t
                  (setf (status file) :error (message file) result)
                  (format-msg "Error updating ~a: ~a" filepath result)))))
        (:delete
         (format-msg "Action ~a for ~a" (status file) filepath)
         (let ((result (handler-case (delete-path (path file))
                         (error (c) c)))
               (key (namestring (path file))))
           (cond ((string= (or (jsown:val-safe result "status") "fail") "ok")
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


;; utility to mass-delete paths

(defun find-missing-paths (all-paths)
  (let ((tag-prefix (format nil "[~a] " *local-tag*)))
    (loop for fp in all-paths
       for path = (when (alexandria:starts-with-subseq tag-prefix fp)
                    (subseq fp (length tag-prefix)))
       for exists = (and path (probe-file path))
       if (and path (not exists)) collect path)))

(defun delete-paths (paths &key (threads 2))
  (setf *cache*
        (if (typep paths 'hash-table)
            (loop for file being each hash-value of paths
               do (setf (status file) :delete)
               finally (return paths))
            (let ((hash (make-hash-table :test 'equal)))
              (loop for path in paths
                   do (setf (gethash (namestring path) hash) (make-file-dummy path :status :delete)))
              hash)))

  (let ((lparallel:*kernel* (lparallel:make-kernel threads)))
    (unwind-protect
         (loop for file in (alexandria:hash-table-values *cache*)
            collect (worker file) into futures
            finally (map nil 'lparallel:force futures))
      (lparallel:end-kernel)))

  (format-msg "Remaining: ~a" (stat-cache *cache*))
  *cache*)
