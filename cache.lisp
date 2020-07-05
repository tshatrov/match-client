(in-package #:match-client)

(defvar *cache-file* "")
(defvar *root-dirs* '())
(defvar *exclude-dirs* '())
(defvar *exclude-dirnames* '())
(defvar *allowed-types* '("jpg" "jpeg" "png" "webp"))
(defvar *max-file-size* 10000000)

;; *** the following is copied from sync-sbcl

;; defining wstat, which works with unicode filenames
#+os-windows
(sb-posix::define-stat-call "_wstat"
    sb-posix::pathname sb-posix::filename
    (function sb-posix::int (sb-posix::c-string :external-format :ucs-2)
              (* sb-posix::alien-stat)))

(defun ewstat (name)
  (declare (optimize (speed 0)))
  ;;(format t "Processing ~a~%" name) (force-output)
  (handler-case #+os-windows (sb-posix::wstat name) #+os-unix (sb-posix:stat name)
    (error ()
      (error "Error while accessing file: ~a" name))))

;; because mtime returns unix time we need to convert it to universal time
;; via http://lisptips.com/post/11649360174/the-common-lisp-and-unix-epochs

(defparameter *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

;; *** end copypaste

(defclass file ()
  ((path :initarg :path :reader path)
   (size :initarg :size :reader size)
   (mtime :initarg :mtime :reader mtime)
   (ctime :initarg :ctime :reader ctime)
   (status :initform nil :accessor status)
   (message :initform nil :accessor message)
   ))

(defmethod print-object ((obj file) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a~@[ ~a~]" (path obj) (status obj))))

;; (defmethod path :around ((file file))
;;   (translate-canonical-path (call-next-method)))

(defmethod initialize-instance :after ((file file) &key path &allow-other-keys)
  (setf (slot-value file 'path) (translate-local-path path)))

(defun make-file (pathname)
  (let ((stat (ewstat pathname)))
    (make-instance 'file :path pathname
                   :size (sb-posix:stat-size stat)
                   :mtime (unix-to-universal-time (sb-posix:stat-mtime stat))
                   :ctime (unix-to-universal-time (sb-posix:stat-ctime stat)))))

(defun make-file-dummy (pathname &key status)
  (let ((file (make-instance 'file :path pathname :size 0 :mtime 0 :ctime 0)))
    (when status
      (setf (status file) status))
    file))

(defun load-cache ()
  (if (probe-file *cache-file*)
      (cl-store:restore *cache-file*)
      (make-hash-table :test 'equal)))

(defun save-cache (cache)
  (cl-store:store cache *cache-file*))

(defun find-images (root)
  (loop for f in (uiop:directory-files (uiop:ensure-directory-pathname root))
     for type = (string-downcase (pathname-type f))
     when (find type *allowed-types* :test 'equal)
     collect (make-file f)))

(defun find-images-rec (root)
  (nconc (find-images root)
         (loop for dir in (uiop:subdirectories root)
            for dirname = (car (last (pathname-directory dir)))
            unless (or (alexandria:starts-with #\. dirname)
                       (some (lambda (p) (equalp p dirname)) *exclude-dirnames*)
                       (some (lambda (p) (uiop:pathname-equal p dir)) *exclude-dirs*))
            nconcing (find-images-rec dir))))

(defun find-images-all ()
  (let ((*exclude-dirs* (mapcar 'uiop:ensure-directory-pathname *exclude-dirs*)))
    (loop for root in *root-dirs*
       nconcing (find-images-rec root))))

(defun update-cache ()
  (let ((cache (load-cache))
        (visited (make-hash-table :test 'equal)))
    (loop for cur-file in (find-images-all)
       for key = (namestring (path cur-file))
       for cached-file = (gethash key cache)
       do (cond ((not cached-file)
                 (setf (status cur-file) :new
                       (gethash key cache) cur-file))
                ((or (/= (size cur-file) (size cached-file))
                     (/= (mtime cur-file) (mtime cached-file)))
                 (setf (status cur-file) :update
                       (gethash key cache) cur-file)))
         (setf (gethash key visited) t))
    (maphash
     (lambda (key value)
       (cond ((not (gethash key visited))
              (setf (status value) :delete))
             ((> (size value) *max-file-size*)
              (setf (status value) :toobig))))
     cache)
    cache))

(defun stat-cache (cache)
  (loop with stat
     for value being each hash-value in cache
     for status = (status value)
     for ass = (assoc status stat)
     if ass do (incf (cdr ass)) else do (push (cons status 1) stat)
     finally (return (sort stat '> :key 'cdr))))
