(in-package #:match-client)

(defvar *tags* nil)
(defvar *translation* nil)

(defclass match-tag ()
  ((name :reader tag-name :initarg :name)
   (cache-file :accessor cache-file :initarg :cache-file)
   (root-dirs :accessor root-dirs :initarg :root-dirs)
   (exclude-dirs :accessor exclude-dirs :initarg :exclude-dirs)
   (exclude-dirnames :accessor exclude-dirnames :initarg :exclude-dirnames)
   (allowed-types :accessor allowed-types :initform *allowed-types* :initarg :allowed-types)
   (translation :accessor tag-translation :initform nil :initarg :translation
                :documentation "Cons of 'canonic wildcard' and 'local wildcard'. For example ('d:/**/' . 'e:/**/')")
   ))

(defun make-tag (name &rest args)
  (apply 'make-instance 'match-tag :name name args))

(defun get-tag (name)
  (cond ((typep name 'match-tag) name)
        ((equal name *local-tag*)
         (make-tag *local-tag*
                   :cache-file *cache-file*
                   :root-dirs *root-dirs*
                   :exclude-dirs *exclude-dirs*
                   :exclude-dirnames *exclude-dirnames*
                   :allowed-types *allowed-types*
                   :translation *translation*))
        (t (find name *tags* :key 'tag-name :test 'equal))))

(define-condition tag-not-found (error)
  ((tag :reader tag-not-found-tag :initarg :tag))
  (:report (lambda (condition stream)
             (format stream "No such tag: ~S."
                     (tag-not-found-tag condition)))))

(defmacro with-tag (name &body body)
  (alexandria:with-gensyms (tag)
    `(let ((,tag (get-tag ,name)))
       (unless ,tag (error 'tag-not-found :tag ,name))
       (let ((*local-tag* (tag-name ,tag))
             (*cache-file* (cache-file ,tag))
             (*root-dirs* (root-dirs ,tag))
             (*exclude-dirs* (exclude-dirs ,tag))
             (*exclude-dirnames* (exclude-dirnames ,tag))
             (*allowed-types* (allowed-types ,tag))
             (*translation* (tag-translation ,tag)))
         ,@body))))

(defun translate-local-path (path)
  (if *translation*
      (translate-pathname path (car *translation*) (cdr *translation*))
      path))

(defun translate-canonical-path (path)
  (if *translation*
      (translate-pathname path (car *translation*) (cdr *translation*))
      path))

(defun get-local-path (tagged-path)
  (ppcre:do-register-groups (tag path) ("^\\[(.*)\\] (.*)$" tagged-path tagged-path)
    (let ((tag (get-tag tag)))
      (return
        (handler-case
            (with-tag tag
              (translate-canonical-path path))
          (tag-not-found () tagged-path))))))
