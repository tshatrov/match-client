(defpackage #:match-client/saucenao
  (:nicknames #:saucenao)
  (:use #:cl)
  (:export
   #:lookup))

(in-package #:match-client/saucenao)

(defparameter *saucenao-root* "https://saucenao.com~a")
(defparameter *saucenao-search* (format nil *saucenao-root* "/search.php"))


(defclass saucenao-result ()
  ((url :reader url)
   (title :reader title)
   (document :reader document :initarg :doc)
   (similarity :reader similarity)
   (result :reader result)))

(defun strip-whitespace (str)
  ;;remove initial whitespace
  (setf str (cl-ppcre:regex-replace "^\\s+" str ""))
  ;;remove trailing whitespace
  (setf str (cl-ppcre:regex-replace "\\s+$" str ""))
  str)

(defun node-text (node)
  (let (values)
    (dom:do-node-list (node (dom:child-nodes node))
      (let ((val (case (dom:node-type node)
                   (:element (node-text node))
                   (:text (dom:node-value node)))))
        (push val values)))
    (strip-whitespace
     (apply #'concatenate 'string (nreverse values)))))

(defmethod initialize-instance :after ((obj saucenao-result) &key)
  (with-slots (url title document similarity result) obj
    (let (results cur-result)
      (setf url nil)
      (dom:do-node-list (node (dom:child-nodes (car (css:query ".resultcontentcolumn" document))))
        (let ((node-name (dom:node-name node)))
          (cond
            ((equalp node-name "br") (when cur-result (push cur-result results)))
            ((equalp node-name "strong")
             (setf cur-result (list (node-text node))))
            ((equalp node-name "a")
             (let ((link (dom:get-attribute node "href"))
                   (text (node-text node)))
               (unless url
                 (setf url link))
               (if cur-result
                   (setf (cdr cur-result) (cons text link))
                   (setf cur-result (cons text link))))))))
      (setf result (nreverse results))
      (let ((ttl (css:query ".resulttitle" document)))
        (setf title (if ttl (node-text (car ttl)) "")))
      (setf similarity (parse-float:parse-float
                        (node-text (car (css:query ".resultsimilarityinfo" document)))
                        :junk-allowed t)))))

(defmethod print-object ((obj saucenao-result) stream)
  (print-unreadable-object (obj stream :type nil :identity nil)
    (format stream "SN ~a [~a] ~,2f%" (url obj) (title obj) (similarity obj))))

(defun parse-saucenao-response (content)
  (let ((doc (chtml:parse content (cxml-dom:make-dom-builder))))
    (loop for panel in (css:query ".resulttablecontent" doc)
       collect (make-instance 'saucenao-result :doc panel))))


(defun lookup (url-or-path &optional (threshold 50.0))
  (match-client:call-on-url-or-path
   url-or-path
   (lambda (f)
     (let ((dex:*use-connection-pool* nil))
       (multiple-value-bind (content code)
           (dex:request *saucenao-search*
                        :method :post
                        :max-redirects 0
                        :content `(("file" . ,(pathname f))))
         (cond ((= code 200)
                (remove-if (lambda (obj) (< (similarity obj) threshold)) (parse-saucenao-response content)))
               (t (values content code))))))))
