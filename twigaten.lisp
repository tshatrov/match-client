(defpackage #:match-client/twigaten
  (:nicknames #:twigaten)
  (:use #:cl)
  (:export
   #:lookup))

(in-package #:match-client/twigaten)

(defparameter *twigaten-root* "https://twigaten.204504byse.info/~a")
(defparameter *twigaten-search* (format nil *twigaten-root* "Search/Media"))


(defun extract-value (doc xpath)
  (parse-integer
   (dom:node-value 
    (xpath:first-node
     (xpath:evaluate xpath doc)))
   :junk-allowed t))

(defclass twigaten-result ()
  ((url :reader url)
   (document :reader document :initarg :doc)
   (retweets :reader retweets)
   (likes :reader likes)
   ))

(defmethod initialize-instance :after ((obj twigaten-result) &key)
  (with-slots (url retweets likes document) obj
    (xpath:with-namespaces (("w" "http://www.w3.org/1999/xhtml"))
      (setf url (dom:get-attribute (car (css:query "span.twigaten-twitterbird a" document)) "href")
            retweets (extract-value document ".//w:span[contains(@class,'glyphicon-retweet')]/../following-sibling::text()[1]")
            likes (extract-value document ".//w:span[contains(@class,'glyphicon-star')]/../following-sibling::text()[1]")))))

(defmethod print-object ((obj twigaten-result) stream)
  (print-unreadable-object (obj stream :type nil :identity nil)
    (format stream "TGT ~a [~aR/~aL]" (url obj) (retweets obj) (likes obj))))

(defun parse-twigaten-response (content)
  (let ((doc (chtml:parse content (cxml-dom:make-dom-builder))))
      (loop for panel in (css:query "div.twigaten-panel" doc)
         collect (make-instance 'twigaten-result :doc panel))))

(defun lookup (url-or-path)
  (match-client:call-on-url-or-path
   url-or-path
   (lambda (f)
     (multiple-value-bind (content code headers)
         (dex:request *twigaten-search*
                      :method :post
                      :max-redirects 0
                      :content `(("File" . ,(pathname f))))
       (cond ((= code 302)
              (parse-twigaten-response
               (dex:request (format nil *twigaten-root* (gethash "location" headers)))))
             ((= code 200) nil)
             (t (values content code)))))))
