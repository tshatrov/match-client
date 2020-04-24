(in-package #:match-client)

(defvar *image-app* nil)

(defun view-file (file)
  (let ((ns (uiop:native-namestring file)))
    (uiop:launch-program (if *image-app*
                             (append *image-app* (list ns))
                             ns))))
