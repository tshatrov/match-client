(in-package #:match-client)

(defvar *image-app* nil)

(defun view-file-native (file)
  (let ((ns (uiop:native-namestring file)))
    (uiop:launch-program (if *image-app*
                             (append *image-app* (list ns))
                             ns))))

#+swank
(defun view-file-slime (file &key (bufname "*image-viewer*"))
  "Requires setting `slime-enable-evaluate-in-emacs' to T in Emacs"
  (let ((ns (namestring file)))
    (swank::eval-in-emacs `(slime-with-popup-buffer (,bufname :connection t :package t)
                             (insert-image (create-image ,ns))
                             (image-mode)
                             (setf buffer-file-name ,ns)
                             (not-modified)
                             (image-toggle-display-image)
                             ))
    ;; try to resize the image after the buffer is displayed
    (swank::eval-in-emacs `(with-current-buffer ,bufname (image-toggle-display-image)))
    ))

(defun view-file (file)
  (view-file-native file))
