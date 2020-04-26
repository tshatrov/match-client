(in-package #:match-client)

(defvar *image-app* nil)
(defvar *prefer-slime* nil)

(defun view-file-native (file)
  (let ((ns (uiop:native-namestring file)))
    (uiop:launch-program (if *image-app*
                             (append *image-app* (list ns))
                             (uiop:escape-shell-token ns)))))

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
  #+swank
  (if (and *prefer-slime* (not (alexandria:ends-with-subseq ".webp" (namestring file))))
      (view-file-slime file)
      (view-file-native file))
  #-swank
  (view-file-native file))

(defun generate-map (target &key exclude)
  (setf target (uiop:ensure-directory-pathname target))
  (loop with i = 0
     for dir in (sort (uiop:subdirectories target) 'string-lessp :key 'namestring)
     for dirname = (car (last (pathname-directory dir)))
     unless (find dirname exclude :test 'equalp)
     collect (list (princ-to-string i) dirname dir) and do (incf i)))

(defun print-choices (choices &optional s)
  (format s "~%~<~@{~<[~a] ~a~:>~^  ~}~:@>" choices))

(defun choice-map (map &key image &aux (special '(("<Enter>" "skip") ("q" "quit") #+swank("v" "View in external viewer"))))
  (print-choices map t)
  (print-choices special t)
  (format t "~%Select target directory: ")
  (loop for choice = (read-line)
     for ass = (assoc choice map :test 'equalp)
     if (equalp choice "q") do (return :quit)
     if (equalp choice "") do (return :skip)
     if ass do (return (third ass))
     if (equalp choice "v") do
       (view-file-native image)
       (format t "~%Select target directory: ")
     else do (format t "~%Invalid choice, try again: ")))

(defun move-file (file dir)
  (let ((destination (uiop:merge-pathnames* (file-namestring file) (uiop:ensure-directory-pathname dir))))
    (format t "Moving ~a to ~a" file destination)
    (rename-file file destination)))

(defun sort-dir (source &key map target exclude)
  (unless target
    (setf target source))
  (setf source (uiop:ensure-directory-pathname source)
        target (uiop:ensure-directory-pathname target))
  (unless map
    (setf map (generate-map target :exclude exclude)))

  (loop for image in (sort (find-images source) '< :key 'mtime)
     for ipath = (path image)
     for choice = (progn (view-file ipath) (choice-map map :image ipath))
     if (eql choice :quit) do (return)
     unless (eql choice :skip)
     do (move-file ipath choice)))
