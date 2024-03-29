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
    (swank::eval-in-emacs
     `(progn
        (slime-with-popup-buffer (,bufname :connection t :package t)
          (insert-image (create-image ,ns))
          (image-mode)
          (setf buffer-file-name ,ns)
          (not-modified))
        ;; try to resize the image after the buffer is displayed
        (with-current-buffer ,bufname (image-toggle-display-image))))))

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

(defun choice-map (map &key image
                   &aux (special '(
                                   ("<Enter>" "skip") ("q" "quit") ("m" "match") ("d" "delete")
                                   #+swank("v" "View in external viewer")
                                   ))
                     (prompt "Select target directory: "))
  (format t "~%~a" image)
  (print-choices map t)
  (print-choices special t)
  (format t "~%~a" prompt)
  (loop for choice = (read-line)
     for ass = (assoc choice map :test 'equalp)
     if (equalp choice "q") do (return :quit)
     if (equalp choice "") do (return :skip)
     if (equalp choice "d") do (return :delete)
     if ass do (return (third ass))
     if (equalp choice "v") do
       (view-file-native image)
       (format t "~a" prompt)
     else if (equalp choice "m") do
       (print (match-resized image))
       (format t "~%~a" prompt)
     else do (format t "Invalid choice, try again: ")))

(defun move-file (file dir)
  (let ((destination (uiop:merge-pathnames* (file-namestring file) (uiop:ensure-directory-pathname dir))))
    (format t "Moving ~a to ~a~%" file destination)
    (rename-file file destination)))

(defun sort-dir (source &key map target exclude)
  (unless target
    (setf target source))
  (setf source (uiop:ensure-directory-pathname source)
        target (uiop:ensure-directory-pathname target))
  (unless map
    (setf map (generate-map target :exclude exclude)))

  (loop for image in (sort (let ((*translation* nil)) (find-images source)) '< :key 'mtime)
     for ipath = (path image)
     for choice = (progn (view-file ipath) (choice-map map :image ipath))
     do (case choice
          (:quit (return))
          (:skip)
          (:delete
           (format t "Deleting ~a~%" ipath)
           (delete-file ipath))
          (t (move-file ipath choice)))))


(defun choice-view-match (path &aux (choices '(("<Enter>" "keep") ("d" "delete") ("q" "quit") #+swank("v" "View in external viewer"))))
  (print-choices choices t)
  (format t "~%Choice: ")
  (loop for choice = (read-line)
     if (equalp choice "q") do (return :quit)
     if (equalp choice "") do (return :skip)
     if (equalp choice "d") do (return :delete)
     if (equalp choice "v") do
       (view-file-native path)
       (format t "Choice: ")
     else do (format t "Invalid choice, try again: ")))

(defun view-matches (&optional (match-result *last-match*))
  (loop
     for match in match-result
     for fp = (jsown:val match "filepath")
     for meta = (jsown:val match "metadata")
     for path = (get-local-path fp)
     for exists = (and path (probe-file path))
     do (cond ((not path) (format t "~%~s is not a local filepath, skipping" fp))
              ((not exists) (format t "~%~s is not an existing file, skipping" path))
              (t (view-file path)
                 (format t "~%~s~%Score: ~a  Resolution: ~a x ~a"
                         path (jsown:val match "score")
                         (jsown:val-safe meta "w")
                         (jsown:val-safe meta "h"))
                 (case (choice-view-match path)
                   (:quit (return))
                   (:skip)
                   (:delete (format t "Deleting ~a~%" path)
                            (delete-file path)))))))
