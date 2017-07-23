(in-package #:match-client)

(defvar *max-dimension* 2048)

(defun resize-image (input-path output-path
                     &key (max-width *max-dimension*) (max-height *max-dimension*)
                       (filter :%QuadraticFilter) (blur 1))
  (gm::with-magick-wand (wand)
    (handler-case (gm::%MagickReadImage wand input-path)
      ;; graphicsmagick cannot read Unicode filenames on Windows so attempt to load a copy
      (gm::magick-error ()
        (uiop:with-temporary-file (:pathname tmp :prefix "gm" :type (pathname-type input-path))
          (uiop:copy-file input-path tmp)
          (setf wand (gm::%NewMagickWand))
          (gm::%MagickReadImage wand (namestring tmp)))))
    (let ((w (gm::%MagickGetImageWidth wand))
          (h (gm::%MagickGetImageHeight wand))
          (res nil))
      (multiple-value-bind (fw fh) (gm::fit-width-height w h max-width max-height)
        (unless (and (= w fw) (= h fh))
          (gm::%MagickResizeImage wand fw fh filter blur)
          (gm::%MagickWriteImage wand output-path)
          (setf res output-path))
        (values res w h fw fh)))))
    
