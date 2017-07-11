(in-package #:match-client)

(defvar *max-dimension* 2048)

(defun resize-image (input-path output-path
                     &key (max-width *max-dimension*) (max-height *max-dimension*)
                       (filter :%QuadraticFilter) (blur 1))
  (gm::with-magick-wand (wand :input-path input-path)
    (let ((w (gm::%MagickGetImageWidth wand))
          (h (gm::%MagickGetImageHeight wand))
          (res nil))
      (multiple-value-bind (fw fh) (gm::fit-width-height w h max-width max-height)
        (unless (and (= w fw) (= h fh))
          (gm::%MagickResizeImage wand fw fh filter blur)
          (gm::%MagickWriteImage wand output-path)
          (setf res output-path))
        (values res w h fw fh)))))
    
