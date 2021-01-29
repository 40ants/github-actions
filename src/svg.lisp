(defpackage #:github-matrix/svg
  (:use #:cl)
  (:import-from #:cl-svg)
  (:import-from #:anafanafo)
  (:export
   #:draw-box-with-text))
(in-package github-matrix/svg)


(defun draw-box-with-text (svg text &key
                                      (margin 5)
                                      (font-family "Helvetica")
                                      (font-weight "bold")
                                      (font-size 11)
                                      (color "blue")
                                      (background "yellow")
                                      (shadow t)
                                      (corners-radius nil))
  (let* ((font-data (anafanafo:load-data :family font-family
                                         :weight font-weight
                                         :size font-size))
         (text-width (anafanafo:string-width font-data text))
         (group (cl-svg:make-group svg ()))
         (full-width (+ text-width
                         (* margin 2)))
         (full-height (+ font-size
                          (* margin 2))))
    (cl-svg:draw group
        (:rect :x 0 :y 0 :width full-width :height full-height
               :rx (if corners-radius
                       4
                       0))
        :fill background)

    (when shadow
      (cl-svg:text group
          (:x (1+ margin)
           :y (1+ (+ font-size
                      (/ margin 2)))
           :font-family font-family
           :font-weight font-weight
           :font-size font-size
           :fill "#010101"
           :fill-opacity 0.5
           :text-length text-width)
        text))
    ;; The text
    (cl-svg:text group
        (:x margin
         :y (+ font-size
                (/ margin 2))
         :font-family font-family
         :font-weight font-weight
         :font-size font-size
         :fill color
         :text-length text-width)
      text)
    (values group)))
