(uiop:define-package #:app/svg
  (:use #:cl)
  (:import-from #:cl-svg)
  (:import-from #:anafanafo
                #:string-width)
  (:export
   #:draw-box-with-text
   #:text))
(in-package app/svg)


(defun text (parent
             text &key
                    x y
                    (color "black")
                    (font-family "Helvetica")
                    (font-weight "bold")
                    (font-size 16)
                    (shadow t)
                    (shadow-opacity 0.4))
  (let* ((font-data (anafanafo:load-data :family font-family
                                         :weight font-weight
                                         :size font-size))
         (text-width (string-width font-data text))
         (shadow-offset (/ font-size 12))
         (group (cl-svg:make-group parent ())))
    (when shadow
      (cl-svg:text group
          (:x (+ x shadow-offset)
           :y (+ y shadow-offset)
           :font-family font-family
           :font-weight font-weight
           :font-size font-size
           :fill "#010101"
           :fill-opacity shadow-opacity
           :text-length text-width)
        text))
    ;; The text
    (cl-svg:text group
        (:x x
         :y y
         :font-family font-family
         :font-weight font-weight
         :font-size font-size
         :fill color
         :text-length text-width)
      text)))


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

    (text group text
      :x margin
      :y (+ font-size
             (/ margin 2))
      :font-family font-family
      :font-weight font-weight
      :font-size font-size
      :color color
      :shadow shadow)
    (values group)))
