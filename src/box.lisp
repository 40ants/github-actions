(uiop:define-package #:app/box
  (:use #:cl)
  (:import-from #:app/base-obj
                #:obj-with-font
                #:draw
                #:width
                #:height
                #:font-family
                #:font-weight
                #:font-size)
  (:import-from #:app/svg
                #:draw-box-with-text))
(in-package app/box)


(defclass box (obj-with-font)
  ((text :initarg :text
     :initform "NO TEXT")
   (margin :initarg :margin
           :initform 5)
   (color :initarg :color
          :initform "white")
   (background :initarg :background
               :initform "black")
   (corners-radius :initarg :corners-radius
                   :initform nil)))


(defclass success-box (box)
  ()
  (:default-initargs
   :background "#4D994D"
   :font-size 10))


(defclass fail-box (box)
  ()
  (:default-initargs
   :background "#E6705C"
   :font-size 10))


(defclass title-box (box)
  ()
  (:default-initargs
   :font-size 10))


(defclass in-progress-box (box)
  ()
  (:default-initargs :background "#838383"))


(defmethod print-object ((obj box) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "\"~A\""
            (slot-value obj 'text))))


(defmethod width ((obj box))
  (with-slots (text font-family font-weight font-size margin)
      obj
    (let* ((font-data (anafanafo:load-data :family font-family
                                           :weight font-weight
                                           :size font-size)))
      (+ (anafanafo:string-width font-data text)
          (* margin 2)))))


(defmethod height ((obj box))
  (with-slots (font-size margin)
      obj
    (+ font-size (* margin 2))))


(defmethod draw ((box box) svg)
  (with-slots (text font-family font-weight font-size
                margin color background
                corners-radius)
      box
    (draw-box-with-text svg text
                        :margin margin
                        :font-family font-family
                        :font-weight font-weight
                        :font-size font-size
                        :color color
                        :background background
                        :corners-radius corners-radius)))
