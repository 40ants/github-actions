(uiop:define-package #:github-matrix/base-obj
  (:use #:cl))
(in-package github-matrix/base-obj)


(defparameter *default-font-size* 16)


(defclass obj-with-font ()
  ((font-family :initarg :font-family
                :initform "Helvetica")
   (font-weight :initarg :font-weight
                :initform "Bold")
   (font-size :initarg :font-size
              :initform *default-font-size*)))


(defgeneric draw (obj svg))

(defgeneric width (obj))

(defgeneric height (obj))
