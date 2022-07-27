(uiop:define-package #:app/colors
  (:use #:cl)
  (:export
   #:*tight-container-title-background*
   #:*tight-container-title-color*
   #:*link-color*
   #:*title-color*))
(in-package app/colors)


(defparameter *tight-container-title-color*
  "#EEE")

(defparameter *tight-container-title-background*
  "#434343")

(defparameter *link-color*
  "#3383FA")

(defparameter *title-color*
  "#555")

