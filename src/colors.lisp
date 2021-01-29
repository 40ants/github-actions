(mgl-pax-minimal:define-package #:github-matrix/colors
  (:use #:cl)
  (:export
   #:*tight-container-title-background*
   #:*tight-container-title-color*))
(in-package github-matrix/colors)


(defparameter *tight-container-title-color*
  "#EEE")

(defparameter *tight-container-title-background*
  "#434343")
