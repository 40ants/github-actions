(defpackage #:github-matrix/playground
  (:use #:cl)
  (:import-from #:cl-svg)
  (:import-from #:github-matrix/container
                #:tight-container
                #:child)
  (:import-from #:github-matrix/box
                #:box
                #:fail-box
                #:success-box)
  (:import-from #:github-matrix/base-obj
                #:draw))
(in-package github-matrix/playground)


(defparameter *repo* nil)

(defparameter *workflow* nil)

(defparameter *root* nil)


(defun render-test-image ()
  (cl-svg:with-svg-to-file (svg 'cl-svg:svg-1.1-toplevel)
      ("test.svg" :if-exists :supersede)
    (draw *root* svg)
    (values)))


(defun retrieve-data ()
  (github-matrix/repo::make-repo "40ants" "cl-info" :branch "master"))


(defun fill-test-data ()
  (setf *root*
        (make-instance 'tight-container :title "Tests"))
  
  (setf (child *root*
               "LISP=sbcl")
        (make-instance 'success-box
                       :text "sbcl"
                       ))
  (setf (child *root*
               "LISP=ccl")
        (make-instance 'box
                       :text "ccl"
                       ))
  (setf (child *root*
               "LISP=allegro")
        (make-instance 'fail-box
                       :text "allegro"
                       ))
  (values))
