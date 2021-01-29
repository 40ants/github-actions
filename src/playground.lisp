(mgl-pax-minimal:define-package #:github-matrix/playground
  (:use #:cl)
  (:import-from #:cl-svg)
  (:import-from #:github-matrix/run-results)
  (:import-from #:github-matrix/workflow)
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


(defvar *repo* nil)

(defvar *workflow* nil)

(defvar *runs* nil)

(defvar *root* nil)


(defun render-test-image ()
  (cl-svg:with-svg-to-file (svg 'cl-svg:svg-1.1-toplevel)
      ("test.svg" :if-exists :supersede)
    (draw *root* svg)
    (values)))


(defun retrieve-data (&key dont-fetch-runs)
  (setf *repo*
        (github-matrix/repo::make-repo "40ants" "cl-info" :branch "master"))

  (setf *workflow*
        (first (github-matrix/workflow::get-workflows *repo*)))

  (unless dont-fetch-runs
    (setf *runs*
          (github-matrix/run::get-last-run *workflow*)))
  
  (setf *root* (github-matrix/run-results::runs-to-boxes *workflow* :runs *runs*)))


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
