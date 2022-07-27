(uiop:define-package #:app/playground
  (:use #:cl)
  (:import-from #:cl-svg)
  (:import-from #:app/run-results)
  (:import-from #:app/workflow)
  (:import-from #:app/container
                #:tight-container
                #:child)
  (:import-from #:app/box
                #:box
                #:fail-box
                #:success-box)
  (:import-from #:app/base-obj
                #:draw))
(in-package app/playground)


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
        (app/repo::make-repo "40ants" "cl-info" :branch "master"))

  (setf *workflow*
        (first (app/workflow::get-workflows *repo*)))

  (unless dont-fetch-runs
    (setf *runs*
          (app/run::get-last-run *workflow*)))
  
  (setf *root* (app/run-results::runs-to-boxes *workflow* :runs *runs*)))


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
