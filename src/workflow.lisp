(mgl-pax-minimal:define-package #:github-matrix/workflow
  (:use #:cl)
  (:import-from #:github)
  (:import-from #:github-matrix/repo)
  (:import-from #:rutils
                #:fmt))
(in-package github-matrix/workflow)


(defclass workflow ()
  ((repo :initarg :repo
         :reader repo)
   (id :initarg :id
       :reader id)
   (name :initarg :name
         :reader name)
   (path :initarg :path
         :reader path)))


(defun make-workflow (repo id name path)
  (make-instance 'workflow
                 :repo repo
                 :id id
                 :name name
                 :path path))


(defmethod print-object ((obj workflow) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A"
            (name obj))))


(defun get-workflows (repo)
  (loop with response = (github:get (fmt "/repos/~A/~A/actions/workflows"
                                         (github-matrix/repo::user repo)
                                         (github-matrix/repo::project repo)))
        for item in (getf response :|workflows|)
        for path = (getf item :|path|)
        ;; Some repositories returns workflows without paths.
        ;; Probably that is because something is broken inside
        ;; the GitHub. Here is an example:
        ;; https://api.github.com/repos/github/licensed/actions/workflows
        ;; 
        ;; We need to know YAML config of the workflow. That is why
        ;; workflows without a path should be filtered out.
        unless (string= path "")
          collect (make-workflow repo
                                 (getf item :|id|)
                                 (getf item :|name|)
                                 (getf item :|path|))))


(defun workflow-content (workflow)
  (github-matrix/repo::get-file (repo workflow)
                                (path workflow)))



