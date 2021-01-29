(defpackage #:github-matrix/workflow
  (:use #:cl)
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
        collect (make-workflow repo
                               (getf item :|id|)
                               (getf item :|name|)
                               (getf item :|path|))))


(defun workflow-content (workflow)
  (get-file (repo workflow)
            (path workflow)))


(defun workflow-matrix (workflow)
  (parse-matrix (workflow-content workflow)))


