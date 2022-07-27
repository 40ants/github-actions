(uiop:define-package #:app/workflow
  (:use #:cl)
  (:import-from #:github)
  (:import-from #:app/repo))
(in-package app/workflow)


(defclass workflow ()
  ((repo :initarg :repo
         :reader repo)
   (id :initarg :id
       :reader id)
   (name :initarg :name
         :reader name)
   (path :initarg :path
         :reader path)
   (content-cache :initform nil
                  :initarg :content-cache)))


(defun make-workflow (repo id name path &optional content)
  (make-instance 'workflow
                 :repo repo
                 :id id
                 :name name
                 :path path
                 :content-cache content))


(defmethod print-object ((obj workflow) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A"
            (name obj))))


(defun get-workflows (repo)
  (loop with response = (github:get "/repos/~A/~A/actions/workflows"
                                    :params (list
                                             (app/repo::user repo)
                                             (app/repo::project repo)))
        for item in (getf response :|workflows|)
        for path = (getf item :|path|)
        for content = (when path
                        (app/repo::get-file repo
                                                      path))
        ;; Some repositories returns workflows without paths.
        ;; Probably that is because something is broken inside
        ;; the GitHub. Here is an example:
        ;; https://api.github.com/repos/github/licensed/actions/workflows
        ;; 
        ;; We need to know YAML config of the workflow. That is why
        ;; workflows without a path should be filtered out.
        unless (or (string= path "")
                   (null content))
        collect (make-workflow repo
                               (getf item :|id|)
                               (getf item :|name|)
                               (getf item :|path|)
                               content)))


(defun workflow-content (workflow)
  (with-slots (content-cache)
      workflow
    (or content-cache
        (setf content-cache
              (app/repo::get-file (repo workflow)
                                  (path workflow))))))



