(uiop:define-package #:github-matrix/run
  (:use #:cl)
  (:import-from #:github-matrix/repo)
  (:import-from #:github-matrix/workflow)
  (:import-from #:alexandria
                #:make-keyword))
(in-package github-matrix/run)


(defclass run ()
  ((name :initarg :name
         :reader name)
   (status :initarg :status
           :reader status)
   (conclusion :initarg :conclusion
               :reader conclusion)))


(defun make-run (name status conclusion)
  (make-instance 'run
                 :name name
                 :status (make-keyword (string-upcase status))
                 :conclusion (make-keyword (string-upcase conclusion))))


(defmethod print-object ((obj run) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "\"~A\" ~A ~A"
            (name obj)
            (status obj)
            (conclusion obj))))


(defun get-last-run (workflow)
  (when workflow
    (let* ((repo (github-matrix/workflow::repo workflow))
           (workflow-id (github-matrix/workflow::id workflow))
           (user (github-matrix/repo::user repo))
           (project (github-matrix/repo::project repo))
           (branch (github-matrix/repo::branch repo))
           (response (github:get "/repos/~A/~A/actions/workflows/~A/runs?branch=~A"
                                 :params (list user
                                               project
                                               workflow-id
                                               branch)
                                 :limit 1))
           (all-runs (getf response :|workflow_runs|))
           (data (first all-runs)))

      (when data
        (let* ((check-suite-id (getf data :|check_suite_id|))
               (response (github:get "/repos/~A/~A/check-suites/~A/check-runs"
                                     :params (list user
                                                   project
                                                   check-suite-id)))
               (runs-data (getf response :|check_runs|)))
          (loop for item in runs-data
                collect (make-run (getf item :|name|)
                                  (getf item :|status|)
                                  (getf item :|conclusion|))))))))
