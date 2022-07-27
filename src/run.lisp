(uiop:define-package #:app/run
  (:use #:cl)
  (:import-from #:app/repo)
  (:import-from #:app/workflow)
  (:import-from #:alexandria
                #:make-keyword)
  (:export
   #:job-name
   #:run-params))
(in-package app/run)


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
    (let* ((repo (app/workflow::repo workflow))
           (workflow-id (app/workflow::id workflow))
           (user (app/repo::user repo))
           (project (app/repo::project repo))
           (branch (app/repo::branch repo))
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


(defun job-name (run)
  (check-type run run)
  (let ((run-name (app/run::name run)))
    (or
     (cl-ppcre:register-groups-bind (job-name)
         ("(.*?) \\((?:.*)\\)" run-name)
       (values job-name))
     ;; If job has no matrix, then
     ;; it will have only one run the the same name
     ;; as job's name:
     (values run-name))))


(defun run-params (run)
  "Parses run name of form:

   \"run_tests (sbcl-bin, ubuntu-latest, quicklisp)\"

   and returns a list:

   ```
   (\"sbcl-bin\" \"ubuntu-latest\" \"quicklisp\")
   ```

   If matrix is NIL then run will have only
   a job's name and this function will return just NIL.
"
  
  (check-type run run)
  
  (let ((run-name (app/run::name run)))
    (cl-ppcre:register-groups-bind (params)
        ("(?:.*?) \\((.*)\\)" run-name)
      (let ((params (cl-ppcre:split ", " params)))
        (values params)))))
