(uiop:define-package #:app/run-results
  (:use #:cl)
  (:import-from #:app/run)
  (:import-from #:app/container)
  (:import-from #:app/workflow)
  (:import-from #:app/matrix)
  (:import-from #:app/box
                #:success-box
                #:fail-box
                #:in-progress-box)
  (:import-from #:cl-ppcre)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:str
                #:containsp))
(in-package app/run-results)


(defun parse-run-name (workflow-matrix run)
  "Принимает workflow матрицу и run.

   Матрица выглядит как:

   ((\"run_tests\" \"lisp\" \"os\" \"quicklisp-dist\"))

   Парсит название run вида:

   \"run_tests (sbcl-bin, ubuntu-latest, quicklisp)\"

   и выдаёт список:

   ```
   (\"job=run_tests\"
    \"lisp=sbcl-bin\"
    \"os=ubuntu-latest\"
    \"quicklisp-dist=quicklisp\")
   ```

   If matrix is NIL for the job, then it will return just:

   ```
   (\"job=linter\")
   ```
"
  (let ((job-name (app/run:job-name run))
        (run-params (app/run:run-params run)))
    (let ((job-matrix (cdr (assoc job-name workflow-matrix
                                  :test #'string-equal))))
      (list* (format nil "JOB = ~A"
                     job-name)
             (loop for param in run-params
                   for matrix-key in job-matrix
                   collect (format nil "~A = ~A"
                                   (string-upcase matrix-key)
                                   param))))))


(defun runs-to-boxes (workflow &key (runs (app/run::get-last-run workflow)))
  "This is the core function which forms the structure of the badge.

   It transforms RUNS (received using GET-LAST-RUN function) in to a containers with run statuses.

   RUNS can contains runs from different jobs."
  (cond
    (workflow
     (let* ((matrix (app/matrix::workflow-matrix workflow))
            (workflow-name (app/workflow::name workflow))
            (workflow-path (app/workflow::path workflow))
            (app/base-obj::*default-font-size* 20)
            ;; This is how much we'll reduce the font size on each
            ;; level of GitHub matrix:
            (font-size-ratio 0.85))

       (labels ((add-box-to (node names &key box-type)
                  (let ((app/base-obj::*default-font-size*
                          (* app/base-obj::*default-font-size*
                             font-size-ratio)))
                    (cond
                      ((= (length names)
                          1)
                       (let ((name (car names)))
                         (cond
                           ((containsp "=" name)
                            (destructuring-bind (group-name cell-name)
                                (cl-ppcre:split " = " name)
                              (let* ((node (app/container::child
                                            node group-name
                                            (app/container::make-tight-container group-name))))
                                (setf (app/container::child node cell-name)
                                      (make-instance box-type
                                                     :text cell-name)))))
                           ;; When there is no =, we just add it as a box of the given type
                           (t
                            (setf (app/container::child node name)
                                  (make-instance box-type
                                                 :text name))))))
                      ((> (length names)
                          1)
                       (add-box-to (app/container::child node (car names))
                                   (cdr names)
                                   :box-type box-type))))))
         (cond
           (runs
            (loop with root = (app/container::make-container workflow-name
                                                                       :comment workflow-path)
                  for run in runs
                  for run-name = (app/run::name run)
                  for status = (app/run::status run)
                  for conclusion = (app/run::conclusion run)
                  for box-type = (case status
                                   (:completed
                                    (case conclusion
                                      (:success 'success-box)
                                      (t 'fail-box)))
                                   (t
                                    'in-progress-box))
                  for chain = (if matrix
                                  (parse-run-name matrix
                                                  run)
                                  ;; When there is no matrix,
                                  ;; we just put to the container
                                  ;; each run's status.
                                  ;; Example of such workflow is
                                  ;; "Gather values for remainder of steps"
                                  ;; from github/licensed project
                                  (list (fmt "JOB = ~A"
                                             run-name)))
                  do (add-box-to root chain
                                 :box-type box-type)
                  finally (return root)))
           ;; When no runs, we want to render an in progress
           ;; box foro this workflow:
           (t
            (let ((root (app/container::make-tight-container workflow-name
                                                                       :comment workflow-path)))
              (add-box-to root (list "Waiting For A Run")
                          :box-type 'app/box::in-progress-box)))))))
    (t
     (make-instance 'app/box::in-progress-box
                    :text "No Github Actions Workflow Found :(" ))))
