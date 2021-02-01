(mgl-pax-minimal:define-package #:github-matrix/run-results
  (:use #:cl)
  (:import-from #:github-matrix/run)
  (:import-from #:github-matrix/container)
  (:import-from #:github-matrix/workflow)
  (:import-from #:github-matrix/matrix)
  (:import-from #:github-matrix/box
                #:success-box
                #:fail-box
                #:in-progress-box)
  (:import-from #:cl-ppcre)
  (:import-from #:rutils
                #:fmt))
(in-package github-matrix/run-results)


(defun parse-run-name (workflow-matrix run)
  "Принимает workflow матрицу и run.

   Матрица выглядит как:

   ((\"run_tests\" \"lisp\" \"os\" \"quicklisp-dist\"))

   Парсит название run вида:

   \"run_tests (sbcl-bin, ubuntu-latest, quicklisp)\"

   и выдаёт список:

   '(\"job=run_tests\"
     \"lisp=sbcl-bin\"
     \"os=ubuntu-latest\"
     \"quicklisp-dist=quicklisp\")
"
  (cl-ppcre:register-groups-bind (job-name params)
      ("(.*?) \\((.*)\\)" (github-matrix/run::name run))
    (let ((job-matrix (cdr (assoc job-name workflow-matrix
                                  :test #'string-equal)))
          (params (cl-ppcre:split ", " params)))
      (list* (format nil "JOB = ~A"
                     job-name)
             (loop for param in params
                   for matrix-key in job-matrix
                   collect (format nil "~A = ~A"
                                   (string-upcase matrix-key)
                                   param))))))


(defun runs-to-boxes (workflow &key (runs (github-matrix/run::get-last-run workflow)))
  "Преобразует список, полученный через get-last-run в containers со статусами."
  (cond
    (workflow
     (let* ((matrix (github-matrix/matrix::workflow-matrix workflow))
            (github-matrix/base-obj::*default-font-size* 20)
            ;; This is how much we'll reduce the font size on each
            ;; level of GitHub matrix:
            (font-size-ratio 0.85))

       (labels ((add-box-to (node names &key box-type)
                  (let ((github-matrix/base-obj::*default-font-size*
                          (* github-matrix/base-obj::*default-font-size*
                             font-size-ratio)))
                    (cond
                      ((= (length names)
                          1)
                       (destructuring-bind (group-name cell-name)
                           (cl-ppcre:split " = " (car names))
                         (let* ((node (github-matrix/container::child
                                       node group-name
                                       (github-matrix/container::make-tight-container group-name))))
                           (setf (github-matrix/container::child node cell-name)
                                 (make-instance box-type
                                                :text cell-name)))))
                      ((> (length names)
                          1)
                       (add-box-to (github-matrix/container::child node (car names))
                                   (cdr names)
                                   :box-type box-type))))))
         (loop with root = (github-matrix/container::make-container (github-matrix/workflow::name workflow))
               for run in runs
               for run-name = (github-matrix/run::name run)
               for status = (github-matrix/run::status run)
               for conclusion = (github-matrix/run::conclusion run)
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
               finally (return root)))))
    (t
     (make-instance 'github-matrix/box::in-progress-box
                    :text "No Github Actions Forkflow Found :(" ))))
