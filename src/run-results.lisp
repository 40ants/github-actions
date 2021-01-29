(mgl-pax-minimal:define-package #:github-matrix/run-results
  (:use #:cl)
  (:import-from #:github-matrix/run)
  (:import-from #:github-matrix/container)
  (:import-from #:github-matrix/workflow)
  (:import-from #:github-matrix/matrix)
  (:import-from #:github-matrix/box
                #:success-box
                #:fail-box
                #:in-progress-box))
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
  (let* ((matrix (github-matrix/matrix::workflow-matrix workflow))
         ;; TODO: Remove this debug info
         ;; (matrix '(("run_tests" "os" "quicklisp-dist" "lisp")))
         )
    (labels ((add-box-to (node names &key box-type)
               (if (= (length names)
                      1)
                   (setf (github-matrix/container::child node (car names))
                         (make-instance box-type
                                        :text (car names)))
                   (add-box-to (github-matrix/container::child node (car names))
                               (cdr names)
                               :box-type box-type))))
      (loop with root = (github-matrix/container::make-container (github-matrix/workflow::name workflow))
            for run in runs
            for status = (github-matrix/run::status run)
            for conclusion = (github-matrix/run::conclusion run)
            for box-type = (case status
                             (:in_progress 'in-progress-box)
                             (t
                              (case conclusion
                                (:success 'success-box)
                                (t 'fail-box))))
            for chain = (parse-run-name matrix
                                        run)
            ;; TODO: Remove this debug info
            ;; for chain = (list (first chain1)
            ;;                   (third chain1)
            ;;                   (fourth chain1)
            ;;                   (second chain1))
            do (add-box-to root chain
                           :box-type box-type)
            finally (return root)))))
