(defpackage #:github-matrix/run-results
  (:use #:cl)
  (:import-from #:github-matrix/run))
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
  (let* (;; (matrix (workflow-matrix workflow))
         ;; TODO: Remove this debug info
         (matrix '(("run_tests" "os" "quicklisp-dist" "lisp"))))
    (labels ((add-box-to (node names &key box-type)
               (if (= (length names)
                      1)
                   (setf (child node (car names))
                         (make-instance box-type
                                        :text (car names)))
                   (add-box-to (child node (car names))
                               (cdr names)
                               :box-type box-type))))
      (loop with root = (make-instance 'container :title (name workflow))
            for run in runs
            for conclusion = (conclusion run)
            for box-type = (if (eql conclusion
                                    :success)
                               'success-box
                               'fail-box)
            for chain1 = (parse-run-name matrix
                                         run)
            ;; TODO: Remove this debug info
            for chain = (list (first chain1)
                              (third chain1)
                              (fourth chain1)
                              (second chain1))
            do (add-box-to root chain
                           :box-type box-type)
            finally (return root)))))
