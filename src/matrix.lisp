(uiop:define-package #:app/matrix
  (:use #:cl)
  (:import-from #:app/workflow)
  (:import-from #:app/yaml))
(in-package app/matrix)



(defun matrix-keyword (text)
  (member text
          '("include" "exclude")
          :test #'string-equal))


(defun parse-matrix (workflow-content)
  (let* ((doc (getf (app/yaml::parse-string workflow-content)
                    :documents)))
    (flet ((g (node name)
             (second (assoc name node :test 'string-equal))))
      (let ((workflow-name (g doc "name"))
            (jobs (g doc "jobs")))
        (list workflow-name jobs)
        (loop for (job-name params) in jobs
              for matrix = (g (g params "strategy")
                              "matrix")
              collect (list* job-name
                             (remove-if #'matrix-keyword
                                        (mapcar #'first
                                                matrix))))))))


(defun workflow-matrix (workflow)
  (let ((content (app/workflow::workflow-content workflow)))
    (when content
      (parse-matrix content))))
