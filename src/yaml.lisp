(uiop:define-package #:app/yaml
  (:use #:cl)
  (:import-from #:cl-yaml))
(in-package app/yaml)


(defun parse-string (yaml-string)
  (parse-yaml-tokens (yaml.parser::parse-yaml yaml-string)))


(defun collect-mapping (plist)
  (loop for (key value) on plist by #'cddr
        collect (list key value)))


(defun parse-yaml-tokens (vector)
  (let ((contexts (list (list :documents))))
    (loop for token across vector do
      (alexandria:destructuring-case token
        ;; Documents
        ((:document-start-event)
         (push (list) contexts))
        ((:document-end-event)
         (let ((con (pop contexts)))
           (setf (first contexts)
                 (append (first contexts)
                         con))))
        ;; Alias event
        ;; Disabled since it's not supported
        #|
        ((:alias-event &key anchor)     ; ; ; ; ; ; ;
        (declare (ignore anchor))       ; ; ; ; ; ; ;
        t)                              ; ; ; ; ; ; ;
        |#
        ;; Scalar
        ((:scalar-event &key anchor tag value length plain-implicit quoted-implicit style)
         (declare (ignore anchor length plain-implicit quoted-implicit))
         (setf (first contexts)
               (append (first contexts)
                       (list (yaml.parser::convert-scalar value tag style)))))
        ;; Sequence start event
        ((:sequence-start-event &key anchor tag implicit style)
         (declare (ignore anchor implicit style))
         (push (list tag) contexts))
        ;; Mapping start event
        ((:mapping-start-event &key anchor tag implicit style)
         (declare (ignore anchor implicit style))
         (push (list tag) contexts))
        ;; End events
        ((:sequence-end-event)
         (destructuring-bind (tag &rest seq) (pop contexts)
           (setf (first contexts)
                 (append (first contexts)
                         (list (yaml.parser::convert-sequence seq tag))))))
        ((:mapping-end-event)
         (destructuring-bind (tag &rest plist) (pop contexts)
           (declare (ignore tag))
           (setf (first contexts)
                 (append (first contexts)
                         (list (collect-mapping plist))))))
        ;; Do nothing
        ((t &rest rest)
         (declare (ignore rest)))))
    (first contexts)))
