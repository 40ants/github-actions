(uiop:define-package #:app/logging
  (:use #:cl)
  (:import-from #:log4cl-extras)
  (:import-from #:log4cl-extras/config))
(in-package #:app/logging)


(defun current-environment ()
  (or (uiop:getenv "ENVIRONMENT")
      "development"))


(defun setup (&key for-dev (level :warn))
  (let ((appenders
          (cond
            ((or (string-equal (current-environment)
                               "development")
                 for-dev)
             (list (list 'this-console
                         :layout :plain
                         :filter level)))
            (t
             (list (list 'this-console
                         :stream *standard-output*
                         :layout :json
                         :filter level))))))

    (log4cl-extras/config:setup
     (list :level :info
           :appenders appenders))))
