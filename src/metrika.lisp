(uiop:define-package #:app/metrika
  (:use #:cl)
  (:import-from #:cl-yandex-metrika)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:export
   #:hit
   #:*enabled*))
(in-package app/metrika)


(defvar *enabled* t)


(defun hit (url &key params user-id)
  (ignore-errors
   (with-log-unhandled ()
     (when *enabled*
       (let ((cl-yandex-metrika:*counter* "71694151"))
         (funcall #'cl-yandex-metrika:hit
                  url
                  :params params
                  :user-id user-id))))))
