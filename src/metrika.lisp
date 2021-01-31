(defpackage #:github-matrix/metrika
  (:use #:cl)
  (:import-from #:cl-yandex-metrika)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:export
   #:hit))
(in-package github-matrix/metrika)


(defun hit (url &key params user-id)
  (ignore-errors
   (with-log-unhandled ()
     (let ((cl-yandex-metrika:*counter* "71694151"))
       (funcall #'cl-yandex-metrika:hit
                url
                :params params
                :user-id user-id)))))
