(uiop:define-package #:app/utils
  (:use #:cl)
  (:import-from #:rutils
                #:fmt)
  (:export
   #:get-base-url))
(in-package app/utils)


(defun get-base-url (env)
  "Returns a root url of the current server without a slash at the end."
  (destructuring-bind (&key
                         headers
                         url-scheme
                       &allow-other-keys)
      env
    (let ((host (when headers
                  (gethash "host" headers))))
      (fmt "~A://~A"
           url-scheme
           host))))
