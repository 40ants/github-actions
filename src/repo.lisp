(defpackage #:github-matrix/repo
  (:use #:cl)
  (:import-from #:rutils
                #:fmt))
(in-package github-matrix/repo)


(defclass repo ()
  ((user :initarg :user
         :reader user)
   (project :initarg :project
            :reader project)
   (branch :initarg :branch
           :reader branch)))


(defun make-repo (user project &key branch)
  (make-instance 'repo
                 :user user
                 :project project
                 :branch branch))

(defmethod print-object ((obj repo) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A/~A@~A"
            (user obj)
            (project obj)
            (branch obj))))


(defun get-file (repo path)
  (let ((url (fmt "https://raw.githubusercontent.com/~A/~A/~A/~A"
                  (user repo)
                  (project repo)
                  (branch repo)
                  path)))
    (values (dex:get url))))
