(mgl-pax-minimal:define-package #:github-matrix/repo
  (:use #:cl)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:dexador))
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


(defmethod initialize-instance :after ((obj repo) &rest rest &key branch)
  (declare (ignore rest))
  
  (unless branch
    (setf (slot-value obj 'branch)
          (get-default-branch obj))))


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


(defun get-default-branch (repo)
  (handler-case
      (getf
       (github:get (fmt "https://api.github.com/repos/~A/~A"
                        (user repo)
                        (project repo)))
       :|default_branch|)
    (dexador:http-request-not-found ()
      nil)))
