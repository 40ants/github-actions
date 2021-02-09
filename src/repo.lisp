(mgl-pax-minimal:define-package #:github-matrix/repo
  (:use #:cl)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:dexador)
  (:import-from #:github))
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
  (handler-case
      (let ((url (fmt "https://raw.githubusercontent.com/~A/~A/~A/~A"
                      (user repo)
                      (project repo)
                      (branch repo)
                      path)))
        (log:info "Fetching ~A" url)
        (values (dex:get url)))
    (dexador.error:http-request-not-found (c)
      (declare (ignore c))
      nil)
    ;; Sometimes workflow path can be just
    ;; something like "Build Failed" :(
    ;; found at https://github.com/facebook/watchman
    (dexador.error:http-request-bad-request (c)
      (declare (ignore c))
      nil)))


(defun get-default-branch (repo)
  (handler-case
      (getf
       (github:get "https://api.github.com/repos/~A/~A"
                   :params (list (user repo)
                                 (project repo)))
       :|default_branch|)
    (dexador:http-request-not-found ()
      nil)))
