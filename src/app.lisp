(defpackage #:github-matrix/app
  (:use #:cl)
  (:import-from #:clack)
  (:import-from #:woo)
  (:import-from #:log4cl-extras/error)
  (:export
   #:stop
   #:start
   #:setup-logging-for-dev))
(in-package github-matrix/app)


(defvar *server* nil)


(defun process-request (env)
  (destructuring-bind (&key request-uri &allow-other-keys)
      env
    (log4cl-extras/context:with-fields (:uri request-uri)
      (log:info "Processing request")
      (log4cl-extras/error:with-log-unhandled ()
        '(200 (:content-type "text/plain") ("Hello, World"))))))


(defun start (port &key (debug nil))
  (setf *server*
        (clack:clackup 'process-request
                       :server :woo
                       :address "0.0.0.0"
                       :debug debug
                       :port port)))

(defun stop ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)))


(defun setup-logging-for-prod ()
  (log4cl-extras/config:setup
   '(:level :debug
     :appenders ((this-console :layout :json)))))


(defun setup-logging-for-dev ()
  (log4cl-extras/config:setup
   '(:level :debug
     :appenders ((this-console :layout :plain)))))


(defun cl-user::initialize-application (&key (port 8080))
  (setup-logging-for-prod)
  
  (start port))


