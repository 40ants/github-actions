(defpackage #:github-matrix/app
  (:use #:cl)
  (:import-from #:clack)
  (:import-from #:woo)
  (:import-from #:log4cl-extras/error)
  (:import-from #:log4cl-extras/config)
  (:import-from #:rutils
                #:fmt)
  (:import-from #:cl-ppcre
                #:register-groups-bind)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:github-matrix/repo)
  (:import-from #:github-matrix/workflow)
  (:import-from #:github-matrix/run-results)
  (:import-from #:github-matrix/base-obj)
  (:export
   #:stop
   #:start
   #:setup-logging-for-dev))
(in-package github-matrix/app)


(defvar *server* nil)


(defun extract-user-and-project (uri)
  (register-groups-bind (user project)
      ("^/(.*?)/(.*?)/matrix.svg$" uri)
    (list user project)))


(defcached (make-svg-response
            ;; Response will be cached for 5 minutes
            :timeout (* 60 5))
    (uri)
  (destructuring-bind (user project)
      (extract-user-and-project uri)
    (let* ((repo (github-matrix/repo::make-repo user project))
           (workflow (first (github-matrix/workflow::get-workflows repo)))
           (document (github-matrix/run-results::runs-to-boxes workflow)))

      (let ((svg (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel)))
        (github-matrix/base-obj::draw document svg)
        (with-output-to-string (s)
          (cl-svg:stream-out s svg))))))


(defun retrieve-data (&key dont-fetch-runs)
  (setf *repo*
        (github-matrix/repo::make-repo "40ants" "cl-info" :branch "master"))

  (setf *workflow*
        (first (github-matrix/workflow::get-workflows *repo*)))

  (unless dont-fetch-runs
    (setf *runs*
          (github-matrix/run::get-last-run *workflow*)))
  
  (setf *root* (github-matrix/run-results::runs-to-boxes *workflow* :runs *runs*)))


(defun process-request (env)
  (destructuring-bind (&key request-uri &allow-other-keys)
      env
    (log4cl-extras/context:with-fields (:uri request-uri)
      (log:info "Processing request")
      (log4cl-extras/error:with-log-unhandled ()
        (cond
          ((string= "/" request-uri)
           (list 302
                 '(:content-type "text/plain"
                   :location "https://github.com/40ants/github-matrix")
                 (list "")))
          ((extract-user-and-project request-uri)
           (list 200
                 '(:content-type "image/svg")
                 (list (make-svg-response request-uri))))
          (t
           (list 404
                 '(:content-type "text/plain")
                 (list (fmt "Path \"~A\" not supported."
                            request-uri)))))))))


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


