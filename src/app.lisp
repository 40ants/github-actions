(defpackage #:github-matrix/app
  (:use #:cl)
  (:import-from #:clack)
  (:import-from #:str)
  (:import-from #:woo)
  (:import-from #:spinneret)
  (:import-from #:alexandria)
  (:import-from #:github)
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

(defvar *debug* nil)

(defvar *last-env* nil)

(defvar *last-document* nil)


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

      (when *debug*
        (setf *last-document*
              document))

      (let* ((width (github-matrix/base-obj::width document))
             (height (github-matrix/base-obj::height document))
             (svg (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                            :width width
                                            :height height)))
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
  (destructuring-bind (&key
                         request-uri
                         headers
                       &allow-other-keys)
      env
    (log4cl-extras/context:with-fields (:uri request-uri)
      (log:info "Processing request")
      
      (when (and *debug*
                 (not (string= request-uri
                               "/debug")))
        (setf *last-env*
              env))
      
      (log4cl-extras/error:with-log-unhandled ()
        (cond
          ((string= "/" request-uri)
           (list 302
                 '(:content-type "text/plain"
                   :location "https://github.com/40ants/github-matrix")
                 (list "")))
          ((and (string= "/debug" request-uri)
                *debug*)
           (list 200
                 '(:content-type "text/plain")
                 (list (fmt "Last env:~%~S~2&Headers:~%~S"
                            *last-env*
                            (alexandria:hash-table-alist
                             (getf *last-env*
                                   :headers))))))
          ((str:starts-with-p "/debug/"
                              request-uri)
           (let* ((host (when headers
                          (gethash "host" headers)))
                  (uri
                    (subseq request-uri
                            (1- (length "/debug/"))))
                  (full-url (fmt "https://~A~A"
                                 host
                                 uri)))
             (list 200
                   '(:content-type "text/html")
                   (list (spinneret:with-html-string
                           (:h1 "Example of rendering")
                           (:p (:raw (fmt "Use <a href=\"~A\">~A</a> and we'll render:"
                                          full-url
                                          full-url)))
                           (:p (:img :src uri)))))))
          ((extract-user-and-project
            request-uri)
           (list 200
                 '(:content-type "image/svg+xml;charset=utf-8")
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
     ;; Here I want to use :json instead of plain
     ;; but don't know how to use structured logs with Heroku.
     :appenders ((this-console :layout :plain)))))


(defun setup-logging-for-dev ()
  (log4cl-extras/config:setup
   '(:level :debug
     :appenders ((this-console :layout :plain)))))


(defun cl-user::initialize-application (&key (port 8080))
  (setup-logging-for-prod)

  (setf github:*token*
        (uiop:getenv "GITHUB_TOKEN"))
  
  (start port :debug (uiop:getenv "DEBUG")))


