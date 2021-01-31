(defpackage #:github-matrix/app
  (:use #:cl)
  (:import-from #:clack)
  (:import-from #:woo)
  (:import-from #:spinneret)
  (:import-from #:alexandria)
  (:import-from #:github)
  (:import-from #:log4cl-extras/error)
  (:import-from #:log4cl-extras/config)
  (:import-from #:rutils
                #:starts-with
                #:fmt)
  (:import-from #:github-matrix/slynk)
  (:import-from #:cl-ppcre
                #:register-groups-bind)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:github-matrix/repo)
  (:import-from #:github-matrix/workflow)
  (:import-from #:github-matrix/run-results)
  (:import-from #:github-matrix/base-obj)
  (:import-from #:github-matrix/svg)
  (:export
   #:stop
   #:start
   #:setup-logging-for-dev))
(in-package github-matrix/app)


(defvar *server* nil)

(defvar *debug* nil)

(defvar *last-env* nil)

(defvar *last-document* nil)

;; Response will be cached for 15 minutes
(progn
  (defvar *make-svg-response-cache* nil)
  (defparameter *cache-timeout* (* 15 60))
  
  (when *make-svg-response-cache*
    (function-cache:clear-cache *make-svg-response-cache*)
    (setf (function-cache:timeout *make-svg-response-cache*)
          *cache-timeout*)))


(defun extract-user-and-project (uri)
  (register-groups-bind (user project)
      ("^/(.*?)/(.*?)/matrix.svg$" uri)
    (list user project)))


(defcached (make-svg-response
            :timeout *cache-timeout*)
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
             (footer-text "Rendered by github-actions.40ants.com")
             (footer-font-family "Helvetica")
             (footer-font-weight "bold")
             (footer-font-size 8)
             (footer-height footer-font-size)
             (font-data (anafanafo:load-data :family footer-font-family
                                             :weight footer-font-weight
                                             :size footer-height))
             (footer-width (anafanafo:string-width font-data
                                                   footer-text))
             (height (+ (github-matrix/base-obj::height document)
                         footer-height))
             (svg (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                            :width width
                                            :height height)))
        (github-matrix/base-obj::draw document svg)
        
        (github-matrix/svg:text
            (cl-svg:link svg (:xlink-href "https://github-actions.40ants.com/"))
            footer-text
          :x (- width footer-width)
          :y (- height footer-height)
          :font-family footer-font-family
          :font-weight footer-font-weight
          :font-size footer-font-size
          :color github-matrix/colors:*link-color*
          :shadow-opacity 0.2)
        
        (with-output-to-string (s)
          (cl-svg:stream-out s svg))))))


(defun process-request (env)
  (destructuring-bind (&key
                         path-info
                         headers
                       &allow-other-keys)
      env
    (log4cl-extras/context:with-fields (:uri path-info)
      (log:info "Processing request")
      
      (when (and *debug*
                 (not (starts-with "/debug"
                                   path-info)))
        (setf *last-env*
              env))
      
      (log4cl-extras/error:with-log-unhandled ()
        (cond
          ((string= "/" path-info)
           (list 302
                 '(:content-type "text/plain"
                   :location "https://github.com/40ants/github-matrix")
                 (list "")))
          ((and (string= "/debug" path-info)
                *debug*)
           (list 200
                 '(:content-type "text/plain")
                 (list (fmt "Last env:~%~S~2&Headers:~%~S"
                            *last-env*
                            (alexandria:hash-table-alist
                             (getf *last-env*
                                   :headers))))))
          ((starts-with "/debug/"
                        path-info)
           (let* ((host (when headers
                          (gethash "host" headers)))
                  (uri
                    (subseq path-info
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
            path-info)
           (list 200
                 (list :content-type "image/svg+xml;charset=utf-8"
                       :cache-control (fmt "max-age=~A"
                                           *cache-timeout*))
                 (list (make-svg-response path-info))))
          (t
           (list 404
                 '(:content-type "text/plain")
                 (list (fmt "Path \"~A\" not supported."
                            path-info)))))))))


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


;; We need this becase Dexador's thread pool is
;; not threadsafe yet. You'll find more details in this issue:
;; https://github.com/fukamachi/dexador/issues/88
(defun dexador:make-connection-pool ()
  (make-hash-table :test 'equal
                   #+sbcl :synchronized #+sbcl t))


(defun cl-user::initialize-application (&key (port 8080))
  (setup-logging-for-prod)

  (setf github:*token*
        (uiop:getenv "GITHUB_TOKEN"))

  (let ((slynk-port (parse-integer (or (uiop:getenv "SLYNK_PORT")
                                       "4005")))
        (slynk-interface (or (uiop:getenv "SLYNK_INTERFACE")
                             "127.0.0.1"))
        (debug (when (uiop:getenv "DEBUG")
                 t)))

    (github-matrix/slynk:start slynk-interface
                               slynk-port)
    
    (log:info "Starting HTTP server on" port "with" debug)
    (start port :debug debug)))


