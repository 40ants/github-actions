(uiop:define-package #:app/server
  (:use #:cl)
  (:import-from #:github)
  (:import-from #:log4cl)
  (:import-from #:woo)
  (:import-from #:app/logging)
  (:import-from #:app/app)
  (:import-from #:app/slynk
                #:start-slynk-if-needed)
  (:export #:start
           #:stop))
(in-package #:app/server)


(defvar *server* nil)


(defun start (&key
                (port 8080)
                (interface "localhost")
                (debug nil))
  (when *server*
    (error "Server already running"))
  
  (setf app/metrika:*enabled*
        (not debug))

  (setf app/app::*debug* debug)

  (when app/app::*debug*
    (app/app::reset-cache-timeout 1))

  (setf github:*token*
        (uiop:getenv "GITHUB_TOKEN"))
  
  ;; Common steps

  (start-slynk-if-needed)
  (setf *server*
        (clack:clackup 'app/app::process-request
                       ;; Clack uses hunchentoot:one-thread-per-connection-taskmaster
                       ;; for processing multiple requests with hunchentoot server
                       ;; By default it may create unlimited number of threads and
                       ;; this may cause DoS.
                       ;; Thus it is better to put some limits;
                       :server :hunchentoot
                       :max-thread-count 32
                       :max-accept-count 64
                       ;; Woo has an issue with free worker selection:
                       ;; https://github.com/fukamachi/woo/issues/100
                       ;; That is why it is disabled for now.
                       ;; :server :woo
                       ;; :worker-num 32
                       :address interface
                       :debug debug
                       :port port))
  (app/logging::setup)
  (log:info "Server started"))


(defun stop ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)))
