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
                       :server :woo
                       :address interface
                       :debug debug
                       :port port))
  (app/logging::setup)
  (log:info "Server started"))


(defun stop ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)))
