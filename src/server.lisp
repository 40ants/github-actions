(uiop:define-package #:app/server
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:woo)
  (:import-from #:app/logging)
  (:import-from #:app/app
                #:app)
  (:import-from #:app/slynk
                #:start-slynk-if-needed)
  (:export
   #:start))
(in-package #:app/server)


(defun start (&key (port 8080)
		(interface "localhost"))
  ;; Just to suppres debug logs to TTY from Reblocks.
  ;; I'll need to fix Reblocks to prohibit it from
  ;; configure logging if they are already configured.
  (app/logging::setup)
  (start-slynk-if-needed)
  (reblocks/server:start :port port
			 :interface interface
                         :apps 'app
			 :server-type :woo)
  (app/logging::setup)
  (log:error "Server started"))


(defun cl-user::start-server ()
  ;; Entry point for webapp, started in the Docker
  (start :port (parse-integer (or (uiop:getenv "APP_PORT")
				  "80"))
	 :interface (or (uiop:getenv "APP_INTERFACE")
			"0.0.0.0"))
  (loop do (sleep 5)))


(defun start (port &key
		     (debug nil)
		     (address "0.0.0.0"))
  (setf app/metrika:*enabled*
        (not debug))

  (setf *debug* debug)

  (when *debug*
    (reset-cache-timeout 1))
  
  (setf *server*
        (clack:clackup 'process-request
                       :server :woo
                       :address address
                       :debug debug
                       :port port)))

(defun stop ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)))
