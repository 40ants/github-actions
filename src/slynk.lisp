(defpackage #:github-matrix/slynk
  (:use #:cl)
  (:import-from #:slynk)
  (:import-from #:log4cl)
  (:import-from #:global-vars
                #:define-global-var)
  (:export
   #:*connections*
   #:setup
   #:start))
(in-package github-matrix/slynk)


(define-global-var *connections* nil
  "Here we'll store all Slynk connections.")

(defvar slynk:*use-dedicated-output-stream* nil
  "This var is defined only on SLY connection by MREPL
   plugin. Here we'll define it before this will happen
   to be able to set the value to nil in the START function.")


(defun on-connection-open (conn)
  (log:info "SLY connected")
  (push conn *connections*))


(defun on-connection-close (conn)
  (log:info "SLY disconnected")
  (setf *connections*
        (remove conn *connections*)))


(defun setup ()
  ;; To make it possible to connect to a remote SLYNK server where ports are closed
  ;; with firewall.
  (setf slynk:*use-dedicated-output-stream* nil)
  
  (slynk-api:add-hook slynk-api:*new-connection-hook*
                      'on-connection-open)
  (slynk-api:add-hook slynk-api:*connection-closed-hook*
                      'on-connection-close)
  (values))


(defun start (interface port)
  (log:info "Starting SLYNK server on" interface port)

  (setup)
  
  (slynk:create-server :dont-close t
                       :interface interface
                       :port port))
