(declaim (optimize (debug 3)))

(ql:quickload :qlot)

(qlot/install:install-qlfile (probe-file "qlfile"))


;; Theoreticall, this should work instead of the next form.
;; But it doesn't :(
;; (qlot:quickload "github-matrix")

(qlot:with-local-quicklisp ((probe-file "qlfile"))
  (push "./" asdf:*central-registry*)
  (ql:quickload "github-matrix"))
