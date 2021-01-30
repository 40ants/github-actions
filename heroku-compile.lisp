(declaim (optimize (debug 3)))

(ql:quickload :qlot)

(let ((qlfile (probe-file (uiop:merge-pathnames* "qlfile"))))
  (qlot/install:install-qlfile qlfile)

  ;; Theoreticall, this should work instead of the next form.
  ;; But it doesn't :(
  ;; (qlot:quickload "github-matrix")

  (qlot:with-local-quicklisp (qlfile)
    (push "./" asdf:*central-registry*)
    (ql:quickload "github-matrix")))
