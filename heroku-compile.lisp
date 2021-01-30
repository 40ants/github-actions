(declaim (optimize (debug 3)))

(ql:quickload :qlot)

(format t "*load-truename*: ~S~%"
        *load-truename*)

(let* ((raw-qlfile (uiop:merge-pathnames* #P"qlfile"
                                          (uiop:pathname-directory-pathname
                                           *load-truename*)))
       (qlfile (probe-file raw-qlfile)))
  
  (format t "qlfile: ~S~%"
          raw-qlfile)

  (unless qlfile
    (format t "qlfile not found!~%"))
  
  (qlot/install:install-qlfile qlfile)

  ;; Theoreticall, this should work instead of the next form.
  ;; But it doesn't :(
  ;; (qlot:quickload "github-matrix")

  (qlot:with-local-quicklisp (qlfile)
    (push "./" asdf:*central-registry*)
    (ql:quickload "github-matrix")))
