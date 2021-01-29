(defpackage #:github-matrix/container
  (:use #:cl)
  (:import-from #:github-matrix/base-obj
                #:obj-with-font
                #:draw
                #:width
                #:height
                #:font-family
                #:font-weight
                #:font-size)
  (:import-from #:github-matrix/box))
(in-package github-matrix/container)


(defclass container (obj-with-font)
  ((title :initarg :title)
   (margin :initarg :margin
           :initform 5)
   (children :initform (make-hash-table :test 'equal)
             :documentation "Alist with children.")))


(defclass tight-container (container)
  ()
  (:default-initargs :margin 0))


(defmethod print-object ((obj container) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "\"~A\""
            (slot-value obj 'title))))


(defmethod (setf child) (new-value (obj container) name)
  (with-slots (children) obj
    (setf (gethash name children)
          new-value)))


(defmethod (setf child) :after ((new-value github-matrix/box::box) (obj tight-container) name)
  "Boxes inside the tight container should not have
   round corners."
  (setf (slot-value new-value
                    'github-matrix/box::corners-radius)
        nil))


;; (defmethod (setf child) (new-value (obj null) name)
;;   (error "Blah")
;;   (with-slots (children) obj
;;     (setf (gethash name children)
;;           new-value)))


(defmethod child ((obj container) name)
  (with-slots (children) obj
    (or (gethash name children)
        (setf (gethash name children)
              (make-instance 'container
                             :title name)))))


(defun orientation (container)
  "Depends on max child width"
  (with-slots (children) container
    (loop for child being the hash-value of children
          maximize (width child) into max-width
          finally (return
                    (if (> max-width 400)
                        :column
                        :row)))))


(defmethod width ((obj container))
  (with-slots (children margin) obj
    (ecase (orientation obj)
      (:row
       (+ (loop for child being the hash-value of children
                summing (width child))
           (* margin
              (1+ (hash-table-count children)))))
      (:column
       (+ (* margin 2)
           (loop for child being the hash-value of children
                 maximizing (width child)))))))


(defmethod header-height ((obj container))
  (with-slots (font-size margin)
      obj
    (+ font-size
        (* margin 2))))


(defmethod height ((obj container))
  (with-slots (children font-size margin)
      obj
    (+ (ecase (orientation obj)
         (:row
          (+ (* margin 2)
              (loop for child being the hash-value of children
                    maximizing (height child))))
         (:column
          (+ (* margin (1+ (hash-table-count children)))
              (loop for child being the hash-value of children
                    summing (height child)))))
        (header-height obj))))


(defvar *level* 0)


(defmethod draw ((obj container) svg)
  (with-slots (children) obj
    (cond
      ;; If there is only one child, then there is no reason
      ;; to render outer container
      ((= (hash-table-count children)
          1)
       (draw (first (alexandria:hash-table-values children))
           svg))
      ;; Otherwise, draw them all!
      (t
       (inner-draw obj svg)))))


(defmethod inner-draw ((obj container) svg)
  (with-slots (children font-family font-weight font-size margin title) obj
    (let* ((*level* (1+ *level*))
           (group (cl-svg:make-group svg ()))
           (full-width (width obj))
           (full-height (height obj))
           (font-data (anafanafo:load-data :family font-family
                                           :weight font-weight
                                           :size font-size))
           (text-width (anafanafo:string-width font-data title)))

      (cl-svg:draw group
          (:rect :x 0 :y 0 :width full-width :height full-height)
          :stroke "#FFAA66"
          :fill "white")

      (let* ((max-title-width (- full-width
                                  (* margin 2)))
             (scale
               (if (> text-width
                      max-title-width)
                   (/ max-title-width
                      text-width)
                   1.0)))
        (cl-svg:transform
            ((cl-svg:translate (1+ (* 2 margin))
                               (1+ (+ margin
                                       font-size)))
             (cl-svg:scale scale))
          (cl-svg:text group
              (:x 0
               :y 0
               :font-family font-family
               :font-weight font-weight
               :font-size font-size
               :fill "#ccc"
               :fill-opacity 0.5
               :text-length text-width)
            title))
       
        (cl-svg:transform
            ((cl-svg:translate (* 2 margin)
                               (+ margin
                                   font-size))
             (cl-svg:scale scale))
          (cl-svg:text group
              (:x 0
               :y 0
               :font-family font-family
               :font-weight font-weight
               :font-size font-size
               :fill "#555"
               :text-length text-width)
            title)))

      (ecase (orientation obj)
        (:row
         (loop with x = margin
               with y = (+ (header-height obj)
                            margin)
               for child being the hash-value of children
               do (cl-svg:transform (cl-svg:translate
                                     x
                                     y)
                    (draw child group))
                  (incf x
                        (+ (width child)
                            margin))))
        (:column
         (loop with x = margin
               with y = (+ (header-height obj)
                            margin)
               for child being the hash-value of children
               do (cl-svg:transform (cl-svg:translate
                                     x
                                     y)
                    (draw child group))
                  (incf y
                        (+ (height child)
                            margin)))))
      (values group))))


(defmethod inner-draw ((obj tight-container) svg)
  ;; Tight container always drawn as a row.
  (with-slots (children font-family font-weight font-size margin title) obj
    (let* ((*level* (1+ *level*))
           (group (cl-svg:make-group svg ()))
           (full-width (width obj))
           (full-height (height obj))
           (font-data (anafanafo:load-data :family font-family
                                           :weight font-weight
                                           :size font-size))
           (text-width (anafanafo:string-width font-data title)))

      (cl-svg:draw group
          (:rect :x 0 :y 0 :width full-width :height full-height)
          :stroke "#FFAA66"
          :fill "white")

      (let* ((max-title-width (- full-width
                                  (* margin 2)))
             (scale
               (if (> text-width
                      max-title-width)
                   (/ max-title-width
                      text-width)
                   1.0)))
        (cl-svg:transform
            ((cl-svg:translate (1+ (* 2 margin))
                               (1+ (+ margin
                                       font-size)))
             (cl-svg:scale scale))
          (cl-svg:text group
              (:x 0
               :y 0
               :font-family font-family
               :font-weight font-weight
               :font-size font-size
               :fill "#ccc"
               :fill-opacity 0.5
               :text-length text-width)
            title))
       
        (cl-svg:transform
            ((cl-svg:translate (* 2 margin)
                               (+ margin
                                   font-size))
             (cl-svg:scale scale))
          (cl-svg:text group
              (:x 0
               :y 0
               :font-family font-family
               :font-weight font-weight
               :font-size font-size
               :fill "#555"
               :text-length text-width)
            title)))

      (loop with x = margin
            with y = (+ (header-height obj)
                         margin)
            for child being the hash-value of children
            do (cl-svg:transform (cl-svg:translate
                                  x
                                  y)
                 (draw child group))
               (incf x
                     (+ (width child)
                         margin)))
      (values group))))


