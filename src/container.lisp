(mgl-pax-minimal:define-package #:github-matrix/container
  (:use #:cl)
  (:import-from #:github-matrix/base-obj
                #:obj-with-font
                #:draw
                #:width
                #:height
                #:font-family
                #:font-weight
                #:font-size)
  (:import-from #:github-matrix/box)
  (:import-from #:github-matrix/colors
                #:*tight-container-title-background*
                #:*tight-container-title-color*)
  (:import-from #:github-matrix/svg
                #:draw-box-with-text)
  (:import-from #:alexandria
                #:hash-table-values))
(in-package github-matrix/container)


(defvar *debug-sizes* nil)


(defclass container (obj-with-font)
  ((title :initarg :title)
   (margin :initarg :margin
           :initform 5)
   (children :initform (make-hash-table :test 'equal)
             :documentation "Alist with children.")))


(defun make-container (title)
  (make-instance 'container
                 :title title))


(defclass tight-container (container)
  ((title-box))
  (:default-initargs
   :margin 0
   :title-background *tight-container-title-background*
   :title-color *tight-container-title-color*))


(defun make-tight-container (title)
  (make-instance 'tight-container
                 :title title))


(defmethod print-object ((obj container) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "\"~A\""
            (slot-value obj 'title))))

(defmethod initialize-instance :after ((obj tight-container)
                                       &rest initargs
                                       &key
                                         title
                                         title-color
                                         title-background)
  (declare (ignore initargs))
  (setf (slot-value obj 'title-box)
        (make-instance 'github-matrix/box::box
                       :text title
                       :color title-color
                       :background title-background)))


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


(defmethod child ((obj container) name &optional default)
  (with-slots (children) obj
    (or (gethash name children)
        (setf (gethash name children)
              (or default
                  (make-instance 'container
                                 :title name))))))


(defun orientation (container)
  "Depends on max child width"
  (with-slots (children) container
    (loop for child being the hash-value of children
          summing (width child) into row-width
          finally (return
                    (if (> row-width 800)
                        :column
                        :row)))))


(defmethod width ((obj container))
  (with-slots (children margin) obj
    (ecase (orientation obj)
      (:row
       (+ (loop for child being the hash-value of children
                summing (width child))
           (* margin
              (1- (hash-table-count children)))))
      (:column
       (loop for child being the hash-value of children
             maximizing (width child))))))


(defmethod header-height ((obj container))
  (with-slots (font-size margin)
      obj
    (+ font-size
        (* margin 2))))


(defmethod height ((obj container))
  (with-slots (children font-size margin)
      obj
    (let ((children (hash-table-values children)))
      (+ (ecase (orientation obj)
           (:row
            (+ (* margin 2)
                (loop for child in children
                      maximizing (height child))))
           (:column
            (+ (* margin (1+ (length children)))
                (loop for child in children
                      summing (height child)))))
          (cond
            ;; If there is only one child and it is not a tight,
            ;; then there is no reason
            ;; to render outer container
            ((and (= (length children)
                     1)
                  (not (typep (first children)
                              'tight-container)))
             0)
            ;; Otherwise, draw them all!
            (t
             ;; We draw header only if there is more than one
             ;; child in the box:
             (header-height obj)))))))


(defmethod width ((obj tight-container))
  (with-slots (children title-box)
      obj
    (+ (width title-box)
        (loop for child being the hash-value of children
              summing (width child)))))


(defmethod height ((obj tight-container))
  (with-slots (children)
      obj
    (loop for child being the hash-value of children
          maximizing (height child))))


(defmethod draw ((obj container) svg)
  (with-slots (children) obj
    (let* ((children (alexandria:hash-table-values children))
           (first-child (car children)))
      (cond
        ;; If there is only one child and it is not a tight,
        ;; then there is no reason
        ;; to render outer container
        ((and (= (length children)
                 1)
              (not (typep first-child
                          'tight-container)))
         (draw first-child svg))
        ;; Otherwise, draw them all!
        (t
         (inner-draw obj svg))))))


(defmethod inner-draw ((obj container) svg)
  (with-slots (children font-family font-weight font-size margin title) obj
    (let* ((group (cl-svg:make-group svg ()))
           (full-width (width obj))
           (full-height (height obj))
           (font-data (anafanafo:load-data :family font-family
                                           :weight font-weight
                                           :size font-size))
           (text-width (anafanafo:string-width font-data title)))
      
      (when *debug-sizes*
        (cl-svg:draw group
            (:rect :x 0 :y 0 :width full-width :height full-height)
            :stroke "#FFAA66"
            :fill "white"))

      (let* ((max-title-width (- full-width
                                  (* margin 2)))
             (scale
               (if (> text-width
                      max-title-width)
                   (/ max-title-width
                      text-width)
                   1.0)))
        (cl-svg:transform
            ((cl-svg:translate (1+ margin)
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
            ((cl-svg:translate margin
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
         (loop with x = 0
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
         (loop with x = 0
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
  (with-slots (children font-family font-weight font-size margin title-box) obj
    (let* ((group (cl-svg:make-group svg ())))

      (when *debug-sizes*
       (let ((full-width (width obj))
             (full-height (height obj)))
         (cl-svg:draw group
             (:rect :x 0 :y 0 :width full-width :height full-height)
             :stroke "#FF2266"
             :fill "white")))
      
      (draw title-box group)

      (loop with x = (width title-box)
            with y = 0
            for child being the hash-value of children
            do (cl-svg:transform (cl-svg:translate
                                  x
                                  y)
                 (let ((child-svg (draw child group)))
                   ;; Now we need to draw a delimiter line
                   (cl-svg:draw child-svg
                       (:line :x1 0 :y1 0
                              :x2 0 :y2 (height child))
                       :stroke "white")
                   child-svg))
               (incf x
                     (width child)))
      (values group))))


