(uiop:define-package #:github-matrix/container
  (:use #:cl)
  (:import-from #:github-matrix/base-obj
                #:obj-with-font
                #:draw
                #:width
                #:height
                #:font-family
                #:font-weight
                #:font-size)
  (:import-from #:github-matrix/box
                #:in-progress-box)
  (:import-from #:github-matrix/colors
                #:*title-color*
                #:*tight-container-title-background*
                #:*tight-container-title-color*)
  (:import-from #:github-matrix/svg
                #:draw-box-with-text)
  (:import-from #:alexandria
                #:hash-table-values)
  (:import-from #:rutils
                #:once-only
                #:fmt)
  (:import-from #:serapeum
                #:defvar-unbound)
  (:export
   #:with-leafs-counted
   #:leafs-count))
(in-package github-matrix/container)


(defvar *debug-sizes* nil)


(defvar-unbound *num-leaf-containers*
  "Here we'll put a number of leaf when executing a body of the WITH-LEAFS-COUNTED macro.")

(defvar *fold-title-with-one-child* t
  "This variable will be changed by the WITH-LEAFS-COUNTED macro as well. It will turn NIL as soon as we encounter a node with more than one child.")


(defclass container (obj-with-font)
  ((title :initarg :title)
   (comment :initarg :comment)
   (margin :initarg :margin
           :initform 5)
   (children :initform (make-hash-table :test 'equal)
             :documentation "Alist with children.")))


(defun make-container (title &key comment)
  (make-instance 'container
                 :title title
                 :comment comment))


(defclass tight-container (container)
  ((title-box))
  (:default-initargs
   :margin 0
   :title-background *tight-container-title-background*
   :title-color *tight-container-title-color*))


(defun make-tight-container (title &key comment)
  (make-instance 'tight-container
                 :title title
                 :comment comment))


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
        (make-instance 'github-matrix/box::title-box
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
                  (make-container name))))))


(defmacro with-leafs-counted ((container) &body body)
  (rutils:with-gensyms (children-sym)
    (once-only (container)
      `(with-slots ((,children-sym children))
           ,container
         (let ((*num-leaf-containers* (if (boundp '*num-leaf-containers*)
                                          *num-leaf-containers*
                                          (count-leafs ,container)))
               (*fold-title-with-one-child* (and *fold-title-with-one-child*
                                                 (< (hash-table-count ,children-sym)
                                                    2)) ))
           ,@body)))))


(defun leafs-count ()
  *num-leaf-containers*)


(defun count-leafs (container)
  (etypecase container
    (in-progress-box 1)
    (tight-container 1)
    (container
     (with-slots (children)
         container
       (if (zerop (hash-table-count children))
           1
           (loop for child being the hash-value of children
                 summing (count-leafs child)))))))


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
  (with-slots (children margin font-family font-weight font-size title)
      obj
    (let ((children-width
            (ecase (orientation obj)
              (:row
               (+ (loop for child being the hash-value of children
                        summing (width child))
                   (* margin
                      (1- (hash-table-count children)))))
              (:column
               (loop for child being the hash-value of children
                     maximizing (width child)))))
          (title-width
            (let* ((font-data (anafanafo:load-data :family font-family
                                                   :weight font-weight
                                                   :size font-size))
                   (text-width (anafanafo:string-width font-data title)))
              (+ text-width
                  (* margin 2)))))
      (max children-width
           title-width))))


(defmethod header-height ((obj container))
  (with-slots (font-size margin)
      obj
    (+ font-size
        (* margin 2))))


(defmethod height :around ((obj container))
  (with-leafs-counted (obj)
    (call-next-method)))


(defmethod height ((obj container))
  (with-slots (children font-size margin)
      obj
    (let ((children (hash-table-values children)))
      (cond
        ((should-we-render-only-a-child obj)
         (height (first children)))
        ;; otherwise
        (t (+ (ecase (orientation obj)
                (:row
                 (+ margin
                     (loop for child in children
                           maximizing (height child))))
                (:column
                 (+ (* margin (length children))
                     (loop for child in children
                           summing (height child)))))
               (cond
                 ;; If there is only one child
                 ;; then there is no reason
                 ;; to render outer container
                 ;; and we don't need to reserve space
                 ;; for its header.
                 ((should-we-render-only-a-child obj)
                  0)
                 ;; Otherwise, draw them all!
                 (t
                  ;; We draw header only if there is more than one
                  ;; child in the box:
                  (header-height obj)))))))))


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


(defun should-we-render-only-a-child (container)
  (and (not (typep container 'tight-container))
       (or (= *num-leaf-containers*
              1)
           *fold-title-with-one-child*)))


(defmethod draw :around ((obj container) svg)
  (with-leafs-counted (obj)
    (call-next-method)))


(defmethod draw ((obj container) svg)
  (with-slots (children) obj
    (let* ((children (alexandria:hash-table-values children))
           (first-child (car children)))

      (cond
        ;; If there is only one child
        ;; then there is no reason
        ;; to render outer container.
        ;; 
        ;; But for tight containers we always want
        ;; to render it's title and can't remove
        ;; it from the rendering pipeline.
        ((should-we-render-only-a-child obj)
         (draw first-child svg))
        ;; Otherwise, draw them all!
        (t
         (inner-draw obj svg))))))


(defmethod inner-draw ((obj container) svg)
  (with-slots (children font-family font-weight font-size margin title comment) obj
    (let* ((group (cl-svg:make-group svg ()))
           (full-width (width obj))
           (full-height (height obj))
           ;; (font-data (anafanafo:load-data :family font-family
           ;;                                 :weight font-weight
           ;;                                 :size font-size))
           ;; (text-width (anafanafo:string-width font-data title))
           )

      (when comment
        (cl-svg:comment group
          (fmt "~A font-size: ~A"
               comment
               font-size)))

      (when *debug-sizes*
        (cl-svg:draw group
            (:rect :x 0 :y 0 :width full-width :height full-height)
            :stroke "#FFAA66"
            :fill "white"))

      (github-matrix/svg:text group title
        :x margin
        :y (+ margin font-size)
        :font-family font-family
        :font-weight font-weight
        :font-size font-size
        :shadow-opacity 0.2
        :color *title-color*)

      (ecase (orientation obj)
        (:row
         (loop with max-height = (loop for child being the hash-value of children
                                       maximize (height child))
               with x = 0
               for child being the hash-value of children
               for y = (+ (header-height obj)
                           margin
                           (- max-height
                               (height child)))
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
  (with-slots (children font-family font-weight font-size margin title-box comment) obj
    (let* ((group (cl-svg:make-group svg ())))

      (when comment
        (cl-svg:comment group
          (fmt "~A font-size: ~A"
               comment
               font-size)))
      
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


