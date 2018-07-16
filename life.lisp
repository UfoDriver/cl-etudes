(defpackage :life
  (:use :common-lisp)
  (:export :run))

(in-package :life)

(defparameter *field* nil)
(defparameter *width* 30)
(defparameter *height* 30)
(defparameter *dead-cell* #\.)
(defparameter *live-cell* #\X)

(defun init-field (&optional (field *field*) (width *width*) (height *height*))
  (setf field (make-array (list width height) :initial-element 0)))

(defun put-random-dots (&optional (field *field*) (num 50))
  (let* ((dimension (array-dimensions field))
         (height (first dimension))
         (width (second dimension)))
    (dotimes (_ num field)
      (setf (aref field (random height) (random width)) 1))))

(defun put-glider ()
  (setf (aref *field* 1 2) 1)  ;;  X
  (setf (aref *field* 2 3) 1)  ;;   X
  (setf (aref *field* 3 1) 1)  ;; Xxx
  (setf (aref *field* 3 2) 1)  ;; xXx
  (setf (aref *field* 3 3) 1)) ;; xxX

(defun put-blinker ()
  (setf (aref *field* 1 2) 1)  ;;  1
  (setf (aref *field* 2 2) 1)  ;;  1
  (setf (aref *field* 3 2) 1))  ;;  1

(defun dump-cell (value)
  (if (zerop value)
      (format t "~a" *dead-cell*)
      (format t "~a" *live-cell*)))

(defun dump-field (&optional (field *field*) debug)
  (let* ((dimension (array-dimensions field))
         (height (first dimension))
         (width (second dimension)))
    (dotimes (y height)
      (dotimes (x width)
        (if debug
            (format t "~a" (aref field y x))
            (dump-cell (aref field y x))))
      (format t "~%")))
  (format t "~%")
  field)

(defun get-neighbor (field x y)
  (let* ((dimension (array-dimensions field))
         (height (first dimension))
         (width (second dimension)))
    (lambda (offset)
      (let ((probe-x (+ x (first offset)))
            (probe-y (+ y (second offset))))
        (cond ((< probe-x 0) 0)
              ((>= probe-x width) 0)
              ((< probe-y 0) 0)
              ((>= probe-y height) 0)
              (t (if (plusp (aref field probe-y probe-x))
                     1
                     0)))))))

(defun calculate-neighbors (x y &optional (field *field*))
  (let ((directions '((-1  0)    ;; left middle
                      (-1 -1)    ;; left top
                      ( 0 -1)    ;; middle top
                      ( 1 -1)    ;; right top
                      ( 1  0)    ;; right middle
                      ( 1  1)    ;; right bottom
                      ( 0  1)    ;; middle bottom
                      (-1  1)))) ;; left bottom
    (apply #'+ (mapcar (get-neighbor field x y) directions))))

(defun calculate-neighbors-field (&optional (field *field*))
  (let* ((dimension (array-dimensions field))
         (height (first dimension))
         (width (second dimension))
         (neighbors (make-array (list height width))))
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref neighbors y x) (calculate-neighbors x y field))))
    neighbors))

(defun next-frame (&optional (field *field*))
  (let* ((new-field (calculate-neighbors-field field))
         (height (first (array-dimensions field)))
         (width (second (array-dimensions field))))
    (dotimes (y width)
      (dotimes (x height)
        (cond
          ;; Any live cell with fewer than two live neighbors dies, as if by under population.
          ((< (aref new-field y x) 2) (setf (aref new-field y x) 0))
          ;; Any live cell with two or three live neighbors lives on to the next generation.
          ((= (aref new-field y x) 2) (setf (aref new-field y x) (aref field y x)))
          ;; Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.
          ((= (aref new-field y x) 3) (setf (aref new-field y x) 1))
          ;; Any live cell with more than three live neighbors dies, as if by overpopulation.
          ((> (aref new-field y x) 3) (setf (aref new-field y x) 0))
          (t (format t "Should not happen~%")))))
    new-field))

(defun run ()
  (setf *field* (init-field *field* 10 10))
  (put-random-dots *field* 20)
  ;; (put-glider)
  ;; (put-blinker)
  (labels ((rec ()
           (dump-field *field*)
           (setf *field* (next-frame *field*))
           (sleep 1)
           (rec)))
    (rec)))
