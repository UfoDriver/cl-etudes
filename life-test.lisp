(defpackage :life-test
  (:use :common-lisp :life :clunit)
  (:shadow :run)
  (:export :run))

(in-package :life-test)


(defsuite life-test-suite ())


;; (defun test ()
;;   (let* ((field-data '((0 0 0)
;;                        (0 1 0)
;;                        (0 0 0)))
;;          (field (make-array '(3 3) :initial-contents field-data))
;;          (neighbors-map '(
;;                           (1 1 1)
;;                           (1 0 1)
;;                           (1 1 1)))
;;          (frame '((0 0 0) (0 0 0) (0 0 0))))
;;     (equalp (calculate-neighbors-field field) (make-array '(3 3) :initial-contents neighbors-map))
;;     (equalp (next-frame field) (make-array '(3 3) :initial-contents frame))))



;; (equalp (make-array '(2 2) :initial-contents '((1 2) (3 4)))
;;         (make-array '(2 2) :initial-contents '((1 2) (3 4))))


(deftest test-neighbors (life-test-suite)
  (let* ((field-data '((0 0 0 0 0)
                       (0 0 0 0 0)
                       (0 0 1 0 0)
                       (0 0 0 0 0)
                       (0 0 0 0 0)))
         (field (make-array '(5 5) :initial-contents field-data))
         (neighbors-map '((0 0 0 0 0)
                          (0 1 1 1 0)
                          (0 1 0 1 0)
                          (0 1 1 1 0)
                          (0 0 0 0 0)))
         (frame '((0 0 0 0 0)
                  (0 0 0 0 0)
                  (0 0 0 0 0)
                  (0 0 0 0 0)
                  (0 0 0 0 0))))
    (assert-equalp
        (life::calculate-neighbors-field field)
        (make-array '(5 5) :initial-contents neighbors-map))
    (assert-equalp
        (life::next-frame field)
        (make-array '(5 5) :initial-contents frame))))

(deftest test-step (life-test-suite)
  (let* ((field-data '((0 0 0 0 0)
                       (0 0 1 0 0)
                       (0 0 1 0 0)
                       (0 0 1 0 0)
                       (0 0 0 0 0)))
         (field (make-array '(5 5) :initial-contents field-data))
         (neighbors-map '((0 1 1 1 0)
                          (0 2 1 2 0)
                          (0 3 2 3 0)
                          (0 2 1 2 0)
                          (0 1 1 1 0)))
         (frame '((0 0 0 0 0)
                  (0 0 0 0 0)
                  (0 1 1 1 0)
                  (0 0 0 0 0)
                  (0 0 0 0 0)))
         (expected (make-array '(5 5) :initial-contents frame)))
    (assert-equalp
        (make-array '(5 5) :initial-contents neighbors-map)
        (life::calculate-neighbors-field field))
    (assert-equalp expected (life::next-frame field))))


(defun run(&optional use-debugger)
  (run-suite 'life-test-suite :use-debugger use-debugger))
