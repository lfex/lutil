(defmodule lfe-utils_tests
  (export all)
  (import
    (from lfe-utils
      (add-tuples 1)
      (dot-product 2)
      (fast-floor 1)
      (round 2))
    (from lfeunit-util
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))
    (from lfeunit
      (assert 1)
      (assert-not 1)
      (assert-equal 2)
      (assert-not-equal 2))
    (from lists
      (map 2)
      (seq 2)
      (zipwith 3))
    ))

(defun add-tuples_test ()
  (let ((data (list (tuple 1 2 3) (tuple 2 3 4))))
    (assert-equal #(1 2 3 2 3 4) (add-tuples data))))

(defun fast-floor_test ()
  (assert-equal 0 (fast-floor 0.0))
  (assert-equal 1 (fast-floor 1.0))
  (assert-equal -5 (fast-floor -4.3))
  (assert-equal 3 (fast-floor 3.1))
  (assert-equal 3 (fast-floor 3.4))
  (assert-equal 3 (fast-floor 3.5))
  (assert-equal 3 (fast-floor 3.9)))

(defun round_test ()
  (assert-equal 2 (round 2 2))
  (assert-equal 2.11 (round 2.11 2))
  (assert-equal 2.11 (round 2.111 2))
  (assert-equal 2.12 (round 2.115 2))
  (assert-equal 2.99985 (round 2.999849 5))
  (let* ((inputs (seq 1 10))
         (results (map (lambda (x) (round (/ x 11) 3)) inputs))
         (expected (list 0.091 0.182 0.273 0.364 0.455
                         0.545 0.636 0.727 0.818 0.909)))
    (zipwith (lambda (a b) (assert-equal a b)) expected results)))

(defun dot-product_test ()
  (assert-equal 32 (dot-product '(1 2 3) '(4 5 6)) )
  (assert-equal 122 (dot-product '(9 2 7) '(4 8 10))))