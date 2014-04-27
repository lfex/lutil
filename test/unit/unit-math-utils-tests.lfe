(defmodule unit-math-utils-tests
  (export all)
  (import
    (from lfe-utils
      (add-tuples 1) (add-tuples 2)
      (color-scale 2)
      (dot-product 2)
      (factorial 1)
      (fast-floor 1)
      (round 2)
      (scale 3)
      (unit-scale 2)
      (uuid4 0) (uuid4 1)
      (partition-list 1)
      (pair-dict 1))
    (from lists
      (map 2)
      (seq 2)
      (zipwith 3))))

(include-lib "deps/lfeunit/include/lfeunit-macros.lfe")

(deftest fast-floor
  (is-equal 0 (fast-floor 0.0))
  (is-equal 1 (fast-floor 1.0))
  (is-equal -5 (fast-floor -4.3))
  (is-equal 3 (fast-floor 3.1))
  (is-equal 3 (fast-floor 3.4))
  (is-equal 3 (fast-floor 3.5))
  (is-equal 3 (fast-floor 3.9)))

(deftest round
  (is-equal 2.0 (round 2 2))
  (is-equal 2.11 (round 2.11 2))
  (is-equal 2.11 (round 2.111 2))
  (is-equal 2.12 (round 2.115 2))
  (is-equal 2.99985 (round 2.999849 5))
  (let* ((inputs (seq 1 10))
         (results (map (lambda (x) (round (/ x 11) 3)) inputs))
         (expected (list 0.091 0.182 0.273 0.364 0.455
                         0.545 0.636 0.727 0.818 0.909)))
    (zipwith (lambda (a b) (is-equal a b)) expected results)))

(deftest dot-product
  (is-equal 32 (dot-product '(1 2 3) '(4 5 6)) )
  (is-equal 122 (dot-product '(9 2 7) '(4 8 10))))

(deftest scale
  (is-equal 0.25 (scale -0.5 #(-1.0 1.0) #(0.0 1.0)))
  (is-equal 0.0 (scale -0.5 #(-0.5 1.0) #(0.0 1.0)))
  (is-equal 0.5 (scale -0.5 #(-1.5 0.5) #(0.0 1.0)))
  (is-equal 1.0 (scale 1 #(-1.0 1.0) #(0.0 1.0)))
  (is-equal 0.5 (scale 0 #(-1.0 1.0) #(0.0 1.0)))
  (is-equal 0.0 (scale -1 #(-1.0 1.0) #(0.0 1.0)))
  (is-equal 0.0 (scale 0 #(0.0 1.0) #(0.0 255.0)))
  (is-equal 127.5 (scale 0.5 #(0.0 1.0) #(0.0 255.0)))
  (is-equal 255.0 (scale 1.0 #(0.0 1.0) #(0.0 255.0))))

(deftest unit-scale
  (is-equal 0.25 (unit-scale -0.5 #(-1.0 1.0)))
  (is-equal 0.0 (unit-scale -0.5 #(-0.5 1.0)))
  (is-equal 0.5 (unit-scale -0.5 #(-1.5 0.5)))
  (is-equal 1.0 (unit-scale 1 #(-1.0 1.0)))
  (is-equal 0.5 (unit-scale 0 #(-1.0 1.0)))
  (is-equal 0.0 (unit-scale -1 #(-1.0 1.0))))

(deftest color-scale
  (is-equal 0 (color-scale 0 #(0.0 1.0)))
  (is-equal 64 (color-scale -0.5 #(-1.0 1.0)))
  (is-equal 128 (color-scale 0.5 #(0.0 1.0)))
  (is-equal 255 (color-scale 1.0 #(0.0 1.0))))

(deftest odd?
  (is (: lfe-utils odd? 1))
  (is-not (: lfe-utils odd? 2))
  (is (: lfe-utils odd? 3))
  (is-not (: lfe-utils odd? 4)))

(deftest even?
  (is-not (: lfe-utils even? 1))
  (is (: lfe-utils even? 2))
  (is-not (: lfe-utils even? 3))
  (is (: lfe-utils even? 4)))

(deftest factorial
  (is-equal 1 (factorial 0))
  (is-equal 1 (factorial 1))
  (is-equal 2 (factorial 2))
  (is-equal 6 (factorial 3))
  (is-equal 120 (factorial 5))
  (is-equal 5040 (factorial 7))
  (is-equal 362880 (factorial 9))
  (is-equal 2432902008176640000 (factorial 20)))
