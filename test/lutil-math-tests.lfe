(defmodule lutil-math-tests
  (behaviour lunit-unit)
  (export all))

(include-lib "deps/lunit/include/lunit-macros.lfe")

(deftest fast-floor
  (is-equal 0 (lutil-math:fast-floor 0.0))
  (is-equal 1 (lutil-math:fast-floor 1.0))
  (is-equal -5 (lutil-math:fast-floor -4.3))
  (is-equal 3 (lutil-math:fast-floor 3.1))
  (is-equal 3 (lutil-math:fast-floor 3.4))
  (is-equal 3 (lutil-math:fast-floor 3.5))
  (is-equal 3 (lutil-math:fast-floor 3.9)))

(deftest round
  (is-equal 2.0 (lutil-math:round 2 2))
  (is-equal 2.11 (lutil-math:round 2.11 2))
  (is-equal 2.11 (lutil-math:round 2.111 2))
  (is-equal 2.12 (lutil-math:round 2.115 2))
  (is-equal 2.99985 (lutil-math:round 2.999849 5))
  (let* ((inputs (lists:seq 1 10))
         (results (lists:map (lambda (x) (lutil-math:round (/ x 11) 3)) inputs))
         (expected (list 0.091 0.182 0.273 0.364 0.455
                         0.545 0.636 0.727 0.818 0.909)))
    (lists:zipwith (lambda (a b) (is-equal a b)) expected results)))

(deftest dot-product
  (is-equal 32 (lutil-math:dot-product '(1 2 3) '(4 5 6)) )
  (is-equal 122 (lutil-math:dot-product '(9 2 7) '(4 8 10))))

(deftest scale
  (is-equal 0.25 (lutil-math:scale -0.5 #(-1.0 1.0) #(0.0 1.0)))
  (is-equal 0.0 (lutil-math:scale -0.5 #(-0.5 1.0) #(0.0 1.0)))
  (is-equal 0.5 (lutil-math:scale -0.5 #(-1.5 0.5) #(0.0 1.0)))
  (is-equal 1.0 (lutil-math:scale 1 #(-1.0 1.0) #(0.0 1.0)))
  (is-equal 0.5 (lutil-math:scale 0 #(-1.0 1.0) #(0.0 1.0)))
  (is-equal 0.0 (lutil-math:scale -1 #(-1.0 1.0) #(0.0 1.0)))
  (is-equal 0.0 (lutil-math:scale 0 #(0.0 1.0) #(0.0 255.0)))
  (is-equal 127.5 (lutil-math:scale 0.5 #(0.0 1.0) #(0.0 255.0)))
  (is-equal 255.0 (lutil-math:scale 1.0 #(0.0 1.0) #(0.0 255.0))))

(deftest unit-scale
  (is-equal 0.25 (lutil-math:unit-scale -0.5 #(-1.0 1.0)))
  (is-equal 0.0 (lutil-math:unit-scale -0.5 #(-0.5 1.0)))
  (is-equal 0.5 (lutil-math:unit-scale -0.5 #(-1.5 0.5)))
  (is-equal 1.0 (lutil-math:unit-scale 1 #(-1.0 1.0)))
  (is-equal 0.5 (lutil-math:unit-scale 0 #(-1.0 1.0)))
  (is-equal 0.0 (lutil-math:unit-scale -1 #(-1.0 1.0))))

(deftest color-scale
  (is-equal 0 (lutil-math:color-scale 0 #(0.0 1.0)))
  (is-equal 64 (lutil-math:color-scale -0.5 #(-1.0 1.0)))
  (is-equal 128 (lutil-math:color-scale 0.5 #(0.0 1.0)))
  (is-equal 255 (lutil-math:color-scale 1.0 #(0.0 1.0))))

(deftest odd?
  (is (lutil-math:odd? 1))
  (is-not (lutil-math:odd? 2))
  (is (lutil-math:odd? 3))
  (is-not (lutil-math:odd? 4)))

(deftest even?
  (is-not (lutil-math:even? 1))
  (is (lutil-math:even? 2))
  (is-not (lutil-math:even? 3))
  (is (lutil-math:even? 4)))

(deftest factorial
  (is-equal 1 (lutil-math:factorial 0))
  (is-equal 1 (lutil-math:factorial 1))
  (is-equal 2 (lutil-math:factorial 2))
  (is-equal 6 (lutil-math:factorial 3))
  (is-equal 120 (lutil-math:factorial 5))
  (is-equal 5040 (lutil-math:factorial 7))
  (is-equal 362880 (lutil-math:factorial 9))
  (is-equal 2432902008176640000 (lutil-math:factorial 20)))

(deftest factors
  (is-equal #(error undefined) (lutil-math:factors -1))
  (is-equal #(error undefined) (lutil-math:factors 0))
  (is-equal '(1) (lutil-math:factors 1))
  (is-equal '(2 1) (lutil-math:factors 2))
  (is-equal '(3 1) (lutil-math:factors 3))
  (is-equal '(2 2 1) (lutil-math:factors 4))
  (is-equal '(5 2 1) (lutil-math:factors 10))
  (is-equal '(5 5 2 2 1) (lutil-math:factors 100))
  (is-equal '(333667 37 3 3 3 1) (lutil-math:factors 333333333)))

(deftest levenshtein-simple
  (is-equal 0 (lutil-math:levenshtein-simple "a" "a"))
  (is-equal 1 (lutil-math:levenshtein-simple "a" ""))
  (is-equal 1 (lutil-math:levenshtein-simple "" "b"))
  (is-equal 0 (lutil-math:levenshtein-simple "" ""))
  (is-equal 0 (lutil-math:levenshtein-simple "abc" "abc"))
  (is-equal 1 (lutil-math:levenshtein-simple "abc" "abd"))
  (is-equal 1 (lutil-math:levenshtein-simple "abc" "abb"))
  (is-equal 3 (lutil-math:levenshtein-simple "abc" "cde"))
  (is-equal 3 (lutil-math:levenshtein-simple "abc" "def"))
  (is-equal 2 (lutil-math:levenshtein-simple "stop" "tops"))
  (is-equal 3 (lutil-math:levenshtein-simple "kitten" "sitting"))
  (is-equal 8 (lutil-math:levenshtein-simple "rosettacode" "raisethysword")))

(deftest levenshtein-distance
  (is-equal 0 (lutil-math:levenshtein-distance "a" "a"))
  (is-equal 1 (lutil-math:levenshtein-distance "a" ""))
  (is-equal 1 (lutil-math:levenshtein-distance "" "b"))
  (is-equal 0 (lutil-math:levenshtein-distance "" ""))
  (is-equal 0 (lutil-math:levenshtein-distance "abc" "abc"))
  (is-equal 1 (lutil-math:levenshtein-distance "abc" "abd"))
  (is-equal 1 (lutil-math:levenshtein-distance "abc" "abb"))
  (is-equal 3 (lutil-math:levenshtein-distance "abc" "cde"))
  (is-equal 3 (lutil-math:levenshtein-distance "abc" "def"))
  (is-equal 2 (lutil-math:levenshtein-distance "stop" "tops"))
  (is-equal 3 (lutil-math:levenshtein-distance "kitten" "sitting"))
  (is-equal 8 (lutil-math:levenshtein-distance "rosettacode" "raisethysword")))

(deftest levenshtein-sort
  (is-equal
    #("aaaa"
      ((1 "aaab") (1 "aaac") (1 "aaba")
       (2 "abab") (3 "abbb") (4 "bbbb")))
    (lutil-math:levenshtein-sort
      "aaaa"
      '("bbbb" "aaac" "abab" "aaba" "aaab" "abbb"))))
