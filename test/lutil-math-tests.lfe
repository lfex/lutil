(defmodule lutil-math-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

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

(deftest get-closest
  (let ((range '(1.0 3.25 5.5 7.75 10.0)))
    (is-equal 1.0 (lutil-math:get-closest 1 range))
    (is-equal 1.0 (lutil-math:get-closest 2 range))
    (is-equal 3.25 (lutil-math:get-closest 3 range))
    (is-equal 3.25 (lutil-math:get-closest 4 range))
    (is-equal 5.5 (lutil-math:get-closest 5 range))
    (is-equal 5.5 (lutil-math:get-closest 6 range))
    (is-equal 7.75 (lutil-math:get-closest 7 range))
    (is-equal 7.75 (lutil-math:get-closest 8 range))
    (is-equal 10.0 (lutil-math:get-closest 9 range))
    (is-equal 10.0 (lutil-math:get-closest 10 range))))

(deftest get-gradations
  (is-equal '(1.0 3.25 5.5 7.75 10.0) (lutil-math:get-gradations 1.0 2.25 4)))

(deftest xform-numbers
  (is-equal '(1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0)
            (lutil-math:xform-numbers 0 (lists:seq 1 10)))
  (is-equal '(1.0 1.0 1.0 1.0 1.0 10.0 10.0 10.0 10.0 10.0)
            (lutil-math:xform-numbers 1 (lists:seq 1 10)))
  (is-equal '(1.0 1.0 1.0 5.5 5.5 5.5 5.5 10.0 10.0 10.0)
            (lutil-math:xform-numbers 2 (lists:seq 1 10)))
  (is-equal '(1.0 1.0 4.0 4.0 4.0 7.0 7.0 7.0 10.0 10.0)
            (lutil-math:xform-numbers 3 (lists:seq 1 10)))
  (is-equal '(1.0 1.0 3.25 3.25 5.5 5.5 7.75 7.75 10.0 10.0)
            (lutil-math:xform-numbers 4 (lists:seq 1 10)))
  (is-equal '(1.0 2.8 2.8 4.6 4.6 6.4 6.4 8.2 8.2 10.0)
            (lutil-math:xform-numbers 5 (lists:seq 1 10)))
  (is-equal '(1.0 2.5 2.5 4.0 5.5 5.5 7.0 8.5 8.5 10.0)
            (lutil-math:xform-numbers 6 (lists:seq 1 10)))
  (is-equal '(1.0 2.29 3.57 3.57 4.86 6.14 7.43 7.43 8.71 10.0)
            (lutil-math:xform-numbers 7 (lists:seq 1 10)))
  (is-equal '(1.0 2.13 3.25 4.38 5.5 5.5 6.63 7.75 8.88 10.0)
            (lutil-math:xform-numbers 8 (lists:seq 1 10)))
  (is-equal '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0)
            (lutil-math:xform-numbers 9 (lists:seq 1 10))))
