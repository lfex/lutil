(defmodule lutil-types-tests
  (behaviour lunit-unit)
  (export all)
  (import
    (from lutil
      (add-tuples 1) (add-tuples 2)
      (color-scale 2)
      (dot-product 2)
      (fast-floor 1)
      (round 2)
      (scale 3)
      (unit-scale 2)
      (uuid4 0) (uuid4 1)
      (partition-list 1)
      (pair-dict 1)
      (capitalized? 1))))

(include-lib "deps/lunit/include/lunit-macros.lfe")

(deftest add-tuples
  (let ((data1 (list (tuple 1 2 3) (tuple 2 3 4)))
        (data2 (list (tuple 2 4) (tuple 6 8) (tuple 10 12))))
    (is-equal #(1 2 3 2 3 4) (add-tuples data1))
    (is-equal #(2 4 6 8 10 12) (add-tuples data2))
    (is-equal #(1 2 3 4) (add-tuples (tuple 1 2) (tuple 3 4)))))

(defun test-dict-data-1 ()
  (list
    'key-1 "value 1"))

(defun test-dict-data-2 ()
  (list
    'key-1 "value 1"
    'key-2 "value 2"))

(defun test-dict-data-3 ()
  (list
    'key-1 "value 1"
    'key-2 "value 2"
    'key-3 "value 3"))

(defun test-dict-2 ()
  (pair-dict (test-dict-data-2)))

(deftest partition-list
  (let ((result (partition-list (test-dict-data-2))))
    (is-equal #((key-1 key-2) ("value 1" "value 2")) result)))

(deftest pair-dict
  (is-equal "value 1" (dict:fetch 'key-1 (test-dict-2)))
  (is-equal "value 2" (dict:fetch 'key-2 (test-dict-2))))

(deftest list->tuple
  (is-equal #(a b c 1 2 3) (lutil:list->tuple '(a b c 1 2 3))))

(deftest atom-cat
  (is-equal 'ab (lutil:atom-cat 'a 'b)))

(deftest strip
  (is-equal "data" (lutil:strip "data\n"))
  (is-equal "data" (lutil:strip "data\n\n"))
  (is-equal "data" (lutil:strip "data   "))
  (is-equal "data" (lutil:strip "data   \n   "))
  (is-equal "data" (lutil:strip "data   \n   \n")))

(deftest capitalized?
  (is (capitalized? "Apple"))
  (is-not (capitalized? "apple"))
  (is (capitalized? "APPLE"))
  (is-not (capitalized? "aPPLE")))

(deftest stinrg?
  (is (lutil:string? "string data! yaya!"))
  (is-not (lutil:string? (list "my" "string" "data"))))

;; XXX add a unit test for (unicode? ...)

(deftest list?
  (is-not (lutil:list? "string data! yaya!"))
  (is (lutil:list? (list "my" "string" "data"))))

(deftest tuple?
  (is-not (lutil:tuple? "string data! yaya!"))
  (is (lutil:tuple? (tuple "my" "string" "data"))))

(deftest atom?
  (is-not (lutil:atom? "string data! yaya!"))
  (is (lutil:atom? 'my-atom))
  (is (lutil:atom? '|more atom data|)))
