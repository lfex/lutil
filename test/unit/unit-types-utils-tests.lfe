(defmodule unit-types-utils-tests
  (export all)
  (import
    (from lfe-utils
      (add-tuples 1) (add-tuples 2)
      (color-scale 2)
      (dot-product 2)
      (fast-floor 1)
      (round 2)
      (scale 3)
      (unit-scale 2)
      (uuid4 0) (uuid4 1)
      (partition-list 1)
      (pair-dict 1))
    (from lfeunit-util
      (check-failed-is 2)
      (check-wrong-is-exception 2))
    (from lists
      (map 2)
      (seq 2)
      (zipwith 3))))

(include-lib "deps/lfeunit/include/lfeunit-macros.lfe")

(deftest add-tuples
  (let ((data1 (list (tuple 1 2 3) (tuple 2 3 4)))
        (data2 (list (tuple 2 4) (tuple 6 8) (tuple 10 12))))
    (is-equal #(1 2 3 2 3 4) (add-tuples data1))
    (is-equal #(2 4 6 8 10 12) (add-tuples data2))
    (is-equal #(1 2 3 4) (add-tuples (tuple 1 2) (tuple 3 4)))))

(defun test-dict-data-1 ()
  (list
    'key-1 '"value 1"))

(defun test-dict-data-2 ()
  (list
    'key-1 '"value 1"
    'key-2 '"value 2"))

(defun test-dict-data-3 ()
  (list
    'key-1 '"value 1"
    'key-2 '"value 2"
    'key-3 '"value 3"))

(defun test-dict-2 ()
  (pair-dict (test-dict-data-2)))

(deftest partition-list
  (let ((result (partition-list (test-dict-data-2))))
    (is-equal #((key-1 key-2) ("value 1" "value 2")) result)))

(deftest pair-dict
  (is-equal '"value 1" (: dict fetch 'key-1 (test-dict-2)))
  (is-equal '"value 2" (: dict fetch 'key-2 (test-dict-2))))

(deftest list->tuple
  (is-equal #(a b c 1 2 3) (: lfe-utils list->tuple '(a b c 1 2 3))))

(deftest atom-cat
  (is-equal 'ab (: lfe-utils atom-cat 'a 'b)))

(deftest strip
  (is-equal '"data" (: lfe-utils strip '"data\n"))
  (is-equal '"data" (: lfe-utils strip '"data\n\n"))
  (is-equal '"data" (: lfe-utils strip '"data   "))
  (is-equal '"data" (: lfe-utils strip '"data   \n   "))
  (is-equal '"data" (: lfe-utils strip '"data   \n   \n")))

(deftest stinrg?
  (is (: lfe-utils string? '"string data! yaya!"))
  (is-not (: lfe-utils string? (list '"my" '"string" '"data"))))

;; XXX add a unit test for (unicode? ...)

(deftest list?
  (is-not (: lfe-utils list? '"string data! yaya!"))
  (is (: lfe-utils list? (list '"my" '"string" '"data"))))

(deftest tuple?
  (is-not (: lfe-utils tuple? '"string data! yaya!"))
  (is (: lfe-utils tuple? (tuple '"my" '"string" '"data"))))

(deftest atom?
  (is-not (: lfe-utils atom? '"string data! yaya!"))
  (is (: lfe-utils atom? 'my-atom))
  (is (: lfe-utils atom? '|more atom data|)))
