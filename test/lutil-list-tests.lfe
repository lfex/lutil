(defmodule lutil-list-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

;;; Testing data

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
  (lutil-list:->dict (test-dict-data-2)))

;;; Tests

(deftest partition
  (let ((result (lutil-list:partition (test-dict-data-2))))
    (is-equal #((key-1 key-2) ("value 1" "value 2")) result)))

(deftest ->dict
  (is-equal "value 1" (dict:fetch 'key-1 (test-dict-2)))
  (is-equal "value 2" (dict:fetch 'key-2 (test-dict-2))))

(deftest ->tuple
  (is-equal #(a b c 1 2 3) (lutil-list:->tuple '(a b c 1 2 3))))

(deftest zip-1
  (is-equal
    '((1 4 7 10 13 16)
      (2 5 8 11 14 17)
      (3 6 9 12 15 18))
    (lutil-list:zip   '((1  2  3)
                        (4  5  6)
                        (7  8  9)
                        (10 11 12)
                        (13 14 15)
                        (16 17 18)))))

(deftest zip-2
  (is-equal
    '((1 2)
      (2 3)
      (3 4)
      (4 5))
    (lutil-list:zip '(1 2 3 4) '(2 3 4 5))))

(deftest zip-3
  (is-equal
    '((1 2 3)
      (2 3 4)
      (3 4 5)
      (4 5 6))
    (lutil-list:zip '(1 2 3 4)
                    '(2 3 4 5)
                    '(3 4 5 6))))

(deftest zip-4
  (is-equal
    '((1 5 8 4)
      (2 6 7 3)
      (3 7 6 2)
      (4 8 5 1))
    (lutil-list:zip '(1 2 3 4)
                    '(5 6 7 8)
                    '(8 7 6 5)
                    '(4 3 2 1))))
