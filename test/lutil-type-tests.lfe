(defmodule lutil-type-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest add-tuples
  (let ((data1 (list (tuple 1 2 3) (tuple 2 3 4)))
        (data2 (list (tuple 2 4) (tuple 6 8) (tuple 10 12))))
    (is-equal #(1 2 3 2 3 4) (lutil-type:add-tuples data1))
    (is-equal #(2 4 6 8 10 12) (lutil-type:add-tuples data2))
    (is-equal #(1 2 3 4) (lutil-type:add-tuples (tuple 1 2) (tuple 3 4)))))

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
  (lutil-type:pair-dict (test-dict-data-2)))

(deftest partition-list
  (let ((result (lutil-type:partition-list (test-dict-data-2))))
    (is-equal #((key-1 key-2) ("value 1" "value 2")) result)))

(deftest pair-dict
  (is-equal "value 1" (dict:fetch 'key-1 (test-dict-2)))
  (is-equal "value 2" (dict:fetch 'key-2 (test-dict-2))))

(defun nested-test-lists ()
  '((1)
    (1 2 3)
    (1 2 (3 4 (5 6 (7 8 9))))))

(deftest get-in-indices
  (let ((data (nested-test-lists)))
    (is-equal '(1) (lutil-type:get-in-list data '(1)))
    (is-equal 1 (lutil-type:get-in-list data '(1 1)))
    (is-equal 2 (lutil-type:get-in-list data '(2 2)))
    (is-equal 3 (lutil-type:get-in-list data '(3 3 1)))
    (is-equal 9 (lutil-type:get-in-list data '(3 3 3 3 3)))
    (is-equal '(1) (lutil-type:get-in data '(1)))
    (is-equal 1 (lutil-type:get-in data '(1 1)))
    (is-equal 2 (lutil-type:get-in data '(2 2)))
    (is-equal 3 (lutil-type:get-in data '(3 3 1)))
    (is-equal 9 (lutil-type:get-in data '(3 3 3 3 3)))
    (is-equal 'undefined (lutil-type:get-in data '(4 4)))
    (is-equal 'undefined (lutil-type:get-in data '(3 3 3 3 4 4 4)))))

(defun nested-test-proplists ()
  '(#(key-1 val-1)
    #(key-2 val-2)
    #(key-3 (#(key-4 val-4)
             #(key-5 val-5)
             #(key-6 (#(key-7 val-7)
                      #(key-8 val-8)))))))

(deftest get-in-proplist
  (let ((data (nested-test-proplists)))
    (is-equal 'val-1 (lutil-type:get-in-proplist data '(key-1)))
    (is-equal 'val-2 (lutil-type:get-in-proplist data '(key-2)))
    (is-equal 'val-4 (lutil-type:get-in-proplist data '(key-3 key-4)))
    (is-equal 'val-8 (lutil-type:get-in-proplist data '(key-3 key-6 key-8)))
    (is-equal 'val-1 (lutil-type:get-in data '(key-1)))
    (is-equal 'val-2 (lutil-type:get-in data '(key-2)))
    (is-equal 'val-4 (lutil-type:get-in data '(key-3 key-4)))
    (is-equal 'val-8 (lutil-type:get-in data '(key-3 key-6 key-8)))
    (is-equal 'undefined (lutil-type:get-in data '(key-10)))
    (is-equal 'undefined (lutil-type:get-in data '(key-10 key-11)))
    (is-equal 'undefined (lutil-type:get-in data '(key-3 key-6 key-8 key-9)))))

(deftest get-in-orddict
  (let ((data (orddict:from_list (nested-test-proplists))))
    (is-equal 'val-1 (lutil-type:get-in-proplist data '(key-1)))
    (is-equal 'val-2 (lutil-type:get-in-proplist data '(key-2)))
    (is-equal 'val-4 (lutil-type:get-in-proplist data '(key-3 key-4)))
    (is-equal 'val-8 (lutil-type:get-in-proplist data '(key-3 key-6 key-8)))
    (is-equal 'val-1 (lutil-type:get-in data '(key-1)))
    (is-equal 'val-2 (lutil-type:get-in data '(key-2)))
    (is-equal 'val-4 (lutil-type:get-in data '(key-3 key-4)))
    (is-equal 'val-8 (lutil-type:get-in data '(key-3 key-6 key-8)))
    (is-equal 'undefined (lutil-type:get-in data '(key-10)))
    (is-equal 'undefined (lutil-type:get-in data '(key-10 key-11)))
    (is-equal 'undefined (lutil-type:get-in data '(key-3 key-6 key-8 key-9)))))

(deftest get-in-dict
  (let ((data (dict:from_list (nested-test-proplists))))
    (is-equal 'val-1 (lutil-type:get-in-dict data '(key-1)))
    (is-equal 'val-2 (lutil-type:get-in-dict data '(key-2)))
    (is-equal 'val-4 (lutil-type:get-in-dict data '(key-3 key-4)))
    (is-equal 'val-8 (lutil-type:get-in-dict data '(key-3 key-6 key-8)))
    (is-equal 'val-1 (lutil-type:get-in data '(key-1)))
    (is-equal 'val-2 (lutil-type:get-in data '(key-2)))
    (is-equal 'val-4 (lutil-type:get-in data '(key-3 key-4)))
    (is-equal 'val-8 (lutil-type:get-in data '(key-3 key-6 key-8)))
    (is-equal 'undefined (lutil-type:get-in data '(key-10)))
    (is-equal 'undefined (lutil-type:get-in data '(key-10 key-11)))
    (is-equal 'undefined (lutil-type:get-in data '(key-3 key-6 key-8 key-9)))))

(deftest get-in-map
  (if (erl_internal:bif 'is_map 1)
    (let ((data (maps:from_list (nested-test-proplists))))
      (is-equal 'val-1 (lutil-type:get-in-map data '(key-1)))
      (is-equal 'val-2 (lutil-type:get-in-map data '(key-2)))
      (is-equal 'val-4 (lutil-type:get-in-map data '(key-3 key-4)))
      (is-equal 'val-8 (lutil-type:get-in-map data '(key-3 key-6 key-8)))
      (is-equal 'val-1 (lutil-type:get-in data '(key-1)))
      (is-equal 'val-2 (lutil-type:get-in data '(key-2)))
      (is-equal 'val-4 (lutil-type:get-in data '(key-3 key-4)))
      (is-equal 'val-8 (lutil-type:get-in data '(key-3 key-6 key-8)))
      (is-equal 'undefined (lutil-type:get-in data '(key-10)))
      (is-equal 'undefined (lutil-type:get-in data '(key-10 key-11)))
      (is-equal 'undefined
                (lutil-type:get-in data '(key-3 key-6 key-8 key-9))))))

(deftest list->tuple
  (is-equal #(a b c 1 2 3) (lutil-type:list->tuple '(a b c 1 2 3))))

(deftest atom-cat-2-arity
  (is-equal 'ab (lutil-type:atom-cat 'a 'b)))

(deftest atom-cat-list-of-atoms
  (is-equal 'ab (lutil-type:atom-cat '(a b)))
  (is-equal 'abc (lutil-type:atom-cat '(a b c)))
  (is-equal 'abcdefg (lutil-type:atom-cat '(a b c d e f g))))

(deftest zip-1
  (is-equal
    '((1 4 7 10 13 16)
      (2 5 8 11 14 17)
      (3 6 9 12 15 18))
    (lutil-type:zip   '((1  2  3)
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
    (lutil-type:zip '(1 2 3 4) '(2 3 4 5))))

(deftest zip-3
  (is-equal
    '((1 2 3)
      (2 3 4)
      (3 4 5)
      (4 5 6))
    (lutil-type:zip '(1 2 3 4)
                    '(2 3 4 5)
                    '(3 4 5 6))))

(deftest zip-4
  (is-equal
    '((1 5 8 4)
      (2 6 7 3)
      (3 7 6 2)
      (4 8 5 1))
    (lutil-type:zip '(1 2 3 4)
                    '(5 6 7 8)
                    '(8 7 6 5)
                    '(4 3 2 1))))
