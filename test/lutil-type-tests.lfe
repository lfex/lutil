(defmodule lutil-type-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "deps/ltest/include/ltest-macros.lfe")

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

(deftest list->tuple
  (is-equal #(a b c 1 2 3) (lutil-type:list->tuple '(a b c 1 2 3))))

(deftest atom-cat
  (is-equal 'ab (lutil-type:atom-cat 'a 'b)))

(deftest string?
  (is (lutil-type:string? "string data! yaya!"))
  (is-not (lutil-type:string? (list "my" "string" "data"))))

;; XXX add a unit test for (unicode? ...)

(deftest list?
  (is-not (lutil-type:list? "string data! yaya!"))
  (is (lutil-type:list? (list "my" "string" "data"))))

(deftest tuple?
  (is-not (lutil-type:tuple? "string data! yaya!"))
  (is (lutil-type:tuple? (tuple "my" "string" "data"))))

(deftest atom?
  (is-not (lutil-type:atom? "string data! yaya!"))
  (is (lutil-type:atom? 'my-atom))
  (is (lutil-type:atom? '|more atom data|)))

(deftest dict?
  (is-not (lutil-type:dict? "a string"))
  (is-not (lutil-type:dict? '("a" "list")))
  (is-not (lutil-type:dict? #b("a binary")))
  (is-not (lutil-type:dict? #("a" "tuple")))
  (is-not (lutil-type:dict? '(#("a" "tuple"))))
  (is (lutil-type:dict? (dict:from_list '(#("a" "tuple"))))))

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
