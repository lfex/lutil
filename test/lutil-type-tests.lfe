(defmodule lutil-type-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "lutil/include/predicates.lfe")

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
    (is-equal 'undefined (lutil-type:get-in data '(key-3 key-6 key-8 key-9)))))

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

(deftest set?
  (is-not (set? '(c a b)))
  (is (set? '(a b c)))
  (is (set? '()))
  (is (set? (sets:new)))
  (is (set? (ordsets:new))))

(deftest proplist?
  (is-not (proplist? 1))
  (is-not (proplist? '(1)))
  (is-not (proplist? '(1 2)))
  (is-not (proplist? '((1 2))))
  (is-not (proplist? '(#(1 2))))
  (is-not (proplist? '(#(a 1) #(2 b) #(c 3))))
  (is (proplist? '(a)))
  (is (proplist? '(a b c)))
  (is (proplist? '(#(a 1) b c)))
  (is (proplist? '(#(a 1) #(b 2) c)))
  (is (proplist? '(#(a 1) #(b 2) #(c 3)))))

(deftest proplist-kv?
  (is (proplist-kv? 'a))
  (is-not (proplist-kv? "a"))
  (is-not (proplist-kv? 1))
  (is (proplist-kv? '#(a b)))
  (is-not (proplist-kv? '(a b))))

(deftest undef?
  (is-not (undef? 42))
  (is-not (undef? 'undef))
  (is (undef? 'undefined)))

(deftest nil?
  (is-not (nil? 32))
  (is-not (nil? 'undefined))
  (is (nil? 'nil))
  (is (nil? '())))

(deftest true?
  (is-not (true? 'false))
  (is (true? 'true)))

(deftest false?
  (is-not (false? 'true))
  (is (false? 'false)))

(deftest in?
  (is-not (in? 0 (1 2 3 4 5 6)))
  (is (in? 6 (1 2 3 4 5 6)))
  (is-not (in? "z" ("a" "b" "c" "d" "e")))
  (is (in? "e" ("a" "b" "c" "d" "e")))
  (is-not (in? 'z ('a 'b 'c 'd 'e)))
  (is (in? 'e ('a 'b 'c 'd 'e))))

(deftest not-in?
  (is (not-in? 0 (1 2 3 4 5 6)))
  (is-not (not-in? 6 (1 2 3 4 5 6)))
  (is (not-in? "z" ("a" "b" "c" "d" "e")))
  (is-not (not-in? "e" ("a" "b" "c" "d" "e")))
  (is (not-in? 'z ('a 'b 'c 'd 'e)))
  (is-not (not-in? 'e ('a 'b 'c 'd 'e))))

(defun test-in-with-guard
  ((arg) (when (in? arg ('a 'b 'c)))
   'found)
  ((_) 'not-found))

(deftest in?-guard
  (is-equal (test-in-with-guard 'a) 'found)
  (is-equal (test-in-with-guard 'b) 'found)
  (is-equal (test-in-with-guard 'c) 'found)
  (is-equal (test-in-with-guard 'd) 'not-found))

(defun test-not-in-with-guard
  ((arg) (when (not-in? arg ('i 'j 'k)))
   'not-found)
  ((_) 'found))

(deftest not-in?-guard
  (is-equal (test-not-in-with-guard 'i) 'found)
  (is-equal (test-not-in-with-guard 'j) 'found)
  (is-equal (test-not-in-with-guard 'k) 'found)
  (is-equal (test-not-in-with-guard 'a) 'not-found))

(deftest identical?
  (is (identical? '(a b c) '(a b c)))
  (is-not (identical? '(a b c) '(a b d))))

(deftest empty?
  (is (empty? '()))
  (is-not (empty? '(1 2 3))))

(deftest every?
  (is (every? #'zero?/1 '(0 0 0 0 0)))
  (is-not (every? #'zero?/1 '(0 0 0 0 1))))

(deftest any?
  (is (any? #'zero?/1 '(0 1 1 1 1)))
  (is-not (any? #'zero?/1 '(1 1 1 1 1))))

(deftest not-any?
  (is-not (not-any? #'zero?/1 '(0 1 1 1 1)))
  (is (not-any? #'zero?/1 '(1 1 1 1 1))))

(deftest element?
  (is (element? 'a '(a b c)))
  (is-not (element? 'z '(a b c)))
  (is (element? 'a (sets:from_list '(a b c))))
  (is-not (element? 'z (sets:from_list '(a b c))))
  (is (element? 'a (ordsets:from_list '(a b c))))
  (is-not (element? 'z (ordsets:from_list '(a b c)))))

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
