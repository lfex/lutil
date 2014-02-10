(defmodule lfe-utils_tests
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

(deftest uuid4
  (is-equal 36 (byte_size (uuid4)))
  (is-equal 288 (bit_size (uuid4)))
  (is (is_binary (uuid4)))
  (is-equal 36 (byte_size (uuid4 (tuple 'type '"binary"))))
  (is-equal 288 (bit_size (uuid4 (tuple 'type '"binary"))))
  (is (is_binary (uuid4 (tuple 'type '"binary"))))
  (is-not (is_binary (uuid4 (tuple 'type '"list"))))
  (is-equal 36 (length (uuid4 (tuple 'type '"list"))))
  (is (is_list (uuid4 (tuple 'type '"list"))))
  (is-not (is_list (uuid4 (tuple 'type '"binary"))))
  (is (is_atom (uuid4 (tuple 'type '"atom"))))
  (is-not (is_atom (uuid4 (tuple 'type '"list")))))

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

(deftest is-home-dir?
  (is-not (: lfe-utils is-home-dir? '"~"))
  (is-not (: lfe-utils is-home-dir? '"/"))
  (is-not (: lfe-utils is-home-dir? '"~home/"))
  (is-not (: lfe-utils is-home-dir? '"/home"))
  (is (: lfe-utils is-home-dir? '"~/"))
  (is (: lfe-utils is-home-dir? '"~/user"))
  (is (: lfe-utils is-home-dir? '"~/user/more/path")))

(deftest expand-home-dir
  (is-equal '"/usr/local/bin"
            (: lfe-utils expand-home-dir '"/usr/local/bin"))
  (is-equal '"/home/oubiwann"
            (: lfe-utils expand-home-dir '"/home/oubiwann"))
  (let* ((tilde-dir '"~/my-data")
         (expanded (: lfe-utils expand-home-dir tilde-dir)))
    (is (: lfe-utils is-home-dir? tilde-dir))
    (is-not (: lfe-utils is-home-dir? expanded))))

(deftest strip
  (is-equal '"data" (: lfe-utils strip '"data\n"))
  (is-equal '"data" (: lfe-utils strip '"data\n\n"))
  (is-equal '"data" (: lfe-utils strip '"data   "))
  (is-equal '"data" (: lfe-utils strip '"data   \n   "))
  (is-equal '"data" (: lfe-utils strip '"data   \n   \n")))


