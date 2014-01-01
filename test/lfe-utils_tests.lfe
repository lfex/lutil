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
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))
    (from lfeunit
      (assert 1)
      (assert-not 1)
      (assert-equal 2)
      (assert-not-equal 2))
    (from lists
      (map 2)
      (seq 2)
      (zipwith 3))))

(defun add-tuples_test ()
  (let ((data1 (list (tuple 1 2 3) (tuple 2 3 4)))
        (data2 (list (tuple 2 4) (tuple 6 8) (tuple 10 12))))
    (assert-equal #(1 2 3 2 3 4) (add-tuples data1))
    (assert-equal #(2 4 6 8 10 12) (add-tuples data2))
    (assert-equal #(1 2 3 4) (add-tuples (tuple 1 2) (tuple 3 4)))))

(defun fast-floor_test ()
  (assert-equal 0 (fast-floor 0.0))
  (assert-equal 1 (fast-floor 1.0))
  (assert-equal -5 (fast-floor -4.3))
  (assert-equal 3 (fast-floor 3.1))
  (assert-equal 3 (fast-floor 3.4))
  (assert-equal 3 (fast-floor 3.5))
  (assert-equal 3 (fast-floor 3.9)))

(defun round_test ()
  (assert-equal 2 (round 2 2))
  (assert-equal 2.11 (round 2.11 2))
  (assert-equal 2.11 (round 2.111 2))
  (assert-equal 2.12 (round 2.115 2))
  (assert-equal 2.99985 (round 2.999849 5))
  (let* ((inputs (seq 1 10))
         (results (map (lambda (x) (round (/ x 11) 3)) inputs))
         (expected (list 0.091 0.182 0.273 0.364 0.455
                         0.545 0.636 0.727 0.818 0.909)))
    (zipwith (lambda (a b) (assert-equal a b)) expected results)))

(defun dot-product_test ()
  (assert-equal 32 (dot-product '(1 2 3) '(4 5 6)) )
  (assert-equal 122 (dot-product '(9 2 7) '(4 8 10))))

(defun scale_test ()
  (assert-equal 0.25 (scale -0.5 #(-1.0 1.0) #(0.0 1.0)))
  (assert-equal 0.0 (scale -0.5 #(-0.5 1.0) #(0.0 1.0)))
  (assert-equal 0.5 (scale -0.5 #(-1.5 0.5) #(0.0 1.0)))
  (assert-equal 1.0 (scale 1 #(-1.0 1.0) #(0.0 1.0)))
  (assert-equal 0.5 (scale 0 #(-1.0 1.0) #(0.0 1.0)))
  (assert-equal 0.0 (scale -1 #(-1.0 1.0) #(0.0 1.0)))
  (assert-equal 0.0 (scale 0 #(0.0 1.0) #(0.0 255.0)))
  (assert-equal 127.5 (scale 0.5 #(0.0 1.0) #(0.0 255.0)))
  (assert-equal 255.0 (scale 1.0 #(0.0 1.0) #(0.0 255.0))))

(defun unit-scale_test ()
  (assert-equal 0.25 (unit-scale -0.5 #(-1.0 1.0)))
  (assert-equal 0.0 (unit-scale -0.5 #(-0.5 1.0)))
  (assert-equal 0.5 (unit-scale -0.5 #(-1.5 0.5)))
  (assert-equal 1.0 (unit-scale 1 #(-1.0 1.0)))
  (assert-equal 0.5 (unit-scale 0 #(-1.0 1.0)))
  (assert-equal 0.0 (unit-scale -1 #(-1.0 1.0))))

(defun color-scale_test ()
  (assert-equal 0 (color-scale 0 #(0.0 1.0)))
  (assert-equal 64 (color-scale -0.5 #(-1.0 1.0)))
  (assert-equal 128 (color-scale 0.5 #(0.0 1.0)))
  (assert-equal 255 (color-scale 1.0 #(0.0 1.0))))

(defun uuid4_test ()
  (assert-equal 36 (byte_size (uuid4)))
  (assert-equal 288 (bit_size (uuid4)))
  (assert `(is_binary ,(uuid4)))
  (assert-equal 36 (byte_size (uuid4 (tuple 'type '"binary"))))
  (assert-equal 288 (bit_size (uuid4 (tuple 'type '"binary"))))
  (assert `(is_binary ,(uuid4 (tuple 'type '"binary"))))
  (assert-not `(is_binary ',(uuid4 (tuple 'type '"list"))))
  (assert-equal 36 (length (uuid4 (tuple 'type '"list"))))
  (assert `(is_list ',(uuid4 (tuple 'type '"list"))))
  (assert-not `(is_list ,(uuid4 (tuple 'type '"binary"))))
  (assert `(is_atom ',(uuid4 (tuple 'type '"atom"))))
  (assert-not `(is_atom ',(uuid4 (tuple 'type '"list")))))

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

(defun partition-list_test ()
  (let ((result (partition-list (test-dict-data-2))))
    (assert-equal #((key-1 key-2) ("value 1" "value 2")) result)))

(defun pair-dict_test ()
  (assert-equal '"value 1" `(: dict fetch 'key-1 ,(test-dict-2)))
  (assert-equal '"value 2" `(: dict fetch 'key-2 ,(test-dict-2))))

(defun is-home-dir?_test ()
  (assert-not `(: lfe-utils is-home-dir? '"~"))
  (assert-not `(: lfe-utils is-home-dir? '"/"))
  (assert-not `(: lfe-utils is-home-dir? '"~home/"))
  (assert-not `(: lfe-utils is-home-dir? '"/home"))
  (assert `(: lfe-utils is-home-dir? '"~/"))
  (assert `(: lfe-utils is-home-dir? '"~/user"))
  (assert `(: lfe-utils is-home-dir? '"~/user/more/path")))

(defun expand-home-dir_test ()
  (assert-equal '"/usr/local/bin"
                `(: lfe-utils expand-home-dir '"/usr/local/bin"))
  (assert-equal '"/home/oubiwann"
                `(: lfe-utils expand-home-dir '"/home/oubiwann"))
  ;; lfeunit has some issues with the following tests...
  ;(let* ((tilde-dir '"~/my-data")
  ;       (expanded (: lfe-utils expand-home-dir tilde-dir)))
  ;  (assert `(: lfe-utils is-home-dir? ,tilde-dir))
  ;  (assert-not `(: lfe-utils is-home-dir? ,expanded)))
  )

(defun strip_test ()
  (assert-equal '"data" `(: lfe-utils strip '"data\n"))
  (assert-equal '"data" `(: lfe-utils strip '"data\n\n"))
  (assert-equal '"data" `(: lfe-utils strip '"data   "))
  (assert-equal '"data" `(: lfe-utils strip '"data   \n   "))
  (assert-equal '"data" `(: lfe-utils strip '"data   \n   \n")))


