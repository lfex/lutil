(defmodule lfe-utils
  (export all)
  (import
    (from lists
      (flatten 1)
      (foldl 3)
      (map 2)
      (zipwith 3))
    (from math
      (pow 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data types and type ops
(defun add-tuples (a b)
  "Given two tuples, add them together."
  (add-tuples (list a b)))

(defun add-tuples (a)
  "Given a list of any number of tuples, add them all together."
  (list_to_tuple
    (flatten
      (map (lambda (x) (tuple_to_list x)) a))))

(defun partition-list (list-data)
  "This function takes lists of even length with an implicit key (atom) value
  pairing and generates a list of two lists: one with all the keys, and the
  other with all the values."
  (: lists partition #'is_atom/1 list-data))

(defun pair-dict (data)
  "'data' is a list of implicit pairs:
    * the odd elements are keys of type 'atom'
    * the even elemnts are the values.

  This list is partitioned. zipped to tuples, and then converted to a dict."
  (let (((tuple keys values) (partition-list data)))
    (: dict from_list
       (: lists zip keys values))))

(defun list->tuple (list-data)
  (let ((quoted (: lists map (lambda (x) `',x) list-data)))
    (eval `(tuple ,@quoted))))

(defun atom-cat (atom-1 atom-2)
  "Concatenate two tuples."
  (list_to_atom (++ (atom_to_list atom-1) (atom_to_list atom-2))))

(defun strip (string)
  (: re replace
     string
     '"(^\\s+)|(\\s+$)"
      ""
      (list 'global (tuple 'return 'list))))

(defun string? (data)
  (: io_lib printable_list data))

(defun unicode? (data)
  (: io_lib printable_unicode_list data))

(defun list? (data)
  (and (is_list data) (not (string? data))))

;;;;;;;
;; math
(defun odd? (x)
  ;; initial experiments with implementations:
  ;; > (set odd1': (set odd1? (match-lambda ((x) (when (== 1 (rem x 2))) 'true) ((_) 'false)))
  ;; > (set odd2': (set odd2? (match-lambda ((x) (when (== 1 (band x 1))) 'true) ((_) 'false)))
  ;; > (set odd3': (set odd3? (lambda (x) (== 1 (rem x 2))))
  ;; > (set odd4': (set odd4? (lambda (x) (== 1 (band x 1))))
  ;; > (set odd5': (set odd5? (lambda (x) (if (== 1 (rem x 2)) 'true 'false)))
  ;; > (set odd6': (set odd6? (lambda (x) (if (== 1 (band x 1)) 'true 'false)))
  ;;
  ;; benchmarks were done with the following:
  ;; (set numbers (lambda (x) (: lists seq 1 x))
  ;; (set testit (lambda (func calls) (: lists map (lambda (x) (funcall func x)) (funcall numbers calls))))
  ;; (set raw-benchmark (lambda (func calls runs) (: lists map (lambda (_) (let (((tuple micro _) (: timer tc (lambda () (funcall testit func calls))))) (* micro 1.0e-6))) (: lists seq 1 runs))))
  ;; (set process-results (lambda (x) (let ((min (: lists min x)) (max (: lists max x)) (med (: lists nth (round (/ (length x) 2)) (: lists sort x))) (avg (/ (: lists foldl (lambda (y acc) (+ y acc)) 0 x) (length x)))) (list min max med avg))))
  ;; (set format-results (lambda (x) (: io format '"Range: ~p - ~p mics~nMedian: ~p mics~nAverage: ~p mics~n" (funcall process-results x))))
  ;; (set benchmark (lambda (func calls runs) (funcall format-results (funcall raw-benchmark func calls runs))))
  ;;
  ;; results:
  ;;
  ;; > (funcall benchmark odd1? 1000 1000)
  ;; Range: 0.010598 - 0.015139999999999999 mics
  ;; Median: 0.011491 mics
  ;; Average: 0.011770212 mics
  ;; ok
  ;; > (funcall benchmark odd2? 1000 1000)
  ;; Range: 0.010702 - 0.015528 mics
  ;; Median: 0.011482 mics
  ;; Average: 0.011741301000000003 mics
  ;; ok
  ;; > (funcall benchmark odd3? 1000 1000)
  ;; Range: 0.005876 - 0.009034 mics
  ;; Median: 0.006233 mics
  ;; Average: 0.0063988080000000015 mics
  ;; ok
  ;; > (funcall benchmark odd4? 1000 1000)
  ;; Range: 0.005928 - 0.009072 mics
  ;; Median: 0.006359999999999999 mics
  ;; Average: 0.006509416000000003 mics
  ;; ok
  ;; > (funcall benchmark odd5? 1000 1000)
  ;; Range: 0.006188 - 0.009649 mics
  ;; Median: 0.006738999999999999 mics
  ;; Average: 0.006882939999999999 mics
  ;; ok
  ;; > (funcall benchmark odd6? 1000 1000)
  ;; Range: 0.006091 - 0.009559 mics
  ;; Median: 0.006624 mics
  ;; Average: 0.006720350000000002 mics
  ;; ok
  ;; >
  ;;
  ;; the winner is... implementation #3!
  (== 1 (rem x 2)))

(defun even? (x)
  ;; > (set even1? (lambda (x) (== 0 (rem x 2))))
  ;; > (set even2? (lambda (x) (not (funcall odd3? x))))
  ;; > (funcall benchmark even1? 1000 1000)
  ;; Range: 0.006228 - 0.009419 mics
  ;; Median: 0.006723 mics
  ;; Average: 0.006849136999999996 mics
  ;; ok
  ;; > (funcall benchmark even2? 1000 1000)
  ;; Range: 0.011061 - 0.016139999999999998 mics
  ;; Median: 0.011913 mics
  ;; Average: 0.012135202000000015 mics
  ;; ok
  (== 0 (rem x 2)))

(defun fast-floor (num)
  "Sadly, this is named 'fast-floor' only because the Racket version was given
  that name. There is no good floor function in Erlang... so this should
  probably have been called 'slow-floor'."
  (let* ((trunc (trunc num))
         (check (- num trunc)))
    (cond
      ((< check 0) (- trunc 1))
      ((> check 0) trunc)
      ('true trunc))))

(defun round (number precision)
  "Round a floating point number to the given number of decimal places."
  (let ((p (pow 10 precision)))
    (/ (round (* number p)) p)))

(defun dot-product (a b)
  "This doesn't appear to be needed for this particular library, but it was fun
  to write, and is quite pretty, so it's staying ;-)"
  (foldl #'+/2 0
    (zipwith #'*/2 a b)))

(defun scale
  "Given a value and a range that value belongs to, calculate a new value based
  upon a new range.

  This is useful, for instance, when one wants to convert a decimal value
  between 0.0 and 1.0 to a value between 0 and 255."
  ((value (tuple lower-bound upper-bound)
          (tuple lower-bound-prime upper-bound-prime))
    (let* ((fraction (/
                   (+ (abs lower-bound) value)
                   (+ (abs lower-bound) upper-bound)))
           (new-range (- upper-bound-prime lower-bound-prime)))
      (+ (* fraction new-range) lower-bound-prime))))

(defun unit-scale (value current-frame)
  "Given a value and a range that value belongs to, calculate the value when
  scaled to the range 0.0 to 1.0."
  (scale value current-frame #(0.0 1.0)))

(defun color-scale (value current-frame)
  "Given a value and a range that value belongs to, calculate the value when
  scaled to the range 0 to 255."
  (round (scale value current-frame #(0.0 255.0))))

;;;;;;;;
;; files
(defun dump-data (filename data)
  "A convenience function for writing Erlang data to disk."
  (: file write_file filename
     (: io_lib fwrite '"~p.~n" (list data))))

(defun get-home-dir ()
  (let (((list (tuple 'home (list home)))
         (: lists sublist (: init get_arguments) 3 1)))
    home))

(defun is-home-dir? (path)
  (cond ((=:= '"~/" (: string substr path 1 2))
         'true)
        ('true 'false)))

(defun expand-home-dir (path-with-home)
  (cond ((is-home-dir? path-with-home)
         (: filename join
            (list (get-home-dir)
                  (: string substr path-with-home 3))))
        ('true path-with-home)))

;;;;;;;;;;
;; records
(defun record-info (record-list-data)
  "This function is intended as a quick-fix only until a complete solution has
  landed in LFE. There are two macros under development for inclusion in LFE
  that provide record metadata introspection:
    * record-info
    * record-list
  Currently, only record-list is working.

  As such, this function was created to take the return values from record-list
  and trasnform them to the tuple that matches the output value of the Erlang
  compile-time macro record_info.

  Once the record-info macro in LFE is working, this function will be
  deprecated."
  'ok)

;;;;;;;
;; misc
(defun uuid4 ()
  "Adapted from the implementation given here:
    https://github.com/afiskon/erlang-uuid-v4/blob/8c03a11524f6bccf984575877b533ef30a009175/src/uuid.erl
  "
  (let* (((binary
            (a (size 32))
            (b (size 16))
            (c (size 16))
            (d (size 16))
            (e (size 48))) (: crypto rand_bytes 16))
        (format-template '"~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b")
        (uuid-data (list a b (band c #x0fff) (band d #x3fff) (bor #x8000 e)))
        (string (: io_lib format format-template uuid-data)))
    (list_to_binary string)))

(defun uuid4
  "A wrapper for uuid4/0."
  ;; Example usage:
  ;;
  ;;   > (: lfe-utils uuid4 (tuple 'type '"binary"))
  ;;   #B(50 101 51 53 49 99 48 97 45 50 100 100 52 45 52 54 56 55 45 50 ...)
  ;;
  ;;   > (: lfe-utils uuid4 (tuple 'type '"list"))
  ;;   "65c0aff3-421e-40bf-0f64-3ac0d1e0b72d"
  ;;
  (((tuple 'type '"binary"))
    (uuid4))
  (((tuple 'type '"list"))
    (binary_to_list (uuid4)))
  (((tuple 'type '"atom"))
    (binary_to_atom (uuid4) 'latin1)))
