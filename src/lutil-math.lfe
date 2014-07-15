;;; math

(defmodule lutil-math
  (export all))

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
  ;; (set numbers (lambda (x) (lists:seq 1 x))
  ;; (set testit (lambda (func calls) (lists:map (lambda (x) (funcall func x)) (funcall numbers calls))))
  ;; (set raw-benchmark (lambda (func calls runs) (lists:map (lambda (_) (let (((tuple micro _) (timer:tc (lambda () (funcall testit func calls))))) (* micro 1.0e-6))) (lists:seq 1 runs))))
  ;; (set process-results (lambda (x) (let ((min (lists:min x)) (max (lists:max x)) (med (lists:nth (round (/ (length x) 2)) (lists:sort x))) (avg (/ (lists:foldl (lambda (y acc) (+ y acc)) 0 x) (length x)))) (list min max med avg))))
  ;; (set format-results (lambda (x) (io:format '"Range: ~p - ~p mics~nMedian: ~p mics~nAverage: ~p mics~n" (funcall process-results x))))
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
  (let ((p (math:pow 10 precision)))
    (/ (erlang:round (* number p)) p)))

(defun dot-product (a b)
  "This doesn't appear to be needed for this particular library, but it was fun
  to write, and is quite pretty, so it's staying ;-)"
  (lists:foldl #'+/2 0
    (lists:zipwith #'*/2 a b)))

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
  (erlang:round (scale value current-frame #(0.0 255.0))))

(defun factorial (n)
  "Tail-recursive factrial function."
  (factorial n 1))

(defun factorial
  ((0 acc) acc)
  ((n acc) (when (> n 0))
    (factorial (- n 1) (* n acc))))

(defun get-next-prime (x)
  "Get the next prime in ascending order."
  (flet ((f (y)
            (cond ((prime? y) y)
                  ('true (get-next-prime (+ x 1))))))
    (f (+ x 1))))

(defun prime? (x)
  "If a number consists of more than two factors, it is not a prime number."
  (let ((factors (lutil:factors x)))
    (cond ((== 2 (length (lists:usort factors))) 'true)
          ('true 'false))))

(defun factors (n)
  "Tail-recursive prime factors function."
  (factors n 2 '()))

(defun factors
  ((1 _ acc) (++ acc '(1)))
  ((n _ acc) (when (=< n 0))
    #(error undefined))
  ((n k acc) (when (== 0 (rem n k)))
    (factors (div n k) k (cons k acc)))
  ((n k acc)
    (factors n (+ k 1) acc)))

(defun levenshtein-simple
  (('() str)
    (length str))
  ((str '())
    (length str))
  (((cons a str1) (cons b str2)) (when (== a b))
    (levenshtein-simple str1 str2))
  (((= (cons _ str1-tail) str1) (= (cons _ str2-tail) str2))
    (+ 1 (lists:min
          (list
           (levenshtein-simple str1 str2-tail)
           (levenshtein-simple str1-tail str2)
           (levenshtein-simple str1-tail str2-tail))))))

; The alternate implementations below were tested with different lengths of
; strings and from 1 to 10 to 100 to 1000 and to 10,000 iterations. Only
; very minor differences in performance were demonstrated. The implementation
; above provided the best overall performance, with the third implementation
; coming in second place, generally. The differences are so little as to
; not matter.
;
; (defun levenshtein-simple-2
;   (('() str)
;     (length str))
;   ((str '())
;     (length str))
;   (((cons a str1) (cons b str2)) (when (== a b))
;     (levenshtein-simple str1 str2))
;   ((str1 str2)
;     (+ 1 (lists:min
;           (list
;            (levenshtein-simple str1 (cdr str2))
;            (levenshtein-simple (cdr str1) str2)
;            (levenshtein-simple (cdr str1) (cdr str2)))))))
;
; (defun levenshtein-simple-3 (str1 str2)
;   (cond
;     ((== '() str1)
;      (length str1))
;     ((== '() str2)
;      (length str1))
;     ((== (car str1) (car str2))
;      (levenshtein-simple (cdr str1) (cdr str2)))
;     ('true
;       (+ 1 (lists:min
;              (list
;                (levenshtein-simple str1 (cdr str2))
;                (levenshtein-simple (cdr str1) str2)
;                (levenshtein-simple (cdr str1) (cdr str2))))))))

(defun levenshtein-distance (str1 str2)
  (let (((tuple distance _) (levenshtein-distance
                               str1 str2 (dict:new))))
    distance))

(defun levenshtein-distance
  (((= '() str1) str2 cache)
    (tuple (length str2)
           (dict:store (tuple str1 str2)
                       (length str2)
                       cache)))
  ((str1 (= '() str2) cache)
    (tuple (length str1)
           (dict:store (tuple str1 str2)
                       (length str1)
                       cache)))
  (((cons a str1) (cons b str2) cache) (when (== a b))
    (levenshtein-distance str1 str2 cache))
  (((= (cons _ str1-tail) str1) (= (cons _ str2-tail) str2) cache)
     (case (dict:is_key (tuple str1 str2) cache)
       ('true (tuple (dict:fetch (tuple str1 str2) cache) cache))
       ('false (let* (((tuple l1 c1) (levenshtein-distance str1 str2-tail cache))
                      ((tuple l2 c2) (levenshtein-distance str1-tail str2 c1))
                      ((tuple l3 c3) (levenshtein-distance str1-tail str2-tail c2))
                      (len (+ 1 (lists:min (list l1 l2 l3)))))
                 (tuple len (dict:store (tuple str1 str2) len c3)))))))

(defun levenshtein-sort (str1 str-list)
  (tuple str1
    (lists:sort
      (lists:map
        (lambda (str2)
          (list (levenshtein-distance str1 str2) str2))
        str-list))))
