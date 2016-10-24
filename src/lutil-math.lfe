(defmodule lutil-math
  (export all))

(defun floor (x)
  (let ((truncated (trunc x)))
    (case (- x truncated)
      (neg (when (< neg 0))
           (- truncated 1))
      (pos (when (> pos 0))
           truncated)
      (_ truncated))))

(defun ceiling (x)
  (let ((truncated (trunc x)))
    (case (- x truncated)
      (neg (when (< neg 0))
           truncated)
      (pos (when (> pos 0))
           (+ truncated 1))
      (_ truncated))))

(defun fast-floor (num)
  "Sadly, this is named 'fast-floor' only because the Racket version was given
  that name. There is no good floor function in Erlang... so this should
  probably have been called 'slow-floor'.

  Old implementation has been replced with the code in floor/1. This function
  is kept for backwards compatibility."
  (floor num))

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

(defun get-closest (number numbers)
  "Given a number and a list of numbers, the number in the list that is closest
  to the given number will be returned."
  (cadr
    (lists:min
      (lists:map
        (lambda (x)
          `(,(abs (- x number)) ,x))
      (lists:reverse numbers)))))

(defun get-gradations (start inc count)
  "Given a starting number, a number to add at each iteration, and a number of
  times to iterate, return a list of these incrementated gradations."
  (lists:reverse
    (lists:foldl
      (lambda (_ acc)
        (cons (+ inc (car acc)) acc))
      `(,start)
      (lists:seq 1 count))))

(defun get-gradations
  ((`(,min ,max) divisions)
    (let* ((dist (- max min))
           (inc (/ dist divisions)))
      (get-gradations min inc divisions))))

(defun xform-numbers
  "Given a list of numbers, transform them into the number of groups
  represented by 'divisions'.

  Keep in mind that 1 division results in two groups; 9 divisions gives 10
  groups."
  ((divisions _ _) (when (< divisions 0))
    (error "The number of divisions must be positive."))
  ((0 numbers _)
    (lists:duplicate (length numbers) (float (lists:min numbers))))
  ((divisions numbers precision)
    (let* ((min (lists:min numbers))
           (max (lists:max numbers))
           (dist (- max min))
           (inc (/ dist divisions))
           (grades (get-gradations min inc divisions)))
      (lists:map
        (lambda (x)
          (round (get-closest x grades) precision)) numbers))))

(defun xform-numbers (divisions numbers)
  "The default precision for rounding decimals is two places."
  (xform-numbers divisions numbers 2))

(defun gcd
  "Get the greatest common divisor."
  ((a 0)
   a)
  ((a b)
   (gcd b (rem a b))))
