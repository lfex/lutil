(defmodule lutil-math
  "Numeric, range-mapping, factorization, and string-distance utilities."
  (export all))

(defun ceiling (x)
  "Return the mathematical ceiling of `X` using `erlang:ceil/1`."
  (erlang:ceil x))

(defun fast-floor (x)
  "Return the mathematical floor of `X`; this alias is retained for library
  compatibility."
  ;; For lib backwards compatibility
  (erlang:floor x))

(defun round (number precision)
  "Round `Number` to `Precision` decimal places and return the numeric result as
  a float."
  (let ((p (math:pow 10 precision)))
    (/ (erlang:round (* number p)) p)))

(defun dot-product (a b)
  "Compute the sum of pairwise products from two equal-length numeric lists."
  (lists:foldl #'+/2 0
    (lists:zipwith #'*/2 a b)))

(defun scale
  "Linearly map `Value` from `{LowerBound, UpperBound}` to `{LowerBoundPrime,
  UpperBoundPrime}`. Frames are two-element tuples; the source frame must have
  nonzero width. Values outside it are extrapolated, not clamped."
  ((value (tuple lower-bound upper-bound)
          (tuple lower-bound-prime upper-bound-prime))
    (let* ((fraction (/
                   (- value lower-bound)
                   (- upper-bound lower-bound)))
           (new-range (- upper-bound-prime lower-bound-prime)))
      (+ (* fraction new-range) lower-bound-prime))))

(defun unit-scale (value current-frame)
  "Map `Value` from the two-element `CurrentFrame` tuple to `{0.0, 1.0}`. Values
  outside the frame are extrapolated, not clamped."
  (scale value current-frame #(0.0 1.0)))

(defun color-scale (value current-frame)
  "Map `Value` from the two-element `CurrentFrame` tuple to 0–255 and round to
  an integer. Values outside the frame may produce values outside that range."
  (erlang:round (scale value current-frame #(0.0 255.0))))

(defun midi-scale (value current-frame)
  "Map `Value` from the two-element `CurrentFrame` tuple to 0–127 and round to
  an integer. Values outside the frame may produce values outside that range."
  (erlang:round (scale value current-frame #(0.0 127.0))))

(defun factorial (n)
  "Return `N!` for a nonnegative integer `N`; zero returns 1."
  (factorial n 1))

(defun factorial
  "Worker for `factorial/1`: return `Accumulator * N!` for a nonnegative integer
  `N`."
  ((0 acc) acc)
  ((n acc) (when (> n 0))
    (factorial (- n 1) (* n acc))))

(defun get-next-prime (x)
  "Return the first prime candidate strictly greater than `X` by starting the
  search at `X + 1`."
  (get-next-prime x (+ x 1)))

(defun get-next-prime (_x0 x1)
  "Continue a prime search from `X1`; `_X0` is ignored and the result is the
  first prime at or above `X1`."
  (if (prime? x1)
    x1
    (get-next-prime x1)))

(defun prime? (x)
  "Return the boolean atom `true` when integer `X` is prime and at least 2;
  smaller integers return `false`."
  (if (< x 2)
    'false
    (let ((factors (factors x)))
      (cond ((< (length (lists:sort factors)) 3) 'true)
            ('true 'false)))))

(defun factors (n)
  "Return the existing factor sequence for integer `N`, including terminal `1`;
  nonpositive inputs return `{error, undefined}`."
  (factors n 2 '()))

(defun factors
  "Factorization worker for integer `N`, with candidate divisor `K` and a
  reversed factor accumulator `Acc`; the base case appends `1`."
  ((1 _ acc) (++ acc '(1)))
  ((n _ acc) (when (=< n 0))
    #(error undefined))
  ((n k acc) (when (== 0 (rem n k)))
    (factors (div n k) k (cons k acc)))
  ((n k acc)
    (factors n (+ k 1) acc)))

(defun levenshtein-simple
  "Compute edit distance recursively for two character lists without
  memoization."
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
  "Compute edit distance for two character lists, using a fresh dictionary cache
  internally; return the distance."
  (let (((tuple distance _) (levenshtein-distance
                               str1 str2 (dict:new))))
    distance))

(defun levenshtein-distance
  "Memoized worker returning `{Distance, Cache}`; cache keys are `{Str1, Str2}`
  pairs."
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
  "Return `{Input, Ranked}`, where `Ranked` is a sorted list of two-element
  lists `[Distance, Candidate]` for `StrList`."
  (tuple str1
    (lists:sort
      (lists:map
        (lambda (str2)
          (list (levenshtein-distance str1 str2) str2))
        str-list))))

(defun get-closest (number numbers)
  "Return the member of a nonempty numeric `Numbers` list with smallest absolute
  distance from `Number`; ties follow `[Distance, Value]` list ordering, so the
  smaller value wins."
  (cadr
    (lists:min
      (lists:map
        (lambda (x)
          `(,(abs (- x number)) ,x))
      (lists:reverse numbers)))))

(defun get-gradations (start inc count)
  "Return the sequence beginning at `Start` and adding `Inc` `Count` times,
  yielding `Count + 1` values."
  (lists:reverse
    (lists:foldl
      (lambda (_ acc)
        (cons (+ inc (car acc)) acc))
      `(,start)
      (lists:seq 1 count))))

(defun get-gradations
  "Use the two-element list `[Min, Max]` to derive `(Max - Min) / Divisions`,
  then return `Min` and its repeated increments. Floating arithmetic may affect
  the final value."
  ((`(,min ,max) divisions)
    (let* ((dist (- max min))
           (inc (/ dist divisions)))
      (get-gradations min inc divisions))))

(defun xform-numbers
  "Map each number in nonempty `Numbers` to its nearest generated grade,
  rounded to `Precision` decimal places. Positive divisions create a grade set
  of `Divisions + 1` values and return one result per input item; zero divisions
  repeat `float(min(Numbers))` once per item. Negative divisions raise an
  error."
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
  "Use precision 2 to map each number in nonempty `Numbers` to its nearest
  generated grade. Positive divisions create `Divisions + 1` grade candidates
  and return one result per input item; zero divisions repeat
  `float(min(Numbers))` once per item. Negative divisions raise an error."
  (xform-numbers divisions numbers 2))

(defun gcd
  "Return the nonnegative greatest common divisor of integer inputs `A` and `B`,
  including signed inputs."
  ((a 0)
   (abs a))
  ((a b)
   (gcd b (rem a b))))

(defun float->str (float)
  "Format `Float` with `io_lib:format` and the `~.20e` format; return the
  flattened character list."
  (lists:flatten (io_lib:format "~.20e" (list float))))
