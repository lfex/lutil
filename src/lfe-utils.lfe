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

(defun add-tuples (a)
  "
  If there's a better way to do this, pull requests welcome!
  "
  (list_to_tuple
    (flatten
      (map (lambda (x) (tuple_to_list x)) a))))

(defun fast-floor (int)
  "
  Sadly, this is named 'fast-floor' only because the Racket version was given
  that name (it makes copying and pasting the code that much easier!). There
  is no good floor function in Erlang... so this should probably have been
  called 'slow-floor'.
  "
  (let* ((trunc (trunc int))
         (check (- int trunc)))
    (cond
      ((< check 0) (- trunc 1))
      ((> check 0) trunc)
      ('true trunc))))

(defun round (number precision)
  "
  Round a floating point number to the given number of decimal places.
  "
  (let ((p (pow 10 precision)))
    (/ (round (* number p)) p)))

(defun dot-product (a b)
  "
  This doesn't appear to be needed for this particular library, but it was fun
  to write, and is quite pretty, so it's staying ;-)
  "
  (foldl #'+/2 0
    (zipwith #'*/2 a b)))

(defun scale
  "
  Given a value and a range that value belongs to, calculate a new value based
  upon a new range.

  This is useful, for instance, when one wants to convert a decimal value
  between 0.0 and 1.0 to a value between 0 and 255.
  "
  ((value (tuple lower-bound upper-bound)
          (tuple lower-bound-prime upper-bound-prime))
    (let* ((fraction (/
                   (+ (abs lower-bound) value)
                   (+ (abs lower-bound) upper-bound)))
           (new-range (- upper-bound-prime lower-bound-prime)))
      (+ (* fraction new-range) lower-bound-prime))))

(defun unit-scale (value current-frame)
  "
  Given a value and a range that value belongs to, calculate the value when
  scaled to the range 0.0 to 1.0.
  "
  (scale value current-frame #(0.0 1.0)))

(defun color-scale (value current-frame)
  "
  Given a value and a range that value belongs to, calculate the value when
  scaled to the range 0 to 255.
  "
  (round (scale value current-frame #(0.0 255.0))))