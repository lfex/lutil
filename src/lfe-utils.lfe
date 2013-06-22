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

(defun add-tuples (a b)
  "
  Given two tuples, add them together.
  "
  (add-tuples (list a b)))

(defun add-tuples (a)
  "
  Given a list of any number of tuples, add them all together.
  "
  (list_to_tuple
    (flatten
      (map (lambda (x) (tuple_to_list x)) a))))

(defun fast-floor (num)
  "
  Sadly, this is named 'fast-floor' only because the Racket version was given
  that name. There is no good floor function in Erlang... so this should
  probably have been called 'slow-floor'.
  "
  (let* ((trunc (trunc num))
         (check (- num trunc)))
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

(defun uuid4 ()
  "
  Adapted from the implementation given here:
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
  "
  A wrapper for uuid4/0.
  "
  ; Example usage:
  ;
  ;   > (: lfe-utils uuid4 (tuple 'type '"binary"))
  ;   #B(50 101 51 53 49 99 48 97 45 50 100 100 52 45 52 54 56 55 45 50 ...)
  ;
  ;   > (: lfe-utils uuid4 (tuple 'type '"list"))
  ;   "65c0aff3-421e-40bf-0f64-3ac0d1e0b72d"
  ;
  (((tuple 'type '"binary"))
    (uuid4))
  (((tuple 'type '"list"))
    (binary_to_list (uuid4)))
  (((tuple 'type '"atom"))
    (binary_to_atom (uuid4) 'latin1)))