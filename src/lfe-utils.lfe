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