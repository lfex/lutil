(defmodule lutil-tuple
  (export all))

(defun cat (a b)
  "Given two tuples, concatenate them to a single tuple."
  (cat (list a b)))

(defun cat (a)
  "Given a list of any number of tuples, concatenate them to a single tuple."
  (list_to_tuple
    (lists:flatten
      (lists:map (lambda (x) (tuple_to_list x)) a))))
