(defmodule lutil-tuple
  "Concatenate tuple elements while preserving their order."
  (export all))

(defun cat (a b)
  "Concatenate the elements of `A` followed by `B` into one tuple."
  (cat (list a b)))

(defun cat (a)
  "Concatenate the elements of each tuple in `Tuples`, in list order, into one
  tuple. An empty list returns an empty tuple."
  (list_to_tuple
    (lists:append
      (lists:map (lambda (x) (tuple_to_list x)) a))))
