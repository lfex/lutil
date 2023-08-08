(defmodule lutil-atom
  (export all))

(defun cat (atom-1 atom-2)
  "Concatenate two atoms."
  (list_to_atom (++ (atom_to_list atom-1) (atom_to_list atom-2))))

(defun cat (list-of-atoms)
  "Concatenate n atoms."
  (list_to_atom
    (lists:foldl
        (lambda (x acc) (++ acc (atom_to_list x)))
        ""
        list-of-atoms)))
