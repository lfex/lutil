(defmodule lutil-atom
  "Construct atom names by concatenating existing atoms. These helpers use
  `list_to_atom/1`; call them only when the resulting names are controlled and
  bounded."
  (export
   (cat 1) (cat 2)))

(defun cat (atom-1 atom-2)
  "Concatenate the character lists of `Atom1` and `Atom2`, then convert the
  result to an atom."
  (list_to_atom (++ (atom_to_list atom-1) (atom_to_list atom-2))))

(defun cat (list-of-atoms)
  "Concatenate the character lists of the atoms in `Atoms`, then convert the
  result to one atom. The empty list produces the empty-name atom."
  (list_to_atom
    (lists:foldl
        (lambda (x acc) (++ acc (atom_to_list x)))
        ""
        list-of-atoms)))
