(defmodule lutil-rec
  (export-macro
   fields))

(defun fields-macro-name (record-name)
  "This utility function takes a record's name as an argument and returns the
  macro name for returning the a record's fields."
  (list_to_atom (++ "fields-" (atom_to_list record-name))))

(defmacro fields
  (`(,record-name . ,_)
   `(,(fields-macro-name record-name))))
