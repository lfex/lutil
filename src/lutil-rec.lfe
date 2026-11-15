(defmodule lutil-rec
  "Record-field macro utilities for LFE record definitions."
  (export-macro
   fields))

(defun fields-macro-name (record-name)
  "Return the atom `fields-RecordName`, which is the generated record-field
  macro name."
  (list_to_atom (++ "fields-" (atom_to_list record-name))))

(defmacro fields
  "Expand a quoted or unquoted record name to its generated `fields-RecordName`
  macro call."
  ;; Input as record name (quoted atom)
  (`((,_ . (,record-name . ,_)) . ())
   `(,(fields-macro-name record-name)))
  ;; Input as record name (unquoted atom)
  (`(,record-name . ())
   `(,(fields-macro-name record-name))))
