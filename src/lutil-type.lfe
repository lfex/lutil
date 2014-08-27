;;; data types and type ops

(defmodule lutil-type
  (export all))

(defun add-tuples (a b)
  "Given two tuples, add them together."
  (add-tuples (list a b)))

(defun add-tuples (a)
  "Given a list of any number of tuples, add them all together."
  (list_to_tuple
    (lists:flatten
      (lists:map (lambda (x) (tuple_to_list x)) a))))

(defun partition-list (list-data)
  "This function takes lists of even length with an implicit key (atom) value
  pairing and generates a list of two lists: one with all the keys, and the
  other with all the values."
  (lists:partition #'is_atom/1 list-data))

(defun pair-dict (data)
  "'data' is a list of implicit pairs:
    * the odd elements are keys of type 'atom'
    * the even elemnts are the values.

  This list is partitioned. zipped to tuples, and then converted to a dict."
  (let (((tuple keys values) (partition-list data)))
    (dict:from_list
       (lists:zip keys values))))

(defun rename-key (old-key new-key old-proplist)
  "Given an old key, a new key, and an proplist to operate upon, rename the
  keys. This is essentially a create-new-k/v followed by a delete-old-k/v
  set of operations."
  (let ((new-proplist (++ old-proplist
                     `(#(,new-key ,(proplists:get_value
                                     old-key
                                     old-proplist))))))
    (proplists:delete old-key new-proplist)))

(defun host->tuple (host)
  (let ((`#(ok ,tuple) (inet:getaddr host 'inet)))
    tuple))

(defun list->tuple (list-data)
  (let ((quoted (lists:map (lambda (x) `',x) list-data)))
    (eval `(tuple ,@quoted))))

(defun atom-cat (atom-1 atom-2)
  "Concatenate two tuples."
  (list_to_atom (++ (atom_to_list atom-1) (atom_to_list atom-2))))

(defun string? (data)
  (io_lib:printable_list data))

(defun unicode? (data)
  (io_lib:printable_unicode_list data))

(defun list? (data)
  (and (is_list data) (not (string? data))))

(defun tuple? (data)
  (is_tuple data))

(defun atom? (data)
  (is_atom data))

(defun binary? (data)
  (is_binary data))

(defun bitstring? (data)
  (is_bitstring data))

(defun bool? (data)
  (is_boolean data))

(defun float? (data)
  (is_float data))

(defun function? (data)
  (is_function data))

(defun function? (data arity)
  (is_function data arity))

(defun integer? (data)
  (is_integer data))

(defun number? (data)
  (is_number data))

(defun record? (data record-tag)
  (is_record data record-tag))

(defun record? (data record-tag size)
  (is_record data record-tag size))

(defun reference? (data)
  (is_reference data))
