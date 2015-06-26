(defmacro in? (item collection)
  (lutil:deprecated "Please use in? in the lfex/clj project.")
  `(orelse ,@(lists:map
               (lambda (x)
                 `(=:= ,x ,item))
               collection)))

(defmacro not-in? (item collection)
  (lutil:deprecated "Please use not-in? in the lfex/clj project.")
  `(not (in? ,item ,collection)))

(defun string? (data)
  (lutil:deprecated "Please use string? in the lfex/clj project.")
  (io_lib:printable_list data))

(defun unicode? (data)
  (lutil:deprecated "Please use unicode? in the lfex/clj project.")
  (io_lib:printable_unicode_list data))

(defun list? (data)
  (lutil:deprecated "Please use list? in the lfex/clj project.")
  (and (is_list data) (not (string? data))))

(defun tuple? (data)
  (lutil:deprecated "Please use tuple in the lfex/clj project.")
  (is_tuple data))

(defun atom? (data)
  (lutil:deprecated "Please use atom? in the lfex/clj project.")
  (is_atom data))

(defun binary? (data)
  (lutil:deprecated "Please use binary? in the lfex/clj project.")
  (is_binary data))

(defun bitstring? (data)
  (lutil:deprecated "Please use bitstring? in the lfex/clj project.")
  (is_bitstring data))

(defun bool? (data)
  (lutil:deprecated "Please use bool? in the lfex/clj project.")
  (is_boolean data))

(defun float? (data)
  (lutil:deprecated "Please use float? in the lfex/clj project.")
  (is_float data))

(defun function? (data)
  (lutil:deprecated "Please use function? in the lfex/clj project.")
  (is_function data))

(defun function? (data arity)
  (lutil:deprecated "Please use function? in the lfex/clj project.")
  (is_function data arity))

(defun func? (data)
  (lutil:deprecated "Please use func? in the lfex/clj project.")
  (is_function data))

(defun func? (data arity)
  (lutil:deprecated "Please use func? in the lfex/clj project.")
  (is_function data arity))

(defun integer? (data)
  (lutil:deprecated "Please use integer? in the lfex/clj project.")
  (is_integer data))

(defun int? (data)
  (lutil:deprecated "Please use int? in the lfex/clj project.")
  (is_integer data))

(defun number? (data)
  (lutil:deprecated "Please use number? in the lfex/clj project.")
  (is_number data))

(defun record? (data record-tag)
  (lutil:deprecated "Please use record? in the lfex/clj project.")
  (is_record data record-tag))

(defun record? (data record-tag size)
  (lutil:deprecated "Please use record? in the lfex/clj project.")
  (is_record data record-tag size))

(defun reference? (data)
  (lutil:deprecated "Please use reference? in the lfex/clj project.")
  (is_reference data))

(defun map? (data)
  (lutil:deprecated "Please use map? in the lfex/clj project.")
  (if (erl_internal:bif 'is_map 1)
      (call 'erlang 'is_map data)
      'false))

(defun set? (x)
  (lutil:deprecated "Please use set? in the lfex/clj project.")
  (or (sets:is_set x)
      (ordsets:is_set x)))

(defun dict?
  ((data) (when (=:= 'dict (element 1 data)))
    (lutil:deprecated "Please use dict? in the lfex/clj project.")
    'true)
  ((_)
    (lutil:deprecated "Please use dict? in the lfex/clj project.")
    'false))

(defun proplist?
  ((data) (when (is_list data))
    (lutil:deprecated "Please use proplist? in the lfex/clj project.")
    (if (lists:all #'proplist-kv?/1 data)
      'true
      'false))
  ((_)
    (lutil:deprecated "Please use proplist? in the lfex/clj project.")
    'false))

(defun proplist-kv?
  ((`#(,key ,_)) (when (is_atom key))
    (lutil:deprecated "Please use proplist-kv? in the lfex/clj project.")
    'true)
  ((bool-key) (when (is_atom bool-key))
    (lutil:deprecated "Please use proplist-kv? in the lfex/clj project.")
    'true)
  ((_)
    (lutil:deprecated "Please use proplist-kv? in the lfex/clj project.")
    'false))

(defun undefined? (x)
  (lutil:deprecated "Please use undefined? in the lfex/clj project.")
  (=:= x 'undefined))

(defun undef? (x)
  (lutil:deprecated "Please use undef? in the lfex/clj project.")
  (=:= x 'undefined))

(defun nil? (x)
  (lutil:deprecated "Please use nil? in the lfex/clj project.")
  (or (=:= x 'nil)
      (=:= x '())))

(defun true? (x)
  (lutil:deprecated "Please use true? in the lfex/clj project.")
  (=:= x 'true))

(defun false? (x)
  (lutil:deprecated "Please use false? in the lfex/clj project.")
  (=:= x 'false))

(defun odd? (x)
  (lutil:deprecated "Please use odd? in the lfex/clj project.")
  (=:= 1 (rem x 2)))

(defun even? (x)
  (lutil:deprecated "Please use even? in the lfex/clj project.")
  (=:= 0 (rem x 2)))

(defun zero? (x)
  (lutil:deprecated "Please use zero? in the lfex/clj project.")
  (=:= 0 x))

(defun pos? (x)
  (lutil:deprecated "Please use pos? in the lfex/clj project.")
  (> x 0))

(defun neg? (x)
  (lutil:deprecated "Please use neq? in the lfex/clj project.")
  (< x 0))

(defun identical? (x y)
  (lutil:deprecated "Please use identical? in the lfex/clj project.")
  (=:= x y))

(defun empty? (x)
  (lutil:deprecated "Please use empty? in the lfex/clj project.")
  (=:= x '()))

(defun every? (pred x)
  (lutil:deprecated "Please use every? in the lfex/clj project.")
  (lists:all pred x))

(defun all? (pred x)
  (lutil:deprecated "Please use all? in the lfex/clj project.")
  (lists:all pred x))

(defun any? (pred x)
  (lutil:deprecated "Please use any? in the lfex/clj project.")
  (lists:any pred x))

(defun not-any? (pred x)
  (not (lists:any pred x)))

(defun element?
  ((element x) (when (is_list x))
    (lutil:deprecated "Please use element? in the lfex/clj project.")
    (any? (lambda (y) (identical? element y)) x))
  ((element x)
    (lutil:deprecated "Please use element? in the lfex/clj project.")
    (cond
      ((sets:is_set x)
        (sets:is_element element x))
      ((ordsets:is_set x)
        (ordsets:is_element element x))
      ('true 'false))))

(defun loaded-predicates ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
