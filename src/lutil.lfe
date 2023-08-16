;;;; misc utility functions
;;;;
(defmodule lutil
  (export all))

(defun uuid4 ()
  (lutil-uuid:four #(type strong bitstring)))

(defun uuid4 (opts)
  (lutil-uuid:four opts))

(defun version ()
  (lutil-vsn:get))

(defun versions ()
  (lutil-vsn:all))

(defun check (x)
  (=/= x 'false))

(defun get-env-funcs (env)
  (lists:sort
    (lists:map
      (lambda (x)
        (element 1 x))
      (element 3 env))))

(defun shuffle (items)
  (list-comp ((<- `#(,_ ,n) (lists:sort
                             (list-comp ((<- m items))
                               `#(,(rand:uniform) ,m)))))
    n))
