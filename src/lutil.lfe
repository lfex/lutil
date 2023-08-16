;;;; misc utility functions
;;;;
(defmodule lutil
  (export all))

(defun check (x)
  (=/= x 'false))

(defun env ()
  (maps:from_list (os:env)))

(defun env (key-str)
  (let ((e (env)))
    (if (maps:is_key key-str e)
      (mref e key-str)
      "")))

(defun user ()
  (env "USER"))

(defun home()
  (env "HOME"))

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

(defun uuid4 ()
  (lutil-uuid:four #(type strong bitstring)))

(defun uuid4 (opts)
  (lutil-uuid:four opts))

(defun version ()
  (lutil-vsn:get))

(defun versions ()
  (lutil-vsn:all))
