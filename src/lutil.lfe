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
  "Return sorted unique function and macro names in an LFE environment."
  (let* ((fun-names
          (lfe_env:fold_funs
            (lambda (name _arity _definition names)
              (cons name names))
            '() env))
         (all-names
          (lfe_env:fold_macros
            (lambda (name _definition names)
              (cons name names))
            fun-names env)))
    (lists:usort all-names)))

(defun shuffle (items)
  (list-comp ((<- `#(,_ ,n) (lists:sort
                             (list-comp ((<- m items))
                               `#(,(rand:uniform) ,m)))))
    n))

(defun version ()
  (lutil-vsn:get))

(defun versions ()
  (lutil-vsn:all))
