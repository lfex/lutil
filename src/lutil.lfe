;;;; misc utility functions
;;;;
(defmodule lutil
  "General environment, LFE environment, shuffle, and version helpers."
  (export all))

(defun check (x)
  "Return `false` only for the atom `false`; every other value returns `true`."
  (=/= x 'false))

(defun env ()
  "Return the operating-system environment as a map from variable-name strings
  to value strings."
  (maps:from_list (os:env)))

(defun env (key-str)
  "Return the environment value for `KeyStr`, or the empty string when the key
  is absent."
  (let ((e (env)))
    (if (maps:is_key key-str e)
      (mref e key-str)
      "")))

(defun user ()
  "Return the value of the `USER` environment variable, or an empty string when
  unset."
  (env "USER"))

(defun home()
  "Return the value of the `HOME` environment variable, or an empty string when
  unset."
  (env "HOME"))

(defun get-env-funcs (env)
  "Return the sorted unique names of functions and macros in `Env`; arities are
  not included."
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
  "Return a permutation produced by sorting elements tagged with
  `rand:uniform/0` values. It is a convenience shuffle and makes no uniformity
  or cryptographic guarantee."
  (list-comp ((<- `#(,_ ,n) (lists:sort
                             (list-comp ((<- m items))
                               `#(,(rand:uniform) ,m)))))
    n))

(defun version ()
  "Return the installed lutil version through `lutil-vsn:get/0`."
  (lutil-vsn:get))

(defun versions ()
  "Return the runtime and application version information through
  `lutil-vsn:all/0`."
  (lutil-vsn:all))
