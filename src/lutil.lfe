;;;; misc utility functions
;;;;
(defmodule lutil
  (export all))

(defun uuid4 ()
  "Adapted from the implementation given here:
    https://github.com/afiskon/erlang-uuid-v4/blob/8c03a11524f6bccf984575877b533ef30a009175/src/uuid.erl
  "
  (let* (((binary
            (a (size 32))
            (b (size 16))
            (c (size 16))
            (d (size 16))
            (e (size 48))) (crypto:strong_rand_bytes 16))
        (format-template '"~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b")
        (uuid-data (list a b (band c #x0fff) (band d #x3fff) (bor #x8000 e)))
        (string (io_lib:format format-template uuid-data)))
    (list_to_binary string)))

(defun uuid4
  "A wrapper for uuid4/0."
  ;; Example usage:
  ;;
  ;;   > (lutil:uuid4 (tuple 'type 'binary))
  ;;   #B(50 101 51 53 49 99 48 97 45 50 100 100 52 45 52 54 56 55 45 50 ...)
  ;;
  ;;   > (lutil:uuid4 (tuple 'type 'list))
  ;;   "65c0aff3-421e-40bf-0f64-3ac0d1e0b72d"
  ;;
  ;;   > (lutil:uuid4 (tuple 'type 'atom))
  ;;  '
  (((tuple 'type 'binary))
    (uuid4))
  (((tuple 'type 'list))
    (binary_to_list (uuid4)))
  (((tuple 'type 'atom))
    (binary_to_atom (uuid4) 'latin1)))

(defun version ()
  (version 'lutil))

(defun version (app-name)
  (application:load app-name)
  (case (application:get_key app-name 'vsn)
    (`#(ok ,vsn) vsn)
    (default default)))

(defun version-arch ()
  `#(architecture ,(erlang:system_info 'system_architecture)))

(defun version+name (app-name)
  `#(,app-name ,(version app-name)))

(defun versions-rebar ()
  `(,(version+name 'rebar)
    ,(version+name 'rebar3_lfe)))

(defun versions-langs ()
  `(,(version+name 'lfe)
    #(erlang ,(erlang:system_info 'otp_release))
    #(emulator ,(erlang:system_info 'version))
    #(driver ,(erlang:system_info 'driver_version))))

(defun versions (bkend)
  (lists:append `((,(version+name 'lutil))
                  ,(versions-langs)
                  ,(versions-rebar)
                  (,(version-arch)))))

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
