;;;; misc utility functions
;;;;
(defmodule lutil
  (export all))

(defun uuid4 ()
  (lutil-uuid:four #(type strong bitstring)))

(defun uuid4 (opts)
  (lutil-uuid:four opts))

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
