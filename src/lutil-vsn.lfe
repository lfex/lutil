(defmodule lutil-vsn
  "Read the lutil version and assemble application and runtime version
  information."
  (export
   (get 0)
   (all 0)))

(defun get ()
  "Attempt to load the `lutil` application, then return its `vsn` value or
  `undefined`. Loading does not start the application."
  (lookup 'lutil))

(defun lookup (app-name)
  "Attempt to load `AppName`, then return its `vsn` application key or
  `undefined`. Loading does not start the application."
  (application:load app-name)
  (case (application:get_key app-name 'vsn)
    (`#(ok ,vsn) vsn)
    (default default)))

(defun version-arch ()
  "Return a tuple containing the current Erlang system architecture."
  `#(architecture ,(erlang:system_info 'system_architecture)))

(defun version+name (app-name)
  "Return `{AppName, Version}` using the application metadata lookup."
  `#(,app-name ,(lookup app-name)))

(defun versions-rebar ()
  "Return version pairs for the `rebar` and `rebar3_lfe` applications."
  `(,(version+name 'rebar)
    ,(version+name 'rebar3_lfe)))

(defun versions-langs ()
  "Return version and runtime tuples for LFE, OTP, emulator, and driver."
  `(,(version+name 'lfe)
    #(erlang ,(erlang:system_info 'otp_release))
    #(emulator ,(erlang:system_info 'version))
    #(driver ,(erlang:system_info 'driver_version))))

(defun all ()
  "Return the combined list of lutil, language/runtime, build-tool, and
  architecture version information."
  (lists:append `((,(version+name 'lutil))
                  ,(versions-langs)
                  ,(versions-rebar)
                  (,(version-arch)))))