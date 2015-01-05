(defmodule lutil-cfg
  (export all))

(defun config-file () "lfe.config")
(defun global-config () (filename:join "~/.lfe" (config-file)))
(defun local-config () (config-file))

(defun read-config ()
  (lutil-type:orddict-merge
    (read-global)
    (read-local)))

(defun read-config
  ((`#(ok ,config-data))
    (orddict:from_list config-data))
  ((`#(error #(none file enoent)))
    (orddict:new))
  (((= `#(error ,_) error))
    error)
  ((_)
    (orddict:new)))

(defun read-global ()
  (read-config
    (lfe_io:read_file
      (lutil-file:expand-home-dir (global-config)))))

(defun read-local ()
  (read-config
    (lfe_io:read_file
      (filename:join (get-cwd)
                     (local-config)))))

(defun get-cwd ()
  (let ((`#(ok ,cwd) (file:get_cwd)))
    cwd))

(defun get-project ()
  (orddict:fetch 'project (read-config)))

(defun get-project-deps ()
  (proplists:get_value 'deps (get-project)))
