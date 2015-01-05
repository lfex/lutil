(defmodule lutil-cfg
  (export all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constants
;;;

(defun config-file () "lfe.config")
(defun global-config () (filename:join "~/.lfe" (config-file)))
(defun local-config () (config-file))
(defun deps-dir () "deps")
(defun github () "https://github.com/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Configuration
;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Project Dependencies
;;;

(defun get-project ()
  (get-project (read-config)))

(defun get-project
  (('())
    '())
  ((config)
    (orddict:fetch 'project config)))

(defun get-project-deps ()
  (lists:map
    #'parse-dep/1
    (get-project-deps (get-project))))

(defun get-project-deps
  (('())
    '())
  ((project)
    (proplists:get_value 'deps project)))

(defun parse-dep
  "Parse an element of the deps list.

  Returns a list of '(user-or-org repo branch). If no branch was given,
  branch gets the value of 'false."
  ;; suport the dep format of '#("user-or-org/repo" "branch")
  ((`#(,dep-element ,branch))
    (parse-dep dep-element branch))
  ;; suport the dep format of '("user-or-org/repo")
  ((dep-element)
    (parse-dep dep-element 'false)))

(defun parse-dep (dep-element branch)
  (++ (string:tokens dep-element "/") `(,branch)))

(defun get-clone-cmds ()
  (lists:map
    #'get-clone-cmd/1
    (get-project-deps)))

(defun get-clone-cmd
  ((`(,org ,name ,branch))
    (++ "git clone "
        (get-branch-option branch)
        (github)
        (filename:join (list org name))
        ".git "
        (filename:join (deps-dir) name)))
  ((_)
    'no-repo))

(defun get-branch-option
  (('false)
    "")
  ((branch)
    (++ "-b " branch)))

(defun do-clone-deps ()
  (lists:map
    #'os:cmd/1
    (get-clone-cmds)))

(defun clone-deps ()
  (lists:foreach
    #'io:format/1
    (do-clone-deps)))
