(defmodule lutil-cfg
  (export all))

(include-lib "lutil/include/compose.lfe")

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
    (list->orddict config-data))
  ;; If the file doesn't exist, let's just return an empty orddict
  ((`#(error #(none file enoent)))
    (orddict:new))
  ;; For other errors, let's see what they are
  (((= `#(error ,_) error))
    error)
  ((_)
    (orddict:new)))

(defun read-global ()
  (->> (global-config)
       (lutil-file:expand-home-dir)
       (read-file)
       (read-config)))

(defun read-local ()
  (->> (local-config)
       (filename:join (get-cwd))
       (read-file)
       (read-config)))

(defun read-file (filename)
  (try
    (lfe_io:read_file filename)
    (catch
      ;; Handle zero-byte files
      (`#(error #(,_ #(eof ,_)) ,_)
        '()))))

(defun get-cwd ()
  (let ((`#(ok ,cwd) (file:get_cwd)))
    cwd))

(defun list->orddict (config-data)
  (orddict:from_list
    (check-contents config-data)))

(defun check-contents (contents)
  "This function should be called immediately before the config data is
  passed to (orddict:from_list ...), as it will ensure that each top-level
  item that was parsed is a tuple. This allows the config loaders to render
  a non-gibberish error message to the user."
  (if (lists:all #'is_tuple/1 contents)
      contents
      (error (++ "Every top-level item in an lfe.config file needs "
                 "to be a tuple."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Project Dependencies
;;;

(defun clone-deps ()
  (lists:foreach
    #'io:format/1
    (do-clone-deps)))

(defun do-clone-deps ()
  (do-clone-deps (get-clone-cmds)))

(defun do-clone-deps
  (('())
    '(no-deps))
  (('no-repo)
    '(no-deps))
  ((commands)
    (lists:map #'do-cmd/1 commands)))

(defun do-cmd (command)
  (clean-cmd (os:cmd command)))

(defun clean-cmd (result)
  (re:replace result "^fatal:" "git:" '(#(return list))))

(defun get-clone-cmds ()
  (lists:map
    #'get-clone-cmd/1
    (get-projects-deps)))

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
    (++ " -b " branch)))

(defun get-projects-deps ()
  (lists:map
    #'parse-dep/1
    (merge-project-deps)))

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

(defun merge-project-deps ()
  (merge-project-deps (read-global) (read-local)))

(defun merge-project-deps (config-1 config-2)
  (select-deps
    (get-project-deps (get-project config-1))
    (get-project-deps (get-project config-2))))

(defun select-deps (deps-1 deps-2)
  "This function takes two lists of dependencies where each dependency may be
  either a list, e.g. 'rvirding/lfe' or a tuple, e.g. #('rvirding/lfe' 'master')
  The deps are compared at the org/repo level only, not at the branch level.

  The convention here is that secondary additions override initial additions.
  As such, if you want one particular set of deps to take precedence over
  another, be sure to pass them second."
  (++ (lists:filter
        (lambda (x)
          (let ((repo (get-repo x)))
            (not (or (lists:keymember repo 1 deps-2)
                     (lists:member repo deps-2)))))
        deps-1)
      deps-2))

(defun get-repo
  ((dep) (when (is_tuple dep))
    (element 1 dep))
  ((dep)
    dep))

(defun get-project-deps
  (('())
    '())
  ((project)
    (proplists:get_value 'deps project '())))

(defun get-project
  (('())
    '())
  ((config)
    (proplists:get_value 'project config '())))
