(defmodule lutil-file
  "File, compiler, dependency, and BEAM-inspection utilities. Several helpers
  expose their underlying OTP result tuples directly."
  (export all))

(defun dump-data (filename data)
  "A convenience wrapper around `file:write_file/2` that writes `Data` as an
  Erlang term followed by a period and newline; returns the file operation
  result."
  (file:write_file filename
                   (io_lib:fwrite "~p.~n" (list data))))

(defun mkdirs (path)
  "Ensure the parent directory for `Path` exists, then create the directory
  itself. The result is from `file:make_dir/1`; this is not idempotent and the
  `ensure_dir/1` result is ignored."
  (filelib:ensure_dir path)
  (file:make_dir path))

(defun is-home-dir? (path)
  "Return `true` only when `Path` begins with the exact prefix `~/`; a bare `~`
  or other tilde form is false."
  (cond ((=:= "~/" (string:substr path 1 2))
         'true)
        ('true 'false)))

(defun expand-home-dir (path)
  "Expand a leading `~/` using `lutil:home/0`; leave every other path
  unchanged."
  (if (is-home-dir? path)
    (filename:join (list (lutil:home)
                         (string:substr path 3)))
    path))

(defun cwd ()
  "Return the current working directory, or pass through the error result from
  `file:get_cwd/0`."
  (case (file:get_cwd)
    (`#(ok ,dir) dir)
    (err err)))

(defun read (filename)
  "Read `Filename`, expanding only a leading `~/`; return file bytes on success
  and the file error tuple otherwise."
  (case (file:read_file (expand-home-dir filename))
    (`#(ok ,bytes) bytes)
    (err err)))

(defun relative-dir (dir)
  "Join `Dir` to the current working directory and return the resulting absolute
  path."
  (filename:join `(,(cwd) ,dir)))

(defun deps-dir ()
  "Return the path to the project `deps` directory relative to the current
  working directory." (relative-dir "deps"))
(defun ebin-dir ()
  "Return the path to the project `ebin` directory relative to the current
  working directory." (relative-dir "ebin"))
(defun src-dir ()
  "Return the path to the project `src` directory relative to the current
  working directory." (relative-dir "src"))
(defun test-dir ()
  "Return the path to the project `test` directory relative to the current
  working directory." (relative-dir "test"))
(defun eunit-dir ()
  "Return the path to the project `.eunit` directory relative to the current
  working directory." (relative-dir ".eunit"))

(defun deps ()
  "Find usable dependency directories below the default project `deps`
  directory."
  (deps `(,(deps-dir))))

(defun deps (deps-dirs)
  "Find usable dependency directories below each directory in `DepsDirs`; hidden
  names and non-directories are filtered out."
  (filter-deps
   (deps-subdirs deps-dirs)))

(defun deps-subdirs ()
  "List child paths found below the default `deps` directory."
  (deps-subdirs `(,(deps-dir))))

(defun deps-subdirs (deps-dirs)
  "Wildcard each supplied dependency root for immediate children and merge the
  resulting sorted path lists."
  (lists:merge
   (lists:map
    (lambda (x)
      (filelib:wildcard (filename:join x "*")))
    deps-dirs)))

(defun check-deps (deps-subdirs)
  "Map each candidate to its path when it is a directory whose basename does not
  start with `.`, or to `false` otherwise."
  (lists:map
   (lambda (x)
     (if (and
          ;; only keep it if it's a dir and
          (filelib:is_dir x)
          ;; it doesn't begin with a "."
          (not (== (car ".")
                   (car (filename:basename x)))))
       x))
   deps-subdirs))

(defun filter-deps (deps-subdirs)
  "Remove the `false` entries produced by `check-deps/1`, retaining candidate
  directory paths in order."
  (lists:filter
   (lambda (x)
     (not (== 'false x)))
   (check-deps deps-subdirs)))

(defun compile (lfe-files)
  "Compile each LFE file to the existing project `ebin` directory using default
  dependency directories. Each successful file retains `{ok, [{ok, Module}]}`;
  a reported `error` becomes `{error, Filename}`."
  (compile lfe-files (deps) (ebin-dir)))

(defun compile (lfe-files out-dir)
  "Compile each LFE file to the existing `OutDir` using default dependency
  directories. Successful files retain `{ok, [{ok, Module}]}`; a reported
  `error` becomes `{error, Filename}`."
  (compile lfe-files (deps) out-dir))

(defun compile (lfe-files deps-dirs out-dir)
  "Add dependencies from `DepsDirs` to the code path, then compile `LfeFiles` to
  the existing `OutDir`. The code-path addition persists. Success retains `{ok,
  [{ok, Module}]}`; a reported file error is `{error, Filename}`. Unexpected
  compiler exceptions propagate."
  ;; update code paths
  (code:set_path (++ (deps deps-dirs)
                     (code:get_path)))
  ;; do actual compile
  (lists:map
   (lambda (x)
     (case (compile-file x out-dir)
       ((= (tuple 'ok mod) result)
        result)
       ('error
        `#(error ,x))))
   lfe-files))

(defun compile-file (filename out-dir)
  "Compile one LFE source file to an existing `OutDir` with verbose reporting
  and the relative `include` directory; return the native `lfe_comp:file/2`
  result."
  (lfe_comp:file filename `(verbose report
                                    #(outdir ,out-dir)
                                    #(i "include"))))

(defun compile-src ()
  "Compile `src/*.lfe` to the existing `ebin` directory and add the application
  resource-copy outcome; the merged result list retains each compiler result."
  (compile-src "ebin"))

(defun compile-src (out-dir)
  "Compile `src/*.lfe` to an existing `OutDir` and add the application
  resource-copy outcome; the merged result list retains each compiler result."
  (lists:merge
   (compile
    (filelib:wildcard
     (filename:join (src-dir) "*.lfe"))
    (deps)
    out-dir)
   (list (compile-app-src out-dir))))

(defun compile-test ()
  "Compile the project `test/*.lfe` files into the existing default `.eunit`
  directory."
  (compile-test (eunit-dir)))

(defun compile-test (out-dir)
  "Compile the project `test/*.lfe` files to an existing `OutDir` and return the
  compiler outcomes."
  (compile
   (filelib:wildcard
    (filename:join (test-dir) "*.lfe"))
   (deps)
   out-dir))

(defun compile-app-src ()
  "Copy the first `src/NAME.app.src` match to `ebin/NAME.app`; both paths are
  relative to the current working directory and `ebin` must exist."
  (compile-app-src "ebin"))

(defun compile-app-src (out-dir)
  "Copy the first `src/NAME.app.src` match to `OutDir/NAME.app`; `OutDir` must
  exist. Success is `{ok, {'app-src', Source}, {'app-dst', Destination}}`;
  missing source is `{error, {app_src_not_found, AbsSrcDir}}`, where
  `AbsSrcDir` is the absolute `src` path; a reported copy failure is `{error,
  {app_src_copy_failed, Source, Destination, Reason}}`. Unexpected exceptions
  propagate."
  (let ((app-srcs (filelib:wildcard
                    (filename:join "src" "*.app.src"))))
    (case app-srcs
      ('()
       (tuple 'error (tuple 'app_src_not_found (src-dir))))
      ((cons app-src _)
       (let* ((app-dst (filename:join out-dir
                                      (filename:basename
                                       (filename:rootname app-src))))
              (copy-result (file:copy app-src app-dst)))
         (case copy-result
           ((tuple 'ok _)
            (tuple 'ok (tuple 'app-src app-src)
                   (tuple 'app-dst app-dst)))
           ((tuple 'error reason)
            (tuple 'error
                   (tuple 'app_src_copy_failed app-src app-dst reason)))))))))

(defun compile-deps ()
  "Compile dependencies in order and retain `{AbsoluteDir, Outcome}` entries.
  Return `ok` if all compile, or `{error, {dependency_compile_failed,
  Results}}` with partial outcomes; a directory-change failure is `{error,
  {set_cwd_failed, Reason}}`. Already written files are retained. Unexpected
  compiler exceptions propagate after the original working directory is
  restored. A restoration failure raises `{restore_cwd_failed, OriginalDir,
  ReasonOrResult}`."
  (let (((tuple 'ok orig-cwd) (file:get_cwd)))
    (try
      (let* ((reversed-results
              (lists:foldl
               (lambda (directory results)
                 (let* ((absolute-dir (filename:absname directory))
                        (outcome
                         (case (file:set_cwd absolute-dir)
                           ('ok
                            (compile
                             (filelib:wildcard
                              (filename:join '("src" "*.lfe")))
                             "ebin"))
                           ((tuple 'error reason)
                            (tuple 'error
                                   (tuple 'set_cwd_failed reason)))))
                        (result (tuple absolute-dir outcome)))
                   (cons result results)))
               '()
               (deps)))
             (results (lists:reverse reversed-results))
             (failed?
              (lists:any
               (lambda (entry)
                 (let ((outcome (element 2 entry)))
                   (case outcome
                     ((tuple 'error _) 'true)
                     (per-file-results
                      (lists:any
                       (lambda (file-result)
                         (case file-result
                           ((tuple 'error _) 'true)
                           (_ 'false)))
                       per-file-results)))))
               results)))
        (if failed?
          (tuple 'error (tuple 'dependency_compile_failed results))
          'ok))
      (after
        (case (file:set_cwd orig-cwd)
          ('ok 'ok)
          ((tuple 'error reason)
           (error (tuple 'restore_cwd_failed orig-cwd reason)))
          (other
           (error (tuple 'restore_cwd_failed orig-cwd other))))))))

(defun files->beams (file-data)
  "Convert a list of `{Module, BeamFilename}` pairs to `{Module, Rootname}`
  pairs, and plain filenames to rootnames; `.beam` suffixes are removed."
  (lists:map
   (match-lambda
     (((tuple mod filename))
      `#(,mod ,(filename:rootname filename)))
     ((filename)
      (filename:rootname filename)))
   file-data))

(defun beams->files (beam-data)
  "Add `.beam` to each beam rootname, preserving `{Module, Beam}` pairs as
  `{Module, BeamFile}`."
  (lists:map
   (match-lambda
     (((tuple mod beam))
      `#(,mod ,(++ beam ".beam")))
     ((beam)
      (++ beam ".beam")))
   beam-data))

(defun beams->modules (beams-list)
  "Read the module identity from each beam path and return the corresponding
  module-name list."
  (lists:map
   #'beam->module/1
   beams-list))

(defun modules->beams (module-list)
  "Return sorted unique beam rootnames for the modules currently resolved by
  `code:which/1`."
  (lists:usort
   (lists:map
    (lambda (x)
      (filename:rootname (code:which x)))
    module-list)))

(defun get-beam-attrs (beam)
  "Return the attributes chunk from the beam at `BeamPath`."
  (let (((tuple 'ok (tuple _ (list (tuple 'attributes attrs))))
         (beam_lib:chunks beam '(attributes))))
    attrs))

(defun module->beam (module)
  "Return the beam path currently resolved for `Module` by `code:which/1`."
  (code:which module))

(defun beam->module (beam)
  "Return the module identity stored in `BeamPath`."
  (let (((tuple 'ok (tuple module _))
         (beam_lib:chunks beam '())))
    module))

(defun get-module-attrs (module)
  "Resolve `Module` with `code:which/1` and return the attributes for that BEAM.
  The helper does not require `code:is_loaded/1` to report true."
  (get-beam-attrs (code:which module)))

(defun get-beam-behaviours (beam)
  "Return the beam behaviour attribute, or an empty list when no behaviour
  attribute is present."
  (let ((behavs (get-behaviour (get-beam-attrs beam))))
    (case behavs
      ('undefined '())
      (_ behavs))))

;; provided for the spelling-impaired
(defun get-beam-behaviors (beam)
  "American-spelling alias for `get-beam-behaviours/1`."
  (get-beam-behaviours beam))

(defun get-module-behaviours (module)
  "Return the behaviour attribute for the beam currently resolved for `Module`,
  or an empty list when absent."
  (get-beam-behaviours (code:which module)))

;; provided for the spelling-impaired
(defun get-module-behaviors (module)
  "American-spelling alias for `get-module-behaviours/1`."
  (get-module-behaviours module))

(defun get-behaviour (attrs)
  "Read the `behaviour` attribute, falling back to `behavior`; return
  `undefined` when neither key is present."
  (proplists:get_value
   'behaviour
   attrs
   (proplists:get_value 'behavior attrs)))

(defun load-beams (beams)
  "Load each beam rootname with `code:load_abs/1` and return the per-beam load
  results."
  (lists:map
   #'code:load_abs/1
   beams))

(defun check-loaded-modules (substring)
  "For each loaded-module entry, return the entry when its module name matches
  the regular expression `.*Substring.*`, or `false` otherwise. `Substring` is
  used as a regular expression fragment."
  (lists:map
   (lambda (x)
     (case (re:run (atom_to_list (element 1 x)) (++ ".*" substring ".*"))
       ((tuple 'match _) x)
       (_ 'false)))
   (code:all_loaded)))

(defun filtered-loaded-modules (substring)
  "Apply `check-loaded-modules/1` to the loaded modules and remove its `false`
  placeholders."
  (filtered #'check-loaded-modules/1 substring))

(defun get-loaded-beams (substring)
  "Find loaded modules whose names match `Substring`, then convert their beam
  paths to module/rootname pairs."
  (files->beams
   (filtered-loaded-modules substring)))

(defun get-beam-exports (beam)
  "Return the exports chunk from the beam at `BeamPath`."
  (let (((tuple 'ok (tuple _ (list (tuple 'exports exports))))
         (beam_lib:chunks beam '(exports))))
    exports))

(defun get-module-exports (module)
  "Return the exports for the beam currently resolved for `Module`."
  (get-beam-exports (code:which module)))

(defun filtered (func beams)
  "Apply `Func` to `Beams`, then retain each result except the atom `false`."
  (lists:filter
   #'lutil:check/1
   (funcall func beams)))

(defun priv (app priv-rel-path)
  "Join `PrivRelPath` to the application private directory returned by
  `code:priv_dir/1`."
  (filename:join (code:priv_dir app)
                 priv-rel-path))

(defun read-priv (app priv-rel-path)
  "Read the private file and return its bytes or the file error tuple."
  (case (file:read_file (priv app priv-rel-path))
    (`#(ok ,data) data)
    (err err)))

(defun read-priv-config (app priv-rel-path)
  "Consult the private file. Unwrap only a successful result containing exactly
  one term; otherwise return the underlying consult result unchanged."
  (case (file:consult (priv app priv-rel-path))
    (`#(ok (,terms)) terms)
    (err err)))
