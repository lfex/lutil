(defmodule lutil-file
  (export all))

(defun dump-data (filename data)
  "A convenience function for writing Erlang data to disk."
  (file:write_file filename
                   (io_lib:fwrite "~p.~n" (list data))))

(defun mkdirs (path)
  (filelib:ensure_dir path)
  (file:make_dir path))

(defun is-home-dir? (path)
  (cond ((=:= "~/" (string:substr path 1 2))
         'true)
        ('true 'false)))

(defun expand-home-dir (path)
  (if (is-home-dir? path)
    (filename:join (list (lutil:home)
                         (string:substr path 3)))
    path))

(defun cwd ()
  (case (file:get_cwd)
    (`#(ok ,dir) dir)
    (err err)))

(defun relative-dir (dir)
  "Get the absolute path to directory relative to the current working dir."
  (filename:join `(,(cwd) ,dir)))

(defun deps-dir () (relative-dir "deps"))
(defun ebin-dir () (relative-dir "ebin"))
(defun src-dir () (relative-dir "src"))
(defun test-dir () (relative-dir "test"))
(defun eunit-dir () (relative-dir ".eunit"))

(defun deps ()
  (deps `(,(deps-dir))))

(defun deps (deps-dirs)
  "This function supports multiple dependency directories.

  Given a list of directories, each of which contains dependencies,
  return the full list of dependency directories, from all of the combined
  directories provided."
  (filter-deps
   (deps-subdirs deps-dirs)))

(defun deps-subdirs ()
  (deps-subdirs `(,(deps-dir))))

(defun deps-subdirs (deps-dirs)
  "Given a set of dependency directories, get a list of lists, where each
  of the lists is the list of directories in one of the passed deps dirs.
  Once the list of lists is obtained, collapse these into a single list."
  (lists:merge
   (lists:map
    (lambda (x)
      (filelib:wildcard (filename:join x "*")))
    deps-dirs)))

(defun check-deps (deps-subdirs)
  "Given a list of dependency directories, check to see which subdirectories
  we actually care about. Those we don't want, return false."
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
  "Filter the dependencies subdirectories to return only the ones that pass
  the check-deps criteria."
  (lists:filter
   (lambda (x)
     (not (== 'false x)))
   (check-deps deps-subdirs)))

(defun compile (lfe-files)
  (compile lfe-files (deps) (ebin-dir)))

(defun compile (lfe-files out-dir)
  (compile lfe-files (deps) out-dir))

(defun compile (lfe-files deps-dirs out-dir)
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
  (lfe_comp:file filename `(verbose report
                                    #(outdir ,out-dir)
                                    #(i "include"))))

(defun compile-src ()
  (compile-src (ebin-dir)))

(defun compile-src (out-dir)
  (lists:merge
   (compile
    (filelib:wildcard
     (filename:join (src-dir) "*.lfe"))
    (deps)
    out-dir)
   (list (compile-app-src))))

(defun compile-test ()
  (compile-test (eunit-dir)))

(defun compile-test (out-dir)
  (compile
   (filelib:wildcard
    (filename:join (test-dir) "*.lfe"))
   (deps)
   out-dir))

(defun compile-app-src ()
  (let* ((app-src (car (filelib:wildcard
                        (filename:join
                         "src"
                         "*.app.src"))))
         (app-dst (filename:join "ebin"
                                 (filename:basename
                                  (filename:rootname app-src)))))
    (case (file:copy app-src app-dst)
      ((tuple 'ok _)
       `#(ok #(app-src ,app-src)
             #(app-dst ,app-dst))))))

(defun compile-deps ()
  (let (((tuple 'ok orig-cwd) (file:get_cwd)))
    (lists:map
     (lambda (x)
       (file:set_cwd x)
       (compile
        (filelib:wildcard (filename:join '("src" "*.lfe")))
        "ebin"))
     (deps))
    (file:set_cwd orig-cwd)))

(defun files->beams (file-data)
  "This function handles two cases:

    * Given a list of 2-tuples #(module-name filename), with the filenames
      ending in '.beam', return a list of tuples with no '.beam' extension,
      e.g.: #(module-name rootname).
    * Given a list of filenames, return a list of beams (i.e., no file
      extensions)."
  (lists:map
   (match-lambda
     (((tuple mod filename))
      `#(,mod ,(filename:rootname filename)))
     ((filename)
      (filename:rootname filename)))
   file-data))

(defun beams->files (beam-data)
  "Given a list of beams (no .beam extension), return a list of files (with
  the .beam extension)."
  (lists:map
   (match-lambda
     (((tuple mod beam))
      `#(,mod ,(++ beam ".beam")))
     ((beam)
      (++ beam ".beam")))
   beam-data))

(defun beams->modules (beams-list)
  (lists:map
   #'beam->module/1
   beams-list))

(defun modules->beams (module-list)
  (lists:usort
   (lists:map
    (lambda (x)
      (filename:rootname (code:which x)))
    module-list)))

(defun get-beam-attrs (beam)
  "Given an atom representing a plugin's name, return its module
  attributes."
  (let (((tuple 'ok (tuple _ (list (tuple 'attributes attrs))))
         (beam_lib:chunks beam '(attributes))))
    attrs))

(defun module->beam (module)
  (code:which module))

(defun beam->module (beam)
  (let (((tuple 'ok (tuple module _))
         (beam_lib:chunks beam '())))
    module))

(defun get-module-attrs (module)
  (get-beam-attrs (code:which module)))

(defun get-beam-behaviours (beam)
  "Given an atom representing a plugin's name, return its module
  attributes."
  (let ((behavs (get-behaviour (get-beam-attrs beam))))
    (case behavs
      ('undefined '())
      (_ behavs))))

;; provided for the spelling-impaired
(defun get-beam-behaviors (beam)
  (get-beam-behaviours beam))

(defun get-module-behaviours (module)
  (get-beam-behaviours (code:which module)))

;; provided for the spelling-impaired
(defun get-module-behaviors (module)
  (get-module-behaviours module))

(defun get-behaviour (attrs)
  (proplists:get_value
   'behaviour
   attrs
   (proplists:get_value 'behavior attrs)))

(defun load-beams (beams)
  (lists:map
   #'code:load_abs/1
   beams))

(defun check-loaded-modules (substring)
  (lists:map
   (lambda (x)
     (case (re:run (atom_to_list (element 1 x)) (++ ".*" substring ".*"))
       ((tuple 'match _) x)
       (_ 'false)))
   (code:all_loaded)))

(defun filtered-loaded-modules (substring)
  (filtered #'check-loaded-modules/1 substring))

(defun get-loaded-beams (substring)
  (files->beams
   (filtered-loaded-modules substring)))

(defun get-beam-exports (beam)
  "Given a beam path, return its exported functions."
  (let (((tuple 'ok (tuple _ (list (tuple 'exports exports))))
         (beam_lib:chunks beam '(exports))))
    exports))

(defun get-module-exports (module)
  "Given an atom representing a module's name, return its exported functions."
  (get-beam-exports (code:which module)))

(defun filtered (func beams)
  (lists:filter
   #'lutil:check/1
   (funcall func beams)))
