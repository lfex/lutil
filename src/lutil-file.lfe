;;; files

(defmodule lutil-file
  (export all))

(defun dump-data (filename data)
  "A convenience function for writing Erlang data to disk."
  (file:write_file filename
     (io_lib:fwrite '"~p.~n" (list data))))

(defun get-home-dir ()
  (let (((list (tuple 'home (list home)))
         (lists:sublist (init:get_arguments) 3 1)))
    home))

(defun is-home-dir? (path)
  (cond ((=:= '"~/" (string:substr path 1 2))
         'true)
        ('true 'false)))

(defun expand-home-dir (path-with-home)
  (cond ((is-home-dir? path-with-home)
         (filename:join
            (list (get-home-dir)
                  (string:substr path-with-home 3))))
        ('true path-with-home)))

(defun get-deps ()
  "Get the default dependency directories for the current directory."
  (get-deps '("./deps")))

(defun get-deps (deps-dirs)
  "This function supports multiple dependency directories.

  Given a list of directories, each of which contains dependencies,
  return the full list of dependency directories, from all of the combined
  directories provided."
  (filter-deps
    (get-deps-subdirs deps-dirs)))

(defun get-deps-subdirs (deps-dirs)
  "Given a set of dependency directories, get a list of lists, where each
  of the lists is the list of directories in one of the passed deps dirs.
  Once the list of lists is obtained, collapse these into a single list."
  (lists:merge
    (lists:map
      (lambda (x)
        (filelib:wildcard (++ x "/*")))
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
  (compile lfe-files (get-deps) "."))

(defun compile (lfe-files deps-dirs out-dir)
  ;; update code paths
  (code:set_path (++ (get-deps deps-dirs)
                     (code:get_path)))
  ;; do actual compile
  (lists:map
    (lambda (x)
      (lfe_comp:file x `(verbose report #(outdir ,out-dir))))
    lfe-files))

(defun compile-src ()
  (compile-src "./ebin"))

(defun compile-src (out-dir)
  (compile (filelib:wildcard "./src/*.lfe") (get-deps) out-dir))

(defun compile-test ()
  (compile-test "./.eunit"))

(defun compile-test (out-dir)
  (compile (filelib:wildcard "./test/*.lfe") (get-deps) out-dir))

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
  (get-behaviour (get-beam-attrs beam)))

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
