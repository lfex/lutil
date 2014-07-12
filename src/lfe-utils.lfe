(defmodule lfe-utils
  (export all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; data types and type ops
(defun add-tuples (a b)
  "Given two tuples, add them together."
  (add-tuples (list a b)))

(defun add-tuples (a)
  "Given a list of any number of tuples, add them all together."
  (list_to_tuple
    (lists:flatten
      (lists:map (lambda (x) (tuple_to_list x)) a))))

(defun partition-list (list-data)
  "This function takes lists of even length with an implicit key (atom) value
  pairing and generates a list of two lists: one with all the keys, and the
  other with all the values."
  (lists:partition #'is_atom/1 list-data))

(defun pair-dict (data)
  "'data' is a list of implicit pairs:
    * the odd elements are keys of type 'atom'
    * the even elemnts are the values.

  This list is partitioned. zipped to tuples, and then converted to a dict."
  (let (((tuple keys values) (partition-list data)))
    (dict:from_list
       (lists:zip keys values))))

(defun list->tuple (list-data)
  (let ((quoted (lists:map (lambda (x) `',x) list-data)))
    (eval `(tuple ,@quoted))))

(defun atom-cat (atom-1 atom-2)
  "Concatenate two tuples."
  (list_to_atom (++ (atom_to_list atom-1) (atom_to_list atom-2))))

(defun strip (string)
  (re:replace
     string
     '"(^\\s+)|(\\s+$)"
      ""
      (list 'global (tuple 'return 'list))))

(defun capitalized? (string)
  "This function checks to see if the first letter of a passed string is
  capitalized.

  Capital letters range from 'A' (ASCII code 65) to 'Z' (ASCII code 90)."
  (lists:member (car string) (lists:seq 65 90)))

(defun string? (data)
  (io_lib:printable_list data))

(defun unicode? (data)
  (io_lib:printable_unicode_list data))

(defun list? (data)
  (and (is_list data) (not (string? data))))

(defun tuple? (data)
  (is_tuple data))

(defun atom? (data)
  (is_atom data))

(defun binary? (data)
  (is_binary data))

(defun bitstring? (data)
  (is_bitstring data))

(defun bool? (data)
  (is_boolean data))

(defun float? (data)
  (is_float data))

(defun function? (data)
  (is_function data))

(defun function? (data arity)
  (is_function data arity))

(defun integer? (data)
  (is_integer data))

(defun number? (data)
  (is_number data))

(defun record? (data record-tag)
  (is_record data record-tag))

(defun record? (data record-tag size)
  (is_record data record-tag size))

(defun reference? (data)
  (is_reference data))

;;;;;;;;
;;; math
(defun odd? (x)
  ;; initial experiments with implementations:
  ;; > (set odd1': (set odd1? (match-lambda ((x) (when (== 1 (rem x 2))) 'true) ((_) 'false)))
  ;; > (set odd2': (set odd2? (match-lambda ((x) (when (== 1 (band x 1))) 'true) ((_) 'false)))
  ;; > (set odd3': (set odd3? (lambda (x) (== 1 (rem x 2))))
  ;; > (set odd4': (set odd4? (lambda (x) (== 1 (band x 1))))
  ;; > (set odd5': (set odd5? (lambda (x) (if (== 1 (rem x 2)) 'true 'false)))
  ;; > (set odd6': (set odd6? (lambda (x) (if (== 1 (band x 1)) 'true 'false)))
  ;;
  ;; benchmarks were done with the following:
  ;; (set numbers (lambda (x) (lists:seq 1 x))
  ;; (set testit (lambda (func calls) (lists:map (lambda (x) (funcall func x)) (funcall numbers calls))))
  ;; (set raw-benchmark (lambda (func calls runs) (lists:map (lambda (_) (let (((tuple micro _) (timer:tc (lambda () (funcall testit func calls))))) (* micro 1.0e-6))) (lists:seq 1 runs))))
  ;; (set process-results (lambda (x) (let ((min (lists:min x)) (max (lists:max x)) (med (lists:nth (round (/ (length x) 2)) (lists:sort x))) (avg (/ (lists:foldl (lambda (y acc) (+ y acc)) 0 x) (length x)))) (list min max med avg))))
  ;; (set format-results (lambda (x) (io:format '"Range: ~p - ~p mics~nMedian: ~p mics~nAverage: ~p mics~n" (funcall process-results x))))
  ;; (set benchmark (lambda (func calls runs) (funcall format-results (funcall raw-benchmark func calls runs))))
  ;;
  ;; results:
  ;;
  ;; > (funcall benchmark odd1? 1000 1000)
  ;; Range: 0.010598 - 0.015139999999999999 mics
  ;; Median: 0.011491 mics
  ;; Average: 0.011770212 mics
  ;; ok
  ;; > (funcall benchmark odd2? 1000 1000)
  ;; Range: 0.010702 - 0.015528 mics
  ;; Median: 0.011482 mics
  ;; Average: 0.011741301000000003 mics
  ;; ok
  ;; > (funcall benchmark odd3? 1000 1000)
  ;; Range: 0.005876 - 0.009034 mics
  ;; Median: 0.006233 mics
  ;; Average: 0.0063988080000000015 mics
  ;; ok
  ;; > (funcall benchmark odd4? 1000 1000)
  ;; Range: 0.005928 - 0.009072 mics
  ;; Median: 0.006359999999999999 mics
  ;; Average: 0.006509416000000003 mics
  ;; ok
  ;; > (funcall benchmark odd5? 1000 1000)
  ;; Range: 0.006188 - 0.009649 mics
  ;; Median: 0.006738999999999999 mics
  ;; Average: 0.006882939999999999 mics
  ;; ok
  ;; > (funcall benchmark odd6? 1000 1000)
  ;; Range: 0.006091 - 0.009559 mics
  ;; Median: 0.006624 mics
  ;; Average: 0.006720350000000002 mics
  ;; ok
  ;; >
  ;;
  ;; the winner is... implementation #3!
  (== 1 (rem x 2)))

(defun even? (x)
  ;; > (set even1? (lambda (x) (== 0 (rem x 2))))
  ;; > (set even2? (lambda (x) (not (funcall odd3? x))))
  ;; > (funcall benchmark even1? 1000 1000)
  ;; Range: 0.006228 - 0.009419 mics
  ;; Median: 0.006723 mics
  ;; Average: 0.006849136999999996 mics
  ;; ok
  ;; > (funcall benchmark even2? 1000 1000)
  ;; Range: 0.011061 - 0.016139999999999998 mics
  ;; Median: 0.011913 mics
  ;; Average: 0.012135202000000015 mics
  ;; ok
  (== 0 (rem x 2)))

(defun fast-floor (num)
  "Sadly, this is named 'fast-floor' only because the Racket version was given
  that name. There is no good floor function in Erlang... so this should
  probably have been called 'slow-floor'."
  (let* ((trunc (trunc num))
         (check (- num trunc)))
    (cond
      ((< check 0) (- trunc 1))
      ((> check 0) trunc)
      ('true trunc))))

(defun round (number precision)
  "Round a floating point number to the given number of decimal places."
  (let ((p (math:pow 10 precision)))
    (/ (erlang:round (* number p)) p)))

(defun dot-product (a b)
  "This doesn't appear to be needed for this particular library, but it was fun
  to write, and is quite pretty, so it's staying ;-)"
  (lists:foldl #'+/2 0
    (lists:zipwith #'*/2 a b)))

(defun scale
  "Given a value and a range that value belongs to, calculate a new value based
  upon a new range.

  This is useful, for instance, when one wants to convert a decimal value
  between 0.0 and 1.0 to a value between 0 and 255."
  ((value (tuple lower-bound upper-bound)
          (tuple lower-bound-prime upper-bound-prime))
    (let* ((fraction (/
                   (+ (abs lower-bound) value)
                   (+ (abs lower-bound) upper-bound)))
           (new-range (- upper-bound-prime lower-bound-prime)))
      (+ (* fraction new-range) lower-bound-prime))))

(defun unit-scale (value current-frame)
  "Given a value and a range that value belongs to, calculate the value when
  scaled to the range 0.0 to 1.0."
  (scale value current-frame #(0.0 1.0)))

(defun color-scale (value current-frame)
  "Given a value and a range that value belongs to, calculate the value when
  scaled to the range 0 to 255."
  (erlang:round (scale value current-frame #(0.0 255.0))))

(defun factorial (n)
  "Tail-recursive factrial function."
  (factorial n 1))

(defun factorial
  ((0 acc) acc)
  ((n acc) (when (> n 0))
    (factorial (- n 1) (* n acc))))

(defun get-next-prime (x)
  "Get the next prime in ascending order."
  (flet ((f (y)
            (cond ((prime? y) y)
                  ('true (get-next-prime (+ x 1))))))
    (f (+ x 1))))

(defun prime? (x)
  "If a number consists of more than two factors, it is not a prime number."
  (let ((factors (lfe-utils:factors x)))
    (cond ((== 2 (length (lists:usort factors))) 'true)
          ('true 'false))))

(defun factors (n)
  "Tail-recursive prime factors function."
  (factors n 2 '()))

(defun factors
  ((1 _ acc) (++ acc '(1)))
  ((n _ acc) (when (=< n 0))
    #(error undefined))
  ((n k acc) (when (== 0 (rem n k)))
    (factors (div n k) k (cons k acc)))
  ((n k acc)
    (factors n (+ k 1) acc)))

(defun levenshtein-simple
  (('() str)
    (length str))
  ((str '())
    (length str))
  (((cons a str1) (cons b str2)) (when (== a b))
    (levenshtein-simple str1 str2))
  (((= (cons _ str1-tail) str1) (= (cons _ str2-tail) str2))
    (+ 1 (lists:min
          (list
           (levenshtein-simple str1 str2-tail)
           (levenshtein-simple str1-tail str2)
           (levenshtein-simple str1-tail str2-tail))))))

; The alternate implementations below were tested with different lengths of
; strings and from 1 to 10 to 100 to 1000 and to 10,000 iterations. Only
; very minor differences in performance were demonstrated. The implementation
; above provided the best overall performance, with the third implementation
; coming in second place, generally. The differences are so little as to
; not matter.
;
; (defun levenshtein-simple-2
;   (('() str)
;     (length str))
;   ((str '())
;     (length str))
;   (((cons a str1) (cons b str2)) (when (== a b))
;     (levenshtein-simple str1 str2))
;   ((str1 str2)
;     (+ 1 (lists:min
;           (list
;            (levenshtein-simple str1 (cdr str2))
;            (levenshtein-simple (cdr str1) str2)
;            (levenshtein-simple (cdr str1) (cdr str2)))))))
;
; (defun levenshtein-simple-3 (str1 str2)
;   (cond
;     ((== '() str1)
;      (length str1))
;     ((== '() str2)
;      (length str1))
;     ((== (car str1) (car str2))
;      (levenshtein-simple (cdr str1) (cdr str2)))
;     ('true
;       (+ 1 (lists:min
;              (list
;                (levenshtein-simple str1 (cdr str2))
;                (levenshtein-simple (cdr str1) str2)
;                (levenshtein-simple (cdr str1) (cdr str2))))))))

(defun levenshtein-distance (str1 str2)
  (let (((tuple distance _) (levenshtein-distance
                               str1 str2 (dict:new))))
    distance))

(defun levenshtein-distance
  (((= '() str1) str2 cache)
    (tuple (length str2)
           (dict:store (tuple str1 str2)
                       (length str2)
                       cache)))
  ((str1 (= '() str2) cache)
    (tuple (length str1)
           (dict:store (tuple str1 str2)
                       (length str1)
                       cache)))
  (((cons a str1) (cons b str2) cache) (when (== a b))
    (levenshtein-distance str1 str2 cache))
  (((= (cons _ str1-tail) str1) (= (cons _ str2-tail) str2) cache)
     (case (dict:is_key (tuple str1 str2) cache)
       ('true (tuple (dict:fetch (tuple str1 str2) cache) cache))
       ('false (let* (((tuple l1 c1) (levenshtein-distance str1 str2-tail cache))
                      ((tuple l2 c2) (levenshtein-distance str1-tail str2 c1))
                      ((tuple l3 c3) (levenshtein-distance str1-tail str2-tail c2))
                      (len (+ 1 (lists:min (list l1 l2 l3)))))
                 (tuple len (dict:store (tuple str1 str2) len c3)))))))

(defun levenshtein-sort (str1 str-list)
  (tuple str1
    (lists:sort
      (lists:map
        (lambda (str2)
          (list (levenshtein-distance str1 str2) str2))
        str-list))))

;;;;;;;;;
;;; files
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

;;;;;;;;;;;
;;; records
(defun record-info (record-list-data)
  "This function is intended as a quick-fix only until a complete solution has
  landed in LFE. There are two macros under development for inclusion in LFE
  that provide record metadata introspection:
    * record-info
    * record-list
  Currently, only record-list is working.

  As such, this function was created to take the return values from record-list
  and trasnform them to the tuple that matches the output value of the Erlang
  compile-time macro record_info.

  Once the record-info macro in LFE is working, this function will be
  deprecated."
  'ok)

;;;;;;;;;
;;; text
(defun wrap-text (text)
  (wrap-text text 78))

(defun wrap-text (text max-len)
  (string:join
    (make-wrapped-lines
      (string:tokens text " ") max-len)
    "\n"))

(defun make-wrapped-lines
  (((cons word rest) max-len)
    (let (((tuple _ len last-line lines) (assemble-lines
                                           max-len
                                           word
                                           rest)))
      (lists:reverse (cons last-line lines)))))

(defun assemble-lines (max-len word rest)
  (lists:foldl
    #'assemble-line/2
    (tuple max-len (length word) word '()) rest))

(defun assemble-line
  ((word (tuple max line-len line acc))
    (when (> (+ (length word) line-len) max))
    (tuple max (length word) word (cons line acc)))
  ((word (tuple max line-len line acc))
    (tuple max (+ line-len 1 (length word)) (++ line " " word) acc)))

;;;;;;;;
;;; misc
(defun uuid4 ()
  "Adapted from the implementation given here:
    https://github.com/afiskon/erlang-uuid-v4/blob/8c03a11524f6bccf984575877b533ef30a009175/src/uuid.erl
  "
  (let* (((binary
            (a (size 32))
            (b (size 16))
            (c (size 16))
            (d (size 16))
            (e (size 48))) (crypto:rand_bytes 16))
        (format-template '"~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b")
        (uuid-data (list a b (band c #x0fff) (band d #x3fff) (bor #x8000 e)))
        (string (io_lib:format format-template uuid-data)))
    (list_to_binary string)))


(defun uuid4
  "A wrapper for uuid4/0."
  ;; Example usage:
  ;;
  ;;   > (lfe-utils:uuid4 (tuple 'type 'binary))
  ;;   #B(50 101 51 53 49 99 48 97 45 50 100 100 52 45 52 54 56 55 45 50 ...)
  ;;
  ;;   > (lfe-utils:uuid4 (tuple 'type 'list))
  ;;   "65c0aff3-421e-40bf-0f64-3ac0d1e0b72d"
  ;;
  ;;   > (lfe-utils:uuid4 (tuple 'type 'atom))
  ;;  '
  (((tuple 'type 'binary))
    (uuid4))
  (((tuple 'type 'list))
    (binary_to_list (uuid4)))
  (((tuple 'type 'atom))
    (binary_to_atom (uuid4) 'latin1)))

(defun get-app-src-version (filename)
  (let* (((tuple 'ok (list app)) (file:consult filename)))
    (proplists:get_value 'vsn (element 3 app))))

(defun get-lfe-version ()
  (get-app-src-version '"deps/lfe/src/lfe.app.src"))

(defun get-version ()
  `(#(erlang ,(erlang:system_info 'otp_release))
    #(emulator ,(erlang:system_info 'version))
    #(driver-version ,(erlang:system_info 'driver_version))
    #(lfe ,(get-lfe-version))))

(defun get-lfe-utils-version ()
  (get-app-src-version "src/lfe-utils.app.src"))

(defun get-versions ()
  (++ (get-version)
      `(#(lfe-utils ,(get-lfe-utils-version)))))
