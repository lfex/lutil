;;;; Various macros and functions that could be considered "core" to LFE,
;;;; should the occasion arise to include them in an LFE standard library.
;;;;

;; List sequence wrapper functions
;;
;; Usage:
;;
;; > (seq 10)
;; (1 2 3 4 5 6 7 8 9 10)
;;
(defun seq (end)
  (lists:seq 1 end))

(defun seq (start end)
  (lists:seq start end))

(defun seq (start end step)
  (lists:seq start end step))

;; Infinite series functions
;;
;; The following are identical:
;; > (take 21 (range))
;; (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)
;; > (take 21 (next #'+/2 1 1))
;; (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)
;; > (take 21 (next (lambda (x y) (+ x y)) 1 1))
;; (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)
;;
;; More usage:
;;
;; > (take 10 (next (lambda (x y) (* 3 (+ x y))) 1 1))
;; (1 6 21 66 201 606 1821 5466 16401 49206)
;; > (take 17 (next (lambda (x _) (* 2 x)) 1 1))
;; (1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536)
;; > (take 7 (next (lambda (x _) (math:pow (+ x 1) 2)) 1 1))
;; (1 4.0 25.0 676.0 458329.0 210066388900.0 4.4127887745906175e22)
;;
(defun next (func)
  (next func 1 1))

(defun next (func start)
  (next start 1))

(defun next (func start step)
  (lambda ()
    (cons start (next func
                      (funcall func start step)
                      step))))

;; Range functions
;;
;; Usage:
;;
;; > (range)
;; #Fun<lfe_eval.23.86468545>
;; > (funcall (range))
;; (1 . #Fun<lfe_eval.23.86468545>)
;; > (funcall (range 100))
;; (100 . #Fun<lfe_eval.23.86468545>)
;;
;; Some more:
;;
;; > (funcall (range))
;; (1 . #Fun<lfe_eval.23.86468545>)
;; > (funcall (cdr (funcall (range))))
;; (2 . #Fun<lfe_eval.23.86468545>)
;; > (funcall (cdr (funcall (cdr (funcall (range))))))
;; (3 . #Fun<lfe_eval.23.86468545>)
;; > (funcall (cdr (funcall (cdr (funcall (cdr (funcall (range))))))))
;; (4 . #Fun<lfe_eval.23.86468545>)
;;
(defun range ()
  (range 1 1))

(defun range (start)
  (range start 1))

(defun range (start step)
  (next #'+/2 start step))

;; Take functions
;;
;; Usage:
;;
;; > (take 4 (range))
;; (1 2 3 4 5)
;; > (take 5 '(1 2 3 4 5 6 7 8 9 10 11 12))
;; (1 2 3 4 5)
;; > (take 'all '(1 2 3 4 5 6 7 8 9 10 11 12))
;; (1 2 3 4 5 6 7 8 9 10 11 12)
;;
(defun take
  (('all data) (when (is_list data))
    data)
  ((x data) (when (is_list data))
    (lists:sublist data x))
  ((x func) (when (is_function func))
    (take x '() (funcall func))))

(defun take
  ((x acc (cons _ func)) (when (>= (length acc) x))
    acc)
  ((x acc (cons item func)) (when (< (length acc) x))
    (take x
          (++ acc `(,item))
          (funcall func))))

;; Partial
;;
;; Usage:
;;
;; > (set f (partial #'+/2 1))
;; #Fun<lfe_eval.28.86468545>
;; > (funcall f 2)
;; 3
;; > (set f (partial #'+/3 1))
;; #Fun<lfe_eval.28.86468545>
;; > (funcall f '(2 3))
;; 6
;; > (set f (partial #'+/3 '(2 3)))
;; #Fun<lfe_eval.28.86468545>
;; > (funcall f 4)
;; 9
;; > (set f (partial #'+/4 '(2 3)))
;; #Fun<lfe_eval.28.86468545>
;; > (funcall f '(4 5))
;; 14
;;
(defun partial
  "The partial function is arity 2 where the first parameter must be a
  function and the second parameter may either be a single item or a list of
  items.

  When funcall is called against the result of the partial call, a second
  parameter is applied to the partial function. This parameter too may be
  either a single item or a list of items."
  ((func args-1) (when (is_list args-1))
    (match-lambda
      ((args-2) (when (is_list args-2))
        (apply func (++ args-1 args-2)))
      ((arg-2)
        (apply func (++ args-1 `(,arg-2))))))
  ((func arg-1)
    (match-lambda
      ((args-2) (when (is_list args-2))
        (apply func (++ `(,arg-1) args-2)))
      ((arg-2)
        (funcall func arg-1 arg-2)))))

;; Interleave
;;
;; Usage:
;;
;; > (set l1 '(a b c d e f g))
;; (a b c d e f g)
;; > (set l2 '(1 2 3 4 5 6 7))
;; (1 2 3 4 5 6 7)
;; > (interleave l1 l2)
;; (a 1 b 2 c 3 d 4 e 5 f 6 g 7)
(defun interleave (list-1 list-2)
  (lists:flatten
    (lists:map
      #'tuple_to_list/1
      (lists:zip list-1 list-2))))

;; Get-In
;;
;; This macro is inspired by the Clojure function 'get-in'. Unlike the
;; Clojure function, however, the LFE version handles both lists as well
;; as proplists, dicts, orddicts, and maps.
;;
;; List-based usage:
;;
;; Given the following data structure assigned to the variable 'data':
;;
;; '((1)
;;   (1 2 3)
;;   (1 2 (3 4 (5 6 (7 8 9)))))
;;
;; > (include-lib "lutil/include/core.lfe")
;; loaded-core
;;
;; > (get-in data 1 1)
;; 1
;; > (get-in data 2 3)
;; 3
;; > (get-in data 3 3 3 3 3)
;; 9
;; > (get-in data 4)
;; undefined
;; > (get-in data 4 3 3 3)
;; undefined
;;
;; Key-value-based usage:
;;
;; Given the following data structure assigned to the variable 'data':
;;
;; '(#(key-1 val-1)
;;   #(key-2 val-2)
;;   #(key-3 (#(key-4 val-4)
;;            #(key-5 val-5)
;;            #(key-6 (#(key-7 val-7)
;;                     #(key-8 val-8))))))
;;
;; > (include-lib "lutil/include/core.lfe")
;; loaded-core
;; > (get-in data 'key-1)
;; val-1
;; > (get-in data 'key-3 'key-5)
;; val-5
;; > (get-in data 'key-3 'key-6 'key-8)
;; val-8
;; > (get-in data 'key-19)
;; undefined
;; > (get-in data 'key-3 'key-6 'key-89)
;; undefined
;; > (get-in data 'key-3 'key-6 'key-89 'key-100)
;; undefined
(defmacro get-in args
  (let ((data (car args))
        (keys (cdr args)))
    `(apply 'lutil-type 'get-in (list ,data (list ,@keys)))))

(defun loaded-core ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
