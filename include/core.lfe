;; List sequence wrapper functions
(defun seq (end)
  (lists:seq 1 end))

(defun seq (start end)
  (lists:seq start end))

(defun seq (start end step)
  (lists:seq start end step))

;; Infinite series functions
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
(defun range ()
  (range 1 1))

(defun range (start)
  (range start 1))

(defun range (start step)
  (next #'+/2 start step))

;; Take functions
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

(defun loaded ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'loaded)
