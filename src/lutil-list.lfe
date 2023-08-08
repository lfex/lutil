(defmodule lutil-list
  (export all))

(defun ->tuple (list-data)
  (let ((quoted (lists:map (lambda (x) `',x) list-data)))
    (eval `(tuple ,@quoted))))

(defun get-in (data indices)
  (lists:foldl #'lists:nth/2 data indices))

(defun partition (list-data)
  "This function takes lists of even length with an implicit key (atom) value
  pairing and generates a list of two lists: one with all the keys, and the
  other with all the values."
  (lists:partition #'is_atom/1 list-data))

;; The zip/2, zip/3, and zip/4 implementations are for kicks; probably *much*
;; better to use Erlang's lists:zip/2 and lists:zip3/3. There's no zip/4, so
;; hey -- have fun. The zip/1 function is what's most interesting: it can be
;; used for lists of lists of any length -- no need for special-case arities.
;; In particular, the zip/1 function was needed for the lmatrix library.
;;
;; Note that, unlike the Erlang zip functions, these functions *do not* return
;; a list of tuples; they return a list of lists.
(defun zip (lists)
  (zip-any lists '()))

(defun zip (list-1 list-2)
  (zip-2 list-1 list-2 '()))

(defun zip (list-1 list-2 list-3)
  (zip-3 list-1 list-2 list-3 '()))

(defun zip (list-1 list-2 list-3 list-4)
  (zip-4 list-1 list-2 list-3 list-4 '()))

(defun zip-2
  ((_ '() acc)
    acc)
  (('() _ acc)
    acc)
  (((cons h1 t1) (cons h2 t2) acc)
    (zip-2 t1 t2 (++ acc `((,h1 ,h2))))))

(defun zip-3
  ((_ _ '() acc)
    acc)
  ((_ '() _ acc)
    acc)
  (('() _ _ acc)
    acc)
  (((cons h1 t1) (cons h2 t2) (cons h3 t3) acc)
    (zip-3 t1 t2 t3 (++ acc `((,h1 ,h2 ,h3))))))

(defun zip-4 (list-1 list-2 list-3 list-4 acc)
  (cond
    ((lists:any
        (lambda (x) (== x '()))
        (list list-1 list-2 list-3 list-4))
     acc)
    ('true
      (let (((list (cons h1 t1) (cons h2 t2) (cons h3 t3) (cons h4 t4))
             (list list-1 list-2 list-3 list-4)))
        (zip-4 t1 t2 t3 t4 (++ acc `((,h1 ,h2 ,h3 ,h4))))))))

;; An alternate implementation:
;; (defun zip-4 (list-1 list-2 list-3 list-4 acc)
;;   ((_ _ _ '() acc)
;;     acc)
;;   ((_ _ '() _ acc)
;;     acc)
;;   ((_ '() _ _ acc)
;;     acc)
;;     (('() _ _ _ acc)
;;     acc)
;;   (((cons h1 t1) (cons h2 t2) (cons h3 t3) (cons h4 t4) acc)
;;     (zip-4 t1 t2 t3 t4 (++ acc `((,h1 ,h2 ,h3 ,h4))))))

(defun zip-any (lists acc)
  (cond
    ((lists:any
        (lambda (x) (== x '()))
        lists)
     acc)
    ('true
      (let ((heads (lists:map #'cl:car/1 lists))
            (tails (lists:map #'cl:cdr/1 lists)))
        (zip-any tails (++ acc `(,heads)))))))
