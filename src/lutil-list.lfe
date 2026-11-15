(defmodule lutil-list
  "List conversion, chunking, positional partitioning, and list-of-lists zipping
  helpers."
  (export all))

(defun ->dict (list-data)
  "Partition an even-length alternating key/value list and build a dictionary
  from the pairs; keys are expected to be atoms, while values may be any term."
  (let (((tuple keys values) (partition list-data)))
    (dict:from_list
     (lists:zip keys values))))

(defun ->tuple (list-data)
  "Quote each item as a literal form, evaluate a tuple form, and return the
  resulting tuple."
  (let ((quoted (lists:map (lambda (x) `',x) list-data)))
    (eval `(tuple ,@quoted))))

(defun chunks (list-data count)
  "Group `ListData` into chunks using the default `by-parts` mode."
  (chunks list-data count #(by-parts)))

(defun chunks
  "Group `ListData` by one-element tuple mode `#(by-parts)` or `#(by-length)`.
  Fixed `Len` must be positive; parts mode requires positive `Count` and a
  positive rounded chunk length. Other modes fail to match."
  ((list-data len #(by-length))
   (chunks-by-length list-data len))
  ((list-data count #(by-parts))
   (chunks-by-parts list-data count)))

(defun chunks-by-length (list-data len)
  "Group items into fixed chunks of positive length `Len`; a short final chunk
  is padded with `undefined` values."
  (let*((leader-len (case (rem (length list-data) len)
                      (0 0)
                      (n (- len n))))
        (leader (lists:duplicate leader-len 'undefined)))
    (chunks-by-length (++ leader (lists:reverse list-data)) '() 0 len)))

;; The following is adapted from an Erlang answer given by user chops on
;; StackOverflow, here:
;; * https://stackoverflow.com/questions/12534898/splitting-a-list-in-equal-sized-chunks-in-erlang
(defun chunks-by-length
  "Worker for fixed-length chunking; it consumes the reversed, padded input
  prepared by `chunks-by-length/2`, using `Acc`, `Pos`, and `Max` to build and
  return ordered chunks."
  (('() acc _ _)
   acc)
  ((`(,h . ,t) acc pos max) (when (== pos max))
   (chunks-by-length t (cons (list h) acc) 1 max))
  ((`(,h . ,t) `(,h-acc . ,t-acc) pos max)
   (chunks-by-length t (cons (cons h h-acc) t-acc) (+ 1 pos) max))
  ((`(,h . ,t) '() pos max)
   (chunks-by-length t (list (list h)) (+ 1 pos) max)))

(defun chunks-by-parts (list-data count)
  "Choose a chunk length by rounding `length(ListData) / Count`, then use
  fixed-length chunking. `Count` and the rounded length must be positive.
  Rounding means the result is not guaranteed to contain exactly `Count`
  chunks."
  (let ((length (round (/ (length list-data) count))))
    (chunks-by-length list-data length)))
  
(defun partition (list-data)
  "Split an even-length alternating key/value list into `{Keys, Values}`,
  preserving input order; values may be atoms or other terms."
  (partition list-data '() '()))

(defun partition
  "Continue positional partitioning with reversed key and value accumulators.
  When the input is exhausted, reverse both accumulators; the input tail must
  contain complete pairs."
  (('() keys values)
   (tuple (lists:reverse keys) (lists:reverse values)))
  (((cons key (cons value rest)) keys values)
   (partition rest (cons key keys) (cons value values))))

;; The zip/2, zip/3, and zip/4 implementations are for kicks; probably *much*
;; better to use Erlang's lists:zip/2 and lists:zip3/3. There's no zip/4, so
;; hey -- have fun. The zip/1 function is what's most interesting: it can be
;; used for lists of lists of any length -- no need for special-case arities.
;; In particular, the zip/1 function was needed for the lmatrix library.
;;
;; Note that, unlike the Erlang zip functions, these functions *do not* return
;; a list of tuples; they return a list of lists.
(defun zip (lists)
  "Transpose rows from a list of lists. Stop at the first empty row; the empty
  outer list returns an empty list."
  (zip-any lists '()))

(defun zip (list-1 list-2)
  "Zip two lists into a list of two-element lists, stopping when either input is
  empty."
  (zip-2 list-1 list-2 '()))

(defun zip (list-1 list-2 list-3)
  "Zip three lists into a list of three-element lists, stopping when any input
  is empty."
  (zip-3 list-1 list-2 list-3 '()))

(defun zip (list-1 list-2 list-3 list-4)
  "Zip four lists into a list of four-element lists, stopping when any input is
  empty."
  (zip-4 list-1 list-2 list-3 list-4 '()))

(defun zip-2
  "Worker for two-list zipping; append each row to `Acc` and return it when
  either input is exhausted."
  ((_ '() acc)
    acc)
  (('() _ acc)
    acc)
  (((cons h1 t1) (cons h2 t2) acc)
    (zip-2 t1 t2 (++ acc `((,h1 ,h2))))))

(defun zip-3
  "Worker for three-list zipping; append each row to `Acc` and return it when
  any input is exhausted."
  ((_ _ '() acc)
    acc)
  ((_ '() _ acc)
    acc)
  (('() _ _ acc)
    acc)
  (((cons h1 t1) (cons h2 t2) (cons h3 t3) acc)
    (zip-3 t1 t2 t3 (++ acc `((,h1 ,h2 ,h3))))))

(defun zip-4 (list-1 list-2 list-3 list-4 acc)
  "Worker for four-list zipping; append each row to `Acc` and return it when any
  input is empty."
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

(defun zip-any
  "Worker for n-list zipping; transpose one head from every row per step and
  stop when the outer list is empty or any row is empty."
  (('() acc)
   acc)
  ((lists acc)
   (cond
     ((lists:any
         (lambda (x) (== x '()))
         lists)
      acc)
     ('true
       (let ((heads (lists:map #'cl:car/1 lists))
             (tails (lists:map #'cl:cdr/1 lists)))
         (zip-any tails (++ acc (list heads))))))))
