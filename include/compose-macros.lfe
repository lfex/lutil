;; Threads the sexp through the sexps. Inserts x as the second item in
;; the first sexp, making a list of it if it is not a list already. If
;; there are more sexps, inserts the first sexp as the second item in
;; second sexp, etc.
;;
;; Copied from Tim Dysinger's lfesl repo here:
;;   https://github.com/lfex/lfesl/blob/master/include/thread.lfe
(defmacro ->
  ((x) x)
  ((x sexp) (when (is_list sexp))
   `(,(car sexp) ,x ,@(cdr sexp)))
  ((x sexp)
   `(list ,sexp ,x))
  ((x sexp . sexps)
   `(-> (-> ,x ,sexp) ,@sexps)))

;; Threads the sexp through the sexps. Inserts x as the last item in
;; the first sexp, making a list of it if it is not a list already. If
;; there are more sexps, inserts the first sexp as the last item in
;; second sexp, etc.
;;
;; Copied from Tim Dysinger's lfesl repo here:
;;   https://github.com/lfex/lfesl/blob/master/include/thread.lfe
(defmacro ->>
  ((x) x)
  ((x sexp) (when (is_list sexp))
   `(,(car sexp) ,@(cdr sexp) ,x))
  ((x sexp)
   `(list ,sexp ,x))
  ((x sexp . sexps)
   `(->> (->> ,x ,sexp) ,@sexps)))
