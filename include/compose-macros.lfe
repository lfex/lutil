;; Threads the sexp through the sexps. Inserts x as the second item in
;; the first sexp, making a list of it if it is not a list already. If
;; there are more sexps, inserts the first sexp as the second item in
;; second sexp, etc.
;;
;; Copied from Tim Dysinger's lfesl repo here:
;;   https://github.com/lfex/lfesl/blob/master/include/thread.lfe
;;
;; Example usage:
;; > (set o '(#(a 1) #(b 2) #(c 3)))
;; (#(a 1) #(b 2) #(c 3))
;; > (-> o
;;       (++ `(#(d 4)))
;;       (++ `(#(e 5)))
;;       (++ `(#(f 6))))
;; (#(a 1) #(b 2) #(c 3) #(d 4) #(e 5) #(f 6))
;;
;; Note that usage of this macro with this examples results in each successive
;; value being APPENDED to the input list.
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
;;
;; Example usage:
;; > (set o '(#(a 1) #(b 2) #(c 3)))
;; (#(a 1) #(b 2) #(c 3))
;; > (-> o
;;       (++ `(#(d 4)))
;;       (++ `(#(e 5)))
;;       (++ `(#(f 6))))
;; (#(f 6) #(e 5) #(d 4) #(a 1) #(b 2) #(c 3))
;;
;; Note that usage of this macro with this examples results in each successive
;; value being PREPENDED to the input list.
(defmacro ->>
  ((x) x)
  ((x sexp) (when (is_list sexp))
   `(,(car sexp) ,@(cdr sexp) ,x))
  ((x sexp)
   `(list ,sexp ,x))
  ((x sexp . sexps)
   `(->> (->> ,x ,sexp) ,@sexps)))
