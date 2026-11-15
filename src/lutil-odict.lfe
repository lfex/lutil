(defmodule lutil-odict
  "Operations on Erlang ordered dictionaries."
  (export
   (merge 2)))

(defun merge (options1 options2)
  "Merge two sorted ordered dictionaries. For keys present in both inputs, the
  value from `Options2` wins."
  (orddict:merge
    #'second-wins/3
    options1
    options2))

(defun second-wins (key val1 val2)
  "Resolver used by `merge/2`; retain `Val2` when a key occurs in both
  dictionaries."
  val2)
