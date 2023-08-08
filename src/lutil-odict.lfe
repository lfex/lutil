(defmodule lutil-odict
  (export
   (merge 2)))

(defun merge (options1 options2)
  "Merge two orddicts.

  That which is added latter will over-write what was previous. As such,
  options2 has presedence over options1."
  (orddict:merge
    #'second-wins/3
    options1
    options2))

(defun second-wins (key val1 val2)
  val2)
