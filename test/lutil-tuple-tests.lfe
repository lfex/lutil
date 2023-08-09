(defmodule lutil-tuple-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest cat
  (let ((data1 (list (tuple 1 2 3) (tuple 2 3 4)))
        (data2 (list (tuple 2 4) (tuple 6 8) (tuple 10 12))))
    (is-equal #(1 2 3 2 3 4) (lutil-tuple:cat data1))
    (is-equal #(2 4 6 8 10 12) (lutil-tuple:cat data2))
    (is-equal #(1 2 3 4) (lutil-tuple:cat (tuple 1 2) (tuple 3 4)))))
