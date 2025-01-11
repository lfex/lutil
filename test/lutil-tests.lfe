(defmodule lutil-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest check
  (is (lutil:check 'true))
  (is-not (lutil:check 'false)))
