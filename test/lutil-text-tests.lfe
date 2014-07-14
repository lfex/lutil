(defmodule lutil-text-tests
  (behaviour lunit-unit)
  (export all)
  (import
    (from lutil
      (wrap-text 2))))

(include-lib "deps/lunit/include/lunit-macros.lfe")

(deftest wrap-text
  (is-equal
    "01234\n56789\n01234\n56789\n01234\n56789"
    (wrap-text "01234 56789 01234 56789 01234 56789" 5))
  (is-equal
    "01234 56789\n01234 56789\n01234 56789"
    (wrap-text "01234 56789 01234 56789 01234 56789" 10))
  (is-equal
    "01234 56789 01234\n56789 01234 56789"
    (wrap-text "01234 56789 01234 56789 01234 56789" 20))
  (is-equal
    "01234 56789 01234 56789 01234 56789"
    (wrap-text "01234 56789 01234 56789 01234 56789" 40)))
