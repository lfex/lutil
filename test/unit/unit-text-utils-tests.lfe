(defmodule unit-text-utils-tests
  (export all)
  (import
    (from lfe-utils
      (wrap-text 2))
    (from lfeunit-util
      (check-failed-is 2)
      (check-wrong-is-exception 2))))

(include-lib "deps/lfeunit/include/lfeunit-macros.lfe")

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
