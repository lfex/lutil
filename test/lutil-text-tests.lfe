(defmodule lutil-text-tests
  (behaviour lunit-unit)
  (export all))

(include-lib "deps/lunit/include/lunit-macros.lfe")

(deftest strip
  (is-equal "data" (lutil-text:strip "data\n"))
  (is-equal "data" (lutil-text:strip "data\n\n"))
  (is-equal "data" (lutil-text:strip "data   "))
  (is-equal "data" (lutil-text:strip "data   \n   "))
  (is-equal "data" (lutil-text:strip "data   \n   \n")))

(deftest capitalized?
  (is (lutil-text:capitalized? "Apple"))
  (is-not (lutil-text:capitalized? "apple"))
  (is (lutil-text:capitalized? "APPLE"))
  (is-not (lutil-text:capitalized? "aPPLE")))

(deftest wrap-text
  (is-equal
    "01234\n56789\n01234\n56789\n01234\n56789"
    (lutil-text:wrap-text "01234 56789 01234 56789 01234 56789" 5))
  (is-equal
    "01234 56789\n01234 56789\n01234 56789"
    (lutil-text:wrap-text "01234 56789 01234 56789 01234 56789" 10))
  (is-equal
    "01234 56789 01234\n56789 01234 56789"
    (lutil-text:wrap-text "01234 56789 01234 56789 01234 56789" 20))
  (is-equal
    "01234 56789 01234 56789 01234 56789"
    (lutil-text:wrap-text "01234 56789 01234 56789 01234 56789" 40)))
