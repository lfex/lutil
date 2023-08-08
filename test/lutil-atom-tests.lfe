(defmodule lutil-atom-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest atom-cat-2-arity
  (is-equal 'ab (lutil-atom:cat 'a 'b)))

(deftest atom-cat-list-of-atoms
  (is-equal 'ab (lutil-atom:cat '(a b)))
  (is-equal 'abc (lutil-atom:cat '(a b c)))
  (is-equal 'abcdefg (lutil-atom:cat '(a b c d e f g))))
