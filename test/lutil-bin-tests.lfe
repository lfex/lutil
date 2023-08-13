(defmodule lutil-bin-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest trim-trailing-ws-empty
  (is-equal #""
            (lutil-bin:trim-trailing-ws #""))
  (is-equal #""
            (lutil-bin:trim-trailing-ws #" "))
  (is-equal #""
            (lutil-bin:trim-trailing-ws #"\n"))
  (is-equal #""
            (lutil-bin:trim-trailing-ws #"\r"))
  (is-equal #""
            (lutil-bin:trim-trailing-ws #"\r\n"))
  (is-equal #""
            (lutil-bin:trim-trailing-ws #"\n\n\n\n\n\n\n"))
  (is-equal #""
            (lutil-bin:trim-trailing-ws #"\r\r\r\r\n\n\n\n       ")))

(deftest trim-trailing-ws
  (is-equal #"foo"
            (lutil-bin:trim-trailing-ws #"foo"))
  (is-equal #"foo"
            (lutil-bin:trim-trailing-ws #"foo "))
  (is-equal #"foo"
            (lutil-bin:trim-trailing-ws #"foo\n"))
  (is-equal #"foo"
            (lutil-bin:trim-trailing-ws #"foo\r"))
  (is-equal #"foo"
            (lutil-bin:trim-trailing-ws #"foo\r\n"))
  (is-equal #"foo"
            (lutil-bin:trim-trailing-ws #"foo\n\n\n\n\n\n\n"))
  (is-equal #"foo"
            (lutil-bin:trim-trailing-ws #"foo\r\r\r\r\n\n\n\n       ")))

(deftest trim-trailing-default
  (is-equal #"hej"
            (lutil-bin:trim-trailing #"hej\n\n\n\n"))
  (is-equal #"hej\r\r\r"
            (lutil-bin:trim-trailing #"hej\r\r\r"))
  (is-equal #"hej   "
            (lutil-bin:trim-trailing #"hej   ")))

(deftest trim-trailing
  (is-equal #"hej "
            (lutil-bin:trim-trailing #"hej   " #"  "))
  (is-equal #"bob's your"
            (lutil-bin:trim-trailing #"bob's your uncle" #" uncle")))
