(defmodule lutil-rec-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defrecord test-record
  field-a
  field-b
  field-c
  last-field)

(deftest fields
  (is-equal '(field-a field-b field-c last-field)
           (lutil-rec:fields test-record)))

(deftest fields-quoted-input
  (is-equal '(field-a field-b field-c last-field)
            (lutil-rec:fields 'test-record)))
