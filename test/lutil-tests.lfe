(defmodule lutil-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "deps/ltest/include/ltest-macros.lfe")

(deftest uuid4
  (is-equal 36 (byte_size (lutil:uuid4)))
  (is-equal 288 (bit_size (lutil:uuid4)))
  (is (is_binary (lutil:uuid4)))
  (is-equal 36 (byte_size (lutil:uuid4 (tuple 'type 'binary))))
  (is-equal 288 (bit_size (lutil:uuid4 (tuple 'type 'binary))))
  (is (is_binary (lutil:uuid4 (tuple 'type 'binary))))
  (is-not (is_binary (lutil:uuid4 (tuple 'type 'list))))
  (is-equal 36 (length (lutil:uuid4 (tuple 'type 'list))))
  (is (is_list (lutil:uuid4 (tuple 'type 'list))))
  (is-not (is_list (lutil:uuid4 (tuple 'type 'binary))))
  (is (is_atom (lutil:uuid4 (tuple 'type 'atom))))
  (is-not (is_atom (lutil:uuid4 (tuple 'type 'list)))))

(deftest check
  (is (lutil:check 'true))
  (is-not (lutil:check 'false)))