(defmodule lutil-file-tests
  (behaviour lunit-unit)
  (export all)
  (import
    (from lutil
      (add-tuples 1) (add-tuples 2)
      (color-scale 2)
      (dot-product 2)
      (fast-floor 1)
      (round 2)
      (scale 3)
      (unit-scale 2)
      (uuid4 0) (uuid4 1)
      (partition-list 1)
      (pair-dict 1))
    (from lunit
      (check-failed-is 2)
      (check-wrong-is-exception 2))))

(include-lib "deps/lunit/include/lunit-macros.lfe")

(deftest is-home-dir?
  (is-not (lutil:is-home-dir? "~"))
  (is-not (lutil:is-home-dir? "/"))
  (is-not (lutil:is-home-dir? "~home/"))
  (is-not (lutil:is-home-dir? "/home"))
  (is (lutil:is-home-dir? "~/"))
  (is (lutil:is-home-dir? "~/user"))
  (is (lutil:is-home-dir? "~/user/more/path")))

(deftest expand-home-dir
  (is-equal "/usr/local/bin"
            (lutil:expand-home-dir "/usr/local/bin"))
  (is-equal "/home/oubiwann"
            (lutil:expand-home-dir "/home/oubiwann"))
  (let* ((tilde-dir "~/my-data")
         (expanded (lutil:expand-home-dir tilde-dir)))
    (is (lutil:is-home-dir? tilde-dir))
    (is-not (lutil:is-home-dir? expanded))))

(defun get-test-subdirs ()
  '("../exemplar/deps/.DS_Store"
    "../exemplar/deps/.eunit"
    "../exemplar/deps/lfe"
    "../exemplar/deps/lfe-sample-rebar-plugin"
    "../exemplar/deps/lutil"
    "../exemplar/deps/lunit"
    "../exemplar/deps/rebar"
    "../lfe-reveal-js/deps/exemplar"
    "../lfe-reveal-js/deps/ibrowse"
    "../lfe-reveal-js/deps/lfe"
    "../lfe-reveal-js/deps/lutil"
    "../lfe-reveal-js/deps/lunit"
    "../lfe-reveal-js/deps/yaws"
    "deps/.DS_Store"
    "deps/lfe"
    "deps/lunit"))

(defun expected-checked-subdirs ()
  '(false
    false
    "../exemplar/deps/lfe"
    "../exemplar/deps/lfe-sample-rebar-plugin"
    "../exemplar/deps/lutil"
    "../exemplar/deps/lunit"
    "../exemplar/deps/rebar"
    "../lfe-reveal-js/deps/exemplar"
    "../lfe-reveal-js/deps/ibrowse"
    "../lfe-reveal-js/deps/lfe"
    "../lfe-reveal-js/deps/lutil"
    "../lfe-reveal-js/deps/lunit"
    "../lfe-reveal-js/deps/yaws"
    false
    "deps/lfe"
    "deps/lunit"))

(defun expected-filtered-subdirs ()
  '("../exemplar/deps/lfe"
    "../exemplar/deps/lfe-sample-rebar-plugin"
    "../exemplar/deps/lutil"
    "../exemplar/deps/lunit"
    "../exemplar/deps/rebar"
    "../lfe-reveal-js/deps/exemplar"
    "../lfe-reveal-js/deps/ibrowse"
    "../lfe-reveal-js/deps/lfe"
    "../lfe-reveal-js/deps/lutil"
    "../lfe-reveal-js/deps/lunit"
    "../lfe-reveal-js/deps/yaws"
    "deps/lfe"
    "deps/lunit"))

(deftest check-deps
  (is-equal
    (expected-checked-subdirs)
    (lutil:check-deps (get-test-subdirs))))

(deftest filtered-deps
  (is-equal
    (expected-filtered-subdirs)
    (lutil:filter-deps (get-test-subdirs))))
