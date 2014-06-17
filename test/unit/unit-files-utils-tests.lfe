(defmodule unit-files-utils-tests
  (export all)
  (import
    (from lfe-utils
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
    (from lfeunit-util
      (check-failed-is 2)
      (check-wrong-is-exception 2))))

(include-lib "deps/lfeunit/include/lfeunit-macros.lfe")

(deftest is-home-dir?
  (is-not (lfe-utils:is-home-dir? "~"))
  (is-not (lfe-utils:is-home-dir? "/"))
  (is-not (lfe-utils:is-home-dir? "~home/"))
  (is-not (lfe-utils:is-home-dir? "/home"))
  (is (lfe-utils:is-home-dir? "~/"))
  (is (lfe-utils:is-home-dir? "~/user"))
  (is (lfe-utils:is-home-dir? "~/user/more/path")))

(deftest expand-home-dir
  (is-equal "/usr/local/bin"
            (lfe-utils:expand-home-dir "/usr/local/bin"))
  (is-equal "/home/oubiwann"
            (lfe-utils:expand-home-dir "/home/oubiwann"))
  (let* ((tilde-dir "~/my-data")
         (expanded (lfe-utils:expand-home-dir tilde-dir)))
    (is (lfe-utils:is-home-dir? tilde-dir))
    (is-not (lfe-utils:is-home-dir? expanded))))

(defun get-test-subdirs ()
  '("../exemplar/deps/.DS_Store"
    "../exemplar/deps/.eunit"
    "../exemplar/deps/lfe"
    "../exemplar/deps/lfe-sample-rebar-plugin"
    "../exemplar/deps/lfe-utils"
    "../exemplar/deps/lfeunit"
    "../exemplar/deps/rebar"
    "../lfe-reveal-js/deps/exemplar"
    "../lfe-reveal-js/deps/ibrowse"
    "../lfe-reveal-js/deps/lfe"
    "../lfe-reveal-js/deps/lfe-utils"
    "../lfe-reveal-js/deps/lfeunit"
    "../lfe-reveal-js/deps/yaws"
    "deps/.DS_Store"
    "deps/lfe"
    "deps/lfeunit"))

(defun expected-checked-subdirs ()
  '(false
    false
    "../exemplar/deps/lfe"
    "../exemplar/deps/lfe-sample-rebar-plugin"
    "../exemplar/deps/lfe-utils"
    "../exemplar/deps/lfeunit"
    "../exemplar/deps/rebar"
    "../lfe-reveal-js/deps/exemplar"
    "../lfe-reveal-js/deps/ibrowse"
    "../lfe-reveal-js/deps/lfe"
    "../lfe-reveal-js/deps/lfe-utils"
    "../lfe-reveal-js/deps/lfeunit"
    "../lfe-reveal-js/deps/yaws"
    false
    "deps/lfe"
    "deps/lfeunit"))

(defun expected-filtered-subdirs ()
  '("../exemplar/deps/lfe"
    "../exemplar/deps/lfe-sample-rebar-plugin"
    "../exemplar/deps/lfe-utils"
    "../exemplar/deps/lfeunit"
    "../exemplar/deps/rebar"
    "../lfe-reveal-js/deps/exemplar"
    "../lfe-reveal-js/deps/ibrowse"
    "../lfe-reveal-js/deps/lfe"
    "../lfe-reveal-js/deps/lfe-utils"
    "../lfe-reveal-js/deps/lfeunit"
    "../lfe-reveal-js/deps/yaws"
    "deps/lfe"
    "deps/lfeunit"))

(deftest check-deps
  (is-equal
    (expected-checked-subdirs)
    (lfe-utils:check-deps (get-test-subdirs))))

(deftest filtered-deps
  (is-equal
    (expected-filtered-subdirs)
    (lfe-utils:filter-deps (get-test-subdirs))))
