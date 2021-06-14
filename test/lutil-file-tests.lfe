(defmodule lutil-file-tests
  (behaviour ltest-system)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest is-home-dir?
  (is-not (lutil-file:is-home-dir? "~"))
  (is-not (lutil-file:is-home-dir? "/"))
  (is-not (lutil-file:is-home-dir? "~home/"))
  (is-not (lutil-file:is-home-dir? "/home"))
  (is (lutil-file:is-home-dir? "~/"))
  (is (lutil-file:is-home-dir? "~/user"))
  (is (lutil-file:is-home-dir? "~/user/more/path")))

(deftest expand-home-dir
  (is-equal "/usr/local/bin"
            (lutil-file:expand-home-dir "/usr/local/bin"))
  (is-equal "/home/oubiwann"
            (lutil-file:expand-home-dir "/home/oubiwann"))
  (let* ((tilde-dir "~/my-data")
         (expanded (lutil-file:expand-home-dir tilde-dir)))
    (is (lutil-file:is-home-dir? tilde-dir))
    (is-not (lutil-file:is-home-dir? expanded))))

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
    false))

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
    "deps/lfe"))

;; TODO: Rethink this
(deftestskip check-deps
  (is-equal
    (expected-checked-subdirs)
    (lutil-file:check-deps (get-test-subdirs))))

;; TODO: Rethink this
(deftestskip filtered-deps
  (is-equal
    (expected-filtered-subdirs)
    (lutil-file:filter-deps (get-test-subdirs))))
