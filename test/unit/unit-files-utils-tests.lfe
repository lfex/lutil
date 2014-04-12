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
      (check-wrong-is-exception 2))
    (from lists
      (map 2)
      (seq 2)
      (zipwith 3))))

(include-lib "deps/lfeunit/include/lfeunit-macros.lfe")

(deftest is-home-dir?
  (is-not (: lfe-utils is-home-dir? '"~"))
  (is-not (: lfe-utils is-home-dir? '"/"))
  (is-not (: lfe-utils is-home-dir? '"~home/"))
  (is-not (: lfe-utils is-home-dir? '"/home"))
  (is (: lfe-utils is-home-dir? '"~/"))
  (is (: lfe-utils is-home-dir? '"~/user"))
  (is (: lfe-utils is-home-dir? '"~/user/more/path")))

(deftest expand-home-dir
  (is-equal '"/usr/local/bin"
            (: lfe-utils expand-home-dir '"/usr/local/bin"))
  (is-equal '"/home/oubiwann"
            (: lfe-utils expand-home-dir '"/home/oubiwann"))
  (let* ((tilde-dir '"~/my-data")
         (expanded (: lfe-utils expand-home-dir tilde-dir)))
    (is (: lfe-utils is-home-dir? tilde-dir))
    (is-not (: lfe-utils is-home-dir? expanded))))
