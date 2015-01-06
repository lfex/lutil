(defmodule lutil-cfg-tests
  (behaviour ltest-unit)
  (export all)
  (import
    (from ltest
      (check-failed-is 2)
      (check-wrong-is-exception 2))))

(include-lib "ltest/include/ltest-macros.lfe")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constants
;;;

(deftest constants
  (is-equal "lfe.config" (lutil-cfg:config-file))
  (is-equal "~/.lfe/lfe.config" (lutil-cfg:global-config))
  (is-equal "lfe.config" (lutil-cfg:local-config))
  (is-equal "deps" (lutil-cfg:deps-dir))
  (is-equal "https://github.com/" (lutil-cfg:github)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Configuration
;;;

(deftest list->orddict
  (is-equal '(#(a 1) #(b 2)) (lutil-cfg:list->orddict '(#(a 1) #(b 2)))))

(deftest list->orddict-fail-content-check
  (try
    (progn
      (lutil-cfg:list->orddict '(1 #(b 2)))
      (error 'unexpected-test-success))
    (catch (`#(,type ,value ,_)
      (is-equal 'error type)
      (is-equal
        "Every top-level item in an lfe.config file needs to be a tuple."
        value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Project Dependencies
;;;

(deftest merge-project-deps-empty
  (is-equal '() (lutil-cfg:merge-project-deps '() '())))

(deftest merge-project-deps-only-one
  (is-equal '("a" "b")
            (lutil-cfg:merge-project-deps
              '(#(project (#(deps ("a" "b")))))
              '()))
  (is-equal '(1 2)
            (lutil-cfg:merge-project-deps
              '()
              '(#(project (#(deps (1 2))))))))

(deftest merge-project-deps-no-shared
  (is-equal '("a" "b" 1 2)
            (lutil-cfg:merge-project-deps
              '(#(project (#(deps ("a" "b")))))
              '(#(project (#(deps (1 2)))))))
  (is-equal '(#("a/b" "c") "d/e" #("f/g" "h") "i/j")
            (lutil-cfg:merge-project-deps
              '(#(project (#(deps (#("a/b" "c") "d/e")))))
              '(#(project (#(deps (#("f/g" "h") "i/j"))))))))

(deftest merge-project-deps-some-shared
  (is-equal '("a" "b" "c")
            (lutil-cfg:merge-project-deps
              '(#(project (#(deps ("a" "b")))))
              '(#(project (#(deps ("b" "c")))))))
  (is-equal '("d/e" #("a/b" "c") "i/j")
            (lutil-cfg:merge-project-deps
              '(#(project (#(deps (#("a/b" "c") "d/e")))))
              '(#(project (#(deps (#("a/b" "c") "i/j")))))))
  (is-equal '("d/e" #("a/b" "f") "i/j")
            (lutil-cfg:merge-project-deps
              '(#(project (#(deps (#("a/b" "c") "d/e")))))
              '(#(project (#(deps (#("a/b" "f") "i/j")))))))
  (is-equal '("d/e" #("a/b" "c") "i/j")
            (lutil-cfg:merge-project-deps
              '(#(project (#(deps (#("a/b" "f") "d/e")))))
              '(#(project (#(deps (#("a/b" "c") "i/j")))))))
  (is-equal '(#("a/b" "c") #("f/g" "h") "d/e")
            (lutil-cfg:merge-project-deps
              '(#(project (#(deps (#("a/b" "c") "d/e")))))
              '(#(project (#(deps (#("f/g" "h") "d/e"))))))))

(deftest merge-project-deps-all-shared
  (is-equal '("a" "b")
            (lutil-cfg:merge-project-deps
              '(#(project (#(deps ("a" "b")))))
              '(#(project (#(deps ("a" "b")))))))
  (is-equal '(#("a/b" "c") "d/e")
            (lutil-cfg:merge-project-deps
              '(#(project (#(deps (#("a/b" "c") "d/e")))))
              '(#(project (#(deps (#("a/b" "c") "d/e"))))))))

(deftest select-deps-no-shared
  (is-equal '("a" "b" 1 2)
            (lutil-cfg:select-deps
              '("a" "b")
              '(1 2)))
  (is-equal '(#("a/b" "c") "d/e" #("f/g" "h") "i/j")
            (lutil-cfg:select-deps
              '(#("a/b" "c") "d/e")
              '(#("f/g" "h") "i/j"))))

(deftest select-deps-some-shared
  (is-equal '("a" "b" "c")
            (lutil-cfg:select-deps
              '("a" "b")
              '("b" "c")))
  (is-equal '("d/e" #("a/b" "c") "i/j")
            (lutil-cfg:select-deps
              '(#("a/b" "c") "d/e")
              '(#("a/b" "c") "i/j")))
  (is-equal '("d/e" #("a/b" "f") "i/j")
            (lutil-cfg:select-deps
              '(#("a/b" "c") "d/e")
              '(#("a/b" "f") "i/j")))
  (is-equal '("d/e" #("a/b" "c") "i/j")
            (lutil-cfg:select-deps
              '(#("a/b" "f") "d/e")
              '(#("a/b" "c") "i/j")))
  (is-equal '(#("a/b" "c") #("f/g" "h") "d/e")
            (lutil-cfg:select-deps
              '(#("a/b" "c") "d/e")
              '(#("f/g" "h") "d/e"))))

(deftest select-deps-all-shared
  (is-equal '("a" "b")
            (lutil-cfg:select-deps
              '("a" "b")
              '("a" "b")))
  (is-equal '(#("a/b" "c") "d/e")
            (lutil-cfg:select-deps
              '(#("a/b" "c") "d/e")
              '(#("a/b" "c") "d/e"))))

(deftest get-repo
  (is-equal "a/b" (lutil-cfg:get-repo "a/b"))
  (is-equal "a/b" (lutil-cfg:get-repo #("a/b" "master")))
  (is-equal "a/b" (lutil-cfg:get-repo #("a/b" "develop"))))

(deftest get-project-deps-empty
  (is-equal '() (lutil-cfg:get-project-deps '())))

(deftest get-project-deps-no-deps
  (is-equal '() (lutil-cfg:get-project-deps '(#(deps ())))))

(deftest get-project-deps-no-deps-with-other
  (is-equal '() (lutil-cfg:get-project-deps '(#(opts (#(opt-1 1)))))))

(deftest get-project-deps
  (is-equal '("a" "b") (lutil-cfg:get-project-deps '(#(deps ("a" "b"))))))

(deftest get-project-deps-with-other
  (is-equal '("a" "b") (lutil-cfg:get-project-deps '(#(opts (#(opt-1 1)))
                                                     #(deps ("a" "b"))))))
(deftest get-project-empty
  (is-equal '() (lutil-cfg:get-project '())))

(deftest get-project-no-project
  (is-equal '() (lutil-cfg:get-project '(#(lfe (#(opt-1 1)))))))

(deftest get-project-no-deps
  (is-equal '() (lutil-cfg:get-project '(#(project ())))))

(deftest get-project-no-deps-with-other
  (is-equal '(#(opt-1 1))
            (lutil-cfg:get-project '(#(project (#(opt-1 1)))))))

(deftest get-project-with-deps-with-other
  (is-equal '(#(deps ("a" "b")))
            (lutil-cfg:get-project '(#(lfe (#(opt-1 1)))
                                     #(project (#(deps ("a" "b"))))))))
