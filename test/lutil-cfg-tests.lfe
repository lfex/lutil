(defmodule lutil-cfg-tests
  (behaviour ltest-unit)
  (export all)
  (import
    (from ltest
      (check-failed-is 2)
      (check-wrong-is-exception 2))))


(include-lib "ltest/include/ltest-macros.lfe")

(deftest get-project-deps-empty-project
  (is-equal '() (lutil-cfg:get-project-deps
                  '#(ok (#(project ()))))))

(deftest get-project-deps-empty-deps
  (is-equal '() (lutil-cfg:get-project-deps
                  '#(ok (#(project (#(deps ()))))))))

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
