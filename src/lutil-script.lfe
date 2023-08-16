(defmodule lutil-script
  (export all))

(defun expand-home-dir (path-with-home)
  (cond ((lutil-file:is-home-dir? path-with-home)
         (filename:join
          (list (get-home-dir)
                (string:substr path-with-home 3))))
        ('true path-with-home)))

(defun get-local-dir (dir)
  "Get the local path to a given dir, using (get-cwd)."
  (filename:join `(,(get-cwd) ,dir)))

(defun get-deps-dir ()
  "Get the default dependency directories for the current directory."
  (get-local-dir "deps"))

(defun get-ebin-dir ()
  "Get the ebin directory for the current directory."
  (get-local-dir "ebin"))

(defun get-src-dir ()
  "Get the src directory for the current directory."
  (get-local-dir "src"))

(defun get-test-dir ()
  "Get the test directory for the current directory."
  (get-local-dir "test"))

(defun get-eunit-dir ()
  "Get the .eunit directory for the current directory."
  (get-local-dir ".eunit"))

(defun get-cwd ()
  "The current workding directory in this case is the directory that the user
  executed lfetool *from*. Shortly after it starts up, the lfetool script
  switches from this dir to the actual directory where the lfetool code/library
  lives. To preserve the original cwd, it is passed as a parameter to erl
  during start up. That value is accessed with this function."
  (caar
   (element 2 (get-arg 'cwd "."))))

(defun get-home-dir ()
  (caar
   (element 2 (get-arg 'home #(ok (("/tmp")))))))

(defun get-arg (arg-name default)
  (let ((arg-value (init:get_argument arg-name)))
    (case arg-value
      ('error
       `#(default ((,default))))
      (_ arg-value))))
