(defmacro create-table
  "Create a Mnesia table using the matching record field macro."
  (`(,record-table-name ())
   `(create-table ,record-table-name
                  (list (tuple 'type 'set))))
  (`(,record-table-name ,table-defs)
   (let* ((record-fields-macro-name
           (lutil-atom:cat 'fields- record-table-name))
          (record-fields-call `(,record-fields-macro-name)))
     `(mnesia:create_table
        ',record-table-name
        (++ ,table-defs
            (list (tuple 'attributes ,record-fields-call)))))))

(defun loaded-mnesia-macros ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
