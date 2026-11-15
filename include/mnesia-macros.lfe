(defmacro create-table
  "Create a Mnesia table for `RecordName` using the generated
  `fields-RecordName` macro. With empty options, use table type `set`;
  otherwise append the generated attributes option to `TableDefs`."
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
  "Include-only helper kept last so the REPL reports the included macro carrier.
  Calling it returns `ok`; it does not load or define Mnesia tables."
  'ok)
