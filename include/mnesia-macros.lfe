(defmacro create-table
  "To use this macro, pass a 'raw' atom for the table name, not a quoted one --
  just like you would do when placing an atom in tuple data: #(...).

  You will also need to supply the second parameter: either table definition
  data or an empty (unquoted) list.

  The 'create-table' macro is needed due to the fact that, right now, one
  cannot call a macro in LFE like one can with a funciton: there is no
  equivalent to '(funcall func ... )' for macros. However, Robert is currently
  working on adding this functionality :-)

  More specifics:
    * whenever an LFE record is created, several macros get created for them;
      these macros let you do things like get and set values, but for our
      purposes here, we're interested in the (fields-*) macros.
    * The (fields-<name>) macro returns the list of fields defined by the
      given recrod. If you have a record named 'employee', then to get that
      record's fields, you can call '(fields-employee)'.
    * This macro builds the macro name from the given table name (which should
      be the same as the record name).
    * It then calls the macro with the assembled macro name, assigning it to the
      'fields' variable.
    * The Mnesia 'create_table' function is then called, passing the table name
      as well as the obtained fields."
  ((record-table-name '())
    `(create-table ,record-table-name (#(type set))))
  ((record-table-name table-defs)
    (let* ((record-fields-macro-name (lutil:atom-cat
                                       'fields- record-table-name))
           (computed-record-fields `(,record-fields-macro-name)))
      `(mnesia:create_table
        ',record-table-name
        (++ ,table-defs
            (list (tuple 'attributes ,computed-record-fields)))))))

