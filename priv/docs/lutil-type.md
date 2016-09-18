# lutil-type

The [[lutil-type:zip/2]], [[lutil-type:zip/3]], and [[lutil-type:zip/4]]
implementations are for kicks; probably *much* better to use Erlang's
`lists:zip/2` and `lists:zip3/3`. There's no `zip/4`, so hey -- have fun. The
[[lutil-type:zip/1]] function is what's most interesting: it can be used for
lists of lists of any length -- no need for special-case arities. In particular,
the [[lutil-type:zip/1]] function was needed for the [lmatrix] library.

Note that, unlike the Erlang zip functions, these functions *do not* return a
list of tuples; they return a list of lists.

[lmatrix]: https://github.com/lfex/lmatrix

An alternate implementation of [[lutil-type:zip/4]]:

```lfe
(defun zip-4 (list-1 list-2 list-3 list-4 acc)
  ((_ _ _ () acc)
    acc)
  ((_ _ () _ acc)
    acc)
  ((_ () _ _ acc)
    acc)
    ((() _ _ _ acc)
    acc)
  (((cons h1 t1) (cons h2 t2) (cons h3 t3) (cons h4 t4) acc)
    (zip-4 t1 t2 t3 t4 (++ acc `((,h1 ,h2 ,h3 ,h4))))))
```
