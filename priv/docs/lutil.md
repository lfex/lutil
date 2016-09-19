# lutil

## [[lutil:uuid4/0]]

```commonlisp
> (lutil:uuid4 (tuple 'type 'binary))
#B(50 101 51 53 49 99 48 97 45 50 100 100 52 45 52 54 56 55 45 50 ...)
> (lutil:uuid4 (tuple 'type 'list))
"65c0aff3-421e-40bf-0f64-3ac0d1e0b72d"
> (lutil:uuid4 (tuple 'type 'atom))
```
