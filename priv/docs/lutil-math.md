# lutil-math

The alternate implementations of [[lutil-math:levenshtein-simple/2]] below were
tested with different lengths of strings and from 1 to 10 to 100 to 1000 and to
10,000 iterations. Only very minor differences in performance were demonstrated.
The implementation above provided the best overall performance, with the third
implementation coming in second place, generally. The differences are so little
as to not matter.

```lfe
(defun levenshtein-simple-2
  ((() str)
    (length str))
  ((str ())
    (length str))
  (((cons a str1) (cons b str2)) (when (== a b))
    (levenshtein-simple str1 str2))
  ((str1 str2)
    (+ 1 (lists:min
          (list
           (levenshtein-simple str1 (cdr str2))
           (levenshtein-simple (cdr str1) str2)
           (levenshtein-simple (cdr str1) (cdr str2)))))))
```

```lfe
(defun levenshtein-simple-3 (str1 str2)
  (cond
    ((== () str1)
     (length str1))
    ((== () str2)
     (length str1))
    ((== (car str1) (car str2))
     (levenshtein-simple (cdr str1) (cdr str2)))
    ('true
      (+ 1 (lists:min
             (list
               (levenshtein-simple str1 (cdr str2))
               (levenshtein-simple (cdr str1) str2)
                (levenshtein-simple (cdr str1) (cdr str2))))))))
```
