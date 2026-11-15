(defmodule lutil-text
  "Text trimming, indentation, modern IOdata wrapping, and deprecated
  flat-string wrapping helpers."
  (export all))

(defun strip (string)
  "Remove leading and trailing whitespace matched by the module regular
  expression and return a character list."
  (re:replace
     string
     "(^\\s+)|(\\s+$)"
     ""
     '(global #(return list))))

(defun capitalized? (string)
  "Return `true` when the first character of a nonempty character list is ASCII
  `A` through `Z`; otherwise return `false`. Empty input fails at the
  first-character lookup."
  (lists:member (car string) (lists:seq 65 90)))

(defun indent (count)
  "Return a list containing `Count` copies of the default space token; the
  result is nested IOdata, not necessarily a flat string."
  (indent count " "))

(defun indent (count char)
  "Return a list containing `Count` copies of `Token`; tokens are not
  flattened."
  (lists:duplicate count char))

(defun wrap (words)
  "Wrap `Words` at the default width of 78, with zero indentation and a space
  delimiter; the returned IOdata begins with a newline."
  (wrap words 78))

(defun wrap (words width)
  "Wrap `Words` at `Width`, with zero indentation and a space delimiter; the
  returned IOdata begins with a newline."
  (wrap words width 0))

(defun wrap (words width indent)
  "Wrap `Words` at `Width` with `Count` spaces of indentation and a space
  delimiter; the result is IOdata beginning with a newline."
  (wrap words width indent " "))

(defun wrap (words width count delim)
  "Tokenize `Words` using `Delim` as a character set, join words with the whole
  `Delim`, and join lines with a newline plus `Count` spaces. The width check
  excludes `Delim`, so line lengths are not a strict maximum; IOdata begins
  with a newline."
  (let ((prefix (++ "\n" (indent count " "))))
    (++ prefix
        (string:join
         (reverse
          (wrap (string:tokens words delim) width count delim '("")))
         prefix))))

(defun wrap
  "Worker that assembles wrapped token lines in reverse order; use the public
  `wrap` arities for ordinary text input."
  (('() width indent delim result)
   result)
  ((`(,word . ,rest) width indent delim `("" . ,prev-lines))
   (wrap rest width indent delim (cons word prev-lines)))
  ((`(,word . ,rest) width indent delim `(,cur-line . ,prev-lines)) (when (< (+ (length word)
                                                                                (length cur-line))
                                                                             width))
   (wrap rest width indent delim (cons (++ cur-line delim word) prev-lines)))
  ((`(,word . ,rest) width indent delim `(,cur-line . ,prev-lines))
   (wrap rest width indent delim (list word (cons cur-line prev-lines)))))

(defun reverse (l)
  "Reverse the two-element-tree representation used by `wrap/4`; this is not a
  general replacement for `lists:reverse/1`."
  (reverse l '()))

(defun reverse
  "Worker for reversing the wrapper tree into `Acc`; this helper handles the
  wrapper representation, not arbitrary nested lists."
  (('() result)
   result)   
  ((`(,head ,tail) acc)
   (reverse tail (lists:append (list head) acc)))
  ((head acc)
   (lists:append (list head) acc)))

;;; Old text wrapping -- deprecated

(defun wrap-text (text)
  "Deprecated flat-string wrapper using width 78 and a single-space token
  separator."
  (wrap-text text 78))

(defun wrap-text (text max-len)
  "Deprecated flat-string wrapper. It counts joining spaces against `MaxLen`,
  joins lines with newlines, returns an empty string for empty or space-only
  input, and leaves overlong words unsplit."
  (string:join
    (make-wrapped-lines
      (string:tokens text " ") max-len)
    "\n"))

(defun make-wrapped-lines
  "Build the deprecated wrapper lines from tokens, preserving order and allowing
  words longer than `MaxLen` to occupy their own line."
  (('() _max-len)
   '())
  (((cons word rest) max-len)
    (let (((tuple _ len last-line lines) (assemble-lines
                                           max-len
                                           word
                                           rest)))
      (lists:reverse (cons last-line lines)))))

(defun assemble-lines (max-len word rest)
  "Fold `Rest` after `Word` and return `{MaxLen, LineLength, LastLine,
  PreviousLines}`."
  (lists:foldl
    #'assemble-line/2
    (tuple max-len (length word) word '()) rest))

(defun assemble-line
  "Update `{MaxLen, LineLength, CurrentLine, PreviousLines}` with `Word`; when
  the word plus its joining space exceeds `MaxLen`, start a new line."
  ((word (tuple max line-len line acc))
    (when (> (+ 1 (length word) line-len) max))
    (tuple max (length word) word (cons line acc)))
  ((word (tuple max line-len line acc))
    (tuple max (+ line-len 1 (length word)) (++ line " " word) acc)))
