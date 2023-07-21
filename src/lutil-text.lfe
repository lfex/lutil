(defmodule lutil-text
  (export all))

(defun strip (string)
  (re:replace
     string
     "(^\\s+)|(\\s+$)"
     ""
     '(global #(return list))))

(defun capitalized? (string)
  "This function checks to see if the first letter of a passed string is
  capitalized.

  Capital letters range from 'A' (ASCII code 65) to 'Z' (ASCII code 90)."
  (lists:member (car string) (lists:seq 65 90)))

(defun wrap (words)
  (wrap words 78))

(defun wrap (words width)
  (wrap words width 0))

(defun wrap (words width indent)
  (wrap words width indent " "))

(defun wrap (words width indent delim)
  (let ((prefix (++ "\n" (lists:duplicate indent " "))))
    (++ prefix
        (string:join
         (reverse
          (wrap (string:tokens words delim) width indent delim '("")))
         prefix))))

(defun wrap
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
  (reverse l '()))

(defun reverse
  (('() result)
   result)   
  ((`(,head ,tail) acc)
   (reverse tail (lists:append (list head) acc)))
  ((head acc)
   (lists:append (list head) acc)))

;;; Old text wrapping -- deprecated

(defun wrap-text (text)
  (wrap-text text 78))

(defun wrap-text (text max-len)
  (string:join
    (make-wrapped-lines
      (string:tokens text " ") max-len)
    "\n"))

(defun make-wrapped-lines
  (((cons word rest) max-len)
    (let (((tuple _ len last-line lines) (assemble-lines
                                           max-len
                                           word
                                           rest)))
      (lists:reverse (cons last-line lines)))))

(defun assemble-lines (max-len word rest)
  (lists:foldl
    #'assemble-line/2
    (tuple max-len (length word) word '()) rest))

(defun assemble-line
  ((word (tuple max line-len line acc))
    (when (> (+ (length word) line-len) max))
    (tuple max (length word) word (cons line acc)))
  ((word (tuple max line-len line acc))
    (tuple max (+ line-len 1 (length word)) (++ line " " word) acc)))
