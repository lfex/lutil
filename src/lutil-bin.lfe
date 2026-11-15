(defmodule lutil-bin
  "Trim selected trailing byte sequences from binaries."
  (export
   (trim-trailing 1) (trim-trailing 2)
   (trim-trailing-ws 1)))

(defun trim-trailing-ws (bitstr)
  "Repeatedly remove trailing space, tab, line-feed, carriage-return, and CRLF
  byte sequences until none remains."
  (let ((trimmed
         (clj:-> bitstr
                 (trim-trailing #" ")
                 (trim-trailing #"\t")
                 (trim-trailing #"\n")
                 (trim-trailing #"\r")
                 (trim-trailing #"\r\n"))))
    (if (== trimmed bitstr)
      bitstr
      (trim-trailing-ws trimmed))))

(defun trim-trailing (bitstr)
  "Remove repeated trailing line-feed bytes from `Binary`; return the trimmed
  binary."
  (trim-trailing bitstr #"\n"))

(defun trim-trailing (bitstr pattern)
  "Remove every repeated exact `Pattern` suffix from `Binary`; `Pattern` is a
  binary."
  (let* ((pattern-size (byte_size pattern))
         (starting-index (- (byte_size bitstr) pattern-size)))
    (trim-trailing bitstr pattern pattern-size starting-index)))

(defun trim-trailing
  "Worker for suffix trimming; `Size` and `Index` are the byte size and
  candidate suffix offset."
  ((bitstr _ _ _) (when (== bitstr #""))
   #"")
  ((bitstr pattern _ _) (when (== bitstr pattern))
   #"")
  ((bitstr _ 0 _)
   bitstr)
  ((bitstr _ pattern-size index) (when (or (=< index 0) (< (byte_size bitstr) (+ pattern-size index))))
   bitstr)
  ((bitstr pattern pattern-size index)
   (let ((head (binary:part bitstr 0 index))
         (tail (binary:part bitstr index pattern-size)))
     (if (=/= pattern tail)
       bitstr
       (trim-trailing head pattern pattern-size (- (byte_size head) pattern-size))))))
