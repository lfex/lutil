(defmodule lutil-bin
  (export
   (trim-trailing 1) (trim-trailing 2)
   (trim-trailing-ws 1)))

(defun trim-trailing-ws (bitstr)
  (clj:-> bitstr          
          (trim-trailing #" ")
          (trim-trailing #"\n")
          (trim-trailing #"\r")
          (trim-trailing #"\r\n")))

(defun trim-trailing (bitstr)
  (trim-trailing bitstr #"\n"))

(defun trim-trailing (bitstr pattern)
  (let* ((pattern-size (byte_size pattern))
         (starting-index (- (byte_size bitstr) pattern-size)))
    (trim-trailing bitstr pattern pattern-size starting-index)))

(defun trim-trailing
  ((bitstr _ _ _) (when (== bitstr #""))
   #"")
  ((bitstr pattern _ _) (when (== bitstr pattern))
   #"")
  ((bitstr _ pattern-size index) (when (or (=< index 0) (< (byte_size bitstr) (+ pattern-size index))))
   bitstr)
  ((bitstr pattern pattern-size index)
   (let ((head (binary:part bitstr 0 index))
         (tail (binary:part bitstr index pattern-size)))
     (lfe_io:format "head: ~p; tail: ~p~n" (list head tail))
     (if (!= pattern tail)
       bitstr
       (trim-trailing head pattern pattern-size (- (byte_size head) pattern-size))))))
