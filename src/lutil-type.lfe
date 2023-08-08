;;;; data types and type ops
;;;;
(defmodule lutil-type
  (export all))

;;; Atom module backwards-compatible aliases

(defun atom-cat (a1 a2) (lutil-atom:cat a1 a2))
(defun atom-cat (alist) (lutil-atom:cat alist))

;;; Tuple module backwards-compatible aliases

(defun add-tuples (t1 t2) (lutil-typle:cat t1 t2))
(defun add-tuples (ts) (lutil-typle:cat ts))
(defun tuple-cat (t1 t2) (lutil-typle:cat t1 t2))
(defun tuple-cat (ts) (lutil-typle:cat ts))

;;; List module backwards-compatible aliases 

(defun partition-list (l) (lutil-list:partition l))
(defun list->tuple (l) (lutil-list:->tuple l))
(defun zip (ls) (lutil-list:zip ls))
(defun zip (l1 l2) (lutil-list:zip l1 l2))
(defun zip (l1 l2 l3) (lutil-list:zip l1 l2 l3))
(defun zip (l1 l2 l3 l4) (lutil-list:zip l1 l2 l3 l4))
(defun zip-2 (l1 l2 acc) (lutil-list:zip-2 l1 l2 acc))
(defun zip-3 (l1 l2 l3 acc) (lutil-list:zip-2 l1 l2 l3 acc))
(defun zip-4 (l1 l2 l3 l4 acc) (lutil-list:zip-2 l1 l2 l3 l4 acc))
(defun zip-any (ls acc) (lutil-list:zip-any ls acc))

;;; Proplist module backwards-compatible aliases 

(defun rename-key (old-key new-key old-proplist)
  (lutil-plist:rename-key old-key new-key old-proplist))

;;; Dict module backwards-compatible aliases 

(defun pair-dict (l) (lutil-list:->dict l))

;;; Orddict module backwards-compatible aliases 

(defun orddict-merge (od1 od2) (lutil-odict:merge od1 od2))

;;; Network module backwards-compatible aliases

(defun host->tuple (host) (lutil-net:host->tuple host))
(defun tuple->host (ip-octets) (lutil-net:tuple->host ip-octets))
