(defmodule lutil-plist
  "Helpers for nested property-list lookup and key renaming."
  (export all))

(defun get-in (data keys)
  "Look up each key in `Keys` successively and return the final lookup value.
  Empty `Keys` returns `Data`; a missing final key returns `undefined`.
  Intermediate results must support property-list lookup."
  (lists:foldl #'proplists:get_value/2 data keys))

(defun rename-key (old-key new-key old-proplist)
  "Append a new `{NewKey, Value}` entry using the value of `OldKey`, then delete
  every entry for `OldKey`. If the keys differ, existing `NewKey` entries
  remain; if they are equal, all entries for that key are deleted. An absent
  `OldKey` supplies `undefined`."
  (let ((new-proplist (++ old-proplist
                     `(#(,new-key ,(proplists:get_value
                                     old-key
                                     old-proplist))))))
    (proplists:delete old-key new-proplist)))
