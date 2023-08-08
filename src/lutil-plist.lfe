(defmodule lutil-plist
  (export all))

(defun get-in (data keys)
  (lists:foldl #'proplists:get_value/2 data keys))

(defun rename-key (old-key new-key old-proplist)
  "Given an old key, a new key, and an proplist to operate upon, rename the
  keys. This is essentially a create-new-k/v followed by a delete-old-k/v
  set of operations."
  (let ((new-proplist (++ old-proplist
                     `(#(,new-key ,(proplists:get_value
                                     old-key
                                     old-proplist))))))
    (proplists:delete old-key new-proplist)))
