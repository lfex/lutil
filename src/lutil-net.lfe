(defmodule lutil-net
  (export all))

(defun host->tuple (host)
  (let ((`#(ok ,tuple) (inet:getaddr host 'inet)))
    tuple))

(defun tuple->host (data)
  (string:join
    (lists:map
      #'integer_to_list/1 (tuple_to_list data))
    "."))
