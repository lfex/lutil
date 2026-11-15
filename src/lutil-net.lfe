(defmodule lutil-net
  "Convert between IPv4 address host names and four-octet tuples."
  (export all))

(defun host->tuple (host)
  "Resolve `Host` as an IPv4 name with `inet:getaddr/2` and return its
  four-integer address tuple. A resolution error fails the match."
  (let ((`#(ok ,tuple) (inet:getaddr host 'inet)))
    tuple))

(defun tuple->host (data)
  "Render an IPv4 address tuple as a dotted-decimal character list."
  (string:join
    (lists:map
      #'integer_to_list/1 (tuple_to_list data))
    "."))
