;;;; Adapted from the implementations given here:
;;;; * https://github.com/okeuday/uuid/blob/7a0824a4158b5a5122ae660f59e7eac5e9138505/src/uuid.erl#L628
;;;; * https://github.com/afiskon/erlang-uuid-v4/blob/8c03a11524f6bccf984575877b533ef30a009175/src/uuid.erl"
(defmodule lutil-uuid
  (export all))

(defun v4 ()
   ;; XXX rand:bytes is Erlang 24+ only; when that's the oldest version we
   ;; support, we can turn this back on
   ;;(v4 (rand:bytes 16)))
   (v4 (crypto:strong_rand_bytes 16)))

(defun v4
  ((type) (when (== type 'strong))
   (v4 (crypto:strong_rand_bytes 16)))
  ((type) (when (is_atom type))
   ;; XXX rand:bytes is Erlang 24+ only; when that's the oldest version we
   ;; support, we can turn this back on
   ;;(v4 (rand:bytes 16)))
   (v4 (crypto:strong_rand_bytes 16)))
  ((in-bytes)
   (let (((binary (r1 (size 48))
                  (_ (size 4))
                  (r2 (size 12))
                  (_ (size 2))
                  (r3 (size 62))) in-bytes))
     (binary (r1 (size 48))
             (0 (size 1))
             (1 (size 1))
             (0 (size 1))
             (0 (size 1))
             (r2 (size 12))
             (1 (size 1))
             (0 (size 1))
             (r3 (size 62))))))

(defun v4->str (uuid-bytes)
  (let* (((binary
           (p1 (size 32))
           (p2 (size 16))
           (p3 (size 16))
           (p4 (size 16))
           (p5 (size 48))) uuid-bytes)
         (segmented (list p1 p2 (band p3 #x0fff) (band p4 #x3fff) (bor #x8000 p5)))
         (format-template "~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b"))
     (io_lib:format format-template segmented)))

(defun four ()
  (four #(type bitstring)))

(defun four
  "A wrapper for uuid4/0."
  ;; Example usage:
  ;;
  ;;   > (lutil-uuid:four (tuple 'type 'binary))
  ;;   #B(50 101 51 53 49 99 48 97 45 50 100 100 52 45 52 54 56 55 45 50 ...)
  ;;
  ;;   > (lutil-uuid:four (tuple 'type 'list))
  ;;   "65c0aff3-421e-40bf-0f64-3ac0d1e0b72d"
  ;;
  ;;   > (lutil-uuid:four (tuple 'type 'atom))
  ;;  '
  ((`#(type binary))
   (v4))
  ((`#(type ,type binary))
   (v4 type))
  ((`#(type bitstring))
   (list_to_binary (v4->str (v4))))
  ((`#(type ,type bitstring))
   (list_to_binary (v4->str (v4 type))))
  ((`#(type list))
   (v4->str (v4)))
  ((`#(type string))
   (v4->str (v4)))
  ((`#(type ,type list))
   (v4->str (v4 type)))
  ((`#(type ,type string))
   (v4->str (v4 type)))
  ((`#(type atom))
   (list_to_atom (v4->str (v4))))
  ((`#(type ,type atom))
   (list_to_atom (v4->str (v4 type)))))
