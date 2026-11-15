# lutil module guide

`lutil` groups small utilities for LFE and Erlang projects. The generated API
reference is organized by module; this page explains where to look and records
contracts that are easy to misread from a function name alone.

## Modules

| Module or carrier | Purpose |
| --- | --- |
| `lutil` | Environment access, LFE environment names, shuffle, and version helpers. |
| `lutil-atom` | Concatenate atoms to form an atom name. |
| `lutil-bin` | Trim repeated trailing byte sequences from binaries. |
| `lutil-file` | Paths, dependency discovery, LFE compilation, and BEAM inspection. |
| `lutil-list` | List conversion, positional pairs, chunking, and list-of-lists zipping. |
| `lutil-math` | Numeric rounding and scaling, factors, primes, and edit distance. |
| `lutil-net` | Resolve IPv4 names and render IPv4 tuples. |
| `lutil-odict` | Merge ordered dictionaries with the second input winning conflicts. |
| `lutil-plist` | Nested property-list lookup and key renaming. |
| `lutil-rec` | The exported `fields/1` macro for record field names. |
| `lutil-text` | Text trimming, indentation, wrapping, and the deprecated wrapper. |
| `lutil-tuple` | Concatenate tuple elements. |
| `lutil-vsn` | Read lutil and runtime version information. |
| `lcycle` | Erlang `gen_server` cycle with list-based stateless and pid-based stateful calls. |

Use the generated site navigation to browse modules and functions.

## List and numeric contracts

`lutil-list:partition/1` accepts alternating key/value items and returns a
2-tuple of lists in input order. The new `partition/3` worker accepts reversed
key and value accumulators and reverses them at the base case. For example,
`partition([k3,v3],[k2,k1],[v2,v1])` returns
`{[k1,k2,k3],[v1,v2,v3]}`. Its input tail must contain complete pairs.

`chunks/3` selects one-element tuple modes `#(by-parts)` or `#(by-length)`.
Fixed-length chunks require a positive length, are ordered like the input, and
pad a short final chunk with `undefined`. Parts mode requires a positive count
and a positive rounded chunk length; it rounds `length(List) / Count` and does
not promise exactly `Count` chunks for every input.

`get-next-prime/2` ignores its first argument and tests candidates beginning
with its second argument. `get-gradations(Start, Inc, Count)` returns the start
plus `Count` increments, or `Count + 1` values. The two-argument form takes a
two-element list `[Min, Max]`; `scale/3` instead takes two-element tuples for
both frames. `factors/1` retains its existing factor order and terminal `1`.

## Records and Mnesia include

`lutil-rec:fields/1` accepts a raw record name or a quoted record name and
expands to the generated `fields-RecordName` macro. Define the record before
using either form. A regular dependency include and example module are:

```lisp
(defmodule account-storage
  (export (account-fields 0) (account-fields-quoted 0) (make-table 0)))
(include-lib "lutil/include/mnesia-macros.lfe")
(defrecord account id owner)
(defun account-fields () (lutil-rec:fields account))
(defun account-fields-quoted () (lutil-rec:fields 'account))
(defun make-table () (create-table account ()))
```

`create-table` expands to `mnesia:create_table/2`. Empty options select table
type `set`; supplied table options are followed by the generated `attributes`
option. The include file supplies compile-time macros and defines the
callable `loaded-mnesia-macros/0` helper, which returns `ok` for REPL inclusion
reporting. `make-table/0` calls `mnesia:create_table/2`; it requires the Mnesia
application and a node with a Mnesia schema. The include file is not a
standalone module.

## Cyclic sequences

The list forms are stateless: each returns the selected value and the rotated
list. `take/2` and `lastn/2` return `{Values, RotatedList}` for list input;
pid input returns only the values and advances that server. A count of zero
returns `{[], List}` for list input and `[]` for pid input, without changing
either form. For `[a,b,c]`, `next/1` returns `{a,[b,c,a]}` and `last/1` returns
`{c,[c,a,b]}`. `take(4,[a,b,c])` returns
`{[a,b,c,a],[b,c,a]}`. `lastn(2,[a,b,c])` returns
`{[b,c],[b,c,a]}`; backward extraction is displayed in forward order. Reset
stateful cycles explicitly and stop each owned pid.

## File and compiler results

`compile-app-src/1` copies the first `src/*.app.src` match to the requested
output directory. Success is `{ok, {'app-src', Source}, {'app-dst', Destination}}`;
missing source and copy failures return tagged error tuples with the relevant
paths and reason. The helper preserves those path strings. Dependency
compilation reports ordered `{AbsoluteDir, Outcome}` entries on partial failure
and restores the original working directory; a restoration failure raises an
error. See each function's reference text for the exact result shapes.
