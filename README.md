# lutil

[![Build Status][gh-actions-badge]][gh-actions-runs]
[![LFE][lfe-badge]][lfe]
[![OTP CI matrix][erlang-badge]][workflow-definition]
[![HexDocs][hex-docs-badge]][hex-docs]
[![Tags][github-tags-badge]][github-tags]
[![Downloads][hex-downloads]][hex-package]

*Utility functions for LFE*

[![Project Logo][logo]][logo-large]

##### Contents

* [Introduction](#introduction-)
* [Dependencies](#dependencies-)
* [Installation](#installation-)
* [Usage](#usage-)
  * [Modules](#modules-)
  * [Macros](#macros-)
* [License](#license-)

## Introduction [&#x219F;](#contents)

lutil offers several modules and macros with convenience functions that can
be easily incorporated into projects without having to re-implement these
little functions all the time.

lutil also explores new LFE functions and macros that may be of interest to
LFE-proper; if they fare well here, we will submit proposals for inclusion.

Note that with the release of 0.14, many deprecated functions and macros that have
either been moved into LFE itself or other LFE libraries were removed and are
no longer available in the library. The last version with those functions
present is  0.13.5.

## Dependencies [&#x219F;](#contents)

The published package is `lutil` on [Hex](https://hex.pm/packages/lutil).
The current CI workflow exercises the listed OTP 27, 28, and 29 matrix; this
matrix records tested releases, not a declared minimum OTP version.

## Installation [&#x219F;](#contents)

In your `rebar.config` file, update your `deps` section to include
`lutil`:

```erlang
{deps, [
  {lutil, "0.17.0"}
]}.
```

## Usage [&#x219F;](#contents)

### Modules [&#x219F;](#contents)

See the [module guide](docs/overview.md) for the library areas and public
reference navigation. These examples use the LFE REPL:

```cl
> (lutil-math:dot-product '(1 2 3) '(4 5 6))
32
> (lutil-math:get-next-prime 0)
2
> (lutil-list:partition '(k1 atom-value k2 "tail"))
#((k1 k2) (atom-value "tail"))
> (lutil-tuple:cat (tuple 1 2) (tuple 3 4))
#(1 2 3 4)
> (lutil-tuple:cat (list (tuple 1 2) (tuple 3 4) (tuple 5 6)))
#(1 2 3 4 5 6)
> (lutil-list:chunks (lists:seq 1 32) 8 #(by-parts))
((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)
 (17 18 19 20) (21 22 23 24) (25 26 27 28) (29 30 31 32))
> (lutil-list:chunks (lists:seq 1 32) 8 #(by-length))
((1 2 3 4 5 6 7 8) (9 10 11 12 13 14 15 16)
 (17 18 19 20 21 22 23 24) (25 26 27 28 29 30 31 32))
```

For the Erlang `lcycle` API, list input is stateless and returns the rotated
list with each value. `next([a,b,c])` returns `{a,[b,c,a]}` and
`last([a,b,c])` returns `{c,[c,a,b]}`. `take(4,[a,b,c])` returns
`{[a,b,c,a],[b,c,a]}`; `lastn(2,[a,b,c])` returns
`{[b,c],[b,c,a]}` with values in forward display order. With count zero, list input returns `{[],List}` without changing the list;
pid input returns `[]` without advancing the server.

### Macros [&#x219F;](#contents)

The `fields` macro in `lutil-rec` expands either a quoted or unquoted record
name to its generated `fields-RecordName` macro. Define the record before using
either form. The included `create-table` macro uses those fields to append the
Mnesia `attributes` option; with an empty options list it also selects table
type `set`:

```cl
(defmodule account-storage
  (export (account-fields 0) (account-fields-quoted 0) (make-table 0)))
(include-lib "lutil/include/mnesia-macros.lfe")
(defrecord account id owner)
(defun account-fields () (lutil-rec:fields account))
(defun account-fields-quoted () (lutil-rec:fields 'account))
(defun make-table () (create-table account ()))
```

The include file supplies compile-time macros and defines the callable
`loaded-mnesia-macros/0` helper, which returns `ok` for REPL inclusion
reporting. `make-table/0` calls `mnesia:create_table/2`; it requires the Mnesia
application and a node with a Mnesia schema. The earlier Clojure macros moved
to their own project and then to the [LFE stdlib][clj docs].

## License [&#x219F;](#contents)

BSD 3-Clause License

```
Copyright © 2013-2026, Duncan McGreggor <oubiwann@gmail.com>
Copyright © 2016, Eric Bailey <eric@ericb.me>
Copyright © 2015, arpunk <arpunk@cryptolab.net>
                  osense <krupicka.adam@gmail.com>
Copyright © 2014, Torbjorn Tornkvist <kruskakli@gmail.com>
                  Døkkarr Hirðisson <dokkarr@lfe.io>
                  Dreki Þórgísl <dreki@billo.systems>\
Copyright © 2009, Tim Dysinger <tim@dysinger.net>
```

[//]: ---Named-Links---

[logo]: priv/images/lutil-x250.png
[logo-large]: priv/images/lutil-x700.png
[gh-actions-badge]: https://github.com/lfex/lutil/actions/workflows/cicd.yml/badge.svg?branch=codex%2Flutil-docs-maintenance
[gh-actions-runs]: https://github.com/lfex/lutil/actions/workflows/cicd.yml
[workflow-definition]: https://github.com/lfex/lutil/blob/codex/lutil-docs-maintenance/.github/workflows/cicd.yml
[lfe]: https://github.com/lfe/lfe
[lfe-badge]: https://img.shields.io/badge/lfe-2.2.2-blue.svg
[erlang-badge]: https://img.shields.io/badge/OTP%20CI-27%20%2F%2028%20%2F%2029-blue.svg
[hex-docs-badge]: https://img.shields.io/badge/HexDocs-reference-blue.svg
[hex-docs]: https://hexdocs.pm/lutil/
[github-tags]: https://github.com/lfex/lutil/tags
[github-tags-badge]: https://img.shields.io/github/tag/lfex/lutil.svg
[clj docs]: https://github.com/rvirding/lfe/blob/develop/doc/lfe_clj.txt
[hex-badge]: https://img.shields.io/hexpm/v/lutil.svg?maxAge=2592000
[hex-package]: https://hex.pm/packages/lutil
[hex-downloads]: https://img.shields.io/hexpm/dt/lutil.svg

## Development

Install Rebar3 to build from source. Run `make check-native` for the native
compile and test checks, and `make docs` to build or refresh the ldoc reference at
`_build/default/ldoc/current/site`.
