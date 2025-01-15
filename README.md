# lutil

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe-badge]][lfe]
[![Erlang Versions][erlang-badge]][versions]
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

As of version 0.7.0, this project assumes that you have
[rebar3](https://github.com/rebar/rebar3) installed somewhere in your `$PATH`.
It no longer uses the old version of rebar. If you do not wish to use rebar3,
you may use the most recent rebar2-compatible release of lutil: 0.6.7.

## Installation [&#x219F;](#contents)

In your `rebar.config` file, update your `deps` section to include
`lutil`:

```erlang
{deps, [
  {lutil, "0.16.0"}}}
]}
```

## Usage [&#x219F;](#contents)

### Modules [&#x219F;](#contents)

For the modules, usage is the same as any other Erlang or LFE library :-)

Some example usage:

```cl
> (lutil-math:dot-product '(1 2 3) '(4 5 6))
32

> (lutil-tuple:cat (tuple 1 2) (tuple 3 4))
#(1 2 3 4)
> (lutil-tuple:cat (list (tuple 1 2) (tuple 3 4) (tuple 5 6)))
#(1 2 3 4 5 6)

lfe> (lutil-list:chunks (lists:seq 1 32) 8 #(by-parts))
((1 2 3 4)
 (5 6 7 8)
 (9 10 11 12)
 (13 14 15 16)
 (17 18 19 20)
 (21 22 23 24)
 (25 26 27 28)
 (29 30 31 32))
lfe> (lutil-list:chunks (lists:seq 1 32) 8 #(by-length))
((1 2 3 4 5 6 7 8)
 (9 10 11 12 13 14 15 16)
 (17 18 19 20 21 22 23 24)
 (25 26 27 28 29 30 31 32))
```

### Macros [&#x219F;](#contents)

lutil offers the `create-table` macro for use with more easily working
generating Mnesia tables. Example usage is available [here](https://github.com/oubiwann/mnesia-tutorial/blob/master/src/structure.lfe).

If you are looking for the Clojure macros which used to be in lutil, they have
an interesting history: they were first moved to their own project, and then
added to the [LFE stdlib][clj docs]!

## License [&#x219F;](#contents)

BSD 3-Clause License

```
Copyright © 2013-2025, Duncan McGreggor <oubiwann@gmail.com>
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
[gh-actions-badge]: https://github.com/lfex/lutil/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfex/lutil/actions
[lfe]: https://github.com/lfe/lfe
[lfe-badge]: https://img.shields.io/badge/lfe-2.1-blue.svg
[erlang-badge]: https://img.shields.io/badge/erlang-21+-blue.svg
[versions]: https://github.com/lfex/lutil/blob/master/.travis.yml
[github-tags]: https://github.com/lfex/lutil/tags
[github-tags-badge]: https://img.shields.io/github/tag/lfex/lutil.svg
[clj docs]: https://github.com/rvirding/lfe/blob/develop/doc/lfe_clj.txt
[hex-badge]: https://img.shields.io/hexpm/v/lutil.svg?maxAge=2592000
[hex-package]: https://hex.pm/packages/lutil
[hex-downloads]: https://img.shields.io/hexpm/dt/lutil.svg
