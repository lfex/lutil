# lutil

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][versions]
[![Tags][github tags badge]][github tags]

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
  {lutil, "0.14.0"}}}
]}
```

## Usage [&#x219F;](#contents)

### Modules [&#x219F;](#contents)

For the modules, usage is the same as any other Erlang or LFE library :-)

```cl
> (lutil-math:dot-product '(1 2 3) '(4 5 6))
32

> (lutil-tuple:cat (tuple 1 2) (tuple 3 4))
#(1 2 3 4)
> (lutil-tuple:cat (list (tuple 1 2) (tuple 3 4) (tuple 5 6)))
#(1 2 3 4 5 6)

> (lutil:uuid4 (tuple 'type "list"))
"f790b655-f139-46d5-08e5-faf132bdd62a"
> (lutil:uuid4 (tuple 'type "atom"))
8ecd6cc2-8580-4ab6-3fc1-8135ed9bb28c
> (lutil:uuid4 (tuple 'type "binary"))
#B(51 49 53 56 102 52 53 54 45 50 51 55 56 45 52 51 56 54 45 50 57 56 ...)
> (lutil:uuid4)
#B(99 101 102 102 54 53 97 50 45 48 57 55 49 45 52 50 49 49 45 50 52 ...)
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
Copyright © 2013-2023, Duncan McGreggor <oubiwann@gmail.com>
Copyright © 2016, Eric Bailey <eric@ericb.me>
Copyright © 2015, arpunk <arpunk@cryptolab.net>
                  osense <krupicka.adam@gmail.com>
Copyright © 2014, Torbjorn Tornkvist <kruskakli@gmail.com>
                  Døkkarr Hirðisson <dokkarr@lfe.io>
                  Dreki Þórgísl <dreki@billo.systems>\
Copyright © 2009, Tim Dysinger <tim@dysinger.net>
```

<!-- Named page links below: /-->

[logo]: priv/images/lutil-x250.png
[logo-large]: priv/images/lutil-x700.png
[gh-actions-badge]: https://github.com/lfex/lutil/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfex/lutil/actions
[lfe]: https://github.com/lfe/lfe
[lfe badge]: https://img.shields.io/badge/lfe-2.1-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-20%20to%2025-blue.svg
[versions]: https://github.com/lfex/lutil/blob/master/.travis.yml
[github tags]: https://github.com/lfex/lutil/tags
[github tags badge]: https://img.shields.io/github/tag/lfex/lutil.svg
[clj docs]: https://github.com/rvirding/lfe/blob/develop/doc/lfe_clj.txt
