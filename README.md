# lutil

[![Build Status][travis badge]][travis] [![LFE Versions][lfe badge]][lfe] [![Erlang Versions][erlang badge]][versions] [![Tags][github tags badge]][github tags] [![Downloads][hex downloads]][hex package]

[![][lutil-logo]][lutil-logo-large]

*Utility functions for LFE*


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
be easily incorported into projects without having to re-implement these
little functions all the time.

Utility modules include:
 * ``lutil-file``
 * ``lutil-math``
 * ``lutil-text``
 * ``lutil-type``
 * ``lutil``

lutil also explores new LFE functions and macros that may be of interest to
LFE-proper; if they fare well here, we will submit proposals for inclusion.

Macros include:
 * ``compose.lfe``
 * ``core.lfe``
 * ``mnesia-macros.lfe``
 * ``predicates.lfe``


## Dependencies [&#x219F;](#contents)

As of version 0.7.0, this project assumes that you have
[rebar3](https://github.com/rebar/rebar3) installed somwhere in your ``$PATH``.
It no longer uses the old version of rebar. If you do not wish to use rebar3,
you may use the most recent rebar2-compatible release of lutil: 0.6.7.


## Installation [&#x219F;](#contents)

In your ``rebar.config`` file, update your ``deps`` section to include
``lutil``:

```erlang
{deps, [
  {lutil, ".*", {git, "git://github.com/lfex/lutil.git"}}
]}
```


## Usage [&#x219F;](#contents)


### Modules [&#x219F;](#contents)

For the modules, usage is the same as any other Erlang or LFE library :-)

```cl
> (lutil-math:dot-product '(1 2 3) '(4 5 6))
32

> (lutil-type:add-tuples (tuple 1 2) (tuple 3 4))
#(1 2 3 4)
> (lutil-type:add-tuples (list (tuple 1 2) (tuple 3 4) (tuple 5 6)))
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

lutil offers the ``create-table`` macro for use with more easily working
generating Mnesia tables. Example usage is available [here](https://github.com/oubiwann/mnesia-tutorial/blob/master/src/structure.lfe).

If you are looking for the Clojure macros which used to be in lutil, they have
an interesting history: they were first moved to their own project, and then
added to the [LFE stdlib][clj docs]!


## License [&#x219F;](#contents)

BSD 3-Clause License

Copyright (c) 2009, Tim Dysinger <tim@dysinger.net>

Copyright (c) 2013-2016, Duncan McGreggor <oubiwann@gmail.com>

Copyright (c) 2014, Torbjorn Tornkvist <kruskakli@gmail.com>

Copyright (c) 2014, Døkkarr Hirðisson <dokkarr@lfe.io>

Copyright (c) 2014, Dreki Þórgísl <dreki@billo.systems>

Copyright (c) 2015, arpunk <arpunk@cryptolab.net>

Copyright (c) 2015, osense <krupicka.adam@gmail.com>

Copyright (c) 2016, Eric Bailey <eric@ericb.me>


<!-- Named page links below: /-->

[lutil-logo]: priv/images/lutil-x250.png
[lutil-logo-large]: priv/images/lutil-x700.png
[org]: https://github.com/lfex
[github]: https://github.com/lfex/lutil
[gitlab]: https://gitlab.com/lfex/lutil
[travis]: https://travis-ci.org/lfex/lutil
[travis badge]: https://img.shields.io/travis/lfex/lutil.svg
[lfe]: https://github.com/rvirding/lfe
[lfe badge]: https://img.shields.io/badge/lfe-1.2.0-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-R15%20to%2019.1-blue.svg
[versions]: https://github.com/lfex/lutil/blob/master/.travis.yml
[github tags]: https://github.com/lfex/lutil/tags
[github tags badge]: https://img.shields.io/github/tag/lfex/lutil.svg
[github downloads]: https://img.shields.io/github/downloads/atom/atom/total.svg
[hex badge]: https://img.shields.io/hexpm/v/lutil.svg?maxAge=2592000
[hex package]: https://hex.pm/packages/lutil
[hex downloads]: https://img.shields.io/hexpm/dt/lutil.svg
[clj docs]: https://github.com/rvirding/lfe/blob/develop/doc/lfe_clj.txt
