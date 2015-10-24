# lutil

[![][lutil-logo]][lutil-logo-large]

[lutil-logo]: resources/images/lutil-x250.png
[lutil-logo-large]: resources/images/lutil-x700.png

*Utility functions for LFE*

##### Contents

* [Introduction](#introduction-)
* [Dependencies](#dependencies-)
* [Installation](#installation-)
* [Usage](#usage-)
  * [Modules](#modules-)
  * [Macros](#macros-)

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

As of version 0.7.0, This project assumes that you have
[rebar3](https://github.com/rebar/rebar3) installed somwhere in your ``$PATH``.
And no longer uses the old version of rebar.

Note that if you do not wish to use rebar3, you may use the most recent
rebar2-compatible release of lutil: 0.6.7.

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

lutil offers several Clojure-alike macros:
 * ``get-in`` (supports lists, proplists, orddicts, dicts, and maps)
 * ``->>`` and ``->``
 * predicates of the ``name?`` form

Here's an example of the ``get-in`` macro:

```cl
> (set data '(#(key-1 val-1)
              #(key-2 val-2)
              #(key-3 (#(key-4 val-4)
                       #(key-5 val-5)
                       #(key-6 (#(key-7 val-7)
                                #(key-8 val-8)))))))
...
> (include-lib "lutil/include/core.lfe")
loaded-core
> (get-in data 'key-1)
val-1
> (get-in data 'key-3 'key-5)
val-5
> (get-in data 'key-3 'key-6 'key-8)
val-8
> (get-in data 'key-19)
undefined
> (get-in data 'key-3 'key-6 'key-89 'key-100)
undefined

```

Here's an example of the thrushing macro, ``->>``:

```cl
> (include-lib "lutil/include/compose.lfe")
loaded-compose
> (->> (seq 42)
       (lists:map (lambda (x) (math:pow x 2)))
       (lists:filter (compose #'even?/1 #'round/1))
       (take 10)
       (lists:foldl #'+/2 0))
1540.0
```

Note that, without the thrushing macro, this would be written as such:

```cl
> (lists:foldl #'+/2 0
    (take 10
      (lists:filter
        (compose #'even?/1 #'round/1)
        (lists:map
          (lambda (x)
            (math:pow x 2))
          (seq 42)))))
1540.0
```
