#########
lfe-utils
#########

Introduction
============

Convenience functions that can be included in a rebar deps declaration instead
of reimplementing across multiple projects.

Dependencies
------------

This project assumes that you have `rebar`_ installed somwhere in your
``$PATH``.

This project depends upon the following, which installed to the ``deps``
directory of this project when you run ``make deps``:

* `LFE`_ (Lisp Flavored Erlang; needed only to compile)
* `lfeunit`_ (needed only to run the unit tests)

Installation
============

In your ``rebar.config`` file, update your ``deps`` section to include
``lfe-utils``:

.. code:: erlang

    {deps, [
      {lfe, ".*", {git, "git://github.com/rvirding/lfe.git", "develop"}},
      {'lfe-utils', ".*", {git, "git://github.com/lfe/lfe-utils.git"}},
      {lfeunit, ".*", {git, "git://github.com/lfe/lfeunit.git"}}
    ]}

Usage
=====

Usage is the same as any other Erlang or LFE library :-)

.. code:: lisp

    > (: lfe-utils dot-product '(1 2 3) '(4 5 6))
    32

    > (: lfe-utils add-tuples (tuple 1 2) (tuple 3 4))
    #(1 2 3 4)
    > (: lfe-utils add-tuples (list (tuple 1 2) (tuple 3 4) (tuple 5 6)))
    #(1 2 3 4 5 6)

    > (: lfe-utils uuid4 (tuple 'type '"list"))
    "f790b655-f139-46d5-08e5-faf132bdd62a"
    > (: lfe-utils uuid4 (tuple 'type '"atom"))
    8ecd6cc2-8580-4ab6-3fc1-8135ed9bb28c
    > (: lfe-utils uuid4 (tuple 'type '"binary"))
    #B(51 49 53 56 102 52 53 54 45 50 51 55 56 45 52 51 56 54 45 50 57 56 ...)
    > (: lfe-utils uuid4)
    #B(99 101 102 102 54 53 97 50 45 48 57 55 49 45 52 50 49 49 45 50 52 ...)

.. Links
.. -----
.. _rebar: https://github.com/rebar/rebar
.. _LFE: https://github.com/rvirding/lfe
.. _lfeunit: https://github.com/lfe/lfeunit
