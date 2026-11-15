# lutil

lutil is a collection of utility functions and macros for LFE and Erlang
projects. The library keeps small, reusable operations together so clients do
not need to reimplement them.

The modules cover list, math, text, binary, tuple, atom, file, network,
property-list, ordered-dictionary, record and version utilities. The `lutil`
module provides general helpers; `lutil-rec` provides the `fields` macro, and
`include/mnesia-macros.lfe` supplies the Mnesia table macro used by clients.
`lcycle` is an Erlang `gen_server` module for cycling through a sequence.

This is an initial project overview. Full public API documentation and a
complete carrier census are forthcoming; source comments do not necessarily
appear as entries in the generated API reference.
