Trealla Prolog
==============

Trealla is a Prolog interpreter that parses to an AST and uses deep-binding and structure sharing.
It supports much of ISO-PROLOG plus:

 - modules
 - first-argument indexing
 - database persistency & transactions
 - unlimited length and number of UTF-8 atoms, strings & blobs
 - 64-bit floats & ints & optional ubounded integers
 - Linda-style processes using tuple-space
 - Erlang-style processes with message-passing
 - advanced networking features with unblocked I/O
 - definite clause grammar (DCG)

There are no bindings to an external database system. The combination of indexing, persistency,
and transactions means that Prolog facts can be used as an efficient relational database. SQL
queries can be mapped closely to Prolog queries.

The rule database usage is currently immediate update view (the traditional way). ISO-PROLOG
however specifies logical update view (ie. snapshot), so this may change in future (a flag?).

Writen in plain-old C with a permissive license. This is alpha release software at this point and
is subject to great change.

[Usage](docs/USAGE.md) | [Features](docs/FEATURES.md)
