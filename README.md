Trealla Prolog
==============

[Usage](docs/USAGE.md) | [Features](docs/FEATURES.md)

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
transactions and *efficiency* means that in Trealla facts can be used as an efficient relational
database. SQL queries can be mapped closely to Prolog queries.

Writen in plain-old C with a permissive license. This is alpha release software at this point and
is subject to great change.

Example
-------

	:-initialization(main).
	:-using([linda]).
	:-define(LOOPS,100).

	main :-
		init,
		eval(consumer('A')),
		eval(consumer('B')),
		\+ producer,
		halt.

	producer :-
		between(1,?LOOPS,I),
			random(R), Ms is ((R * 99) // 1) + 1,
			sys:delay(Ms),
			out({msg:I}),
			fail.

	consumer(N) :-
		in({msg:Y}),
		atomic_list_concat(['consumer ',N,' got = ',Y],Line),
		writeln(Line),
		fail.

Output

	$ ./tpl -l samples/testlinda.pro
	consumer B got = 1
	consumer B got = 2
	consumer A got = 3
	consumer A got = 4
	consumer B got = 5
	consumer A got = 6
	consumer A got = 7
	consumer A got = 8
	consumer A got = 9
	consumer B got = 10
