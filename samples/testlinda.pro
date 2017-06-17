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
