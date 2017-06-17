:-initialization(main).
:-using([linda]).

main :-
	fork,
	init,
	eval(consumer('A')),
	eval(consumer('B')),
	producer.
main :-
	wait,
	halt.

producer :-
	between(1,100,I),
		random(R),
		Ms is ((R * 99) // 1) + 1,
		sys:delay(Ms),
		out({msg:I}),
		fail.
producer :-
	end_wait.

consumer(N) :-
	in({msg:Y}),
		atomic_list_concat(['consumer ',N,' got = ',Y],Line),
		writeln(Line),
		fail.
