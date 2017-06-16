:-initialization(main).

main :-
	linda:fork,
	linda:init,
	linda:eval(consumer('A')),
	linda:eval(consumer('B')),
	producer.
main :-
	linda:wait,
	halt.

producer :-
	between(1,100,I),
		random(R),
		Ms is ((R * 99) // 1) + 1,
		sys:delay(Ms),
		linda:out({msg:I}),
		fail.
producer :-
	linda:end_wait.

consumer(N) :-
	linda:in({msg:Y}),
		atomic_list_concat(['consumer ',N,' got = ',Y],Line),
		writeln(Line),
		fail.
