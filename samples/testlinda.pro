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
	sys:delay(10),
	linda:end_wait.

consumer(N) :-
	linda:in({msg:Y}),
		sys:concat('consumer ',N,' got = ',Y,Line),
		writeln(Line),
		fail.
consumer(N) :-
	sys:concat('Done ',N,Msg),
	writeln(Msg).
