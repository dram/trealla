:-initialization(main).

main :-
	proc:fork,
	linda:init,
	linda:eval(consumer('A')),
	linda:eval(consumer('B')),
	producer.
main :-
	proc:wait,
	halt.

producer :-
	between(1,100,I),
		sys:delay(10),
		linda:out({msg:I}),
		fail.
producer :-
	sys:delay(10),
	proc:end_wait.

consumer(N) :-
	linda:rd({msg:Y}),
		sys:concat('consumer ',N,' got = ',Y,Line),
		writeln(Line),
		fail.
consumer(N) :-
	sys:concat('Done ',N,Msg),
	writeln(Msg).

