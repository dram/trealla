:-initialization(main).

main :-
	proc:fork, !,
	linda:init([]),
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
	linda:in({msg:Y}),
		write('consumer '),write(N),write(' got = '),writeln(Y),
		fail.
consumer(N) :-
	sys:concat('Done ',N,Msg),writeln(Msg).

