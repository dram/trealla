% Not yet implemented

:- initialization(main).

main :-
	freeze(X, foo(X)),
	sys:sleep(1),
	X = hello,
	sleep(1),
	halt.

foo(X) :-
	writeln([got,X]).
