:-using([proc,sys]).

test :-
	fork,
	parent.

test :-
	sys:delay(1000),
	wait.

parent :-
	spawn_link(exit('die')),
	recv(ErrMsg),
	ErrMsg = {X,Y,Z},
	write('X = '), writeln(X),
	write('Y = '), writeln(Y),
	write('Z = '), writeln(Z),
	end_wait.
