:-using([proc]).

test :-
	fork,
	parent.

test :-
	wait.

parent :-
	spawn_link(exit('die')),
	receive {X,Y,Z},
	write('X = '),writeln(X),
	write('Y = '),writeln(Y),
	write('Z = '),writeln(Z),
	end_wait.

