:- initialization(main).

main :-
	main0, main1, main2, halt.

main0 :-
	S = fun(X,Y,Z),
	V = [X,Y,Z],
	setv(V,1),
	writeln(S).

main1 :-
	S = fun(X,Y,Z),
	term_variables(S, V),
	setv(V,1),
	writeln(S).

main2 :-
	Atom = 'fun(X,Y,Z)',
	read_term_from_atom(Atom, S, [variables(V)]),
	setv(V,1),
	writeln(S).

setv([],_).
setv([H|T],N) :-
	H is N, N1 is N+1, setv(T,N1).
