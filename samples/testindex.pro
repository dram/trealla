:-dynamic(f/1).
:-dynamic(g/2).
:-define(ITEMS,10000).

:-use_module(sys).

test1a :-
	write('Load...'), nl,
	between(1,?ITEMS,I),
		assertz(f(I)),
		fail.
test1a :-
	write('Search using 1st-arg index...'), nl,
	between(1,?ITEMS,I),
		f(I),
		fail.
test1a :-
	write('Done... '), write(?ITEMS), write(' items'), nl,
	true.

test1b :-
	write('Load...'), nl,
	between(1,?ITEMS,I),
		assertz(g(I,I)),
		fail.
test1b :-
	write('Search NOT using an index...'), nl,
	between(1,?ITEMS,I),
		g(_,I),
		fail.
test1b :-
	write('Done... '), write(?ITEMS), write(' items'), nl,
	true.

test2a :-
	write('Load...'), nl,
	between(1,?ITEMS,I),
		assertz(f(I)),
		fail.
test2a :-
	write('Iterate over set...'), nl,
	f(X),
		fail.
test2a :-
	write('Done... '), write(?ITEMS), write(' items'), nl,
	true.

test2b :-
	write('Load...'), nl,
	between(1,?ITEMS,I),
		assertz(f(I)),
		fail.
test2b :-
	write('Use findall...'), nl,
	findall(N,f(N),L),
	length(L,Count),
	write('Done... '), write(Count), write(' items'), nl,
	true.

test3 :-
	write('Load...'), nl,
	between(1,?ITEMS,I),
		assertz(g(I,I)),
		fail.
test3 :-
	write('Iterate over 2nd-arg...'), nl,
	g(_,X),
		fail.
test3 :-
	write('Done... '), write(?ITEMS), write(' items'), nl,
	true.

test4 :-
	write('Load...'), nl,
	between(1,?ITEMS,I),
		assertz(f(I)),
		fail.
test4 :-
	write('Retract...'), nl,
	retract(f(X)),
		fail.
test4 :-
	write('Done... '), write(?ITEMS), write(' items'), nl,
	true.

test5 :-
	write('Load...'), nl,
	between(1,10,I),
		between(1,?ITEMS,J),
			assertz(f(J)),
			fail.
test5 :-
	write('Search using 1st-arg index...'), nl,
	between(1,?ITEMS,I),
		once(f(I)),
		%write(I), nl,
		fail.
test5 :-
	write('Done... '), write(?ITEMS), write(' items'), nl,
	true.

