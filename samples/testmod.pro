:-module(testmod,[test/0,test1/0,test2/0]).
:-dynamic(rec1/1).
:-persist(rec2/1).

test :-
	assertz(rec1(2)),
	assertz(rec1(3)),
	asserta(rec1(1)),
	retract(rec1(X)), write(X), nl,
	fail.

test1 :-
	dbs:load,
	dbs:begin,
	assertz(rec1(2)),
	assertz(rec1(3)),
	asserta(rec1(1)),
	dbs:end,
	retract(rec1(X)), write(X), nl,
	fail.

test1 :-
	listing(rec1).

test2 :-
	dbs:load,
	dbs:begin,
	assertz(rec2(2)),
	assertz(rec2(3)),
	asserta(rec2(1)),
	dbs:end,
	retract(rec2(X)), write(X), nl,
	fail.

test2 :-
	listing(rec2).

