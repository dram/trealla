:-dynamic(rec1/2).
:-dynamic(rec2/2,[persist]).
:-dynamic(rec3/2,[storage]).

test :-
	assertz(rec1(2,two)),
	assertz(rec1(3,three)),
	asserta(rec1(1,one)),
	retract(rec1(X,Y)), write(X), write(' ==> '), write(Y), nl,
	fail.

test1 :-
	dbs:load,
	dbs:begin,
	assertz(rec1(2,two)),
	assertz(rec1(3,three)),
	asserta(rec1(1,one)),
	dbs:end,
	retract(rec1(X,Y)), write(X), write(' ==> '), write(Y), nl,
	fail.

test1 :-
	listing(rec1).

test2 :-
	dbs:load,
	listing(rec2),
	dbs:begin,
	assertz(rec2(2,two)),
	assertz(rec2(3,three)),
	asserta(rec2(1,one)),
	dbs:end,
	retract(rec2(X,Y)), write(X), write(' ==> '), write(Y), nl,
	fail.

test2 :-
	listing(rec2).

test3 :-
	dbs:load,
	listing(rec2),
	dbs:begin,
	assertz(rec2(2,two)),
	assertz(rec2(3,three)),
	asserta(rec2(1,one)),
	dbs:end,
	fail.

test3 :-
	listing(rec2).

