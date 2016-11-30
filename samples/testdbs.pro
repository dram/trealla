:-dynamic(rec1/2).
:-dynamic(rec2/2,[persist]).
:-dynamic(rec3/2,[storage]).

test :-
	assertz(rec1(2,two)),
	assertz(rec1(3,three)),
	asserta(rec1(1,one)),
	retract(rec1(X,Y)), write(X), write(' ==> '), write(Y), nl,
		fail.
test :-
	true.

test1 :-
	dbs:load,
	dbs:begin,
	assertz(rec1(2,two)),
	assertz(rec1(3,three)),
	asserta(rec1(1,one)),
	dbs:end,
	retract(rec1(X,Y)), write(X), write(' ==> '), write(Y), nl,
		fail
test1 :-
	true.

test2 :-
	dbs:load,
	dbs:begin,
	assertz(rec2(2,two)),
	assertz(rec2(3,three)),
	asserta(rec2(1,one)),
	dbs:end,
	retract(rec2(X,Y)), write(X), write(' ==> '), write(Y), nl,
		fail.
test2 :-
	true.

test3a :-
	dbs:load,
	%dbs:begin,
	assertz(rec3(2,two)),
	assertz(rec3(3,three)),
	asserta(rec3(1,one)),
	%dbs:end,
	true.

test3b :-
	dbs:load,
	rec3(X,Y), write(X), write(' ==> '), write(Y), nl,
		fail.
test3b :-
	true.

