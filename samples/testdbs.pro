:-dynamic(rec0/2,[alpha]).
:-dynamic(rec1/2,[numeric]).
:-dynamic(rec2/2,[numeric,persist]).
:-dynamic(rec3/2,[numeric,persist]).
:-dynamic(rec4/3,[numeric,persist]).

test :-
	writeln('Phase1...'),
	between(1,10,I),
		sys:rand(R),
		J is (R mod 1000) + 1,
		assertz(rec0(J,I)),
		fail.
test :-
	%listing(rec0),
	writeln('Phase2...'),
	retract(rec0(X,Y)),
		write(X), write(' ==> '), write(Y), nl,
		fail.
test :-
	writeln('Done'),
	true.

test0a :-
	assertz(rec0(2,two)),
	assertz(rec0(3,three)),
	asserta(rec0(1,one)),
	retract(rec0(X,Y)),
		write(X), write(' ==> '), write(Y), nl,
		fail.
test0a :-
	true.

test0b :-
	assertz(rec0(2,two)),
	assertz(rec0(3,three)),
	asserta(rec0(1,one)),
	retract(rec0(1,Y1)), write(1), write(' ==> '), write(Y1), nl, !,
	retract(rec0(2,Y2)), write(2), write(' ==> '), write(Y2), nl, !,
	retract(rec0(3,Y3)), write(3), write(' ==> '), write(Y3), nl, !,
	true.

test0c :-
	assertz(rec0(2,two)),
	assertz(rec0(3,three)),
	asserta(rec0(1,one)),
	rec0(1,Y1), write(1), write(' ==> '), write(Y1), nl, !,
	rec0(2,Y2), write(2), write(' ==> '), write(Y2), nl, !,
	rec0(3,Y3), write(3), write(' ==> '), write(Y3), nl, !,
	true.

test1a :-
	assertz(rec1(2,two)),
	assertz(rec1(3,three)),
	asserta(rec1(1,one)),
	dbs:end,
	retract(rec1(X,Y)),
		write(X), write(' ==> '), write(Y), nl,
		fail.
test1a :-
	true.

test1b :-
	assertz(rec1(2,two)),
	assertz(rec1(3,three)),
	asserta(rec1(1,one)),
	retract(rec1(1,Y1)), write(1), write(' ==> '), write(Y1), nl, !,
	retract(rec1(2,Y2)), write(2), write(' ==> '), write(Y2), nl, !,
	retract(rec1(3,Y3)), write(3), write(' ==> '), write(Y3), nl, !,
	true.

test1c :-
	assertz(rec1(2,two)),
	assertz(rec1(3,three)),
	asserta(rec1(1,one)),
	rec1(1,Y1), write(1), write(' ==> '), write(Y1), nl, !,
	rec1(2,Y2), write(2), write(' ==> '), write(Y2), nl, !,
	rec1(3,Y3), write(3), write(' ==> '), write(Y3), nl, !,
	true.

test2a :-
	dbs:load,
	assertz(rec2(2,two)),
	assertz(rec2(3,three)),
	asserta(rec2(1,one)),
	true.

test2b :-
	dbs:load,
	retract(rec2(X,Y)),
		write(X), write(' ==> '), write(Y), nl,
		fail.
test2b :-
	true.

test2c :-
	dbs:load,
	retractall(rec2(X,Y)).
test2c :-
	true.

test3a :-
	dbs:load,
	between(1,100,I),
		sys:rand(R),
		J is (R mod 1000) + 1,
		assertz(rec3(J,I)),
		write(J), write(' ==> '), writeln(I),
		fail.
test3a :-
	true.

test3b :-
	dbs:load,
	retract(rec3(X,Y)),
		write(X), write(' ==> '), write(Y), nl,
		fail.
test3b :-
	true.

test4a :-
	dbs:load,
	between(1,100,I),
		sys:rand(R),
		J is (R mod 1000) + 1,
		dbs:begin,
		assertz(rec4(J,I,dummy)),
		dbs:end,
		write(J), write(' ==> '), writeln(I),
		fail.
test4a :-
	true.

test4b :-
	dbs:load,
	between(1,1000,I),
		sys:rand(R),
		J is (R mod 1000) + 1,
		retract(rec4(J,K,dummy)), !,
		write(J), write(' ==> '), writeln(K),
		fail.
test4b :-
	true.

test4c :-
	dbs:load,
	retract(rec4(J,K,dummy)),
		write(J), write(' ==> '), writeln(K),
		fail.
test4c :-
	true.

test4d :-
	dbs:load,
	between(1,1000,I),
		retract(rec4(I,K,dummy)), !,
		write(I), write(' --> '), writeln(K),
		fail.
test4d :-
	true.

test4e :-
	dbs:load,
	rec4(J,K,dummy),
		write(J), write(' ==> '), writeln(K),
		fail.
test4e :-
	true.

test5 :-
	writeln('Phase1...'),
	between(1,1000000,I),
		sys:rand(R),
		J is (R mod 1000) + 1,
		assertz(rec1(J,I)),
		%write(J), write(' ==> '), writeln(I),
		fail.
test5 :-
	writeln('Phase2...'),
	between(1,1000000,I),
		sys:rand(R),
		J is (R mod 1000) + 1,
		retract(rec1(J,K)), !,
		%write(I), write(' ==> '), write(J), write(' ==> '), writeln(K),
		fail.
test5 :-
	true.

test6 :-
	between(1,10,I),
		test5,
		retractall(rec1(_,_)),
		writeln('Done'),
		fail.
