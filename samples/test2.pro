:-module(test2,[test1/0,test2/0]).

:-dynamic(age/2,[storage]).

test1 :-
	dbs:load,
	sys:now(T),
	assert(age(T,fred)).

test2 :-
	dbs:load,
	findall(T,age(T,X),L),
	writeln(L).
