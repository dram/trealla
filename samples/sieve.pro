primes(Limit,Ps) :-
	integers(2,Limit,Is),
	sift(Is,Ps), !.

integers(Low,High,[Low|Rest]) :-
	Low =< High, !,
	M is Low+1,
	integers(M,High,Rest).
integers(_,_,[]).

sift([],[]).
sift([I|Is],[I|Ps]) :-
	remove(I,Is,New),
	sift(New,Ps).

remove(_,[],[]).
remove(P,[I|Is],Nis) :-
	0 is I mod P, !,
	remove(P,Is,Nis).
remove(P,[I|Is],[I|Nis]) :-
	X is I mod P,
	X \= 0, !,
	remove(P,Is,Nis).

test :-
	write('TEST: '),
	primes(100, X),
	X=[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97],
	write(' PASSED'), nl.

test1 :- primes(10, X), write(X), nl.
test2 :- primes(100, X), write(X), nl.
test3 :- primes(1000, X), write(X), nl.
test4 :- primes(5000, X), write(X), nl.
test5 :- primes(10000, X), write(X), nl.
test6 :- primes(50000, X), write(X), nl.
test7 :- primes(100000, X), write(X), nl.

