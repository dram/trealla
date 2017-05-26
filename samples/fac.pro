fac(0,1).
fac(N,F) :- N>0, N1 is N-1, fac(N1,F1), F is F1*N.

test :-
	write('TEST: fac'), nl,

	fac(5,F1),
	F1 = 120,
	write('fac(5)='), write(F1), write(' PASSED'), nl,

	fac(20,F2),
	F2 = 2432902008176640000,
	write('fac(20)='), write(F2), write(' PASSED'), nl.
