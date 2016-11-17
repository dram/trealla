:-function([fac/1]).

fac(0.0,1).
fac(0,1).
fac(N,F) :- N>0, N1 is N-1, fac(N1,F1), F is N*F1.

pfac(N) :- F is fac(N), write(F), nl.

test :-
	write('TEST: fac'), nl,

	F1 is fac(5),
	F1 = 120,
	write('fac(5)='), write(F1), write(' PASSED'), nl,

	F2 is fac(20),
	F2 = 2432902008176640000,
	write('fac(20)='), write(F2), write(' PASSED'), nl,

	F3 is fac(170.0), term_to_atom(F3,S3),
	S3 = '7.25741561530799e+306',
	write('fac(170.0)='), write(F3), write(' PASSED'), nl.

