% Iterative factorial: uses no env, choice, or trail space

% Use ints...

fac(N,F) :-
	factorial(N,1,F).

% Use floats...

facf(N,F) :-
	factorial(N,1.0,F).

% Use bignums...

facb(N,F) :-
	factorial(N,1B,F).

factorial(0,F,F).
factorial(N,T,F) :-
	N > 0,
	T1 is T * N,
	N1 is N - 1,
	factorial(N1,T1,F).

test :-
	write('TEST: fac'), nl,

	fac(5,F1),
	F1 = 120,
	write('fac(5)='), write(F1), write(' PASSED'), nl,

	fac(20,F2),
	F2 = 2432902008176640000,
	write('fac(20)='), write(F2), write(' PASSED'), nl.
