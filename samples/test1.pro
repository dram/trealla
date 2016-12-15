test1 :-
  \+ (I = 1, \+ write(I)), nl, halt.

upto(N, X) :- N > 0, N1 is N - 1, upto(N1, X).
upto(N, X) :- N > 0, X = N.

test2 :-
  \+ (upto(3, I), upto(I, J), \+ (write([I, J]), nl)), nl, halt.

qux3(A, B) :- write(A), nl.
foo3([]).
foo3([X|Rest]) :- qux3(X, a), foo3(Rest).
bar3(b(1)).
baz3(A, B, C, D) :- D = ''.

test3 :- foo3([a, b]), bar3(B), baz3(a, b, C, D), fail.
test3 :- nl, halt.

bar4(B) :- write(B), nl, halt.
foo4(A) :- bar4([A]).

test4 :- foo4(a).

baz5([]).
baz5([_|T]) :- baz5(T).

bar5(A, _) :- baz5(A).

foo5(A) :- bar5(A, _), write(A), nl.

test5 :- foo5([a]), foo5([b]), fail.
test5 :- halt.

:-dynamic(age6/2).

test6 :-
	assert(age6(abc,123)),
	findall(Name,age6(Name,Age),L),
	writeln(L).

test7 :-
    open('foo.out', write, S),
    write(S, foo), nl(S),
    close(S),
    halt.

test8 :-
	close.
