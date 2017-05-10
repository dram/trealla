maplist(P,[],[],[]).
maplist(P,[X1|X1s],[X2|X2s],[X3|X3s]) :-
	call(P,X1,X2,X3),maplist(P,X1s,X2s,X3s).

maplist(P,[],[],[],[]).
maplist(P,[X1|X1s],[X2|X2s],[X3|X3s],[X4|X4s]) :-
	call(P,X1,X2,X3,X4),maplist(P,X1s,X2s,X3s,X4s).

maplist(P,[],[],[],[],[]).
maplist(P,[X1|X1s],[X2|X2s],[X3|X3s],[X4|X4s],[X5|X5s]) :-
	call(P,X1,X2,X3,X4,X5),maplist(P,X1s,X2s,X3,X4s,X5s).

maplist(P,[],[],[],[],[],[]).
maplist(P,[X1|X1s],[X2|X2s],[X3|X3s],[X4|X4s],[X5|X5s],[X6|X6s]) :-
	call(P,X1,X2,X3,X4,X5,X6),maplist(P,X1s,X2s,X3,X4s,X5s,X6s).

maplist(P,[],[],[],[],[],[],[]).
maplist(P,[X1|X1s],[X2|X2s],[X3|X3s],[X4|X4s],[X5|X5s],[X6|X6s],[X7|X7s]) :-
	call(P,X1,X2,X3,X4,X5,X6,X7),maplist(P,X1s,X2s,X3,X4s,X5s,X6s,X7s).

prefix([],_).
prefix([X|L],[X|M]) :- prefix(L,M).

sublist([X|L],[X|M]) :- prefix(L,M),!.
sublist(L,[_|M]) :- sublist(L,M).

subst(X,[],Y,[]).
subst(X,[X|L],A,[A|M]) :- !, subst(X,L,A,M).
subst(X,[Y|L],A,[Y|M]) :- subst(X,L,A,M).
