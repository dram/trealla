:-module(dict).
:-export([get/3,lget/3,set/4,del/3]).

get([],N,0) :- !.
get([N:V|T],N,V) :- !.
get([H|T],N,V) :- get(T,N,V).

lget([],N,[]) :- !.
lget([N:V|T],N,V) :- !.
lget([H|T],N,V) :- lget(T,N,V).

set(L,N,V,[N:V|L2]) :- del(L,N,L2).

del([],N,[]) :- !.
del([N:_|T],N,T) :- !.
del([H|T],N,[H|L]) :- del(T,N,L).
