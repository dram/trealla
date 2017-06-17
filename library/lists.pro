member(X,X) :- var(X), !, fail.
member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

select(X,[X|T],T).
select(X,[H|T],[H|Rest]) :- select(X,T,Rest).

subtract([],_,[]) :- !.
subtract([H|T],L2,L3) :- memberchk(H,L2), !, subtract(T,L2,L3).
subtract([H|T1],L2,[H|T3]) :- subtract(T1,L2,T3).

union([],L,L).
union([H|T],Y,Z):- member(H,Y), !, union(T,Y,Z).
union([H|T],Y,[H|Z]):- union(T,Y,Z).

intersection([],L,[]).
intersection([H|T],Y,[H|Z]) :- member(H,Y), !, intersection(T,Y,Z).
intersection([H|T],Y,Z) :- intersection(T,Y,Z).

revzap([],L,L) :- !.
revzap([H|L],L2,L3) :- revzap(L,[H|L2],L3).
reverse(L1,L2) :- revzap(L1,[],L2).

append([],L,L).
append([H|T],L2,[H|L3]) :- append(T,L2,L3).

find(N,[],L) :- !.
find(1,[H|_],H) :- !.
find(N,[_|T],L) :- N1 is N-1, find(N1,T,L).

% Misc other stuff that belongs elsewhere

recorda(K,V) :- recorda(K,V,_).
recorda(K,V,R) :- nonvar(K), nonvar(V), var(R), asserta('$record'(K,V),R).

recordz(K,V) :- recordz(K,V,_).
recordz(K,V,R) :- nonvar(K), nonvar(V), var(R), assertz('$record'(K,V),R).

recorded(K,V) :- recorded(K,V,_).
recorded(K,V,R) :- clause('$record'(K,V),_,R).

current_key(K) :- var(K), '$record'(K,_).

instance(R,V) :- nonvar(R), clause('$record'(_,V),_,R).

atomic_concat(L,R,S) :- atomic_list_concat([L,R],S).

atomic_list_concat([],'').
atomic_list_concat([H|T],S) :- atomic_list_concat(T,S2), !, '$concat'(H,S2,S).
atomic_list_concat([],_,'').

atomic_list_concat([H|T],Sep,S) :- atomic_list_concat(T,Sep,S2), !,
	(S2 \= '' -> '$concat'(H,Sep,S2,S) ; '$concat'(H,S2,S)), !.

display(T) :- write_term(T,[ignore_ops(true)]).
display(S,T) :- write_term(S,T,[ignore_ops(true)]).

put(C) :- integer(C) -> put_code(C) ; put_char(C).
put(S,C) :- integer(C) -> put_code(S,C) ; put_char(S,C).
