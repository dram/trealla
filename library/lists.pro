member(X,X) :- var(X), !, fail.
member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

select(X,[X|T],T).
select(X,[H|T],[H|Rest]) :- select(X,T,Rest).

subtract([],_,[]) :- !.
subtract([Head|Tail],L2,L3) :- memberchk(Head,L2), !, subtract(Tail,L2,L3).
subtract([Head|Tail1],L2,[Head|Tail3]) :- subtract(Tail1,L2,Tail3).

union([],X,X).
union([X|R],Y,Z):- member(X,Y), !, union(R,Y,Z).
union([X|R],Y,[X|Z]):- union(R,Y,Z).

intersection([],X,[]).
intersection([X|R],Y,[X|Z]) :- member(X,Y), !, intersection(R,Y,Z).
intersection([X|R],Y,Z) :- intersection(R,Y,Z).

revzap([],L,L) :- !.
revzap([X|L],L2,L3) :- revzap(L,[X|L2],L3).
reverse(L1,L2) :- revzap(L1,[],L2).

append([],L,L).
append([H|T],L2,[H|L3]) :- append(T,L2,L3).

find(N,[],X) :- !.
find(1,[H|_],H) :- !.
find(N,[_|T],X) :- N1 is N-1, find(N1,T,X).
