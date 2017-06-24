:-module(lists).
:-export([member/2]).
:-export([select/3]).
:-export([subtract/3]).
:-export([union/3]).
:-export([intersection/3]).
:-export([reverse/2]).
:-export([append/3]).
:-export([find/3]).
:-export([display/1,display/2]).
:-export([put/1,put/2]).

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

% These should be somewhere else, but for now...

display(T) :-
	write_term(T,[ignore_ops(true)]).

display(S,T) :-
	write_term(S,T,[ignore_ops(true)]).

put(C) :-
	integer(C) ->
		put_code(C) ;
		put_char(C).

put(S,C) :-
	integer(C) ->
		put_code(S,C) ;
		put_char(S,C).
