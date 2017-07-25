:-module(lists).
:-export([member/2]).
:-export([memberchk/2]).
:-export([select/3]).
:-export([subtract/3]).
:-export([union/3]).
:-export([intersection/3]).
:-export([reverse/2]).
:-export([append/3]).
:-export([nth/3, nth1/3, nth0/3]).
:-export([put/1, put/2]).

member(X, X) :- var(X), !, fail.
member(X, [X|_]).
member(X, [_|T]) :- member(X,T).

memberchk(T, L) :- member(T, L), !.

select(X, [X|T], T).
select(X, [H|T], [H|Rest]) :- select(X, T, Rest).

subtract([], _, []) :- !.
subtract([H|T], L2, L3) :- memberchk(H, L2), !, subtract(T, L2, L3).
subtract([H|T1], L2, [H|T3]) :- subtract(T1, L2, T3).

union([], L, L).
union([H|T], Y, Z):- member(H, Y), !, union(T, Y, Z).
union([H|T], Y, [H|Z]):- union(T, Y, Z).

intersection([], L, []).
intersection([H|T], Y, [H|Z]) :- member(H, Y), !, intersection(T, Y, Z).
intersection([H|T], Y, Z) :- intersection(T, Y, Z).

revzap([], L, L) :- !.
revzap([H|L], L2, L3) :- revzap(L, [H|L2], L3).
reverse(L1, L2) :- revzap(L1, [], L2).

append([], L, L).
append([H|T], L2, [H|L3]) :- append(T, L2, L3).

nth(1, [H|_], H).
nth(N, [_|T], H) :- nth(M, T, H), N is M + 1.

nth1(1, [H|_],H).
nth1(N, [_|T], H) :- nth1(M, T, H), N is M + 1.

nth0(0, [H|_], H).
nth0(N, [_|T], H) :- nth0(M, T, H), N is M + 1.

