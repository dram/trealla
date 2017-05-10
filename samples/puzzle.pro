:-initialization(main).

%% Each sq is repr by sq(SqNum,Var,RowDig,ColDig,RegReg)
%% were XXXDig is a "bitmask" used(D1,...D9) and Dk is 1 iff digit k used in
%% that row/col/region else it's unbound.

main :-
	puzzle(P),
	make_cons(P,0,_,_,_,Cons),
	solve_cons(Cons),
	print_sol(P),
	halt.

xmember(X, [X|_]).
xmember(X, [_|T]) :- xmember(X, T).

xmaplist(_,[]).
xmaplist(P,[H|T]) :- call(P,H), xmaplist(P,T).

xselect(X,[X|T],T).
xselect(X,[H|T],[H|Rest]) :- xselect(X,T,Rest).

puzzle(P):-
	P=[_,_,_,_,_,_,_,1,2,
	   _,_,_,_,_,_,_,_,3,
	   _,_,2,3,_,_,4,_,_,
	   _,_,1,8,_,_,_,_,5,
	   _,6,_,_,7,_,8,_,_,
	   _,_,_,_,_,9,_,_,_,
	   _,_,8,5,_,_,_,_,_,
	   9,_,_,_,4,_,5,_,_,
	   4,7,_,_,_,6,_,_,_].

%% Make a constraint list using Rows, Cols and Boxs as
%% shared variables for each row, col and box.
%% Shared vars will be used to communicate digit choices
%% between different parts of the puzzle and allow
%% also to find the position that has the largest number
%% of clues at a given time during solution.

make_cons([X|Xs],I,Rows,Cols,Boxs,Out) :-
	get_row(I,Rows,R),
	get_col(I,Cols,C),
	get_box(I,Boxs,B),
	(
		var(X) -> Out=[sq(I,X,R,C,B)|Cs] ;
		(Cs=Out, set_dig(X,R), set_dig(X,C), set_dig(X,B))
	),
	I1 is I + 1,
	make_cons(Xs,I1,Rows,Cols,Boxs,Cs).
make_cons([],_,_,_,_,[]).

%% Extract given row, col or box from relevant structure
%% using "vector" of term arguments.

get_row(I,Rows,R) :-
	Rows = rows(_,_,_,_,_,_,_,_,_),
	%functor(Rows,rows,9),
	Rn is (I//9) + 1,
	arg(Rn,Rows,R).

get_col(I,Cols,C) :-
	Cols = cols(_,_,_,_,_,_,_,_,_),
	%functor(Cols,cols,9),
	Cn is (I mod 9) + 1,
	arg(Cn,Cols,C).

get_box(I,Boxs,B) :-
	Boxs = boxs(_,_,_,_,_,_,_,_,_),
	%functor(Boxs,boxs,9),
	Rn is I//9,
	Cn is I mod 9,
	C1 is Cn//3,
	R1 is Rn//3,
	Bn is (R1*3) + C1 + 1,
	arg(Bn,Boxs,B).

%% Set a digit D into the "bitmask" -- a structure
%% that looks like used(D1,...D9).
%% An arg of the struct is "1" if that digit has been "set"
%% otherwise it will be left unbound.
%% This repr allows bitmasks to be "unioned" using unification.

set_dig(D,Used) :-
	Used = used(_,_,_,_,_,_,_,_,_),
	%functor(Used,used,9),
	arg(D,Used,Bit), 	%% D is 1-9 -- a legal arg index
	var(Bit),			%% can only set "bit" if not already set
	Bit=1.

%% Solve list of constraints, best one first.
%% The best constraint to solve is the one with the most clues
%% aka the most restrictive.

solve_cons([]) :- !.
solve_cons(Cons) :-
	get_sq(Sq,Cons,Rest),
	solve1(Sq),
	solve_cons(Rest).

%% Find the constraint Sq in the list Cons that
%% has the least number of un-tried digits (aka unbound "bits").

get_sq(Sq,Cons,Rest) :-
	length(_,N),		%% generate 0..inf -- incr depending
	xselect(Sq,Cons,Rest),
	Sq=sq(_,V,_,_,_),
	var(V),				%% make sure sq has not been solved already
	num_vars(Sq,N),		%% count number of unused digits in Sq
	!.					%% take 1st answer

%% Union the "bitmasks" for row, col and box and count
%% the number of unbound bits. Make sure not to change
%% the bitmasks still associated with row, col, box in the
%% puzzle config.

num_vars(Sq,Num) :-
	copy_term(Sq,Sq1),	%% don't pollute the data take a copy
	Sq1=sq(_,_,R,C,B),
	R=C,				%% union of bits set in row, col and box
	C=B,
	num_vars1(R,Num).

%% Count num vars in "bitmask". Long form is 2x speed. This
%% pred seems to be in critical path.

num_vars1(used(X1,X2,X3,X4,X5,X6,X7,X8,X9),Num) :-
	N1 is 0,
	(var(X1) -> N2 is N1+1 ; N2 = N1),
	(var(X2) -> N3 is N2+1 ; N3 = N2),
	(var(X3) -> N4 is N3+1 ; N4 = N3),
	(var(X4) -> N5 is N4+1 ; N5 = N4),
	(var(X5) -> N6 is N5+1 ; N6 = N5),
	(var(X6) -> N7 is N6+1 ; N7 = N6),
	(var(X7) -> N8 is N7+1 ; N8 = N7),
	(var(X8) -> N9 is N8+1 ; N9 = N8),
	(var(X9) -> Num is N9+1; Num = N9).

%% Solve 1 square -- i.e. find a digit that can be legally assigned
%% by ref to bitmask in associated row, col and box.

solve1(sq(_,V,R,C,B)) :-
	digit(V),
	set_dig(V,R),
	set_dig(V,C),
	set_dig(V,B).

digit(D) :-
	xmember(D,[1,2,3,4,5,6,7,8,9]).

print_sol(X) :-
	print_row(X,X1),
	print_sol(X1).
print_sol([]).

print_row([X1,X2,X3,X4,X5,X6,X7,X8,X9|Rest],Rest) :-
	xmaplist(write,[X1,' ',X2,' ',X3,'  ',X4,' ',X5,' ',X6,'  ',X7,' ',X8,' ',X9]),
	nl.

