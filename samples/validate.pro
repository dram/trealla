% This is just a bunch of conformance tests.
% Hopefully more will get added over time.

%   swipl -q -l samples/validate.pro -t start --traditional
%   yap -q -l samples/validate.pro -z start,halt
%   tpl -q -l samples/validate.pro --start

start :- test1, test2, test3, test4a, test4b, test5a, test5b,
		test6a, test6b, test6c, test7, test8, test9a, test9b, test9c,
		test10, test11.

test1 :-
	write('Test1:\t'),
	F = f(a,_,c),
	functor(F,f,3),
	arg(2,F,b),
	F = f(a,b,c),
	write('PASSED!'), nl.

test2 :-
	write('Test2:\t'),
	F = f(a,b,c),
	copy_term(F,C),
	F=C,
	write('PASSED!'), nl.

test3 :-
	write('Test3:\t'),
	F = f(A,B,C),
	copy_term(F,X),
	A=a, B=b, C=c,
	F=X,
	arg(2,X,b),
	write('PASSED!'), nl.

test4a :-
	write('Test4a:\t'),
	(true -> true ; fail),
	write('PASSED!'), nl.

test4b :-
	write('Test4b:\t'),
	(fail -> fail ; true),
	write('PASSED!'), nl.

name(john).
name(mary).
name(tom).

test5a :-
	write('Test5a:\t'),
	(name(john) ; fail),
	write('PASSED!'), nl.

test5b :-
	write('Test5b:\t'),
	(fail ; name(mary)),
	write('PASSED!'), nl.

test6a :-
	write('Test6a:\t'),
	F = {a,b,c},
	functor(F,{},1),
	write('PASSED!'), nl.

test6b :-
	write('Test6b:\t'),
	F = [a,b,c],
	functor(F,'.',2),
	write('PASSED!'), nl.

test6c :-
	write('Test6c:\t'),
	F = (a,b,c),
	functor(F,',',2),
	write('PASSED!'), nl.

test7 :-
	write('Test7:\t'),
	(a,b,c) = (a,(b,c)),
	(1/2/3) = ((1/2)/3),
	write('PASSED!'), nl.

test8 :-
	write('Test8:\t'),
	arg(1,f(a,b,c),a),
	arg(2,f(a,b,c),b),
	arg(3,f(a,b,c),c),
	write('PASSED!'), nl.

test9a :-
	write('Test9a:\t'),
	arg(1,{a,b,c},(a,b,c)),
	write('PASSED!'), nl.

test9b :-
	write('Test9b:\t'),
	arg(1,[a,b,c],a),
	arg(2,[a,b,c],[b,c]),
	write('PASSED!'), nl.

test9c :-
	write('Test9c:\t'),
	arg(1,(a,b,c),a),
	arg(2,(a,b,c),(b,c)),
	write('PASSED!'), nl.

test10 :-
	write('Test10:\t'),
	Item = {
       'author': 'Philip K Dick',
       'works': [
          {'title': 'The Man in the High Castle'},
          {'title': 'Do Androids Dream of Electric Sheep'}
       ]
	},
	Item = {Author,_Works},
	Author = (_N:V),
	V == 'Philip K Dick',
	write('PASSED!'), nl.

age(peter,7).
age(anne,5).
age(pat,8).
age(tom,5).

test11 :-
	write('Test11:\t'),
	findall(Name,age(Name,Age),L1),
	L1 = [peter,anne,pat,tom],
	findall(Age,age(Name,Age),L2),
	L2 = [7,5,8,5],
	write('PASSED!'), nl.

