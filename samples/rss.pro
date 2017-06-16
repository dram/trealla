% Grab a Google Alerts feed and display each entry/title

:-module(rss).
:-export([fetch/2,test/0]).
:-using([sys,net,http]).

test :-
	fetch('alerts/feeds/04214518390159871182/17485558169717044541',Xml),
	I = 0,
	get_entry(Xml,I).

fetch(Code,Xml) :-
	client('www.google.com.au:80',S),
	get10(S,Code,Status),
	Status = 200,
	build(S,'',Xml).

build(S,S1,S2) :-
	bread(S,Len,Line),
	\+ at_end_of_stream(S),
	atomic_list_concat([S1,Line],S3),
	build(S,S3,S2).

build(_,S1,S2) :-
	S2 = S1.

get_entry(R,I) :-
	xmlq(R,I,'entry',Entry),
	xmlq(Entry,'title',Title),
	xmlq(Entry,'content',Content),
	I2 is I+1,
	write(I2),write('. '),writeln(Title),
	%write(Content),nl,
	get_entry(R,I2).

get_entry(_,_).

