:-module(updater,[start/0]).
:-export([quotes/0,charts/0]).
:-use_module(http_client).
:-use_module(dict).
:-using([sys]).

:-persist(quote/2).
:-persist(daily/2).

:-define(HOST_CHART,'http://ichart.finance.yahoo.com').
:-define(HOST_QUOTE,'http://download.finance.yahoo.com').
:-define(SYMBOLS,'forbes.txt').

start :-
	fail.

quotes :-
	load_file(?SYMBOLS,Data),
	split(Data,'\n',Symbols),
	maplist(save_quote,Symbols).

charts :-
	load_file(?SYMBOLS,Data),
	split(Data,'\n',Symbols),
	maplist(save_chart,Symbols).

save_quote(Symbol) :-
	writeln(Symbol),
	yahoo_quote(Symbol,L),
	dbs:log(assertz(quote(Symbol,L))).

save_chart(Symbol),
	writeln(Symbol),
	yahoo_chart(Symbol,L),
	dbs:log(assertz(daily(Symbol,L))).

yahoo_quote(Symbol,L) :-
	concat('/d/quotes?s=',Symbol,'&d=t&f=spol1vbad1t1',Path),
	http_client:get11_data(?HOST_QUOTE,Path,Data),
	%writeln(Data),
	split(Data,'\n',L1),
	line(L1,[],L).

yahoo_chart(Symbol,L) :-
	concat('/table.csv?s=',Symbol,'',Path),
	http_client:get11_data(?HOST_CHART,Path,Data),
	%writeln(Data),
	split(Data,'\n',L1),
	L1 = [_|L2],
	line(L2,[],L3),
	reverse(L3,L).

line([],Old,Old).
line([Data|T],Old,New) :-
	parse_csv(Data,V),
	line(T,[V|Old],New).
