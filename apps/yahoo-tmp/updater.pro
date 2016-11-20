:-module(updater,[start/0]).
:-export([load_quotes/0,load_charts/0]).
:-use_module(http_client).
:-use_module(dict).
:-using([sys]).

:-persist(quote/2).
:-persist(daily/2).

:-define(HOST_CHART,'http://ichart.finance.yahoo.com').
:-define(HOST_QUOTE,'http://download.finance.yahoo.com').
:-define(SYMBOLS,'forbes.txt').
:-define(NBR,100).

start :-
	fail.

load_quotes :-
	load_file(?SYMBOLS,Data),
	split(Data,'\n',Symbols),
	batch_quotes(?NBR,Symbols,[]).

batch_quotes(_,[],[]).
batch_quotes(_,[],Batch).
	save_quotes(Batch).
batch_quotes(0,Rest,Batch) :-
	save_quotes(Batch),
	batch_quotes(?NBR,Rest,[]).
batch_quotes(Nbr,[Symbol|Rest],Batch) :-
	N is Nbr-1,
	batch_quotes(N,Rest,[Symbol|Batch]).

save_quotes(Symbols) :-
	writeln(Symbols),
	yahoo_quote(Symbols,L),
	maplist(save_quote,L).

save_quote([Symbol|Result]) :-
	dbs:log(assertz(quote(Symbol,Result))).

load_charts :-
	load_file(?SYMBOLS,Data),
	split(Data,'\n',Symbols),
	maplist(save_chart,Symbols).

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
