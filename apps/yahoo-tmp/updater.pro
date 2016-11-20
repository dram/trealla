:-module(updater).
:-export([start/0]).
:-use_module(http_client).
:-use_module(dict).
:-using([sys]).

:-persist(quote/2).
:-persist(daily/2).

:-define(HOST_CHART,'http://ichart.finance.yahoo.com').
:-define(HOST_QUOTE,'http://download.finance.yahoo.com').
:-define(SYMBOLS,'forbes.txt').

start :-
	dbs:load,
	load_file(?SYMBOLS,Data),
	split(Data,'\n',Symbols),
	maplist(update_quote,Symbols).

update_quote(Symbol) :-
	writeln(Symbol),
	yahoo_quote(Symbol,L),
	assertz(quote(Symbol,L)).

update_chart(Symbol),
	writeln(Symbol),
	yahoo_chart(Symbol,L),
	assertz(daily(Symbol,L)).


% Return a list of current values:
%
%	 [SYMBOL,PREV-CLOSE,OPEN,LAST-PRICE,VOLUME,BID,ASK,DATE,TIME]
%

yahoo_quote(Symbol,L) :-
	concat('/d/quotes?s=',Symbol,'&d=t&f=spol1vbad1t1',Path),
	http_client:get11_data(?HOST_QUOTE,Path,Data),
	%writeln(Data),
	parse_csv(Data,L1),
	L1 = [_|L].

% Return a list of daily values, each of
% which is a list of values...
%
%	 [DATE,OPEN,HIGH,LOW,CLOSE,VOLUME,ADJ-CLOSE]

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
