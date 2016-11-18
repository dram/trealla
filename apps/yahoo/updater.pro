:-module(updater,[start_quotes/0,start_charts/0]).
:-use_module(http_client).
:-use_module(dict).
:-using([sys]).

:-persist(quote/2).
:-persist(chart/2).

:-define(HOST_CHART,'ichart.finance.yahoo.com:80').
:-define(HOST_QUOTE,'download.finance.yahoo.com:80').

start_quotes :-
	dbs:load,
	update_quote('GOOG').

update_quote(Symbol) :-
	get_quote(Symbol,L),
	assertz(quote(Symbol,L)).

start_charts :-
	dbs:load,
	update_chart('GOOG').

update_chart(Symbol),
	get_chart(Symbol,L),
	assertz(chart(Symbol,L)).


% Return a list of current values:
%
%	 [SYMBOL,PREV-CLOSE,OPEN,LAST-PRICE,VOLUME,BID,ASK,DATE,TIME]
%

get_quote(Symbol,L) :-
	concat('/d/quotes?s=',Symbol,'&d=t&f=spol1vbad1t1',Path),
	http_client:get11_data(?HOST_QUOTE,Path,Data),
	%writeln(Data),
	parse_csv(Data,L1),
	L1 = [_|L].

% Return a list of daily values, each of
% which is a list of values...
%
%	 [DATE,OPEN,HIGH,LOW,CLOSE,VOLUME,ADJ-CLOSE]

get_chart(Symbol,L) :-
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
