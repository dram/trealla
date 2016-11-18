:-use_module(http_client).
:-using([sys]).

:-define(HOST_CHART,'ichart.finance.yahoo.com:80').
:-define(HOST_QUOTE,'download.finance.yahoo.com:80').

test1 :-
	get_quote('GOOG',L),
	writeln(L),
	true.

test2 :-
	get_chart('GOOG',L),
	writeln(L),
	true.

% Return a list of current values:
%
%	 [SYMBOL,PREV-CLOSE,OPEN,LAST-PRICE,VOLUME,BID,ASK,DATE,TIME]
%

get_quote(Symbol,L) :-
	concat('/d/quotes?s=',Symbol,'&d=t&f=spol1vbad1t1',Path),
	http_client:get11_data(?HOST_QUOTE,Path,Data),
	%writeln(Data),
	parse_csv(Data,L).

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
	line(L2,[],L).

line([],Old,Old).
line([Data|T],Old,New) :-
	parse_csv(Data,V),
	line(T,[V|Old],New).
