:-use_module(http_client).
:-using([sys,net]).

:-define(HOST_CHART,'ichart.finance.yahoo.com:80').
:-define(HOST_QUOTE,'download.finance.yahoo.com:80').

test1 :-
	quote('GOOG').

test2 :-
	chart('GOOG').

quote(Symbol) :-
	concat('/d/quotes?s=',Symbol,'&d=t&f=spol1vd1t1',Path),
	http_client:get11_data(?HOST_QUOTE,Path,Data),
	parse_csv(Data,L),
	writeln(L),
	true.

chart(Symbol) :-
	concat('/table.csv?s=',Symbol,'',Path),
	http_client:get11_data(?HOST_CHART,Path,Data),
	writeln(Data),
	true.
