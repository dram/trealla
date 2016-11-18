:-use_module(http_client).
:-using([sys,net]).

:-define(HOST_CHART,'ichart.finance.yahoo.com:80').
:-define(HOST_QUOTE,'download.finance.yahoo.com:80').

test1 :-
	quote('BHP.AX').

test2 :-
	chart('BHP.AX').

quote(Symbol) :-
	concat('/d/quotes?s=',Symbol,'&d=t&f=spol1vd1t1',Path),
	http_client:get11_data(?HOST_QUOTE,Path,Data),
	writeln(Data),
	true.

chart(Symbol) :-
	concat('/table.csv?s=',Symbol,'',Path),
	http_client:get11_data(?HOST_CHART,Path,Data),
	writeln(Data),
	true.
