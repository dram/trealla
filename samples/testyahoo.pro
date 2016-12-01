:-use_module(http_client).
:-use_module(sys).

:-define(CHART_SERVER,'http://ichart.finance.yahoo.com').
:-define(QUOTE_SERVER,'http://download.finance.yahoo.com').

test1 :-
	quote('GOOG').

test2 :-
	chart('GOOG').

quote(Symbol) :-
	yahoo_quote(Symbol,Data),
	writeln(Data),
	true.

chart(Symbol) :-
	yahoo_chart(Symbol,Data),
	writeln(Data),
	true.

yahoo_quote(Symbol,Data) :-
	concat('/d/quotes?s=',Symbol,'&d=t&f=spol1vbad1t1',Path),
	http_client:get11_data(?QUOTE_SERVER,Path,Data).

yahoo_chart(Symbol,Data) :-
	concat('/table.csv?s=',Symbol,Path),
	http_client:get11_data(?CHART_SERVER,Path,Data).
