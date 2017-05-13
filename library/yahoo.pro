:-module(yahoo).
:-export([get_chart/2,get_quote/2]).
:-import(library(http_client)).
:-define(CHART_SERVER,'https://ichart.finance.yahoo.com').
:-define(QUOTE_SERVER,'https://download.finance.yahoo.com').

get_chart(Symbol,Data) :-
	atom(Symbol),
	sys:url_encode(Symbol,Symbol2),
	sys:concat('/table.csv?s=',Symbol2,Path),
	http_client:get11_data(?CHART_SERVER,Path,Data).

get_quote(Symbols,Data) :-
	atom(Symbols),
	sys:url_encode(Symbols,Symbols2),
	sys:concat('/d/quotes?s=',Symbols2,'&d=t&f=spol1vbad1t1',Path),
	http_client:get11_data(?QUOTE_SERVER,Path,Data).
