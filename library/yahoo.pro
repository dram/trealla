:-module(yahoo).
:-export([get_chart/2,get_quote/2]).
:-import(library(http_client)).
:-define(CHART_SERVER,'https://ichart.finance.yahoo.com').
:-define(QUOTE_SERVER,'http://download.finance.yahoo.com').

get_chart(Symbol,Data) :-
	atom(Symbol),
	sys:concat('/table.csv?s=',Symbol,Path),
	http_client:get11_data(?CHART_SERVER,Path,Data).

get_quote(Symbol,Data) :-
	atom(Symbol),
	sys:concat('/d/quotes?s=',Symbol,'&d=t&f=spol1vbad1t1',Path),
	http_client:get11_data(?QUOTE_SERVER,Path,Data).
