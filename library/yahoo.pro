:-module(yahoo).
:-export([get_chart/2,get_quote/2,get_name/2,get_fields/3]).
:-import(library(http_client)).

:-define(CHART_SERVER,'https://ichart.finance.yahoo.com').
:-define(QUOTE_SERVER,'https://download.finance.yahoo.com').
:-define(DEFAULT_FIELDS,'spol1vbad1t1m').

:-define(FIELD_SYMBOL,'s').
:-define(FIELD_PREVIOUS_CLOSE,'p').
:-define(FIELD_OPEN,'o').
:-define(FIELD_LAST_TRADE,'l1').
:-define(FIELD_VOLUME,'v').
:-define(FIELD_BID,'b').
:-define(FIELD_BID_SIZE,'b6').
:-define(FIELD_ASK,'a').
:-define(FIELD_ASK_SIZE,'a5').
:-define(FIELD_LAST_TRADE_DATE,'d1').
:-define(FIELD_LAST_TRADE_TIME,'t1').
:-define(FIELD_LAST_TRADE_SIZE,'k3').
:-define(FIELD_NAME,'n').
:-define(FIELD_DAYS_RANGE,'m').

get_chart(Symbol,Data) :-
	atom(Symbol),
	sys:url_encode(Symbol,Symbol2),
	atomic_list_concat(['/table.csv?s=',Symbol2],Path),
	http_client:get11_data(?CHART_SERVER,Path,Data).

get_fields(Symbols,Fields,Data) :-
	atom(Symbols),
	sys:url_encode(Fields,Fields2),
	sys:url_encode(Symbols,Symbols2),
	atomic_list_concat(['/d/quotes?s=',Symbols2,'&d=t&f=',Fields2],Path),
	http_client:get11_data(?QUOTE_SERVER,Path,Data).

get_quote(Symbols,Data) :-
	get_fields(Symbols,?DEFAULT_FIELDS,Data).

get_name(Symbols,Data) :-
	get_fields(Symbols,?FIELD_NAME,Data).

