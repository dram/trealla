:-use_module(http_client).
:-using([sys]).

:-define(HOST_CHART,'http://ichart.finance.yahoo.com').
:-define(HOST_QUOTE,'http://download.finance.yahoo.com').

test1 :-
	yahoo_quote('GOOG',L),
	writeln(L),
	true.

test2 :-
	yahoo_chart('GOOG',L),
	writeln(L),
	true.

% Return a list of current values:
%
%	 [SYMBOL,PREV-CLOSE,OPEN,LAST-PRICE,VOLUME,BID,ASK,DATE,TIME]
%

yahoo_quote(Symbol,L) :-
	concat('/d/quotes?s=',Symbol,'&d=t&f=spol1vbad1t1',Path),
	http_client:get11_data(?HOST_QUOTE,Path,Data),
	%writeln(Data),
	split(Data,'\n',L1),	% one line per symbol
	line(L1,[],L).			% parse each line

% Return a list of daily values, each of
% which is a list of values...
%
%	 [DATE,OPEN,HIGH,LOW,CLOSE,VOLUME,ADJ-CLOSE]

yahoo_chart(Symbol,L) :-
	concat('/table.csv?s=',Symbol,'',Path),
	http_client:get11_data(?HOST_CHART,Path,Data),
	%writeln(Data),
	split(Data,'\n',L1),	% one line per day
	L1 = [_|L2],			% skip CSV header
	line(L2,[],L3),			% parse each line
	reverse(L3,L).

line([],Old,Old).
line([Data|T],Old,New) :-
	parse_csv(Data,V),
	line(T,[V|Old],New).
