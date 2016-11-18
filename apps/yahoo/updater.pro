:-module(updater,[test1/0,test2/0,start/0]).
:-use_module(http_client).
:-use_module(dict).
:-using([sys]).

:-persist(quote/2).

:-define(HOST_CHART,'ichart.finance.yahoo.com:80').
:-define(HOST_QUOTE,'download.finance.yahoo.com:80').

test1 :-
	dbs:load,
	get_quote('GOOG',L),
	%writeln(L),
	update_quote(L).

test2 :-
	dbs:load,
	get_chart('GOOG',L),
	%writeln(L),
	update_chart(L).

%

update_quote(L) :-
	find(1,L,Sym),
	find(2,L,Prev),
	find(3,L,Open),
	find(4,L,Last),
	find(5,L,Vol),
	find(6,L,Bid),
	find(7,L,Ask),
	find(8,L,Date),
	find(9,L,Time),
	dict:set([],'prev',Prev,D1),
	dict:set(D1,'open',Open,D2),
	dict:set(D2,'last',Last,D3),
	dict:set(D3,'vol',Vol,D4),
	dict:set(D4,'bid',Bid,D5),
	dict:set(D5,'ask',Ask,D6),
	dict:set(D6,'date',Date,D7),
	dict:set(D7,'time',Time,D),
	dbs:begin,
	retract(quote(Symbol,_)).
	assertz(quote(Symbol,D)).
	dbs:end,
	true.
update_quote(L).

%

update_chart(L) :-
	fail.

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
	L1 = [H|L2],
	reverse(L2,L).
