:-initialization(main).
:-import(library(yahoo)).

main :-
	yahoo:get_quote('GOOG,IBM,AAPL,MSFT',Data),
	writeln('Raw data...'),
	write(Data),
	sys:split(Data,'\n',List),
	writeln('Processed data...'),
	print(List),
	halt.

print([]).
print([H|T]) :-
	sys:parse_csv(H,H2),
	writeln(H2),
	print(T).
