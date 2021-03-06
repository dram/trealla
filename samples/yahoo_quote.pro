:-initialization(main).
:-import(library(yahoo)).

main :-
	yahoo:get_quote('GOOG,MSFT,IBM,AAPL',Data),
	writeln('Raw data...'),
	write(Data),
	writeln('Processed data...'),
	sys:split_all(Data,'\n',List),
	print(List),
	halt.

print([]).
print([H|T]) :-
	sys:parse_csv(H,Data),
	writeln(Data),
	print(T).
