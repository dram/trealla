:-initialization(main).
:-import(library(yahoo)).

main :-
	chart('GOOG'),
	halt.

chart(Symbol) :-
	yahoo:get_chart(Symbol,Data),
	writeln(Data).

