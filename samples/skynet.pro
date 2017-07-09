% Skynet - see https://github.com/atemerev/skynet

:-initialization(main).
:-using([proc]).

main :-
	run_skynet(1000000),
	halt.

run_skynet(Size) :-
	fork, !,
	spawn_link(skynet(0,Size,100)),
	recv(Tot),
	write('###=> '),write(Tot),nl,
	end_wait.
run_skynet(_) :-
	wait.

skynet(Num,1,_) :-
	send(Num).
skynet(Num,Size,Div) :-
	NewSize is Size div Div,
	between(1,Div,Idx),
		NewNum is ((Idx - 1) * NewSize) + Num,
		spawn_link(skynet(NewNum,NewSize,Div)),
		fail.
skynet(_,_,Div) :-
	process_sum(0,Div).

process_sum(Tot,0) :-
	send(parent,Tot).
process_sum(Tot,Idx) :-
	recv(N),
	NewTot is Tot + N,
	NewIdx is Idx - 1,
	process_sum(NewTot,NewIdx).
