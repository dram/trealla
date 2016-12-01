% Spawn a proc & exchange messages

:-use_module(sys).
:-use_module(proc).

test :-
	fork,
	parent.

test :-
	wait.

parent :-
	N=100000,
	spawn(child(N)),
	write('### Spawn and rendezvous '),write(N),write(' messages ==> '),
	flush_output,
	between(1,N,I),
		rsvp(I,I),
		fail.

parent :-
	writeln('Done.'),
	abort_wait.

child(N) :-
	between(1,N,I),
		recv(Msg),
		send(Msg),
		fail.

child(N).

% Spawn a proc and exchange 10 messages:

test0 :-
	fork,
	spawn(child0),
	parent0.

test0 :-
	wait.

parent0 :-
	N=10,
	concat('Synchronous exchange ',N,' messages...',Msg),
	writeln(Msg),
	between(1,N,I),
		rsvp(I,Msg),
		concat('Parent got: ',Msg,Msg2),
		writeln(Msg2),
		fail.

parent0 :-
	rsvp(false,Msg),
	concat('Parent got: ',Msg,Msg2),
	writeln(Msg2),
	abort_wait.

child0 :-
	repeat,
		recv(Msg),
		child0_handle(Msg).

child0_handle(Msg) :-
	Msg \= false,
	send(Msg),
	!, fail.

child0_handle(_) :-
	writeln('Child finished.'),
	send(false).

% Test timeout after 3 sec

test1 :-
	fork,
	writeln('Test spawning child with timeout after 3 secs'),
	spawn(child1),
	recv(Resp),
	concat('Parent got: ',Resp,Msg),
	writeln(Msg),
	abort_wait.

test1 :-
	wait.

child1 :-
	tmo(3000),
	recv(Msg),
	send(timeout).

child1 :-
	writeln('Child timeout!'),
	send(timeout).

% Test spawning procs, rendezvous with each

test2 :-
	fork,
	parent2(10000).

test2 :-
	wait.

parent2(N) :-
	concat('Test spawning ',N,' procs...',Msg),
	writeln(Msg),
	between(1,N,I),
		spawn(I,child2),
		fail.

parent2(N) :-
	procinfo(pids,Procs),
	atomic-concat('Processes: ',Procs,Msg),
	writeln(Msg),
	writeln('Now RSVP...'),
	between(1,N,I),
		rsvp(I,I,I),
		fail.

parent2(_) :-
	writeln('Done...'),
	procinfo(pids,Procs),
	concat('Processes: ',Procs,Msg),
	writeln(Msg),
	writeln('Parent abort wait'),
	abort_wait.

child2 :-
	recv(Msg),
	send(Msg).

% Test spawning procs with 3 sec timeout

test3 :-
	fork,
	parent3.

test3 :-
	writeln('Waiting...'),
	wait.

parent3 :-
	N=10000,
	concat('Test spawning ',N,' procs with 3 sec timeout',Msg),
	writeln(Msg),
	between(1,N,_),
		spawn(child3),
		fail.

parent3 :-
	procinfo(pids,N),
	concat('Processes: ',N,Msg),
	writeln(Msg),
	writeln('Parent done'),
	sleep(4),
	abort_wait.

child3 :-
	tmo(3000),
	recv(_),
	true.

% Create a ring of procs, then
% send 2 messages racing around the ring, then
% flag it everytime it passes go

test4 :-
	fork,
	parent4(10000),
	abort_wait.

test4 :-
	wait.

parent4(N) :-
	concat('Test spawning ',N,' procs in a ring...',Msg),
	writeln(Msg),
	between(1,N,I),
		spawn(I,child4(I,N)),
		fail.

parent4(N) :-
	writeln('Ready, now send 2 messages round and round...'),
	send(1,'the quick brown fox'),
	send(1,'jumped over the lazy dog'),
	sleep(10),
	writeln('Parent done').

child4(I,N) :-
	repeat,
		recv(Msg),
		Next is (I mod N)+1,
		send(Next,Msg),
		I=1,
		timestamp(T),
		term_to_atom(T,S),atom_concat(S,': Zoom got: ',S2),
		atom_concat(S2,Msg,S3),
		writeln(S3),
		fail.

% Test abort after 3 seconds

test5 :-
	fork,
	writeln('Test spawning child and aborting it after 3 seconds'),
	spawn(child5),
	pid(Pid),
	sleep(3),
	abort(Pid),
	sleep(1),
	abort_wait.

test5 :-
	wait.

child5 :-
	recv(Msg).

% Test receive with case

test6 :-
	fork,
	spawn(child6_priority),
	send(4),send(17),send(1),send(12),
	sleep(3),
	abort_wait.

test6 :-
	wait.

child6_priority :-
	hmsleep(250),   	   % hard-sleep
	tmo(0),
	recv(Level),
	after -> child6_normal ;
	(Level > 10 -> child6_do(priority,Level) ; undo(Level)),
	child6_priority.

child6_priority.

child6_normal :-
	repeat,
		hmsleep(250),      % hard-sleep
		tmo(0),
		recv(Level),
		(after -> abort(self) ; child6_do(normal,Level)),
		fail.

child6_do(Type, Level) :-
	write('Got '),write(Type),write(' message,level: '),
	writeln(Level).

