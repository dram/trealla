% An echo server

:-using([sys,net]).

echod :-
	server([':12345;tcp',':12345;udp'],S),
	repeat,
		readmsg(S,Line),
		write(S,Line),
		fail.

echo(Msg) :-
	client('localhost:12345;tcp',S),
	atom_concat(Msg,'\n',Msg2),
	write(S,Msg2),
	readmsg(S,Line),
	write(Line),
	close(S).

% A quick-brown-fox server

qbfd :-
	server([':12345;tcp',':12345;udp'],S),
	qbfd_send(S).

qbfd_send(S) :-
	repeat,
		sleep(1),
		now(Time),
		concat(Time,' thequickbrownfoxjumpedoverthelazydog\n',Msg),
		\+ write(S,Msg).

qbf :-
	client('localhost:12345;tcp',S),
	repeat,
		readmsg(S,Line) ->
			( write(Line), fail ) ;
			true.

% A distributed chat server (uses discovery)

chatd(Nick) :-
	munge(Nick,Nick2),
	atom_concat(';name=',Nick2,S2),
	server([';scope=CHAT',S2],S),
	repeat,
		read(S,msg(From,Msg)),
		concat(From,': ',Msg,Msg2),
		writeln(Msg2),
		fail.

chat(Nick,To,Msg) :-
	atomic(Msg),
	munge(Nick,Nick2),
	munge(To,To2),
	atom_concat(';scope=CHAT;name=',To2,S2),
	client(S2,S),
		writeln(S,msg(Nick2,Msg)),
		close(S).

