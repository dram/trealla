:-using([sys,proc]).

% Use discovery

echod1 :-
	fork,
	server([';name=ECHO'],Pid),
	repeat,
		recv(Pid,Msg),
		send(Pid,Msg),
		fail.

echod1 :-
	wait.

echo1(Msg) :-
	pid(';name=ECHO',Pid),
	rsvp(Pid,Msg,Resp),
	writeln('Got:',Resp).

% Use port 9000

echod2 :-
	fork,
	server([':9000'],Pid),
	repeat,
		recv(Pid,Msg),
		send(Pid,Msg),
		fail.

echod2 :-
	wait.

echo2(Host,Msg) :-
	concat(Host,':',9000,Server),
	pid(Server,Pid),
	rsvp(Pid,Msg,Resp),
	writeln(Resp).
