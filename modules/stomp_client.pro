:-module(stomp_client).
:-export([test/1,connect/2,disconnect/1]).

:-using([sys,net,proc,stomp]).

test(Host) :-
	connect(Host,S),
	disconnect(S).

connect(Host,S) :-
	client(Host,S),
	split(Host,':',Name,Rest),
	awrite(Hdrs,'host:',Name,'\naccept-version:1.0,1.1,1.2\n'),
	msg(S,'CONNECT',Hdrs,''),
	parse(S,Ver,Cmd),
	Cmd = 'CONNECTED',
	writeln('Connected...').

disconnect(S) :-
	uuid(Id),
	awrite(Hdrs,'receipt:',Id,'\n'),
	msg(S,'DISCONNECT',Hdrs,''),
	repeat,
		parse(S,_,Cmd),
		Cmd = 'RECEIPT',
		close(S),
		writeln('Disconnected').

% Send blindly:

send(S,Dest,Ct,Data) :-
	awrite(Hdrs,'destination:',Dest,'\ncontent-type:',Ct,'\n'),
	msg(S,'SEND',Hdrs,Data).

% Send and ask for a receipt, which will be processed internally. Any
% messages will be forwarded on to the designated process:

send_with_receipt({S,Pid},Dest,Ct,Data,Id) :-
	uuid(Id),
	awrite(Hdrs,'receipt:',Id,'\ndestination:',Dest,'\ncontent-type:',Ct,'\n'),
	msg(S,'SEND',Hdrs,Data),
	wait_receipt({S,Pid}).

% etc

subscribe(S,Dest,Id) :-
	uuid(Id),
	awrite(Hdrs,'id:',Id,'\ndestination:',Dest,'\n'),
	msg(S,'SUBSCRIBE',Hdrs,'').

subscribe_with_receipt({S,Pid},Dest,Id) :-
	uuid(Id),
	awrite(Hdrs,'id:',Id,'\nreceipt:',Id,'\ndestination:',Dest,'\n'),
	msg(S,'SUBSCRIBE',Hdrs,''),
	wait_receipt({S,Pid}).

unsubscribe(S,Id) :-
	awrite(Hdrs,'id:',Id,'\n'),
	msg(S,'UNSUBSCRIBE',Hdrs,'').

unsubscribe_with_receipt({S,Pid},Id) :-
	awrite(Hdrs,'id:',Id,'receipt:',Id,'\n'),
	msg(S,'UNSUBSCRIBE',Hdrs,''),
	wait_receipt({S,Pid}).

% Receipt handling:

wait_receipt({S,Pid}) :-
	repeat,
		parse(S,_,Cmd),
		Cmd = 'MESSAGE' -> forward({S,Pid},Cmd); true.

forward({S,Pid},Cmd) :-
	stash_get(S,'STOMP_CONTENT_TYPE',Ct,'text/plain'),
	stash_get(S,'STOMP_CONTENT_LENGTH',LenStr,'0'),
	stash_get(S,'STOMP_MESSAGE_ID',Mid,''),
	stash_get(S,'STOMP_SUBSCRIPTION',Sub,''),
	stash_get(S,'STOMP_DESTINATION',Dest,''),
	atom_number(LenStr,Len),
	bread(S,Len,Data),
	send(Pid,{Cmd,Mid,Sub,Dest,Ct,Data}),
	fail.
