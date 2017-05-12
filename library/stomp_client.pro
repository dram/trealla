:-module(stomp_client).
:-export([test/1,connect/2,disconnect/1]).

% Does this still work???

:-using([sys,net]).

test(Host) :-
	connect(Host,S),
	disconnect(S).

connect(Host,S) :-
	client(Host,S),
	split(Host,':',Name,Rest),
	concat('host:',Name,'\naccept-version:1.0,1.1,1.2\n',Hdrs),
	stomp:msg(S,'CONNECT',Hdrs,''),
	stomp:parse(S,Ver,Cmd),
	Cmd = 'CONNECTED',
	writeln('Connected...').

disconnect(S) :-
	uuid(Id),
	concat('receipt:',Id,'\n',Hdrs),
	msg(S,'DISCONNECT',Hdrs,''),
	repeat,
		stomp:parse(S,_,Cmd),
		Cmd = 'RECEIPT',
		close(S),
		writeln('Disconnected').

% Send blindly:

send(S,Dest,Ct,Data) :-
	concat('destination:',Dest,'\ncontent-type:',Ct,'\n',Hdrs),
	stomp:msg(S,'SEND',Hdrs,Data).

% Send and ask for a receipt, which will be processed internally. Any
% messages will be forwarded on to the designated process:

send_with_receipt({S,Pid},Dest,Ct,Data,Id) :-
	uuid(Id),
	concat('receipt:',Id,'\ndestination:',Dest,'\ncontent-type:',Ct,'\n',Hdrs),
	stomp:msg(S,'SEND',Hdrs,Data),
	wait_receipt({S,Pid}).

% etc

subscribe(S,Dest,Id) :-
	uuid(Id),
	concat('id:',Id,'\ndestination:',Dest,'\n',Hdrs),
	stomp:msg(S,'SUBSCRIBE',Hdrs,'').

subscribe_with_receipt({S,Pid},Dest,Id) :-
	uuid(Id),
	concat('id:',Id,'\nreceipt:',Id,'\ndestination:',Dest,'\n',Hdrs),
	stomp:msg(S,'SUBSCRIBE',Hdrs,''),
	wait_receipt({S,Pid}).

unsubscribe(S,Id) :-
	concat('id:',Id,'\n',Hdrs),
	stomp:msg(S,'UNSUBSCRIBE',Hdrs,'').

unsubscribe_with_receipt({S,Pid},Id) :-
	concat('id:',Id,'receipt:',Id,'\n',Hdrs),
	stomp:msg(S,'UNSUBSCRIBE',Hdrs,''),
	wait_receipt({S,Pid}).

% Receipt handling:

wait_receipt({S,Pid}) :-
	repeat,
		stomp:parse(S,_,Cmd),
		Cmd = 'MESSAGE' -> forward({S,Pid},Cmd); true.

forward({S,Pid},Cmd) :-
	stash_get(S,'CONTENT_TYPE',Ct,'text/plain'),
	stash_get(S,'CONTENT_LENGTH',LenStr,'0'),
	stash_get(S,'Message-Id',Mid,''),
	stash_get(S,'Subscription',Sub,''),
	stash_get(S,'Destination',Dest,''),
	atom_number(LenStr,Len),
	bread(S,Len,Data),
	proc:send(Pid,{Cmd,Mid,Sub,Dest,Ct,Data}),
	fail.
