% This is a fairly simple implementation that does not support
% persistent queues. Subscriptions are held in memory and do not
% outlast connections, that is: a disconnect will unsubscribe.

% So far it support (DIS)CONNECT, (UN)SUBSCRIBE, and SEND.

:-module(stomp_server).
:-export([start/0]).
:-define(BindStomp,':9000').
:-define(BindStomps,':9001;+tls').
:-define(Params,[]).

:-using([sys,net,proc]).

start :-
	start_server([?BindStomp,?BindStomps],?Params).

start_server(Bind,Param) :-
	open('stomp.log','append',Log),
	server(Bind,S),
	repeat,
		stomp:parse(S,Ver,Cmd),
		process_request(S,Log,Param,Ver,Cmd),
		fail.

process_request(S,Log,Param,Ver,Cmd) :-
	member(Cmd,['CONNECT','STOMP']),
	concat('server:trealla\nversion:',Ver,'\n',Hdrs),
	stomp:msg(S,'CONNECTED',Hdrs,''),
	log_message(S,Log,1).

process_request(S,Log,Param,Ver,'DISCONNECT') :-
	process_receipt(S),
	log_message(S,Log,1),
	close(S).

process_request(S,Log,Param,Ver,'SUBSCRIBE') :-
	stash_get(S,'Destination',Dest,_),
	stash_get(S,'Id',Id,_),
	stash_set(S,Id,Dest),
	lput(Dest,Old,[S|Old]),
	process_receipt(S),
	log_message(S,Log,1).

process_request(S,Log,Param,Ver,'UNSUBSCRIBE') :-
	stash_get(S,'Id',Id,_),
	stash_get(S,Id,Dest),
	lerase(Dest,S),
	process_receipt(S),
	log_message(S,Log,1).

process_request(S,Log,Param,Ver,'SEND') :-
	stash_get(S,'Destination',Dest,_),
	stash_get(S,'CONTENT_LENGTH',LenStr,0),
	atom_integer(LenStr,Len),
	bread(S,LenStr,Data),
	lget(Dest,Subs),
	uuid(Mid),
	send_message(S,Subs,Mid,Data),
	concat('message-id:',Mid,'\n',Hdrs),
	process_receipt(S,Hdrs),
	log_message(S,Log,1).

send_message(S,[],Mid,Data) :- !.

send_message(S,[Who|Rest],Mid,Data) :-
	stash_get(S,'Destination',Dest,_),
	stash_get(S,'CONTENT_TYPE',Ct,'text/plain'),
	stash_get(S,'CONTENT_LENGTH',Len,0),
	concat('destination:',Dest,'\ncontent-type:',Ct,'\ncontent-length:',Len,'\nmessage-id:',Mid,'\n',Hdrs),
	stomp:msg(Who,'MESSAGE',Hdrs,Data),
	send_message(S,Rest,Mid,Data).

process_request(S,Log,Param,Ver,'ACK') :-
	fail,
	process_receipt(S),
	log_message(S,Log,1).

process_request(S,Log,Param,Ver,'NACK') :-
	fail,
	process_receipt(S),
	log_message(S,Log,1).

process_request(S,Log,Param,Ver,'BEGIN') :-
	fail,
	process_receipt(S),
	log_message(S,Log,1).

process_request(S,Log,Param,Ver,'COMMIT') :-
	fail,
	process_receipt(S),
	log_message(S,Log,1).

process_request(S,Log,Param,Ver,'ABORT') :-
	fail,
	process_receipt(S),
	log_message(S,Log,1).

process_request(S,Log,Param,Ver,Cmd) :-
	concat('server:trealla\nversion:',Ver,'\n',Hdrs),
	stomp:msg(S,'ERROR',Hdrs,''),
	log_message(S,Log,0),
	close(S).

process_receipt(S) :-
	process_receipt(S,'').

process_receipt(S,Hdrs) :-
	stash_get(S,'Receipt',Rcpt,_),
	nonvar(Rcpt),
	concat('receipt:',Rcpt,'\n',Hdrs,Hdrs2),
	stomp:msg(S,'RECEIPT',Hdrs2,'').

process_receipt(S,Hdrs).

log_message(S,Log,Status) :-
	now(Now),format_rfcdate(Now,Date),
	stash_get(S,'REQUEST_METHOD',Cmd,''),
	stash_get(S,'STOMP',VerStr,''),
	stash_get(S,'REMOTE_ADDR',Addr,''),
	stash_get(S,'SERVER_NAME',Host,''),
	stash_get(S,'CONTENT_LENGTH',Len,'0'),
	Path = '',
	Refer = '',
	concat('"',Date,'","',Addr,'","STOMP/',VerStr,'","',Status,'","',Host,'","',Cmd,'","',Path,'","',Len,'","',Refer,'"',Msg),
	writeln(Log,Msg),
	true.
