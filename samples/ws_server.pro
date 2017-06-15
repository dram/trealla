:-module(ws_server).
:-export([start/0,start/1,start/2]).

:-using([sys,net]).

% Note: defined values can also be supplied in a 'ws_server.conf'
% config file in the current directory, which will override the
% following values:

:-define(BindHttp,':80').
:-define(BindHttps,':443;+tls').
:-define(PreferHttps,false).
:-define(Protocol,'').

test :-
	start_server([':8080',':8443;+tls'],?DirRoot).

start :-
	start_server([?BindHttp,?BindHttps],?DirRoot).

start(Bind) :-
	start_server(Bind,?DirRoot).

start(Bind,Root) :-
	start_server(Bind,Root).

start_server(Bind,Root) :-
	open('http.log','append',Log),
	server(Bind,S),
	repeat,
		ws:is_ws(S,true) ->
			process_websocket(S,Log) ;
			process_http(S,Log).

process_websocket(S,Log) :-
	ws:parse(S,Op,Data),
	process_message(S,Op,Data),
	fail.

process_message(S,'ping',Data) :- !.

process_message(S,Op,Data) :-
	% Do something here, just echo for now
	ws:msg(S,Op,Data).

% Handle the protocol switch

process_http(S,Log) :-
	http:parse(S,Ver,Cmd,Path),
	process_request(S,Log,Root,Ver,Cmd,Path),
	fail.

process_request(S,Log,Root,Ver,Cmd,Path) :-
	Ver = 1.1, !,
	security(S,Log,Path),
	check_method(S,Log,Ver,Cmd,Path).

process_request(S,Log,Root,Ver,Cmd,Path) :-
	error_message(S,Log,505,'HTTP VERSION NOT SUPPORTED').

security(S,Log,Path) :-
	tls(S,false),
	?PreferHttps = true, !,
	Code = 301,
	Msg = 'MOVED PERMANENTLY',
	stash_get(S,'HTTP',VerStr,''),
	stash_get(S,'SERVER_NAME',Host,''),
	stash_get(S,'SERVER_PORT',Port,''),
	stash_set(S,'Connection',_,'close'),
	atomic_list_concat(['HTTP/',VerStr,' ',Code,' ',Msg,'\r\nLocation: https://',Host,Path,'\r\nConnection: close\r\nContent-Length: 0\r\n\r\n'],Msg),
	write(S,Msg),
	log_message(S,Log,Code,0),
	fail.                          % NOTE

security(S,Log,Path).

check_method(S,Log,Ver,Cmd,Path) :-
	Cmd = 'GET', !,
	stash_get(S,'CONTENT-LENGTH',CtLenStr,'0'),
	atom_number(CtLenStr,CtLen),
	(CtLen > 0 -> error_message(S,Log,400,'BAD REQUEST'); true),
	process_get(S,Log,Ver,Cmd,Path).

check_method(S,Log,Ver,Cmd,Path) :-
	error_message(S,Log,501,'NOT IMPLEMENTED').

process_get(S,Log,Ver,Cmd,Path) :- !,
	stash_get(S,'Upgrade','websocket',''),
	stash_get(S,'Connection','Upgrade',''),
	ws:upgrade(S,?Protocol),
	log_message(S,Log,101,0).

process_get(S,Log,Ver,Cmd,Path) :-
	warn_message(S,Log,Ver,400,'BAD REQUEST').

warn_message(S,Log,Ver,Code,Msg) :-
	atomic_list_concat(['<html><body><h1>',Msg],Msg1),
	atomic_list_concat([Msg1,'</h1></body></html>\r\n'],Msg2),
	atom_length(Msg2,Len2),
	stash_get(S,'HTTP',VerStr,''),
	stash_get(S,'Connection',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 = 'close' -> Conn3 = 'close' ; Conn3 = 'keep-alive'),
	atomic_list_concat(['HTTP/',VerStr,' ',Code,' ',Msg,'\r\nServer: Trealla\r\nConnection: ',Conn3,'\r\nContent-Type: text/html\r\nContent-Length: ',Len2,'\r\n\r\n',Msg2],Msg3),
	write(S,Ms3),
	log_message(S,Log,Code,0).

error_message(S,Log,Code,Msg) :-
	stash_get(S,'HTTP',VerStr,''),
	stash_set(S,'Connection',_,'close'),
	atomic_list_concat(['HTTP/',VerStr,' ',Code,' ',Msg,'\r\nConnection: close\r\nContent-Length: 0\r\n\r\n'],Msg),
	write(S,Msg),
	log_message(S,Log,Code,0),
	fail.                          % NOTE

log_message(S,Log,Status,Len) :-
	now(Now),format_rfcdate(Now,Date),
	stash_get(S,'REQUEST_METHOD',Cmd,''),
	stash_get(S,'HTTP',VerStr,''),
	stash_get(S,'PATH_INFO',Path,''),
	stash_get(S,'REMOTE_ADDR',Addr,''),
	stash_get(S,'Referer',Refer,''),
	stash_get(S,'SERVER_NAME',Host,''),
	atomic_list_concat(['"',Date,'","',Addr,'","HTTP/',VerStr,'","',Status,'","',Host,'","',Cmd,'","',Path,'","',Len,'","',Refer,'"'],Msg),
	writeln(Log,Msg),
	stash_get(S,'Connection',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 = 'close' -> close(S); true).

