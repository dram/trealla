:-module(app_server).
:-export([test/0,start/0,start/1,start/2]).
:-export([response/5,response_body/6]).
:-use_module(app).
:-using([sys,net]).

% Note: defined values can also be supplied in a 'app_server.conf'
% config file in the current directory, which will override the
% following values:

:-define(DirRoot,'.').
:-define(FileDefault,'index.html').
:-define(BindHttp,':80').
:-define(BindHttps,':443;+tls').
:-define(PreferHttps,false).
:-define(MaxAge,300).
:-define(KeyFile,'key.pem').
:-define(CertFile,'cert.pem').

test :- start_server([':8080',':8443;+tls'],?DirRoot).
start :- start_server([?BindHttp,?BindHttps],?DirRoot).

start(Bind) :- start_server(Bind,?DirRoot).
start(Bind,Root) :- start_server(Bind,Root).

start_server(Bind,Root) :-
	app:app_init,
	open('http.log','append',Log),
	server(Bind,S,?KeyFile,?CertFile),
	repeat,
		http:parse(S,Ver,Cmd,Path),
		process_request(S,Log,Root,Ver,Cmd,Path),
		fail.

process_request(S,Log,Root,Ver,Cmd,Path) :-
	Ver =< 1.1, !,
	security(S,Log,Path),
	app:app_entry(S,Log,Cmd,Path),
	stash_get(S,'HTTP_HOST',Host),
	concat(Root,'/',Host,Path,Path2),
	check_method(S,Log,Ver,Cmd,Path,Path2).

process_request(S,Log,Root,Ver,Cmd,Path) :-
	error_message(S,Log,501,'NOT IMPLEMENTED').

security(S,Log,Path) :-
	tls(S,false),
	?PreferHttps, !,
	Code = 301,
	ErrMsg = 'MOVED PERMANENTLY',
	stash_get(S,'HTTP_VERSION',Ver),
	stash_get(S,'HTTP_HOST',Host),
	stash_get(S,'HTTP_PORT',Port),
	stash_set(S,'HTTP_CONNECTION',_,'close'),
	concat('HTTP/',Ver,' ',Code,' ',ErrMsg,'\r\nLocation: https://',Host,Path,'\r\n',Msg1),
	concat(Msg1,'Connection: close\r\nContent-Length: 0\r\n\r\n',Msg),
	write(S,Msg),
	log_message(S,Log,Code,0),
	fail.                          % NOTE

security(S,Log,Path).

check_method(S,Log,Ver,Cmd,Path,FullPath) :-
	member(Cmd,['GET','HEAD']), !,
	process_get(S,Log,Ver,Cmd,Path,FullPath).

process_get(S,Log,Ver,Cmd,Path,FullPath) :-
	exists_file(FullPath,Len,Mod), !,
	format_rfcdate(Mod,ModStr),
	check_modified(S,Log,Ver,ModStr,Len,Cmd,Path,FullPath).

process_get(S,Log,Ver,Cmd,Path,FullPath) :-
	exists_dir(FullPath), !,
	concat(FullPath,?FileDefault,NewFullPath),
	process_get(S,Log,Ver,Cmd,Path,NewFullPath).

process_get(S,Log,Ver,Cmd,Path,FullPath) :-
	response(S,Log,404,'NOT FOUND','').

check_modified(S,Log,Ver,Mod,Len,Cmd,Path,FullPath) :-
	stash_get(S,'HTTP_IF_MODIFIED_SINCE',Mod),
	stash_get(S,'HTTP_CONNECTION',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 == 'close' -> Conn3 = 'close' ; Conn3 = 'keep-alive'),
	concat('HTTP/',Ver,' 304 NOT MODIFIED\r\nServer: Trealla/app_server\r\nCache-Control: max-age=',?MaxAge,'\r\n',Msg1),
	concat(Msg1,'Connection: ',Conn3,'\r\nContent-Length: 0\r\n\r\n',Msg),
	write(S,Msg),
	log_message(S,Log,304,0).

check_modified(S,Log,Ver,Mod,Len,Cmd,Path,FullPath) :-
	process_file(S,Ver,Mod,Len,Cmd,FullPath),
	log_message(S,Log,200,Len).

process_file(S,Ver,Mod,Len,'HEAD',FullPath) :- !,
	stash_get(S,'HTTP_CONNECTION',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 == 'close' -> Conn3 = 'close' ; Conn3 = 'keep-alive'),
	concat('HTTP/',Ver,' 200 OK\r\nServer: Trealla/app_server\r\nCache-Control: max-age=',?MaxAge,'\r\n',Msg1),
	concat(Msg1,'Last-Modified: ',Mod,'\r\nConnection: ',Conn3,'\r\nContent-Length: ',Len,'\r\n\r\n',Msg),
	write(S,Msg),
	true.

process_file(S,Ver,Mod,Len,'GET',FullPath) :-
	Ver = 1.1, !,
	stash_get(S,'HTTP_CONNECTION',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 == 'close' -> Conn3 = 'close' ; Conn3 = 'keep-alive'),
	(right(FullPath,5,'.html') -> Ct = 'text/html' ; Ct = 'application/octet-stream'),
	concat('HTTP/',Ver,' 200 OK\r\nServer: Trealla/app_server\r\nContent-Type: ',Ct,'\r\nCache-Control: max-age=',?MaxAge,'\r\n',Msg1),
	concat(Msg1,'Last-Modified: ',Mod,'\r\nConnection: ',Conn3,'\r\nTransfer-Encoding: chunked\r\n\r\n',Msg),
	write(S,Msg),
	http:put_file(S,FullPath).

process_file(S,Ver,Mod,Len,'GET',FullPath) :-
	stash_get(S,'HTTP_CONNECTION',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 == 'close' -> Conn3 = 'close' ; Conn3 = 'keep-alive'),
	(right(FullPath,5,'.html') -> Ct = 'text/html' ; Ct = 'application/octet-stream'),
	concat('HTTP/',Ver,' 200 OK\r\nServer: Trealla/app_server\r\nCache-Control: max-age=',?MaxAge,'\r\n',Msg1),
	concat(Msg1,'Last-Modified: ',Mod,'\r\nConnection: ',Conn3,'\r\nContent-Type: y',Ct,'\r\nContent-Length: ',Len,'\r\n\r\n',Msg),
	write(S,Msg),
	write_file(S,FullPath).

response(S,Log,Code,InfoMsg,Hdr) :-
	concat('<html><body><div>',InfoMsg,'</div></body></html>\r\n',Body),
	atom_length(Body,Len),
	stash_get(S,'HTTP_VERSION',Ver),
	stash_get(S,'HTTP_CONNECTION',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 == 'close' -> Conn3 = 'close' ; Conn3 = 'keep-alive'),
	concat('HTTP/',Ver,' ',Code,' ',InfoMsg,'\r\nServer: Trealla/app_server\r\n',Msg1),
	concat(Msg1,'Connection: ',Conn3,'\r\nContent-Type: text/html\r\nContent-Length: ',Len,'\r\n',Msg2),
	(Hdr \== '' -> concat(Msg2,Hdr,'\r\n',Body,Resp) ; concat(Msg2,'\r\n',Body,Resp)),
	write(S,Resp),
	log_message(S,Log,Code,Len).

response_body(S,Log,Code,InfoMsg,Body,Hdr) :-
	atom_length(Body,Len),
	stash_get(S,'HTTP_VERSION',Ver),
	stash_get(S,'HTTP_CONNECTION',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 == 'close' -> Conn3 = 'close' ; Conn3 = 'keep-alive'),
	concat('HTTP/',Ver,' ',Code,' ',InfoMsg,'\r\nServer: Trealla/app_server\r\n',Msg1),
	concat(Msg1,'Connection: ',Conn3,'\r\nContent-Type: text/html\r\nContent-Length: ',Len,'\r\n',Msg2),
	(Hdr \== '' -> concat(Msg2,Hdr,'\r\n',Body,Resp) ; concat(Msg2,'\r\n',Body,Resp)),
	write(S,Resp),
	log_message(S,Log,Code,Len).

error_message(S,Log,Code,ErrMsg) :-
	stash_get(S,'HTTP_VERSION',Ver),
	stash_set(S,'HTTP_CONNECTION',_,'close'),
	concat('HTTP/',Ver,' ',Code,' ',ErrMsg,'\r\nConnection: close\r\nContent-Length: 0\r\n\r\n',Msg),
	write(S,Msg),
	log_message(S,Log,Code,0),
	fail.                          % NOTE

log_message(S,Log,Status,Len) :-
	now(Now), format_rfcdate(Now,Date),
	stash_get(S,'HTTP_REQUEST_METHOD',Cmd),
	stash_get(S,'HTTP_VERSION',Ver),
	stash_get(S,'HTTP_PATH',Path),
	stash_get(S,'HTTP_REMOTE_ADDR',Addr),
	stash_get(S,'HTTP_REFERER',Refer),
	stash_get(S,'HTTP_HOST',Host),
	concat('"',Date,'","',Addr,'","HTTP/',Ver,'","',Status,'","',Host,'","',Cmd,'","',Path,'","',Len,'","',Refer,'"\n',Msg),
	write(Log,Msg),
	stash_get(S,'HTTP_CONNECTION',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 == 'close' -> close(S); true).
