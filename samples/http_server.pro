% A basic http server
%
% To use the default ports 80/443 you need to:
%
%   ./tpl samples/http_server --start
%
% To run a test server on ports 8080/8443:
%
%   ./tpl samples/http_server --test
%
% Or run as a daemon (such as with @reboot in CRON):
%
%   /var/www/tpl http_server -d -w --cd=/var/www --http_server:start
%
% Documents are served from the dir /var/www/DOMAIN/html where
% 'DOMAIN' is from the supplied 'Host' HTTP header value.

:-module(http_server).
:-export([test/0,start/0,start/1,start/2]).
:-import(library(mime)).

% Note: defined values can also be supplied in a 'http_server.conf'
% config file in the current directory, which will override the
% following values:

:-define(DirRoot,'.').
:-define(DirFiles,'/html').
:-define(FileDefault,'index.html').
:-define(BindHttp,':80').
:-define(BindHttps,':443;+tls').
:-define(PreferHttps,false).
:-define(MaxAge,31536000).
:-define(AdminUser,'admin').
:-define(AdminPass,?RANDOMSTR).
:-define(KeyFile,'key.pem').       % TLS private key
:-define(CertFile,'cert.pem').     % TLS fullchain of certificates

:-using([sys,net]).

test :- start_server([':8080',':8443;+tls'],?DirRoot).
start :- start_server([?BindHttp,?BindHttps],?DirRoot).

start(Bind) :- start_server(Bind,?DirRoot).
start(Bind,Root) :- start_server(Bind,Root).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_server(Bind,Root) :-
	write('Started: '), write(?AdminPass), nl,
	open('http.log','append',Log),
	server(Bind,S,?KeyFile,?CertFile),
	repeat,
		http:parse(S,Ver,Cmd,Path),
		process_request(S,Log,Root,Ver,Cmd,Path),
		fail.

process_request(S,Log,Root,Ver,Cmd,Path) :-
	security(S,Log,Path),
	stash_get(S,'SERVER_NAME',Host,''),
	atomic_list_concat([Root,'/',Host,?DirFiles,Path],Path2),
	check_method(S,Log,Ver,Cmd,Path,Path2).

security(S,Log,Path) :-
	tls(S,false),
	?PreferHttps = true, !,
	Code = 301,
	ErrMsg = 'MOVED PERMANENTLY',
	stash_get(S,'HTTP',Ver,''),
	stash_get(S,'SERVER_NAME',Host,''),
	stash_get(S,'SERVER_PORT',Port,''),
	stash_set(S,'Connection',_,'close'),
	atomic_list_concat(['HTTP/',Ver,' ',Code,' ',ErrMsg,'\r\n'],Msg1),
	atomic_list_concat([Msg1,'Location: https://',Host,Path,'\r\n'],Msg2),
	atomic_list_concat([Msg2,'Connection: close\r\nContent-Length: 0\r\n\r\n'],Msg),
	write(S,Msg),
	log_message(S,Log,Code,0).

security(S,Log,Path).

authorize(S,Log,Path) :-
	stash_get(S,'USER',User,''),
	stash_get(S,'PASS',Pass,''),
	{User,Pass} == {?AdminUser,?AdminPass},
	!.

authorize(S,Log,Path) :-
	stash_get(S,'HTTP',Ver,''),
	stash_set(S,'Connection',_,'close'),
	stash_get(S,'SERVER_NAME',Host,''),
	Code = 401,
	atomic_list_concat(['HTTP/',Ver,' ',Code,' Unauthorized\r\n'],Msg1),
	atomic_list_concat([Msg1,'WWW-Authenticate: Basic realm="',Host,'"\r\n'],Msg2),
	atomic_list_concat([Msg2,'Connection: close\r\nContent-Length: 0\r\n\r\n'],Msg),
	write(S,Msg),
	log_message(S,Log,Code,0).

check_method(S,Log,Ver,Cmd,Path,FullPath) :-
	member(Cmd,['GET','HEAD']), !,
	stash_get(S,'CONTENT_LENGTH',CtLenStr,'0'),
	atom_number(CtLenStr,CtLen),
	(CtLen > 0 -> error_message(S,Log,400,'BAD REQUEST'); true),
	process_get(S,Log,Ver,Cmd,Path,FullPath).

check_method(S,Log,Ver,'POST',Path,FullPath) :-
	http:www_form(S),
	process_post(S,Log,Ver,Path,FullPath).

check_method(S,Log,Ver,'POST',Path,FullPath) :-
	error_message(S,Log,501,'NOT IMPLEMENTED').

check_method(S,Log,Ver,'PUT',Path,FullPath) :-
	authorize(S,Log,Path), !,
	process_put(S,Log,Ver,Path,FullPath).

check_method(S,Log,Ver,'PUT',Path,FullPath) :-
	error_message(S,Log,501,'NOT IMPLEMENTED').

check_method(S,Log,Ver,'DELETE',Path,FullPath) :-
	authorize(S,Log,Path), !,
	process_delete(S,Log,Ver,Path,FullPath).

check_method(S,Log,Ver,'DELETE',Path,FullPath) :-
	error_message(S,Log,501,'NOT IMPLEMENTED').

check_method(S,Log,Ver,Cmd,Path,FullPath) :-
	error_message(S,Log,501,'NOT IMPLEMENTED').

process_get(S,Log,Ver,Cmd,Path,FullPath) :-
	exists_file(FullPath,Len,Mod), !,
	format_rfcdate(Mod,Lmod),
	check_modified(S,Log,Ver,Lmod,Len,Cmd,Path,FullPath).

process_get(S,Log,Ver,Cmd,Path,FullPath) :-
	exists_directory(FullPath), !,
	atomic_list_concat([FullPath,?FileDefault],NewFullPath),
	process_get(S,Log,Ver,Cmd,Path,NewFullPath).

process_get(S,Log,Ver,Cmd,Path,FullPath) :-
	info_message(S,Log,404,'NOT FOUND').

process_post(S,Log,Ver,Path,FullPath) :-
	error_message(S,Log,501,'NOT IMPLEMENTED').

process_put(S,Log,Ver,Path,FullPath) :-
	error_message(S,Log,501,'NOT IMPLEMENTED').

process_delete(S,Log,Ver,Path,FullPath) :-
	error_message(S,Log,501,'NOT IMPLEMENTED').

check_modified(S,Log,Ver,Lmod,Len,Cmd,Path,FullPath) :-
	stash_get(S,'If-Modified-Since',Lmod,''),
	stash_get(S,'Connection',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 == 'close' -> Conn3 = 'close' ; Conn3 = 'keep-alive'),
	atomic_list_concat(['HTTP/',Ver,' 304 NOT MODIFIED\r\nServer: Trealla\r\nCache-Control: max-age=',?MaxAge,'\r\nConnection: ',Conn3,'\r\nContent-Length: 0\r\n\r\n'],Msg),
	write(S,Msg),
	log_message(S,Log,304,0).

check_modified(S,Log,Ver,Lmod,Len,Cmd,Path,FullPath) :-
	process_file(S,Ver,Lmod,Len,Cmd,FullPath),
	log_message(S,Log,200,Len).

process_file(S,Ver,Lmod,Len,'HEAD',FullPath) :- !,
	stash_get(S,'Connection',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 == 'close' -> Conn3 = 'close' ; Conn3 = 'keep-alive'),
	atomic_list_concat(['HTTP/',Ver,' 200 OK\r\nServer: Trealla\r\nCache-Control: max-age=',?MaxAge,'\r\nLast-Modified: ',Lmod,'\r\nConnection: ',Conn3,'\r\nContent-Length: ',Len,'\r\n\r\n'],Msg),
	write(S,Msg).

process_file(S,Ver,Lmod,Len,'GET',FullPath) :- !,
	stash_get(S,'Connection',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 == 'close' -> Conn3 = 'close' ; Conn3 = 'keep-alive'),
	mime:mime_type(FullPath,Ct),
	atomic_list_concat(['HTTP/',Ver,' 200 OK\r\nServer: Trealla\r\nCache-Control: max-age=',?MaxAge,'\r\nLast-Modified: ',Lmod,'\r\nConnection: ',Conn3,'\r\nContent-Type: ',Ct,'\r\nContent-Length: ',Len,'\r\n\r\n'],Msg),
	write(S,Msg),
	write_file(S,FullPath).

info_message(S,Log,Code,InfoMsg) :-
	atomic_list_concat(['<html><body><h1>',InfoMsg,'</h1></body></html>\r\n'],Body),
	atom_length(Body,Len),
	stash_get(S,'HTTP',Ver,''),
	stash_get(S,'Connection',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 == 'close' -> Conn3 = 'close' ; Conn3 = 'keep-alive'),
	atomic_list_concat(['HTTP/',Ver,' ',Code,' ',InfoMsg,'\r\nServer: Trealla\r\nConnection: ',Conn3,'\r\nContent-Type: text/html\r\nContent-Length: ',Len,'\r\n\r\n',Body],Msg),
	write(S,Msg),
	log_message(S,Log,Code,0).

error_message(S,Log,Code,ErrMsg) :-
	stash_get(S,'HTTP',Ver,''),
	stash_set(S,'Connection',_,'close'),
	atomic_list_concat(['HTTP/',Ver,' ',Code,' ',ErrMsg,'\r\nConnection: close\r\nContent-Length: 0\r\n\r\n'],Msg),
	write(S,Msg),
	log_message(S,Log,Code,0).

log_message(S,Log,Status,Len) :-
	now(Now),
	format_rfcdate(Now,Date),
	stash_get(S,'REQUEST_METHOD',Cmd,''),
	stash_get(S,'HTTP',Ver,''),
	stash_get(S,'PATH_INFO',Path,''),
	stash_get(S,'REMOTE_ADDR',Addr,''),
	stash_get(S,'Referer',Refer,''),
	stash_get(S,'SERVER_NAME',Host,''),
	atomic_list_concat(['"',Date,'","',Addr,'","HTTP/',Ver,'","',Status,'","',Host,'","',Cmd,'","',Path,'","',Len,'","',Refer,'"\n'],Msg),
	write(Log,Msg),
	%flush_output(Log),
	stash_get(S,'Connection',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 == 'close' -> close(S); true).
