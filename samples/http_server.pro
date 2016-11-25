% A basic http server
%
% To use the default ports 80/443 you need to:
%
%  cd ~/trealla
%  sudo ./tpl samples/http_server --start
%
% To run a test server on ports 8080/8443:
%
%  cd ~/trealla
%  ./tpl samples/http_server --test
%
% Or run as a daemon (such as with @reboot in CRON):
%
%  sudo /var/www/tpl http_server.pro -d -w --cd=/var/www --goal=start
%
% Documents are served from the dir /var/www/DOMAIN/html where
% 'DOMAIN' is from the supplied 'Host' HTTP header value. Try:
%
%	ln -s /var/www /var/www/localhost
%	ln -s /var/www /var/www/DOMAIN
%	ln -s /var/www /var/www/www.DOMAIN
%
% to start with.

:-module(http_server).
:-export([test/0,start/0,start/1,start/2]).
:-using([sys,net,http]).

% Note: defined values can also be supplied in a 'http_server.conf'
% config file in the current directory, which will override the
% following values:

:-define(DirRoot,'.').
:-define(DirFiles,'/html').
:-define(FileDefault,'index.html').
:-define(BindHttp,':80').
:-define(BindHttps,':443;+tls').
:-define(PreferHttps,false).
:-define(MaxAge,300).
:-define(AdminUser,'admin').
:-define(AdminPass,?RANDOMSTR).
:-define(KeyFile,'key.pem').       % TLS private key
:-define(CertFile,'cert.pem').     % TLS fullchain of certificates

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
	Ver =< 1.1, !,
	security(S,Log,Path),
	stash_get(S,'HTTP_HOST',Host,''),
	concat(Root,'/',Host,?DirFiles,Path,Path2),
	check_method(S,Log,Ver,Cmd,Path,Path2).

process_request(S,Log,Root,Ver,Cmd,Path) :-
	error_message(S,Log,505,'HTTP VERSION NOT SUPPORTED').

security(S,Log,Path) :-
	tls(S,false),
	?PreferHttps = true, !,
	Code = 301,
	ErrMsg = 'MOVED PERMANENTLY',
	stash_get(S,'HTTP_VERSION',Ver,''),
	stash_get(S,'HTTP_HOST',Host,''),
	stash_get(S,'HTTP_PORT',Port,''),
	stash_set(S,'HTTP_CONNECTION',_,'close'),
	concat('HTTP/',Ver,' ',Code,' ',ErrMsg,'\r\n',Msg1),
	concat(Msg1,'Location: https://',Host,Path,'\r\n',Msg2),
	concat(Msg2,'Connection: close\r\nContent-Length: 0\r\n\r\n',Msg),
	write(S,Msg),
	log_message(S,Log,Code,0),
	fail.                          % NOTE

security(S,Log,Path).

authorize(S,Log,Path) :-
	stash_get(S,'USER',User,''),
	stash_get(S,'PASS',Pass,''),
	{User,Pass} == {?AdminUser,?AdminPass},
	!.

authorize(S,Log,Path) :-
	stash_get(S,'HTTP_VERSION',Ver,''),
	stash_set(S,'HTTP_CONNECTION',_,'close'),
	stash_get(S,'HTTP_HOST',Host,''),
	Code = 401,
	concat('HTTP/',Ver,' ',Code,' Unauthorized\r\n',Msg1),
	concat(Msg1,'WWW-Authenticate: Basic realm="',Host,'"\r\n',Msg2),
	concat(Msg2,'Connection: close\r\nContent-Length: 0\r\n\r\n',Msg),
	write(S,Msg),
	log_message(S,Log,Code,0),
	fail.                          % NOTE

check_method(S,Log,Ver,Cmd,Path,FullPath) :-
	member(Cmd,['GET','HEAD']), !,
	stash_get(S,'HTTP_CONTENT-LENGTH',CtLenStr,'0'),
	atom_number(CtLenStr,CtLen),
	(CtLen > 0 -> error_message(S,Log,400,'BAD REQUEST'); true),
	process_get(S,Log,Ver,Cmd,Path,FullPath).

check_method(S,Log,Ver,'POST',Path,FullPath) :-
	www_form(S),
	process_post(S,Log,Ver,Path,FullPath).

check_method(S,Log,Ver,'PUT',Path,FullPath) :-
	authorize(S,Log,Path), !,
	process_put(S,Log,Ver,Path,FullPath).

check_method(S,Log,Ver,'PUT',Path,FullPath) :-
	true.

check_method(S,Log,Ver,'DELETE',Path,FullPath) :-
	authorize(S,Log,Path), !,
	process_delete(S,Log,Ver,Path,FullPath).

check_method(S,Log,Ver,'DELETE',Path,FullPath) :-
	true.

check_method(S,Log,Ver,Cmd,Path,FullPath) :-
	error_message(S,Log,501,'NOT IMPLEMENTED').

process_get(S,Log,Ver,Cmd,Path,FullPath) :-
	exists_file(FullPath,Len,Mod), !,
	format_rfcdate(Mod,Lmod),
	check_modified(S,Log,Ver,Lmod,Len,Cmd,Path,FullPath).

process_get(S,Log,Ver,Cmd,Path,FullPath) :-
	exists_dir(FullPath), !,
	concat(FullPath,?FileDefault,NewFullPath),
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
	stash_get(S,'HTTP_IF_MODIFIED_SINCE',Lmod,''),
	stash_get(S,'HTTP_CONNECTION',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 == 'close' -> Conn3 = 'close' ; Conn3 = 'keep-alive'),
	concat('HTTP/',Ver,' 304 NOT MODIFIED\r\nServer: Trealla\r\nCache-Control: max-age=',?MaxAge,'\r\nConnection: ',Conn3,'\r\nContent-Length: 0\r\n\r\n',Msg),
	write(S,Msg),
	log_message(S,Log,304,0).

check_modified(S,Log,Ver,Lmod,Len,Cmd,Path,FullPath) :-
	process_file(S,Ver,Lmod,Len,Cmd,FullPath),
	log_message(S,Log,200,Len).

process_file(S,Ver,Lmod,Len,'HEAD',FullPath) :- !,
	stash_get(S,'HTTP_CONNECTION',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 == 'close' -> Conn3 = 'close' ; Conn3 = 'keep-alive'),
	concat('HTTP/',Ver,' 200 OK\r\nServer: Trealla\r\nCache-Control: max-age=',?MaxAge,'\r\nLast-Modified: ',Lmod,'\r\nConnection: ',Conn3,'\r\nContent-Length: ',Len,'\r\n\r\n',Msg),
	write(S,Msg),
	true.

process_file(S,Ver,Lmod,Len,'GET',FullPath) :-
	Ver = 1.1, !,
	stash_get(S,'HTTP_CONNECTION',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 == 'close' -> Conn3 = 'close' ; Conn3 = 'keep-alive'),
	(right(FullPath,5,'.html') -> Ct = 'text/html' ; Ct = 'application/octet-stream'),
	concat('HTTP/',Ver,' 200 OK\r\nServer: Trealla\r\nCache-Control: max-age=',?MaxAge,'\r\nLast-Modified: ',Lmod,'\r\nConnection: ',Conn3,'\r\nContent-Type: ',Ct,'\r\nTransfer-Encoding: chunked\r\n\r\n',Msg),
	write(S,Msg),
	http:put_file(S,FullPath).

process_file(S,Ver,Lmod,Len,'GET',FullPath) :-
	stash_get(S,'HTTP_CONNECTION',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 == 'close' -> Conn3 = 'close' ; Conn3 = 'keep-alive'),
	(right(FullPath,5,'.html') -> Ct = 'text/html' ; Ct = 'application/octet-stream'),
	concat('HTTP/',Ver,' 200 OK\r\nServer: Trealla\r\nCache-Control: max-age=',?MaxAge,'\r\nLast-Modified: ',Lmod,'\r\nConnection: ',Conn3,'\r\nContent-Type: ',Ct,'\r\nContent-Length: ',Len,'\r\n\r\n',Msg),
	write(S,Msg),
	write_file(S,FullPath).

info_message(S,Log,Code,InfoMsg) :-
	concat('<html><body><h1>',InfoMsg,'</h1></body></html>\r\n',Body),
	atom_length(Body,Len),
	stash_get(S,'HTTP_VERSION',Ver,''),
	stash_get(S,'HTTP_CONNECTION',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 == 'close' -> Conn3 = 'close' ; Conn3 = 'keep-alive'),
	concat('HTTP/',Ver,' ',Code,' ',InfoMsg,'\r\nServer: Trealla\r\nConnection: ',Conn3,'\r\nContent-Type: text/html\r\nContent-Length: ',Len,'\r\n\r\n',Body,Msg),
	write(S,Msg),
	log_message(S,Log,Code,0).

error_message(S,Log,Code,ErrMsg) :-
	stash_get(S,'HTTP_VERSION',Ver,''),
	stash_set(S,'HTTP_CONNECTION',_,'close'),
	concat('HTTP/',Ver,' ',Code,' ',ErrMsg,'\r\nConnection: close\r\nContent-Length: 0\r\n\r\n',Msg),
	write(S,Msg),
	log_message(S,Log,Code,0),
	fail.                          % NOTE

log_message(S,Log,Status,Len) :-
	now(Now),format_rfcdate(Now,Date),
	stash_get(S,'HTTP_REQUEST_METHOD',Cmd,''),
	stash_get(S,'HTTP_VERSION',Ver,''),
	stash_get(S,'HTTP_PATH',Path,''),
	stash_get(S,'HTTP_REMOTE_ADDR',Addr,''),
	stash_get(S,'HTTP_REFERER',Refer,''),
	stash_get(S,'HTTP_HOST',Host,''),
	concat('"',Date,'","',Addr,'","HTTP/',Ver,'","',Status,'","',Host,'","',Cmd,'","',Path,'","',Len,'","',Refer,'"\n',Msg),
	write(Log,Msg),
	stash_get(S,'HTTP_CONNECTION',Conn,'keep-alive'),
	lower(Conn,Conn2),
	(Conn2 == 'close' -> close(S); true).
