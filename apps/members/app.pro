:-module(app).
:-export([app_init/0,app_entry/4]).
:-use_module(app_server).
:-use_module(auth).
:-using([sys,net]).

% Note: defined values can also be supplied in an 'app.conf'
% config file in the current directory, which will override the
% following values:

:-define(SessionId,'sid').
:-define(VerifiedUser,'VERIFIED:USER').
:-define(Landing,'/landing.html').

% This is the application layer...

app_post(S,Log,'/signup') :-
	http:www_form(S),
	http:form(S,'username',User),
	http:form(S,'password',Pass),
	http:form(S,'password2',Pass2),
	http:form(S,'keep',Keep),
	Pass == Pass2,
	atom_length(User,N1), N1 >= 3,
	atom_length(Pass,N2), N2 >= 6,
	adduser(User,Pass),
	(Keep == 'on -> Keep2 = true ; Keep2 = false),
	login(User,Pass,SessId,Keep2,Expires), !,
	format_rfcdate(Expires,ExpiresStr),
	concat('Set-Cookie: ',?SessionId,'=',SessId,'; Expires=',ExpiresStr,'; HttpOnly\r\n',Hdr1),
	concat(Hdr1,'Location: ',?Landing,'\r\n',Hdr),
	response(S,Log,302,'SIGNUP OK',Hdr).

app_post(S,Log,'/signup') :- !,
	Hdr = 'Location: /signup.html\r\n',
	response(S,Log,302,'PLEASE LOGIN',Hdr).

app_post(S,Log,'/login') :-
	http:www_form(S),
	http:form(S,'username',User),
	http:form(S,'password',Pass),
	http:form(S,'keep',Keep),
	(Keep == 'on -> Keep2 = true ; Keep2 = false),
	login(User,Pass,SessId,Keep2,Expires), !,
	format_rfcdate(Expires,ExpiresStr),
	concat('Set-Cookie: ',?SessionId,'=',SessId,'; Expires=',ExpiresStr,'; HttpOnly\r\n',Hdr1),
	concat(Hdr1,'Location: ',?Landing,'\r\n',Hdr),
	response(S,Log,302,'LOGIN OK',Hdr).

app_post(S,Log,'/login') :- !,
	Hdr = 'Location: /login.html\r\n',
	response(S,Log,302,'PLEASE LOGIN',Hdr).

app_post(S,Log,'/logout') :- !,
	http:cookie(S,?SessionId,SessId),
	logout(SessId), !
	concat('Set-Cookie: ',?SessionId,'=; Expires=; HttpOnly\r\n',Hdr1),
	concat(Hdr1,'Location: /\r\n',Hdr),
	response(S,Log,302,'LOGOUT OK',Hdr),
	stash_clr(S,?VerifiedUser,_).

app_get(S,Log,'/logout') :-
	app_post(S,Log,'/logout').


% If true fall back to HTTP serving files...

app_entry(S,Log,Cmd,Path) :-
	begins(Path,['/public/','/images/']),
	!.

app_entry(S,Log,Cmd,Path) :-
	member(Path,['/','/index.html','/signup.html','/login.html','/favicon.ico','/robots.txt']),
	!.

% Else handle our forms for signup/login...

app_entry(S,Log,'POST',Path) :-
	app_post(S,Log,Path), !,
	fail.

% or handle logout...

app_entry(S,Log,'GET',Path) :-
	app_get(S,Log,Path), !,
	fail.

% Otherwise check auth credentials...

app_entry(S,Log,Cmd,Path) :-
	http:cookie(S,?SessionId,SessId),
	checkin(SessId,User,NewExpires),
	stash_set(S,?VerifiedUser,User),
	!.

% Nope, ask to log in...

app_entry(S,Log,Cmd,Path) :-
	Hdr = 'Location: /\r\n',
	response(S,Log,302,'PLEASE LOGIN OR SIGNUP',Hdr),
	fail.

% Anything to do on startup

app_init :-
	auth:init,
	true.
