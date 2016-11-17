% Test echo server & client (must be on same LAN)
%
% For example, to start the server process:
%
%  ./yxl samples/echo '--goal=echo:start'
%
% then in another terminal window run a client:
%
%  ./yxl samples/echo '--goal=echo:sendmsg("Hello, world!")'
%

:-module(echo).
:-export([start/0,startw/0,sendmsg/1]).
:-using([sys,proc]).

start :-
	spawn(echod),
	true.

startw :-
	start,
	wait.

echod :-
	netproc([';name=ECHO'],Pid),
	repeat,
		recv(Pid,Msg),
		writeln(Msg),
		send(Pid,Msg),
		fail.

sendmsg(Msg) :-
	pid(';name=ECHO',Pid),
	rsvp(Pid,Msg,Resp),
	writeln(Resp),
	close(Pid).
