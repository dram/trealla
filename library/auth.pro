:-module(auth).
:-export([init/0]).
:-export([adduser/2,deluser/1,login/5,logout/1,checkin/3]).
:-export([listusers/1,dumpusers/0]).
:-export([setuser_email/2,getuser_email/2]).
:-export([setuser_nick/2,getuser_nick/2]).
:-export([setuser_locked/2,getuser_locked/2]).
:-export([setuser_pass/2,getuser_uuid/2]).
:-export([session_set/3,session_get/3]).
:-import(library(dict)).

% Note: defined values can also be supplied in an 'auth.conf'
% config file in the current directory, which will override the
% following values:

:-define(MaxAge,600).			% 10 mins
:-define(KeepDays,7).

:-define(FieldNick,nick).
:-define(FieldEmail,email).
:-define(FieldCreated,created).
:-define(FieldModified,modified).
:-define(FieldDeleted,deleted).
:-define(FieldSalt,salt).
:-define(FieldHash,hash).
:-define(FieldUser,user).
:-define(FieldExpires,expires).
:-define(FieldMaxAge,maxage).
:-define(FieldLocked,locked).
:-define(FieldUuid,uuid).

% These are our database records:

:-dynamic(auth_user/2,[persist]).
:-dynamic(auth_session/2).

:-using([sys]).

init :-
	dbs:load.

adduser(User,Pass) :-
	rand(Salt),
	concat(Salt,Pass,Str),
	sha256(Str,Hash),
	split(User,'@',L,R),
	dict:set([],?FieldCreated,Now,D0),
	(var(R) -> dict:set(D0,?FieldNick,User,D1) ; dict:set(D0,?FieldEmail,User,D1)),
	now(Now),
	uuid(Uuid),
	dict:set(D1,?FieldModified,Now,D2),
	dict:set(D2,?FieldSalt,Salt,D3),
	dict:set(D3,?FieldHash,Hash,D4),
	dict:set(D4,?FieldUuid,Uuid,D5),
	dbs:begin,
	\+ auth_user(User,_),
	assertz(auth_user(User,D5)),
	dbs:end.

deluser(User) :-
	now(Now),
	dbs:begin,
	retract(auth_user(User,D)),
	dict:set(D,?FieldDeleted,1,D1),
	assertz(auth_user(User,D1)),
	dbs:end.

login(User,Pass,SessId,Keep,Expires) :-
	auth_user(User,D),
	dict:get(D,?FieldSalt,Salt),
	dict:get(D,?FieldHash,Hash),
	concat(Salt,Pass,Str),
	sha256(Str,Hash2),
	Hash == Hash2,
	dict:get(D,?FieldDeleted,0),
	dict:get(D,?FieldLocked,0),
	uuid(Uuid),
	sha256(Uuid,SessId),
	(Keep == true -> MaxAge is 1440 * 60 * ?KeepDays ; MaxAge is ?MaxAge),
	Expires is now + MaxAge,
	dict:set([],?FieldUser,User,D1),
	dict:set(D1,?FieldExpires,Expires,D2),
	dict:set(D2,?FieldMaxAge,MaxAge,D3),
	assert(auth_session(SessId,D3)).

checkin(SessId,ValidatedUser,NewExpires) :-
	auth_session(SessId,D),
	dict:get(D,?FieldUser,User),
	nonvar(User),
	dict:get(D,?FieldExpires,Expires),
	dict:get(D,?FieldMaxAge,MaxAge),
	Expires > now,
	ValidatedUser = User,
	NewExpires is now + MaxAge,
	dbs:begin,
	retract(auth_session(SessId,_)),
	dict:set(D,?FieldExpires,NewExpires,D1),
	assert(auth_session(SessId,D1)),
	dbs:end.
checkin(SessId,ValidatedUser,NewExpires) :-
	retract(auth_session(SessId,_)),
	fail.

logout(SessId) :-
	retract(auth_session(SessId,_)).

setuser_email(User,Email) :-
	now(Now),
	dbs:begin,
	retract(auth_user(User,D)),
	dict:set(D,?FieldEmail,Email,D1),
	dict:set(D1,?FieldModified,Now,D2),
	assertz(auth_user(User,D2)),
	dbs:end.

getuser_email(User,Email) :-
	auth_user(User,D),
	dict:get(D,?FieldEmail,Email).

setuser_nick(User,Nick) :-
	now(Now),
	dbs:begin,
	retract(auth_user(User,D)),
	dict:set(D,?FieldNick,Nick,D1),
	dict:set(D1,?FieldModified,Now,D2),
	assertz(auth_user(User,D2)),
	dbs:end.

getuser_nick(User,Nick) :-
	auth_user(User,D),
	dict:get(D,?FieldNick,Nick).

setuser_locked(User,0) :-
	now(Now),
	dbs:begin,
	retract(auth_user(User,D)),
	dict:del(D,?FieldLocked,D1),
	assertz(auth_user(User,D1)),
	dbs:end.

setuser_locked(User,Locked) :-
	now(Now),
	dbs:begin,
	retract(auth_user(User,D)), !,
	dict:set(D,?FieldLocked,Locked,D1),
	assertz(auth_user(User,D1)),
	dbs:end.

getuser_locked(User,Locked) :-
	auth_user(User,D),
	dict:get(D,?FieldLocked,Locked).

setuser_pass(User,Pass) :-
	now(Now),
	rand(Salt),
	concat(Salt,Pass,Str),
	sha256(Str,Hash),
	dbs:begin,
	retract(auth_user(User,D)),
	dict:set(D,?FieldHash,Hash,D1),
	dict:set(D1,?FieldSalt,Salt,D2),
	dict:set(D2,?FieldModified,Now,D3),
	assertz(auth_user(User,D3)),
	dbs:end.

getuser_uuid(User,Uuid) :-
	auth_user(User,D),
	dict:get(D,?FieldUuid,Uuid).

session_set(SessId,Name,Value) :-
	dbs:begin,
	retract(auth_session(SessId,D)),
	concat('user$',Name,ActualName),
	dict:set(D,ActualName,Value,D1),
	assertz(auth_session(SessId,D1)),
	dbs:end.

session_get(SessId,Name,Value) :-
	auth_session(SessId,D),
	concat('user$',Name,ActualName),
	dict:get(D,ActualName,Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

listusers(L) :-
	findall(User,auth_user(User,_),L).

dumpuser([]).
dumpuser([User|Tail]) :-
	writeln(User),
	dumpuser(Tail).

dumpusers :-
	listusers(L),
	dumpuser(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
