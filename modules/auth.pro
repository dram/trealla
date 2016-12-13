:-module(auth,[init/0]).
:-export([adduser/2,deluser/1,login/5,logout/1,checkin/3]).
:-export([listusers/1,dumpusers/0]).
:-export([setuser_email/2,getuser_email/2]).
:-export([setuser_nick/2,getuser_nick/2]).
:-export([setuser_locked/2,getuser_locked/2]).
:-export([setuser_pass/2,getuser_uuid/2]).

:-use_module(dict).
:-using([sys]).

% Note: defined values can also be supplied in an 'auth.conf'
% config file in the current directory, which will override the
% following values:

:-define(MaxAge,300).
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

init :-
	dbs:load.

adduser(User,Pass) :-
	auth_user(User,_),
	!, fail.

adduser(User,Pass) :-
	\+ auth_user(User,D),
	rand(Salt),
	concat(Salt,Pass,Str),
	sha2(Str,Hash),
	split(User,'@',L,R),
	dict:set([],?FieldCreated,Now,D0),
	(var(R) -> dict:set(D0,?FieldNick,User,D1) ; dict:set(D0,?FieldEmail,User,D1)),
	now(Now),
	uuid(Uuid),
	dict:set(D1,?FieldModified,Now,D2),
	dict:set(D2,?FieldSalt,Salt,D3),
	dict:set(D3,?FieldHash,Hash,D4),
	dict:set(D4,?FieldUuid,Uuid,D5),
	assertz(auth_user(User,D5)).

deluser(User) :-
	dbs:begin,
	now(Now),
	retract(auth_user(User,D)), !,
	dict:set(D,?FieldDeleted,1,D1),
	dict:set(D2,?FieldModified,Now,D3),
	assertz(auth_user(User,D3)),
	dbs:end.

% Note: we assert/1 the SesssId. This means it is held in memory only
% and is not persisted to the file system. An attempt to login with an
% expired SessId will cause it to be deleted, but otherwise zombie
% SessIds will collect and only be cleared out on restart.

login(User,Pass,SessId,Keep,Expires) :-
	auth_user(User,D),
	dict:get(D,?FieldSalt,Salt),
	dict:get(D,?FieldHash,Hash),
	concat(Salt,Pass,Str),
	sha2(Str,Hash2),
	Hash == Hash2,
	dict:get(D,?FieldDeleted,0),
	dict:get(D,?FieldLocked,0),
	uuid(Uuid),
	sha2(Uuid,SessId),
	(Keep == true -> MaxAge is 1440*60*?KeepDays ; MaxAge is ?MaxAge),
	Expires is now+MaxAge,
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
	dict:get(D,?FieldDeleted,0),
	dict:get(D,?FieldLocked,0),
	Expires > now, !,
	ValidatedUser = User,
	dbs:begin,
	retract(auth_session(SessId,D)),
	NewExpires is now+MaxAge,
	dict:set(D,?FieldExpires,NewExpires,D1),
	dict:set(D1,?FieldModified,Now,D2),
	assert(auth_session(SessId,D2)),
	dbs:end.

checkin(SessId,ValidatedUser,NewExpires) :-
	retract(auth_session(SessId,_D)),
	fail.

logout(SessId) :-
	retract(auth_session(SessId,_D)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

listusers(L) :-
	findall(User,auth_user(User,_D),L).

dumpuser([]).
dumpuser([User|Tail]) :-
	writeln(User),
	dumpuser(Tail).

dumpusers :-
	listusers(L),
	dumpuser(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	dict:get(D,?FieldEmail,Email),
	!.

getuser_email(User,'').

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
	dict:get(D,?FieldNick,Nick),
	!.

getuser_nick(User,'').

setuser_locked(User,0) :-
	now(Now),
	dbs:begin,
	retract(auth_user(User,D)),
	dict:del(D,?FieldLocked,D1),
	dict:set(D1,?FieldModified,Now,D2),
	assertz(auth_user(User,D2)),
	dbs:end.

setuser_locked(User,Locked) :-
	now(Now),
	dbs:begin,
	retract(auth_user(User,D)), !,
	dict:set(D,?FieldLocked,Locked,D1),
	dict:set(D1,?FieldModified,Now,D2),
	assertz(auth_user(User,D2)),
	dbs:end.

getuser_locked(User,Locked) :-
	auth_user(User,D),
	dict:get(D,?FieldLocked,Locked).

setuser_pass(User,Pass) :-
	now(Now),
	rand(Salt),
	concat(Salt,Pass,Str),
	sha2(Str,Hash),
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

