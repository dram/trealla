:-module(blog,[init/0]).
:-export([addpost/5,delpost/1,lockpost/1,getpost/8,getposts/2]).
:-export([addcomment/4,replycomment/2,delcomment/1,undelcomment/1]).
:-export([getcomment/6,getcomments/2]).
:-use_module(dict).
:-using([sys]).

:-define(FieldCreated,'created').
:-define(FieldModified,'modified').
:-define(FieldOwner,'owner').
:-define(FieldLocked,'locked').
:-define(FieldDeleted,'deleted').
:-define(FieldPost,'post').
:-define(FieldHead,'head').
:-define(FieldBody,'body').
:-define(FieldReplies,'replies').
:-define(FieldReplyto,'replyto').

% These are our database records:

:-persist(blog_name/2).
:-persist(blog_blog/2).
:-persist(blog_post/2).
:-persist(blog_comments/2).
:-persist(blog_comment/2).
:-persist(blog_reply/3).

% At the moment we are creating blogs on demand. To rename a
% blog you can just alter the BlogName -> BlogId mapping in the
% database.

init :-
	dbs:load.

addpost(BlogName,Id,User,Head,Body) :-
	\+ blog_name(BlogName,BlogId),
	uuid(BlogId),
	assertz(blog_name(BlogName,BlogId)),
	fail.

addpost(BlogName,Id,User,Head,Body) :-
	blog_name(BlogName,BlogId),
	uuid(Id),
	now(Now),
	dict:set([],?FieldCreated,Now,D0),
	dict:set(D0,?FieldModified,Now,D1),
	dict:set(D1,?FieldOwner,User,D2),
	dict:set(D2,?FieldHead,Head,D3),
	dict:set(D3,?FieldBody,Body,D4),
	dbs:begin,
	assertz(blog_blog(BlogId,Id)),
	assertz(blog_post(Id,D4)),
	dbs:end.

lockpost(Id) :-
	now(Now),
	dbs:begin,
	retract(blog_post(Id,D)),
	dict:set(D,?FieldLocked,1,D1),
	dict:set(D1,?FieldModified,Now,D2),
	assertz(blog_post(Id,D2)),
	dbs:end.

unlockpost(Id) :-
	now(Now),
	dbs:begin,
	retract(blog_post(Id,D)),
	dict:del(D,?FieldLocked,D1),
	dict:set(D1,?FieldModified,Now,D2),
	assertz(blog_post(Id,D2)),
	dbs:end.

delpost(Id) :-
	now(Now),
	dbs:begin,
	retract(blog_post(Id,D)),
	dict:set(D,?FieldDeleted,1,D1),
	dict:set(D1,?FieldModified,Now,D2),
	assertz(blog_post(Id,D2)),
	dbs:end.

undelpost(Id) :-
	now(Now),
	dbs:begin,
	retract(blog_post(Id,D)),
	dict:del(D,?FieldDeleted,D1),
	dict:set(D1,?FieldModified,Now,D2),
	assertz(blog_post(Id,D2)),
	dbs:end.

getpost(Id,Deleted,Locked,Created,Modified,User,Head,Body) :-
	blog_post(Id,D),
	dict:get(D,?FieldDeleted,Deleted),
	dict:get(D,?FieldLocked,Locked),
	dict:get(D,?FieldCreated,Created),
	dict:get(D,?FieldModified,Modified),
	dict:get(D,?FieldOwner,User),
	dict:get(D,?FieldHead,Head),
	dict:get(D,?FieldBody,Body),
	true.

getposts(BlogName,L) :-
	blog_name(BlogName,BlogId),
	findall(Id,blog_blog(BlogId,Id),L).

getposts(BlogName,[]).

%

addcomment(PostId,Id,User,Body) :-
	uuid(Id),
	now(Now),
	dict:set([],?FieldCreated,Now,D0),
	dict:set(D0,?FieldModified,Now,D1),
	dict:set(D1,?FieldOwner,User,D2),
	dict:set(D2,?FieldBody,Body,D3),
	dbs:begin,
	assertz(blog_comments(PostId,Id)),
	assertz(blog_comment(,Id,D3)),
	dbs:end.

replycomment(WhoId,Id) :-
	assertz(blog_reply(WhoId,Id,[])).

delcomment(Id) :-
	now(Now),
	dbs:begin,
	retract(blog_comment(Id,D)),
	dict:set(D,?FieldDeleted,1,D1),
	dict:set(D1,?FieldModified,Now,D2),
	assertz(blog_comment(Id,D2)),
	dbs:end.

undelcomment(Id) :-
	now(Now),
	dbs:begin,
	retract(blog_comment(Id,D)),
	dict:del(D,?FieldDeleted,D1),
	dict:set(D1,?FieldModified,Now,D2),
	assertz(blog_comment(Id,D2)),
	dbs:end.

getcomment(Id,Created,Modified,User,Body,ReplyTo,L) :-
	blog_comment(Id,D),
	dict:get(D,?FieldDeleted,Deleted),
	!, Deleted = 0,
	dict:get(D,?FieldCreated,Created),
	dict:get(D,?FieldModified,Modified),
	dict:get(D,?FieldOwner,User),
	dict:get(D,?FieldBody,Body),
	dict:get(D,?FieldReplyto,ReplyTo),
	dict:get(D,?FieldReplies,Replies),
	findall(Who,blog_reply(Id,Who,_D),L).

getcomments(PostId,L) :-
	findall(Id,blog_comments(PostId,Id),L).
