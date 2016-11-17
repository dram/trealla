:-module(app).
:-export([app_init/0,app_entry/4]).
:-use_module(app_server).
:-use_module(smtp_client).
:-use_module(auth).
:-use_module(blog).
:-using([sys,net]).

% Note: defined values can also be supplied in an 'app.conf'
% config file in the current directory, which will override the
% following values:

:-define(SessionId,'sid').
:-define(ValidUser,'VALID:USER').
:-define(AdminUser,'admin').
:-define(AdminPass,'secret').
:-define(Keep,true).
:-define(Title,'This is a blog').
:-define(Heading,'This is a description of the blog').
:-define(Footer,'Blog software by <a href="https://github.com/trealla-lang/webapps/">Trealla Webapps</a>').
:-define(AnonUser,'anonymous').
:-define(BlogPath,'/').
:-define(PageSize,20).
:-define(VerifyByEmail,false).
:-define(VerifySubject,'Verify your email address').
:-define(VerifyMessage,'Complete your registration: ').
:-define(ResetSubject,'Reset your password').
:-define(ResetMessage,'Complete your password reset: ').
:-define(ReversePosts,true).
:-define(ReverseComments,false).
:-define(Style,'<style>body {font-family:helvetica,arial,sans-serif} a:link {text-decoration:none}</style>').
:-define(Color1,'#C0C0C0').
:-define(Color2,'#707070').
:-define(ViewPort,'<meta name="viewport" content="width=device-width, initial-scale=1"/>').
:-define(More,'More >>>').

% This is the application layer...

app_post(S,Log,'/signup') :-
	http:form(S,'email',User),
	http:form(S,'nick',Nick),
	http:form(S,'password',Pass),
	http:form(S,'password2',Pass2),
	stash_get(S,'HTTP_HOST',Host),
	replace(Host,'www.','',Host2),
	concat(?AdminUser,'@',Host2,AdminUser),
	User == AdminUser, !,
	Pass == ?AdminPass,
	Pass == Pass2,
	nonvar(Nick),
	atom_length(Nick,N1), N1 >= 3,
	atom_length(Pass,N2), N2 >= 6,
	adduser(User,Pass),
	setuser_nick(User,Nick),
	%concat('DEBUG: adduser ',Nick,Msg), writeln(Msg),
	login(User,Pass,SessId,?Keep,Expires), !,
	stash_set(S,?ValidUser,User),
	format_rfcdate(Expires,ExpiresStr),
	concat('Set-Cookie: ',?SessionId,'=',SessId,'; Expires=',ExpiresStr,'; HttpOnly\r\n',Hdr1),
	concat(Hdr1,'Location: ',?BlogPath,'\r\n',Hdr),
	response(S,Log,302,'OK',Hdr).

app_post(S,Log,'/signup') :-
	http:form(S,'email',User),
	http:form(S,'nick',Nick),
	http:form(S,'password',Pass),
	http:form(S,'password2',Pass2),
	Pass == Pass2,
	atom_length(Nick,N1), N1 >= 3,
	atom_length(Pass,N2), N2 >= 6,
	split(User,'@',L,R),
	nonvar(R),
	adduser(User,Pass), !,
	setuser_nick(User,Nick),
	%concat('DEBUG: adduser ',Nick,Msg), writeln(Msg),
	!,

	(?VerifyByEmail ->
		start_verify(S,Log,User) ;
		login(User,Pass,SessId,?Keep,Expires)),

	stash_set(S,?ValidUser,User),
	format_rfcdate(Expires,ExpiresStr),
	concat('Set-Cookie: ',?SessionId,'=',SessId,'; Expires=',ExpiresStr,'; HttpOnly\r\n',Hdr1),
	concat(Hdr1,'Location: ',?BlogPath,'\r\n',Hdr),
	response(S,Log,302,'OK',Hdr).

app_post(S,Log,'/signup') :- !,
	Hdr = 'Location: /signup.html\r\n',
	response(S,Log,302,'PLEASE SIGN UP',Hdr).

app_post(S,Log,'/login') :-
	http:form(S,'email',User),
	http:form(S,'login',Login),
	nonvar(Login),
	http:form(S,'password',Pass),
	login(User,Pass,SessId,?Keep,Expires), !,
	getuser_nick(User,Nick),
	stash_set(S,?ValidUser,User),
	%concat('DEBUG: login ',Nick,Msg), writeln(Msg),
	format_rfcdate(Expires,ExpiresStr),
	concat('Set-Cookie: ',?SessionId,'=',SessId,'; Expires=',ExpiresStr,'; HttpOnly\r\n',Hdr1),
	concat(Hdr1,'Location: ',?BlogPath,'\r\n',Hdr),
	response(S,Log,302,'OK',Hdr).

app_post(S,Log,'/login') :-
	?VerifyByEmail,
	http:form(S,'email',User),
	http:form(S,'reset',Reset),
	nonvar(Reset),
	start_reset(S,Log,User).

app_post(S,Log,'/login') :- !,
	Hdr = 'Location: /login.html\r\n',
	response(S,Log,302,'PLEASE LOGIN',Hdr).

app_post(S,Log,'/logout') :- !,
	http:cookie(S,?SessionId,SessId),
	logout(SessId), !
	concat('Set-Cookie: ',?SessionId,'=; Expires=; HttpOnly\r\n',Hdr1),
	concat(Hdr1,'Location: ',?BlogPath,'\r\n',Hdr),
	response(S,Log,302,'OK',Hdr),
	stash_clr(S,?ValidUser,_).

app_post(S,Log,'/post') :-
	http:cookie(S,?SessionId,SessId),
	checkin(SessId,User,Expires),
	stash_set(S,?ValidUser,User),
	stash_get(S,'HTTP_HOST',Host),
	replace(Host,'www.','',Host2),
	concat(?AdminUser,'@',Host2,AdminUser),
	User == AdminUser, !,
	http:form(S,'head',Head),
	http:form(S,'body',Body),
	replace(Host,'www.','',BlogName),
	addpost(BlogName,Id,User,Head,Body),
	format_rfcdate(Expires,ExpiresStr),
	concat('Set-Cookie: ',?SessionId,'=',SessId,'; Expires=',ExpiresStr,'; HttpOnly\r\n',Hdr1),
	concat(Hdr1,'Location: ',?BlogPath,'\r\n',Hdr),
	response(S,Log,302,'OK',Hdr).

app_post(S,Log,'/post') :- !,
	Hdr = 'Location: /login.html\r\n',
	response(S,Log,302,'PLEASE LOGIN AS ADMIN',Hdr).

app_post(S,Log,'/addcomment') :-
	http:cookie(S,?SessionId,SessId),
	checkin(SessId,User,Expires),
	stash_set(S,?ValidUser,User),
	http:query(S,'id',PostId),
	http:query(S,'reply',ReplyId),
	http:form(S,'body',Body),
	getpost(PostId,Deleted,Locked,Created,Modified,Owner,TmpHead,TmpBody), !,
	Deleted = 0,
	Locked = 0,
	replace(Body,'<','&lt;',Body1),
	replace(Body1,'>','&gt;',Body2),
	replace(Body2,'&','&amp;',Body3),
	replace(Body3,'\r\n','<br/>',Body4),
	replace(Body4,'\n','<br/>',Body5),
	addcomment(PostId,Id,User,Body5),

	(ReplyId \= '' ->
		replycomment(ReplyId,Id) ;
		true),

	format_rfcdate(Expires,ExpiresStr),
	concat('Set-Cookie: ',?SessionId,'=',SessId,'; Expires=',ExpiresStr,'; HttpOnly\r\n',Hdr1),
	concat(Hdr1,'Location: /getpost?id=',PostId,'#',Id,'\r\n',Hdr),
	response(S,Log,302,'OK',Hdr).

app_post(S,Log,'/addcomment') :- !,
	Hdr = 'Location: /login.html\r\n',
	response(S,Log,302,'PLEASE LOGIN',Hdr).

app_post(S,Log,'/modnick') :-
	http:cookie(S,?SessionId,SessId),
	checkin(SessId,User,Expires),
	stash_set(S,?ValidUser,User),
	http:form(S,'nick',Nick),
	nonvar(Nick),
	atom_length(Nick,N1), N1 >= 3,
	setuser_nick(User,Nick),
	format_rfcdate(Expires,ExpiresStr),
	concat('Set-Cookie: ',?SessionId,'=',SessId,'; Expires=',ExpiresStr,'; HttpOnly\r\n',Hdr1),
	concat(Hdr1,'Location: ',?BlogPath,'\r\n',Hdr),
	response(S,Log,302,'OK',Hdr).

app_post(S,Log,'/modnick') :- !,
	Hdr = 'Location: /login.html\r\n',
	response(S,Log,302,'PLEASE LOGIN',Hdr).

app_post(S,Log,'/modpass') :-
	http:cookie(S,?SessionId,SessId),
	checkin(SessId,User,Expires),
	stash_set(S,?ValidUser,User),
	http:form(S,'password',Pass),
	http:form(S,'password2',Pass2),
	Pass == Pass2,
	setuser_pass(User,Pass),
	format_rfcdate(Expires,ExpiresStr),
	concat('Set-Cookie: ',?SessionId,'=',SessId,'; Expires=',ExpiresStr,'; HttpOnly\r\n',Hdr1),
	concat(Hdr1,'Location: ',?BlogPath,'\r\n',Hdr),
	response(S,Log,302,'OK',Hdr).

app_post(S,Log,'/modpass') :- !,
	Hdr = 'Location: /login.html\r\n',
	response(S,Log,302,'PLEASE LOGIN',Hdr).

app_get(S,Log,'/logout') :-
	app_post(S,Log,'/logout').

app_get(S,Log,'/delpost') :-
	http:cookie(S,?SessionId,SessId),
	checkin(SessId,User,Expires), !,
	stash_set(S,?ValidUser,User),
	http:query(S,'id',Id),
	getpost(Id,Deleted,Locked,Created,Modified,Owner,TmpHead,TmpBody), !,
	Deleted = 0,
	stash_get(S,'HTTP_HOST',Host),
	replace(Host,'www.','',Host2),
	concat(?AdminUser,'@',Host2,AdminUser),
	member(User,[AdminUser,Owner]), !,
	delpost(Id),
	format_rfcdate(Expires,ExpiresStr),
	concat('Set-Cookie: ',?SessionId,'=',SessId,'; Expires=',ExpiresStr,'; HttpOnly\r\n',Hdr1),
	concat(Hdr1,'Location: ',?BlogPath,'\r\n',Hdr),
	response(S,Log,302,'OK',Hdr).

app_get(S,Log,'/delpost') :-
	response(S,Log,404,'NOT FOUND','').

app_get(S,Log,'/undelpost') :-
	http:cookie(S,?SessionId,SessId),
	checkin(SessId,User,Expires), !,
	stash_set(S,?ValidUser,User),
	http:query(S,'id',Id),
	getpost(Id,Deleted,Locked,Created,Modified,Owner,TmpHead,TmpBody), !,
	stash_get(S,'HTTP_HOST',Host),
	replace(Host,'www.','',Host2),
	concat(?AdminUser,'@',Host2,AdminUser),
	member(User,[AdminUser,Owner]), !,
	undelpost(Id),
	format_rfcdate(Expires,ExpiresStr),
	concat('Set-Cookie: ',?SessionId,'=',SessId,'; Expires=',ExpiresStr,'; HttpOnly\r\n',Hdr1),
	concat(Hdr1,'Location: ',?BlogPath,'\r\n',Hdr),
	response(S,Log,302,'OK',Hdr).

app_get(S,Log,'/undelpost') :-
	response(S,Log,404,'NOT FOUND','').

app_get(S,Log,'/lockpost') :-
	http:cookie(S,?SessionId,SessId),
	checkin(SessId,User,Expires), !,
	stash_set(S,?ValidUser,User),
	http:query(S,'id',Id),
	getpost(Id,Deleted,Locked,Created,Modified,Owner,TmpHead,TmpBody), !,
	stash_get(S,'HTTP_HOST',Host),
	replace(Host,'www.','',Host2),
	concat(?AdminUser,'@',Host2,AdminUser),
	member(User,[AdminUser,Owner]), !,
	lockpost(Id),
	format_rfcdate(Expires,ExpiresStr),
	concat('Set-Cookie: ',?SessionId,'=',SessId,'; Expires=',ExpiresStr,'; HttpOnly\r\n',Hdr1),
	concat(Hdr1,'Location: ',?BlogPath,'\r\n',Hdr),
	response(S,Log,302,'OK',Hdr).

app_get(S,Log,'/lockpost') :-
	response(S,Log,404,'NOT FOUND','').

app_get(S,Log,'/unlockpost') :-
	http:cookie(S,?SessionId,SessId),
	checkin(SessId,User,Expires), !,
	stash_set(S,?ValidUser,User),
	http:query(S,'id',Id),
	getpost(Id,Deleted,Locked,Created,Modified,Owner,TmpHead,TmpBody), !,
	stash_get(S,'HTTP_HOST',Host),
	replace(Host,'www.','',Host2),
	concat(?AdminUser,'@',Host2,AdminUser),
	member(User,[AdminUser,Owner]), !,
	unlockpost(Id),
	format_rfcdate(Expires,ExpiresStr),
	concat('Set-Cookie: ',?SessionId,'=',SessId,'; Expires=',ExpiresStr,'; HttpOnly\r\n',Hdr1),
	concat(Hdr1,'Location: ',?BlogPath,'\r\n',Hdr),
	response(S,Log,302,'OK',Hdr).

app_get(S,Log,'/unlockpost') :-
	response(S,Log,404,'NOT FOUND','').

app_get(S,Log,'/delcomment') :-
	http:cookie(S,?SessionId,SessId),
	checkin(SessId,User,Expires),
	stash_set(S,?ValidUser,User),
	http:query(S,'post',PostId),
	http:query(S,'id',Id),
	getpost(PostId,Deleted,Locked,Created,Modified,PostOwner,_,_),
	getcomment(Id,_,_,Owner,_,_,_),
	stash_get(S,'HTTP_HOST',Host),
	replace(Host,'www.','',Host2),
	concat(?AdminUser,'@',Host2,AdminUser),
	member(User,[AdminUser,PostOwner,Owner]), !,
	delcomment(Id),
	format_rfcdate(Expires,ExpiresStr),
	concat('Set-Cookie: ',?SessionId,'=',SessId,'; Expires=',ExpiresStr,'; HttpOnly\r\n',Hdr1),
	concat(Hdr1,'Location: /getpost?id=',PostId,'\r\n',Hdr),
	response(S,Log,302,'OK',Hdr).

app_get(S,Log,'/delcomment') :- !,
	Hdr = 'Location: /login.html\r\n',
	response(S,Log,302,'PLEASE LOGIN AS ADMIN OR THE OWNER',Hdr).

app_get(S,Log,'/reply') :-
	http:cookie(S,?SessionId,SessId),
	checkin(SessId,User,Expires),
	stash_set(S,?ValidUser,User),
	http:query(S,'post',PostId),
	http:query(S,'id',Id),
	getpost(PostId,Deleted,Locked,Created,Modified,PostOwner,_,_),
	Locked = 0,
	getcomment(Id,_,_,Owner,CommentBody,_,_),
	concat(
		'<html>',
			'<head>',?Style,'<title>',?Title,' - ',Headline,'</title>',?ViewPort,'</head>\n',
			'<body>',
				'<div align="center">',
				'<table style="width:95%">',
					'<tr valign="top" bgcolor="',?Color1,'">',
						'<td style="width:50%" colspan="3">&nbsp;<font size="+2">',
							?Title,'</font><br/>&nbsp;',
							?Heading,'</td>',
						'<td style="width:50%" colspan="3" valign="middle" align="right">',
							'<a href="/account.html">',Nick,'</a>',
							Extras,
							'&nbsp;|&nbsp;<a href="/logout">Logout</a>&nbsp;',
						'</td>',
					'</tr>\n',
					'<tr>',
						'<td style="width:10%">&nbsp;</td>',
						'<td><br/><blockquote><i>',CommentBody,'</i></blockquote></td>',
					'</tr>\n',
					'<tr>',
						'<td style="width:10%">&nbsp;</td>',
						'<td colspan="4">','<br/>',
							'<form method="POST" action="/addcomment?id=',PostId,'&reply=',Id,'">',
								'<textarea name="body" placeholder="some text" cols="70" rows="10" required></textarea>',
								'<br/><br/>',
								'<input type="submit" value="Reply to Comment"/>',
							'</form>',
							'<br/>',
						'</td>',
						'<td>&nbsp;</td>',
					'</tr>\n',
					'<tr>',
						'<td colspan="6"><br/><hr/></td>',
					'</tr>\n',
				'</table>\n',
				?Footer,
				'</div>',
			'</body>\n',
		'</html>\n',
		Body),
	format_rfcdate(Expires,ExpiresStr),
	concat('Set-Cookie: ',?SessionId,'=',SessId,'; Expires=',ExpiresStr,'; HttpOnly\r\n',Hdr),
	response_body(S,Log,200,'OK',Body,Hdr).

app_get(S,Log,'/reply') :- !,
	Hdr = 'Location: /login.html\r\n',
	response(S,Log,302,'PLEASE LOGIN AS ADMIN OR THE OWNER',Hdr).

app_get(S,Log,'/getpost') :-
	http:cookie(S,?SessionId,SessId),
	checkin(SessId,User,Expires), !,
	stash_set(S,?ValidUser,User),
	getuser_nick(User,Nick),
	http:query(S,'id',Id),
	get_post(Id,Owner,Headline,Resp),
	list_comments(S,User,Owner,Id,Comments),
	stash_get(S,'HTTP_HOST',Host),
	replace(Host,'www.','',Host2),
	concat(?AdminUser,'@',Host2,AdminUser),

	(User == AdminUser ->
		Extras = '&nbsp;|&nbsp;<a href="/post.html">Create post</a>' ;
		Extras = ''),

	concat(
		'<html>',
			'<head>',?Style,'<title>',?Title,' - ',Headline,'</title>',?ViewPort,'</head>\n',
			'<body>',
				'<div align="center">',
				'<table style="width:95%">',
					'<tr valign="top" bgcolor="',?Color1,'">',
						'<td style="width:50%" colspan="3">&nbsp;<font size="+2">',
							?Title,'</font><br/>&nbsp;',
							?Heading,'</td>',
						'<td style="width:50%" colspan="3" valign="middle" align="right">',
							'<a href="/account.html">',Nick,'</a>',
							Extras,
							'&nbsp;|&nbsp;<a href="/logout">Logout</a>&nbsp;',
						'</td>',
					'</tr>\n',
					'<tr>',
						'<td style="width:10%">&nbsp;</td>',
						'<td>',Resp,'</td>',
					'</tr>\n',
					Comments,
					'<tr>',
						'<td style="width:10%">&nbsp;</td>',
						'<td colspan="4">','<br/>',
							'<form method="POST" action="/addcomment?id=',Id,'">',
								'<textarea name="body" placeholder="some text" cols="70" rows="10" required></textarea>',
								'<br/><br/>',
								'<input type="submit" value="New Comment"/>',
							'</form>',
							'<br/>',
						'</td>',
						'<td>&nbsp;</td>',
					'</tr>\n',
					'<tr>',
						'<td colspan="6"><br/><hr/></td>',
					'</tr>\n',
				'</table>\n',
				?Footer,
				'</div>',
			'</body>\n',
		'</html>\n',
		Body),
	format_rfcdate(Expires,ExpiresStr),
	concat('Set-Cookie: ',?SessionId,'=',SessId,'; Expires=',ExpiresStr,'; HttpOnly\r\n',Hdr),
	response_body(S,Log,200,'OK',Body,Hdr).

app_get(S,Log,'/getpost') :-
	User = ?AnonUser,
	stash_set(S,?ValidUser,User),
	http:query(S,'id',Id),
	get_post(Id,Owner,Headline,Resp),
	list_comments(S,User,Owner,Id,Comments),
	concat(
		'<html>',
			'<head>',?Style,'<title>',?Title,' - ',Headline,'</title>',?ViewPort,'</head>\n',
			'<body>',
				'<div align="center">',
				'<table style="width:95%">',
					'<tr valign="top" bgcolor="',?Color1,'">',
						'<td style="width:50%" colspan="3">&nbsp;<font size="+2">',
							?Title,'</font><br/>&nbsp;',
							?Heading,'</td>',
						'<td style="width:50%" colspan="3" valign="middle" align="right">'
							'<a href="/login.html">Login</a>&nbsp;|&nbsp;',
							'<a href="/signup.html">Signup</a>&nbsp;',
						'</td>',
					'</tr>\n',
					'<tr>',
						'<td style="width:10%">&nbsp;</td>',
						'<td>',Resp,'</td>',
					'</tr>\n',
					Comments,
					'<tr>',
						'<td colspan="6"><br/><hr/></td>',
					'</tr>\n',
				'</table>\n',
				?Footer,
				'</div>',
			'</body>\n',
		'</html>\n',
		Body),
	response_body(S,Log,200,'OK',Body,'').

app_get(S,Log,'/verify') :-
	http:query(S,'email',User),
	http:query(S,'code',CodeStr),
	atom_number(CodeStr,Code),
	getuser_locked(User,Code), !,						% check code matches
	setuser_locked(User,0),
	Hdr = 'Location: /login.html\r\n',
	response(S,Log,302,'PLEASE VERIFY THEN LOGIN',Hdr).

app_get(S,Log,'/verify') :-
	response(S,Log,404,'INVALID VERIFICATION CODE','').

app_get(S,Log,'/reset') :-
	http:query(S,'email',User),
	http:query(S,'uuid',Uuid),
	getuser_uuid(User,Uuid), !,							% check code matches
	setuser_pass(User,Uuid),
	login(User,Uuid,SessId,?Keep,Expires), !,
	format_rfcdate(Expires,ExpiresStr),
	concat('Set-Cookie: ',?SessionId,'=',SessId,'; Expires=',ExpiresStr,'; HttpOnly\r\n',Hdr1),
	concat(Hdr1,'Location: /modpass.html\r\n',Hdr),
	response(S,Log,302,'OK',Hdr).

app_get(S,Log,'/reset') :-
	response(S,Log,404,'INVALID VERIFICATION CODE','').

app_get(S,Log,?BlogPath) :-
	http:cookie(S,?SessionId,SessId),
	checkin(SessId,User,Expires), !,
	getuser_nick(User,Nick),
	stash_set(S,?ValidUser,User),
	http:query(S,'from',FromStr),

	(FromStr \== '' ->
		atom_number(FromStr,From) ;
		From = 1),

	stash_get(S,'HTTP_HOST',Host),
	replace(Host,'www.','',BlogName),
	list_posts(S,BlogName,User,From,Resp),
	replace(Host,'www.','',Host2),
	concat(?AdminUser,'@',Host2,AdminUser),

	(User == AdminUser ->
		Extras = '&nbsp;|&nbsp;<a href="/post.html">Create post</a>' ;
		Extras = ''),

	concat(
		'<html>',
			'<head>',?Style,'<title>',?Title,'</title>',?ViewPort,'</head>\n',
			'<body>',
				'<div align="center">',
				'<table style="width:95%">',
					'<tr valign="top" bgcolor="',?Color1,'">',
						'<td style="width:50%" colspan="3">',
							'&nbsp;<font size="+2">',?Title,'</font><br/>&nbsp;',?Heading,
						'</td>',
						'<td style="width:50%" colspan="3" valign="middle" align="right">',
							'<a href="/account.html">',Nick,'</a>',
							Extras,
							'&nbsp;|&nbsp;<a href="/logout">Logout</a>&nbsp;',
						'</td>',
					'</tr>\n',
					Resp,
				'</table>\n',
				?Footer,
				'</div>',
			'</body>\n',
		'</html>\n',
		Body),
	format_rfcdate(Expires,ExpiresStr),
	concat('Set-Cookie: ',?SessionId,'=',SessId,'; Expires=',ExpiresStr,'; HttpOnly\r\n',Hdr),
	response_body(S,Log,200,'OK',Body,Hdr).

app_get(S,Log,?BlogPath) :-
	User = ?AnonUser,
	stash_set(S,?ValidUser,User),
	http:query(S,'from',FromStr),

	(FromStr \== '' ->
		atom_number(FromStr,From) ;
		From = 1),

	stash_get(S,'HTTP_HOST',Host),
	replace(Host,'www.','',BlogName),
	list_posts(S,BlogName,User,From,Resp),
	concat(
		'<html>',
			'<head>',?Style,'<title>',?Title,'</title>',?ViewPort,'</head>\n',
			'<body>',
				'<div align="center">',
				'<table style="width:95%">',
					'<tr valign="top" bgcolor="',?Color1,'">',
						'<td style="width:50%" colspan="3">',
							'&nbsp;<font size="+2">',?Title,'</font><br/>&nbsp;',?Heading,
						'</td>',
						'<td style="width:50%" colspan="3" valign="middle" align="right">',
							'<a href="/login.html">Login</a>',
							'&nbsp;|&nbsp;',
							'<a href="/signup.html">Signup</a>&nbsp;',
						'</td>',
					'</tr>\n',
					Resp,
				'</table>\n',
				?Footer,
				'</div>',
			'</body>\n',
		'</html>\n',
		Body),
	response_body(S,Log,200,'OK',Body,'').

app_get(S,Log,Path) :-
	fail.

% Email verification step

start_verify(S,Log,User) :-
	rand(Rand),
	R is Rand+1,
	setuser_locked(User,R),
	stash_get(S,'HTTP_SCHEME',Scheme),
	stash_get(S,'HTTP_HOST',Host),
	stash_get(S,'HTTP_PORT',Port),
	concat(Scheme,'://',Host,':',Port,'/verify?email=',User,'&code=',R,Link),
	concat(
		'<html>',
			'<head>',?Style,'<title>',?VerifySubject,'</title>','</head>\n',
			'<body>',
				'<h3>',?VerifyMessage,
					'<a href="',Link,'">Verify</a>',
				'</h3>',
			'</body>\n',
		'</html>\n',
		Msg),

	(Host == 'localhost' ->
		Host2 = 'example.com' ;
		Host2 = Host),

	concat('noreply@',Host2,From),
	smtp_client:send_html(From,User,?VerifySubject,Msg),
	concat('Location: /login.html\r\n',Hdr),
	response(S,Log,302,'OK',Hdr).

start_verify(S,Log,User) :-
	concat('WARN: could not send verify email: ',User,'\n',Msg),
	write(Msg),
	fail.

start_reset(S,Log,User) :-
	stash_get(S,'HTTP_SCHEME',Scheme),
	stash_get(S,'HTTP_HOST',Host),
	stash_get(S,'HTTP_PORT',Port),
	getuser_uuid(User,Uuid),
	concat(Scheme,'://',Host,':',Port,'/reset?email=',User,'&code=',Uuid,Link),
	concat(
		'<html>',
			'<head>',?Style,'<title>',?ResetSubject,'</title>','</head>\n',
			'<body>',
				'<h3>',?ResetMessage,
					'<a href="',Link,'">Verify</a>',
				'</h3>',
			'</body>\n',
		'</html>\n',
		Msg),

	(Host == 'localhost' ->
		Host2 = 'example.com' ;
		Host2 = Host),

	concat('noreply@',Host2,From),
	smtp_client:send_html(From,User,?VerifySubject,Msg),
	concat('Location: /login.html\r\n',Hdr),
	response(S,Log,302,'OK',Hdr).

start_reset(S,Log,User) :-
	concat('WARN: could not send verify email: ',User,'\n',Msg),
	write(Msg),
	fail.

% Get blog post...

get_post(Id,Owner,Headline,Out) :-
	getpost(Id,Deleted,Locked,Created,Modified,Owner,Headline,Body), !,
	replace(Body,'\r\n','<br/>',Body1),
	replace(Body1,'\n','<br/>',Body2),
	getuser_nick(Owner,Nick),
	Elapsed is now-Created,
	DaysAgo is floor(Elapsed/60/60/24),
	getcomments(Id,CL),
	length(CL,Nbr),
	concat('<a href="/getpost?id=',Id,'">',Headline,'</a>',Line1),
	concat(Line1,'<br/><font size="-1" color="',?Color2,'">by ',Nick,' ',DaysAgo,' days ago&nbsp;|&nbsp;',Nbr,'&nbsp;comments</font>',Line2),
	Spacer = '<tr><td colspan="6"></td></tr>\n',
	concat(Spacer,
		'<tr valign="top">',
			'<td style="width:10%"></td>',
			'<td colspan="4">',Line2,'</td>',
			'<td>&nbsp;</td>',
		'</tr>\n',Spacer,
		'<tr valign="top">',
			'<td>&nbsp;</td>',
			'<td colspan="4">',Body2,'</td>',
			'<td>&nbsp;</td>',
		'</tr>\n',
		Spacer,Out).

get_post(Id,Owner,Headline,Out) :-
	response(S,Log,404,'NOT FOUND','').

% List comments for post

list_comment(S,User,PostOwner,PostId,[],N,Out,Out) :- !.

list_comment(S,User,PostOwner,PostId,[Id|L1],N,In,Out) :-
	getcomment(Id,Created,Modified,Owner,Body,ReplyTo,Replies), !,
	Elapsed is now-Created,
	HrsAgo is floor(Elapsed/60/60),
	getuser_nick(Owner,Nick),
	list_comment(S,User,PostOwner,PostId,Replies,1,'',RepliesStr),
	efface(Replies,L1,L),
	stash_get(S,'HTTP_HOST',Host),
	replace(Host,'www.','',Host2),
	concat(?AdminUser,'@',Host2,AdminUser),

	(member(User,[AdminUser,PostOwner,Owner]) ->
		concat(' | <a href="/delcomment?post=',PostId,'&id=',Id,'">&nbsp;X&nbsp;</a>',Extras1) ;
		Extras1 = ''), !,

	(User \== ?AnonUser ->
		(nonvar(ReplyTo) ->
			concat(Extras1,' | <a href="/reply?post=',PostId,'&id=',Id,'"><font size="+1">&nbsp;&laquo;&nbsp;</font></a>',Extras) ;
			Extras = '') ;
		Extras = ''),

	concat(In,
		'<tr>',
			'<td colspan="6">&nbsp;</td>',
		'</tr>\n',
		'<tr>',
			'<td style="width:10%"><a id="',Id,'">&nbsp;</a></td>',
			'<td colspan="4"><font size="-1" color="',?Color2,'">',
				'<a href="/getpost?id=',PostId,'#',Id,'">',N,'. </a>', Nick,
				'&nbsp;',HrsAgo,'&nbsp;hrs&nbsp;ago',Extras,'</font>',
			'</td>',
			'<td>&nbsp;</td>',
		'</tr>\n',
		'<tr>',
			'<td>&nbsp;</td>',
			'<td colspan="4">',Body,'</td>',
			'<td>&nbsp;</td>',
		'</tr>\n',
		'<tr>',
			'<td>&nbsp;</td>',
			'<td><table style="width:95%">',
			RepliesStr,
			'</table></td>',
			'<td>&nbsp;</td>',
		'</tr>\n',
			In3),
	N2 is N+1,
	list_comment(S,User,PostOwner,PostId,L,N2,In3,Out).

list_comment(S,User,PostOwner,PostId,[Id|L],N,In,Out) :-
	N2 is N+1,
	list_comment(S,User,PostOwner,PostId,L,N2,In,Out).

list_comments(S,User,Owner,Id,Out) :-
	getcomments(Id,L),
	(?ReverseComments -> reverse(L,L2) ; L2 = L),
	list_comment(S,User,Owner,Id,L2,1,'',Out).

% List blog posts...

list_post(S,User,[],I,From,I,N,Out,Out) :- !.
list_post(S,User,L,I,From,I,0,Out,Out) :- !.

list_post(S,User,[Id|L],I,From,End,N,In,Out) :-
	getpost(Id,Deleted,Locked,Created,Modified,Owner,Headline,Body),
	stash_get(S,'HTTP_HOST',Host),
	replace(Host,'www.','',Host2),
	concat(?AdminUser,'@',Host2,AdminUser),
	(member(User,[AdminUser,Owner]) -> true ; Deleted = 0), !,
	I >= From, !,
	getuser_nick(Owner,Nick),
	Elapsed is now-Created,
	DaysAgo is floor(Elapsed/60/60/24),
	getcomments(Id,CL),
	length(CL,Nbr),
	concat('<a href="/getpost?id=',Id,'">',Headline,'</a>',Line1),
	concat(Line1,'<br/><font size="-1" color="',?Color2,'">by ',Nick,' ',DaysAgo,' days ago&nbsp;|&nbsp;',Nbr,'&nbsp;comments',Line2),

	(member(User,[AdminUser,Owner]) ->
		(Deleted \= 0 ->
			concat(Line2,'&nbsp;|&nbsp;[<a href="/undelpost?id=',Id,'">&nbsp;_&nbsp;</a>]',Line3) ;
			concat(Line2,'&nbsp;|&nbsp;[<a href="/delpost?id=',Id,'">&nbsp;X&nbsp;</a>]',Line3)) ;
		concat(Line2,Line3)), !,

	(member(User,[AdminUser,Owner]) ->
		(Locked \= 0 ->
			concat(Line3,'&nbsp;|&nbsp;[<a href="/unlockpost?id=',Id,'">&nbsp;&para;&nbsp;</a>]',Line) ;
			concat(Line3,'&nbsp;|&nbsp;[<a href="/lockpost?id=',Id,'">&nbsp;&sect;&nbsp;</a>]',Line)) ;
		concat(Line3,'</font>',Line)), !,

	(I < 10 ->
		concat('&nbsp;',I,Istr) ;
		concat('',I,Istr)),

	concat(In,
		'<tr valign="top">',
			'<td style="width:10%" align="right"><code>',Istr,'.</code></td>',
			'<td colspan="4">',Line,'</font></td>',
			'<td>&nbsp;</td>',
		'</tr>\n',
		In2),
	I2 is I+1,
	N2 is N-1,
	list_post(S,User,L,I2,From,End,N2,In2,Out).

list_post(S,User,[Id|L],I,From,End,N,In,Out) :-
	I2 is I+1,
	list_post(S,User,L,I2,From,End,N,In,Out).

list_posts(S,BlogName,User,From,Resp) :-
	getposts(BlogName,L),
	(?ReversePosts -> reverse(L,L2) ; L2 = L),
	list_post(S,User,L2,1,From,Next,?PageSize,'',Body),
	Spacer1 = '<tr><td colspan="6">&nbsp;</td></tr>\n',
	Spacer2 = '<tr><td colspan="6"><hr/></td></tr>\n',
	concat(Spacer1,Body,Spacer1,
		'<tr>',
			'<td></td>',
			'<td colspan="4"><font size="-1" face="sans-serif">',
				'<a href="/?from=',Next,'">&nbsp;',?More,'&nbsp;</a>',
				'</font>',
			'</td>',
			'<td>&nbsp;</td>',
		'</tr>\n',
		Spacer2,Resp).

% If true fall back to HTTP serving files...

app_entry(S,Log,Cmd,Path) :-
	begins(Path,['/public/','/images/']),
	!.

app_entry(S,Log,Cmd,Path) :-
	member(Path,['/signup.html','/login.html','/favicon.ico','/robots.txt']),
	!.

% Else handle our forms for signup/login...

app_entry(S,Log,'POST',Path) :-
	http:www_form(S),
	app_post(S,Log,Path),
	!, fail.

% or handle logout...

app_entry(S,Log,'GET',Path) :-
	app_get(S,Log,Path),
	!, fail.

% Otherwise check auth credentials...

app_entry(S,Log,Cmd,'/post.html') :-
	http:cookie(S,?SessionId,SessId),
	stash_get(S,'HTTP_HOST',Host),
	replace(Host,'www.','',Host2),
	concat(?AdminUser,'@',Host2,AdminUser),
	User = AdminUser,
	checkin(SessId,User,Expires),
	stash_set(S,?ValidUser,User),
	!.

app_entry(S,Log,Cmd,Path) :-
	http:cookie(S,?SessionId,SessId),
	checkin(SessId,User,Expires),
	stash_set(S,?ValidUser,User),
	!.

% Nope, ask to log in...

app_entry(S,Log,Cmd,Path) :-
	concat('Location: ',?BlogPath,'\r\n',Hdr),
	response(S,Log,302,'PLEASE LOGIN OR SIGNUP',Hdr),
	fail.

% Anything to do on startup

app_init :-
	auth:init,
	blog:init,
	true.

