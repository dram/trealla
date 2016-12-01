:-module(smtp_client).
:-export([send_plain/4,send_html/4,send_base64/4]).
:-export([send_plain/5,send_html/5,send_base64/5]).
:-export([send/6]).
:-define(Server,'localhost:25').

:-use_module(sys).
:-use_module(net).

send_plain(From,To,Subject,Body) :-
	replace(Body,'\n.','\n..',Body2),
	send(?Server,From,To,Subject,Body2,'text/plain; charset=utf-8\r\nContent-Transfer-Encoding: 8bit').

send_plain(Server,From,To,Subject,Body) :-
	replace(Body,'\n.','\n..',Body2),
	send(Server,From,To,Subject,Body2,'text/plain; charset=utf-8\r\nContent-Transfer-Encoding: 8bit').

send_html(From,To,Subject,Body) :-
	replace(Body,'\n.','\n..',Body2),
	send(?Server,From,To,Subject,Body,'text/html; charset=utf-8\r\nContent-Transfer-Encoding: 8bit').

send_html(Server,From,To,Subject,Body) :-
	replace(Body,'\n.','\n..',Body2),
	send(Server,From,To,Subject,Body,'text/html; charset=utf-8\r\nContent-Transfer-Encoding: 8bit').

send_base64(From,To,Subject,Body) :-
	b64_encode(Body,Body2),
	send(?Server,From,To,Subject,Body2,'application/octet-stream; charset=us-ascii\r\nContent-Transfer-Encoding: base64').

send_base64(Server,From,To,Subject,Body) :-
	b64_encode(Body,Body2),
	send(Server,From,To,Subject,Body2,'application/octet-stream; charset=us-ascii\r\nContent-Transfer-Encoding: base64').

reply(S,Code) :-
	term_to_atom(Code,Code2),
	readmsg(S,Resp),
	left(Resp,_,Code2),
	%write(Resp),
	true.

send(Server,From,To,Subject,Body,ContentType) :-

	client(Server,S),
	reply(S,220),

	local_host(S,Name),
	concat('HELO ',Name,'\r\n',Helo),
	write(S,Helo),
	reply(S,250),

	concat('MAIL FROM: <',From,'>\r\n',MailFrom),
	write(S,MailFrom),
	reply(S,250),

	concat('RCPT TO: <',To,'>\r\n',RcptTo),
	write(S,RcptTo),
	reply(S,250),

	concat('DATA\r\n',Data),
	write(S,Data),
	reply(S,354),

	concat('From: <',From,'>\r\n',Hdr1),
	concat(Hdr1,'To: <',To,'>\r\n',Hdr2),
	concat(Hdr2,'Subject: ',Subject,'\r\n',Hdr3),
	concat(Hdr3,'Content-Type: ',ContentType,'\r\n',Hdr4),
	concat(Hdr4,'MIME-Version: 1.0\r\n\r\n',Body,'\r\n.\r\n',Msg),
	write(S,Msg),
	reply(S,250),

	write(S,'QUIT\r\n'),
	reply(S,221),

	close(S).
