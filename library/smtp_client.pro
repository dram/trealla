:-module(smtp_client).
:-export([send_plain/4,send_html/4,send_base64/4]).
:-export([send_plain/5,send_html/5,send_base64/5]).
:-export([send/6]).
:-define(Server,'localhost:25').

:-using([sys]).

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
	net:readmsg(S,Resp),
	left(Resp,_,Code2),
	%write(Resp),
	true.

send(Server,From,To,Subject,Body,ContentType) :-

	net:client(Server,S),
	reply(S,220),

	net:local_host(S,Name),
	atomic_list_concat(['HELO ',Name,'\r\n'],Helo),
	write(S,Helo),
	reply(S,250),

	atomic_list_(['MAIL FROM: <',From,'>\r\n'],MailFrom),
	write(S,MailFrom),
	reply(S,250),

	atomic_list_concat(['RCPT TO: <',To,'>\r\n'],RcptTo),
	write(S,RcptTo),
	reply(S,250),

	atomic_list_concat(['DATA\r\n'],Data),
	write(S,Data),
	reply(S,354),

	atomic_list_concat(['From: <',From,'>\r\n'],Hdr1),
	atomic_list_concat([Hdr1,'To: <',To,'>\r\n'],Hdr2),
	atomic_list_concat([Hdr2,'Subject: ',Subject,'\r\n'],Hdr3),
	atomic_list_concat([Hdr3,'Content-Type: ',ContentType,'\r\n'],Hdr4),
	atomic_list_concat([Hdr4,'MIME-Version: 1.0\r\n\r\n',Body,'\r\n.\r\n'],Msg),
	write(S,Msg),
	reply(S,250),

	write(S,'QUIT\r\n'),
	reply(S,221),

	close(S).
