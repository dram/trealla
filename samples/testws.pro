:-using([sys,net,ws]).

echo(Server,Msg) :-
	client(Server,S),
	request(S,'/',Status),
	Status = 101,
	msg(S,'data',Msg),
	parse(S,Op2,Resp),
	Op2 = 'data',
	concat('Sent: "',Msg,'", Received: "',Resp,'"',Msg2),
	write(Msg2), nl,
	close(S).

ping(Server,Msg) :-
	client(Server,S),
	request(S,'/',Status),
	Status = 101,
	msg(S,'ping',Msg),
	parse(S,Op2,Resp),
	Op2 = 'pong',
	concat('Ping: "',Msg,'", Pong: "',Resp,'"',Msg2),
	write(Msg2), nl,
	close(S).

