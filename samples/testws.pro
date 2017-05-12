:-using([sys]).

echo(Server,Msg) :-
	net:client(Server,S),
	ws:request(S,'/',Status),
	Status = 101,
	ws:msg(S,'data',Msg),
	ws:parse(S,Op2,Resp),
	Op2 = 'data',
	concat('Sent: "',Msg,'", Data: "',Resp,'"',Msg2),
	write(Msg2), nl,
	close(S).

ping(Server,Msg) :-
	net:client(Server,S),
	ws:request(S,'/',Status),
	Status = 101,
	ws:msg(S,'ping',Msg),
	ws:parse(S,Op2,Resp),
	Op2 = 'pong',
	concat('Ping: "',Msg,'", Pong: "',Resp,'"',Msg2),
	write(Msg2), nl,
	close(S).

