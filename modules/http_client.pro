:-module(http_client).
:-export([get10_data/3,get10_file/3,put10_file/3]).
:-export([get11_data/3,get11_file/3,put11_file/3]).

:-using([sys,net]).

get10_data(Host,Path,Data) :-
	client(Host,S),
	get10_internal(S,Path,Data2),
	close(S),
	%write(Data2),
	Data = Data2.

get10_file(Host,Path,Filename) :-
	client(Host,S),
	get10_internal(S,Path,Data),
	close(S),
	save_file(Filename,Data).

put10_file(Host,Path,Filename) :-
	client(Host,S),
	exists_file(Filename,Len,_Mod),
	http:put10(S,Path,'application/octet-stream',Len,Status),
	Status = 200,
	write_file(S,Filename),
	close(S).

get11_data(Host,Path,Data) :-
	client(Host,S),
	get11_internal(S,Path,Data2),
	close(S),
	%write(Data2),
	Data = Data2.

get11_file(Host,Path,Filename) :-
	client(Host,S),
	get11_internal(S,Path,Data),
	close(S),
	save_file(Filename,Data).

put11_file(Host,Path,Filename) :-
	client(Host,S),
	http:put11(S,Path,'application/octet-stream',Status),
	Status = 200,
	http:put_file(S,Filename),
	close(S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get10_internal(S,Path,Data) :-
	http:get10(S,Path,Status),
	Status = 200,
	stash_get(S,'HTTP_CONTENT_LENGTH',LenStr,'0'),
	atom_number(LenStr,Len),
	(Len > 0 -> bread(S,Len,Data) ; get10_block(S,'',Data)),
	true.

get10_block(S,Running,Data) :-
	bread(S,_,Block), !,
	concat(Running,Block,Running2),
	get10_block(S,Running2,Data).

get10_block(S,Data,Data).

get11_internal(S,Path,Data) :-
	http:get11(S,Path,Status),
	Status = 200,
	stash_get(S,'HTTP_CONTENT_LENGTH',LenStr,'0'),
	atom_number(LenStr,Len),
	(Len > 0 -> bread(S,Len,Data) ; get11_chunk(S,'',Data)),
	true.

get11_chunk(S,Running,Data) :-
	http:get_chunk(S,Chunk,Len),
	Len > 0, !,
	concat(Running,Chunk,Running2),
	get11_chunk(S,Running2,Data).

get11_chunk(S,Data,Data).
