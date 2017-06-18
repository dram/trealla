:-module(http_client).
:-export([get10_data/3,get10_file/3,post10_data/4,post10_file/3]).
:-export([get11_data/3,get11_file/3,pu11_data/4,put11_file/3]).
:-import(library(mime)).
:-define(ConnKeep,1).
:-define(ConnClose,0).

:-using([sys]).

get10_data(Host,Path,Data) :-
	net:client(Host,S),
	get10_internal(S,Path,?ConnClose,Data),
	close(S).

get10_file(Host,Path,Filename) :-
	net:client(Host,S),
	get10_internal(S,Path,?ConnClose,Data),
	close(S),
	save_file(Filename,Data).

post10_data(Host,Path,MimeType,Data) :-
	net:client(Host,S),
	term_to_blob(Data,Data2),
	atom_length(Data2,Len),
	http:post10(S,Path,MimeType,Len,?ConnClose,Status),
	Status = 200,
	write(S,Data2),
	close(S).

post10_file(Host,Path,Filename) :-
	exists_file(Filename,Len,Mod),
	net:client(Host,S),
	mime:mime_type(Filename,MimeType),
	http:post10(S,Path,MimeType,Len,?ConnClose,Status),
	Status = 200,
	write_file(S,Filename),
	close(S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get11_data(Host,Path,Data) :-
	net:client(Host,S),
	get11_internal(S,Path,?ConnClose,Data),
	close(S).

get11_file(Host,Path,Filename) :-
	net:client(Host,S),
	get11_internal(S,Path,?ConnClose,Data),
	close(S),
	save_file(Filename,Data).

post11_data(Host,Path,MimeType,Data) :-
	net:client(Host,S),
	term_to_blob(Data,Data2),
	atom_length(Data2,Len),
	http:post11(S,Path,MimeType,Len,?ConnClose,Status),
	Status = 200,
	write(S,Data2),
	close(S).

post11_file(Host,Path,Filename) :-
	exists_file(Filename,Len,Mod),
	net:client(Host,S),
	mime:mime_type(Filename,MimeType),
	http:post11(S,Path,MimeType,Len,?ConnClose,Status),
	Status = 200,
	write_file(S,Filename),
	close(S).

put11_data(Host,Path,MimeType,Data) :-
	net:client(Host,S),
	term_to_blob(Data,Data2),
	atom_length(Data2,Len),
	http:put11(S,Path,MimeType,Len,?ConnClose,Status),
	Status = 200,
	write(S,Data2),
	close(S).

put11_file(Host,Path,Filename) :-
	exists_file(Filename,Len,Mod),
	net:client(Host,S),
	mime:mime_type(Filename,MimeType),
	http:put11(S,Path,MimeType,Len,?ConnClose,Status),
	Status = 200,
	write_file(S,Filename),
	close(S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get10_internal(S,Path,Keep,Data) :-
	http:get10(S,Path,Keep,Status),
	Status = 200,
	net:stash_get(S,'CONTENT_LENGTH',LenStr,'0'),
	atom_number(LenStr,Len),
	(Len > 0 -> bread(S,Len,Data) ; get10_block(S,'',Data)),
	true.

get10_block(S,Running,Data) :-
	bread(S,_,Block), !,
	atomic_list_concat([Running,Block],Running2),
	get10_block(S,Running2,Data).
get10_block(S,Data,Data).

get11_internal(S,Path,Keep,Data) :-
	http:get11(S,Path,Keep,Status,['X-Hdrs1: blah1','X-Hdrs2: blah2']),
	Status = 200,
	net:stash_get(S,'CONTENT_LENGTH',LenStr,'-1'),
	atom_number(LenStr,Len),
	(Len >= 0 -> bread(S,Len,Data) ; get11_chunk(S,'',Data)),
	true.

get11_chunk(S,Running,Data) :-
	http:get11_chunk(S,Chunk,Len),
	Len > 0, !,
	atomic_list_concat([Running,Chunk],Running2),
	get11_chunk(S,Running2,Data).
get11_chunk(S,Data,Data).
