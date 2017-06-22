:-module(http_client).
:-export([get10_data/3,get10_file/3,post10_data/4,post10_file/3]).
:-export([get11_data/3,get11_file/3,post11_data/4,post11_file/3,put11_data/4,put11_file/3]).
:-import(library(mime)).

:-using([sys]).

get10_data(Host,Path,Data) :-
	net:client(Host,S),
	get10_internal(S,Path,Data) ->
	close(S).

get10_file(Host,Path,Filename) :-
	net:client(Host,S),
	get10_internal(S,Path,Data),
	close(S),
	save_file(Filename,Data).

post10_data(Host,Path,MimeType,Data) :-
	net:client(Host,S),
	term_to_blob(Data,Data2),
	atom_length(Data2,Len),
	http:post(S,Path,[version(1.0),type(MimeType),length(Len)]),
	write(S,Data2),
	http:parse(S,Status),
	close(S),
	Status = 200.

post10_file(Host,Path,Filename) :-
	exists_file(Filename,Len,Mod),
	net:client(Host,S),
	mime:mime_type(Filename,MimeType),
	http:post(S,Path,[version(1.0),type(MimeType),length(Len)]),
	write_file(S,Filename),
	http:parse(S,Status),
	close(S),
	Status = 200.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get11_data(Host,Path,Data) :-
	net:client(Host,S),
	get11_internal(S,Path,Data),
	close(S).

get11_file(Host,Path,Filename) :-
	net:client(Host,S),
	get11_internal(S,Path,Data),
	close(S),
	save_file(Filename,Data).

post11_data(Host,Path,MimeType,Data) :-
	net:client(Host,S),
	term_to_blob(Data,Data2),
	atom_length(Data2,Len),
	http:post(S,Path,[type(MimeType),length(Len),persist(false)]),
	write(S,Data2),
	http:parse(S,Status),
	close(S),
	Status = 200.

post11_file(Host,Path,Filename) :-
	exists_file(Filename,Len,Mod),
	net:client(Host,S),
	mime:mime_type(Filename,MimeType),
	http:post(S,Path,[type(MimeType),length(Len),persist(false)]),
	write_file(S,Filename),
	http:parse(S,Status),
	Status = 200,
	close(S).

put11_data(Host,Path,MimeType,Data) :-
	net:client(Host,S),
	term_to_blob(Data,Data2),
	atom_length(Data2,Len),
	http:put(S,Path,[type(MimeType),length(Len),persist(false)]),
	write(S,Data2),
	http:parse(S,Status),
	close(S),
	Status = 200.

put11_file(Host,Path,Filename) :-
	exists_file(Filename,Len,Mod),
	net:client(Host,S),
	mime:mime_type(Filename,MimeType),
	http:put(S,Path,[type(MimeType),length(Len),persist(false)]),
	write_file(S,Filename),
	http:parse(S,Ver,Status),
	close(S),
	Status = 200.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get10_internal(S,Path,Data) :-
	http:get(S,Path,[version(1.0),length(0),persist(false)]),
	http:parse(S,Ver,Status),
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

get11_internal(S,Path,Data) :-
	http:get(S,Path,[version(1.1),length(0),persist(false)]),
	http:parse(S,Ver,Status),
	Status = 200,
	net:stash_get(S,'CONTENT_LENGTH',LenStr,'-1'),
	atom_number(LenStr,Len),
	(Len >= 0 -> bread(S,Len,Data) ; get11_chunk(S,'',Data)),
	true.

get11_chunk(S,Running,Data) :-
	http:get_chunk(S,Chunk,Len),
	Len > 0, !,
	atomic_list_concat([Running,Chunk],Running2),
	get11_chunk(S,Running2,Data).
get11_chunk(S,Data,Data).
