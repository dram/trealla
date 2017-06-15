:-module(blockchain,[]).
:-export([init/0,verify/1]).
:-export([get_data/2,get_block/5]).
:-export([put_data/2,put_data/5,put_block/5]).

% Manage a cryptographic chain of data blocks.
% Each 'key' designates an independent chain.
% There is no 'proof of work' included.

:-dynamic(block/6,[storage]).

init :-
	dbs:load.

% Verify whole chain...

verify(Key) :-
	dbs:load,
	block(Key,Idx,Ts,Data,Hash,PrevHash),
		Idx > 0,
		write(Key),write(' '),write(Idx),write(' ... '),
		LastIdx is Idx - 1,
		once(block(Key,LastIdx,LastTs,LastData,LastHash,LastPrevHash)),
		atomic_list_concat([Key,LastIdx,LastPrevHash,LastTs,LastData],Tmp),
		sys:sha256(Tmp,VerifyHash),
		(
			PrevHash = VerifyHash ->
				writeln('.') ;
				writeln('*** ERROR ***')
		),
		fail.
verify(Key).

% Return latest data...

get_data(Key,Data) :-
	dbs:load,
	get_block(Key,_,_,Data,_,_).

% Return block, optionally using index...

get_block(Key,Idx,Ts,Data,Hash) :-
	dbs:load,
	get_block(Key,Idx,Ts,Data,Hash,_).

% Create a new block from data and forget about it...

put_data(Key,Data) :-
	dbs:load,
	sys:timestamp(Ts),
	store_block(Key,_,Ts,Data,_).

% Create a new block from data, returning details. Use
% this if the block is to be shared with peers...

put_data(Key,Idx,Ts,Data,Hash) :-
	dbs:load,
	var(Idx),
	var(Ts),
	var(Hash),
	sys:timestamp(Ts),
	store_block(Key,Idx,Ts,Data,Hash).

% Put existing block. Use this if the block was shared by a peer...

put_block(Key,Idx,Ts,Data,Hash) :-
	dbs:load,
	integer(Idx),
	integer(Ts),
	nonvar(Data),
	atom(Hash),
	store_block(Key,Idx,Ts,Data,Hash).

% Create a new block...

store_block(Key,Idx,Ts,Data,Hash) :-
	nonvar(Key),
	integer(Ts),
	nonvar(Data),
	dbs:begin,
	get_block(Key,LastIdx,_,_,LastHash,_),
	Idx is LastIdx + 1,                                       % Verify
	term_to_blob(Data,Blob),
	atomic_list_concat([Key,Idx,LastHash,Ts,Blob],Tmp),
	sys:sha256(Tmp,VerifyHash),
	Hash = VerifyHash,                                        % Verify
	asserta(block(Key,Idx,Ts,Blob,Hash,LastHash)),
	dbs:end.

get_block(Key,Idx,Ts,Data,Hash,PrevHash) :-
	nonvar(Key),
	block(Key,Idx,Ts,Data,Hash,PrevHash),
	!.
get_block(Key,Idx,Ts,Data,Hash,PrevHash) :-
	nonvar(Key),
	Idx = -1,
	Hash = 0.
