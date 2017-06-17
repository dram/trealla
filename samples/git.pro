:-define(GITDIR,'.git').

init :- init_repo('.').
init(Name) :- init_repo(Name).

init_repo(Name) :-
	atomic_list_concat([Name,'/',?GITDIR],Path),
	\+ sys:exists_directory(Path),
	make_directory(Name),
	make_directory(Path),
	atomic_list_concat([Path,'/','HEAD'],File),
	sys:save_file(File,'ref: refs/heads/master'),
	write('Initialized empty Git repository: '),
	write(Name), nl,
	true.
init_repo(Name) :-
	write('Error existing Git repository: '),
	write(Name), nl,
	fail.
