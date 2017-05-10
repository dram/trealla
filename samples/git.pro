:-define(GITDIR,'.git').

init :-
	init2('.').

init(Name) :-
	init2(Name).

init2(Name) :-
	sys:concat(Name,'/',?GITDIR,Path),
	\+ sys:exists_dir(Path),
	sys:make_dir(Name),
	sys:make_dir(Path),
	sys:concat(Path,'/','HEAD',File),
	sys:save_file(File,'ref: refs/heads/master'),
	write('Initialized empty Git repository: '),
	write(Name), nl,
	true.
init2(Name) :-
	write('Error existing Git repository: '),
	write(Name), nl,
	fail.
