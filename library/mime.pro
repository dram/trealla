:-module(mime).
:-export([mime_type/2]).

exts([
	{'.css',	'text/css'},
	{'.csv',	'text/csv'},
	{'.flac',	'audio/flac'},
	{'.gif',	'image/gif'},
	{'.htm',	'text/html'},
	{'.html',	'text/html'},
	{'.ico',	'image/x-icon'},
	{'.jpeg',	'image/jpeg'},
	{'.jpg',	'image/jpeg'},
	{'.js',		'application/javascript'},
	{'.json',	'application/json'},
	{'.midi',	'audio/midi'},
	{'.mov',	'video/quicktime'},
	{'.mp3',	'audio/mpeg'},
	{'.mp4',	'video/mp4'},
	{'.mpg',	'video/mpeg'},
	{'.ogg',	'audio/ogg'},
	{'.ogv',	'video/ogg'},
	{'.pdf',	'application/pdf'},
	{'.png',	'image/png'},
	{'.svg',	'image/svg+xml'},
	{'.tiff',	'image/tiff'},
	{'.txt',	'text/plain'},
	{'.vcard',	'text/vcard'},
	{'.vcf',	'text/vcard'},
	{'.webm',	'video/webm'},
	{'.xml',	'application/xml'}
]).

index([V|_],V).
index([_|T],V) :- index(T,V).

mime_type(File,Type) :-
	exts(Exts),
	sys:splitl(File,'.',_,Ext),
	sys:concat('.',Ext,Ext2),
	index(Exts,{Ext2,Type}),
	!.
mime_type(File,'application/octet-stream').
