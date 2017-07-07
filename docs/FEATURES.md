Trealla Prolog
==============

[Home](../README.md) | [Usage](../docs/USAGE.md)


Pair operator
-------------

Trealla supports as built-in the :/2 operator. It's main use is with modules where it distinguishes
the module namespace from the functor, but more generally it allows JSON-like syntax with tuples:

  Item =
    {
      author: 'Philip K Dick',
      works: [
        {title: 'The Man in the High Castle'},
        {title: 'Do Androids Dream of Electric Sheep'}
      ]
    }.

Modules
-------

Each module specifies a separate and self-contained database for both its static rules and dynamic
assertions. It also represents a unit of locking for updates.

	:-module(+Name).
	:-export(+PublicList).					% Specify public list

The public list elements take the form Functor/Arity. The *module* directive will also look for a
NAME.conf JSON file in the current directory where *NAME* is the module name. This config file
defines name/value pairs in the module namespace that will override any explicit defines (that can
set default values) thus:

	:-define(Name,Value).

defines a name/value pair in the module namespace.

	?Name

Will substitute during load a defined value from the namespace. The name *?MODULE* will give the
module name. This is a lexical pasting of the value. Other pre-defined names are *?RANDOM*,
*?RANDOMSTR*, *?TIME* and *?TIMESTR*.

To load a module and make its exported predicates available for use:

	:-import(+Name).
	:-import(library(+Atom)).
	:-import(+Name,+List).

The *List* arg is currently ignored. This directive is a synonym for the slightly more
portable *use_module* directive.

The *include/1* directive loads a file that is lexically pasted into the source. Use the
*unload_file/1* directive to remove a module from the system.

Directives
----------

Most directives can also be written using operator format:

	:-module(+Name).
	:-module +Name.

	:-export([+Name/+Arity, +Name/+Arity]).
	:-export +Name/+Arity, +Name/+Arity.

User-defined Functions
----------------------

UDFs are ones that can be evaluated in *is/2* expressions, their specification does not form part
of standard Prolog, so there is no standard way to define one. Trealla does this:

	:-function([fac/1]).

	fac(0.0,1).
	fac(0,1).
	fac(N,F) :- N>0, N1 is N-1, fac(N1,F1), F is N*F1.

then:

	?- F is fac(5).
	F: 120

	?- fac(5).
	 120

Built-in Functions
------------------

BIFs are either compiled-in C functions or Prolog rules. Some common and/or miscellaneous
functions that are extra to the ISO standard but are commonly found in other Prolog
implementations:

	consult(+File)              - consult file
	deconsult(+File)            - deconsult file
	reconsult(+File)            - reconsult file
	between(+From,+To,?Int)     - ranges
	term_to_atom(+Term,?Atom)   - convert term to atom
	term_to_blob(+Term,-Blob)   - convert term to BLOB
	is_list(+Term)              - is the arg a list?
	is_struct(+Term)            - is the arg a structure?
	is_tuple(+Term)             - is the arg a tuple?
	is_stream(+Term)            - is the arg a stream?
	findnsols(+N,@Term,:G,-L)   - as per SWI-Prolog
	time(:Goal,-Float)          - run goal and return elapsed time (seconds)
	time(:Goal)                 - run goal and print elapsed time
	writeln(+Term1)             - does buffered write/1 + nl/0 to stdout
	writeln(+S,+Term1)          - does buffered write/2 + nl/1 to stream
	random(-Float)              - random float value >= 0.0 and <= 1.0
	term_hash(+Term,-Int)		- 32-bit non-crypto hash
	atomic_concat(+S1,+S2,-S)
	tab(+Spaces)
	tab(+Stream,+Spaces)
	atomic_list_concat(+L,-S)
	atomic_list_concat(+L,+Sep,-S)
	getenv(+Atom,?Term)         - get environment variable
	setenv(+Atom,+Atomic)       - set environment variable
	unsetenv(+Atom)             - unset environment variable
	exists_file(+File)
	delete_file(+File)
	rename_file(+Old,+New)
	make_directory(+Path)
	display(+Term)
	display(+Stream,+Term)
	get0(-Code)					- same as get_code/1 (DEPRECATED)
	get0(+Stream,-Code)			- same as get_code/2 (DEPRECATED)
	get(-Code)
	get(+Stream,-Code)
	name(?Atomic,?CodeList)
	trace(?Int)
	trace/0                     - same as trace(1)
	put(+Char)                  - put_code/1 or put_char/1 based on type of Char
	put(+Stream,+Char)          - put_code/2 or put_char/2 based on type of Char

	read_term_from_atom(+Atom,?Term,+Opts) - convert atom to term

	atom_number(+A,?V)          - convert atom to Int or Float
	unbounded(+Int)             - function to convert integer to unbounded
	fixed(+Int,+Digs,+Prec,-V)  - format integer with fixed number of decimal digits
	rational(+Int,+Int)         - function to create a rational with numerator and denominator
	rdiv/2

and for accessing the rule database:

	assert(+Clause)             - same as assertz (DEPRECATED)
	asserta(+Clause,-Ref)
	assertz(+Clause,-Ref)
	erase(+Ref)
	clause(?Head,?Body,?Ref)
	abolish(+Name,+Arity)
	retractw(+Clause)           - retract or wait (see dynamic 'notify')
	clausew(+Head,-Body)        - clause or wait (see dynamic 'notify')

and for accessing the recorded database:

	recorda(+K,+V)
	recorda(+K,+V,-Ref)
	recordz(+K,+V)
	recordz(+K,+V,-Ref)
	recorded(?K,?V)
	recorded(?K,?V,?Ref)
	instance(+Ref,-V)
	current_key(+K)

Plus the Edinburgh file routines: see,seeing, seen, tell (+append), telling and told.

System-extras: namespace 'sys'
------------------------------

These are an assortment of utilities:

	now(-Secs)                  - get the seconds C-epoch (also a function now())
	timestamp(-Usecs)           - get the useconds C-epoch
	sleep(+Secs)                - yield for seconds (also hsleep/1 for hard-sleep)
	delay(+Msecs)               - yield for milli-seconds (also hdelay/1 ...)
	getline(-Atom)              - read a line into atom (CR and/or LF removed)
	getline(+S,-Atom)           - ... same but from a stream
	exists_directory(+Path)     - check if dir exists
	make_directory(+Path,+Mode) - make dir if not exists
	exists_file(+File,-L,-S)    - ... length in bytes and last-modified in seconds (C-epoch)
	load_file(+File,-Blob)      - read named file
	save_file(+File,+Atom)      - create named file
	append_file(+File,+Atom)    - append to named file

	write_file(+S,+File,+From,+To) - write ranged contents of named file to stream. Range is in
								   byte offsets starting at 0. An offset of -1 means to the end.

	write_file(+S,+File)        - is equivalent to write_file(S,File,0,-1)

	bread(+S,?len,-Blob)        - block read from stream
	bwrite(+S,+Atom)            - block write to stream
	rand(-Int)                  - random int value >= 0 and <= RAND_MAX
	uuid(-Atom)                 - return representation of a new UUID
	split_all(+Atom,+Sep,-L)    - split atom based on separator into list
	split(+Atom,+Sep,?L,?R)     - split atom based on first separator into left & right
	split_last(+Atom,+Sep,?L,?R) - split atom based on last separator into left & right
	xmlq(+Atom,+N,-Atom)        - quick get named value from XML source (can retry))
	xmlq(+Atom,+N,+Idx,-Atom)   - quick get I'th named value from XML source
	upper(+Atom,-Atom)          - convert to upper-case
	lower(+Atom,-Atom)          - convert to lower-case
	title(+Atom,-Atom)          - convert to upper-case start of each word
	sha1(+Atom,-Atom)           - SHA1 (128-bit) hash
	sha256(+Atom,-Atom)         - SHA2 (256-bit) hash
	sha512(+Atom,-Atom)         - SHA2 (512-bit) hash
	url_encode(+Atom,-Atom)
	url_decode(+Atom,-Atom)
	b64_encode(+Atom,-Atom)     - Base64 encode
	b64_decode(+Atom,-Blob)     - Base64 decode
	begins(+Atom,+List)         - does atom begin with any atom in the list?
	left(+Atom,?Len,?Sub)       - len and/or Sub must be instantiated
	right(+Atom,?Len,?Sub)      - len and/or Sub must be instantiated
	replace(+Atom,+S1,+S2,-Atom) - replace every occurrance of S1 with S2

	atom_timestamp(+A,-Usecs)   - atom is YYYY-MM-DD!HH:MM:SS.SSS (Note: the actual
	                              separators can be any character at all) to usecs.

	format_rfcdate(+Int,-Atom)  - format C-epoch to RFC datetime
	parse_rfcdate(+Atom,-Int)   - parse RFC datetime to C-epoch
	parse_csv(+Atom,-List)      - parse comma-separated variables into list
	parse_tab(+Atom,-List)      - parse tab-delimited variables into list
	exit(+Atom)                 - exit with reason string
	system(+Atom)               - execute shell command
	system(+Atom,?Int)          - execute shell command
	stream(-S)                  - create a dummy stream

Each stream has access to it's own private in-memory dictionary:

	lput(+S,+Key,?Old,+New)     - set value under Key
	put(+S,+Key,?Old,+New)      - set value under Key
	put(+S,+Key,+Term)          - set value under Key
	lget(+S,+Key,-Term)         - get value under Key (or [])
	get(+S,+Key,-Term)          - get value under Key (or 0)
	get_keys(+S,+V,-L)          - get list of keys with specified value
	get(+S,-L)                  - get list of tuples {key,value}
	erase(+S,+Key)              - erase value under Key
	erase(+S)                   - erase all

The *stream/1* predicate creates a dummy stream for use as a dictionary. Like all streams, it
should be disposed of by a call to close/1. Dictionary operations are not undone on bactracking
nor are they backed to the database.

The following are designed for quickly getting top-level config values:

	jsonq(+Atom,+Name,-Atom)       - quick get named value from JSON source
	jsonq(+Atom,+Name,-Atom,+Def)  - quick get named value from JSON source (or default)
	jsonqi(+Atom,+Idx,-Name,-Atom) - quick get indexed name+value from JSON source

The parse_XXX/2 predicates accept double-quoted column items, and removes any spurious whitespace
before and after items.

BLOBs and atoms can often be used interchangeably. While BLOBs can contain embedded NULs etc,
atoms can't. Atoms have a terminating NUL that is not returned as part of the length, but BLOBs
also store a length internally. BLOBS can be written and read in back-quotes in which case the
enclosed characters are output base64-encoded in canonical form, eg.

	`SGVsbG8sIHdvcmxkIQ==` == 'Hello, world!'

Note: *atom_length/2* returns the number of UTF-8 characters in an atom while for BLOBs it's the
stored length. Atoms are assumed UTF-8.

Database store: namespace 'dbs'
-------------------------------

The rule database exists per module (or *default*) and will index on the first argument any
asserted clauses that have first been declared dynamic:

	:-dynamic(+Name/Arity).
	:-dynamic(+Name/Arity,+List).

This is an alphanumeric index unless it is declared otherwise:

	:-dynamic(+Name/Arity,[numeric]).

in which case any assert/retract will fail if the first argument is not an integer.

The database will wake any waiting processes that have asked to be notified on new assertions:

	:-dynamic(+Name/Arity,[notify]).

The database will save to the transaction log asserted clauses that have first been declared with
the *persist* modifier:

	:-dynamic(+Name/Arity,[persist]).

The alternative declaration with a *storage* modifier:

	:-dynamic(+Name/Arity,[storage]).

means that all terms apart from the first-arg index are stored in the transaction log only and not
held in memory. Instead a filepos into the log is held and the extra data is loaded on demand. In
this way databases far larger than physical memory can be accessed, especially useful as blobs can
be stored.

To access the persistent database:

	init/0               - prepare only
	load/0               - prepare & load existing data
	tail/0               - load and tail for new data (every 1000ms)
	tail(+Msecs)         - load and tail for new data (specified ms)

Transactions within the same database can be done via bracketing a series of one or more updates
with *begin/end* calls:

	begin/0              - start atomic transaction sequence
	end/0                - commit changes with fsync
	end(+Boolean)        - commit changes with optional fsync

and occurs as a single (all-or-nothing) update to the rule database and (if persistent) write to
the transaction log. A transaction locks out any other transactions on the module for the
duration. The *begin/0* call creates a choicepoint and backtracking will cause it to rollback the
transaction. The *end* call does a cut. Inside a transaction changes to the database are not visible.

To write to the transaction log only, without updating the database:

	log(:Term)

where *Term* is usally an asserta/assertz/retract (or other) operation. This can be useful for
external adapters that need to write to the log what to do, without actually doing it themselves.
Another program could then tail the log and update the real database.

Periodically the log and the master are merged and a new master is created, saving the old files.
This happens when the *--merge* option is specified on the command-line.

Linda data-coordination: namespace 'linda'
------------------------------------------

Implements the Linda data-coordination primitives. This is an alternative, simpler and more
Prolog-like system for concurrent processing than the Erlang-style actor model described later. It
uses the regular rule database for passing tuples between worker processes. The tuples can be
persistent allowing naturally resilient and optionally distributed systems to be built.

	eval(+Goal)          - create concurrent worker process (spawn)
	out(+Tuple)          - assert new tuple
	in(-Tuple)           - retract tuple (blocking)
	inp(-Tuple)          - ... (non-blocking)
	rd(-Tuple)           - match tuple (blocking)
	rdp(-Tuple)          - ... (non-blocking)

Also:

	init/0               - initialization
	init(+List)          - initialization with dynamic modifiers
	fork/0               - continue as child, parent fails
	wait/0               - wait indefinitely (used by parent)
	end_wait/0           - abort the wait (used by child)

Note: 'linda:init/0' does a call to:

	dynamic({}/1,[notify])

in the current module, while *linda:init/1* allows adding extra modifiers (eg. for persistence).
The first argument of the tuple, as with all dynamics, is indexed. When creating persistent tuples
use the *dbs:init/0* or *dbs:load/0* calls to first restore the database state.

All of the input predicates will attempt to resatisfy on backtracking. The blocking predicates
will be notified (awoken) on a relevant assert or out. The non-blocking predicates can be used to
implement polling behaviour.

Note: a worker process can use the per-process dictionary.

See *samples/testlinda.pro* for guidance.

Concurrent Processes: namespace 'proc'
--------------------------------------

The whole concept of light-weight concurrent processes and message passing as outlined here is
borrowed from Erlang. Such a process in Prolog is just a free (or asynchronous) goal. Processes
have little system overhead (using just 2K bytes of memory initially), so creating tens or even
hundreds of thousands of processes is easy (samples/skynet.pro creates a million). They run in
isolated memory and use preemptive multitasking based on thread-pools.

A message is a term and can be as simple as an atom or number, or as complex as a list, tuple or
compound. A process yields when entering a blocking recv, send, read, write, delay or sleep.

	spawn_link(+Name,:Goal)    - run goal as named linked process
	spawn_link(:Goal)          - run goal as unnamed linked process
	spawn(+Name,:Goal)         - run goal as named process
	spawn(:Goal)               - run goal as unnamed process
	send(+Term)                - send message
	recv(?Term)                - receive message
	undo(?Term)                - undo message (push back)
	rsvp(+Term,?Term)          - rendezvous
	tmo(+Msecs)                - set receive timeout
	after/0                    - succeeds if recv timed-out
	procinfo(+Atom,-V)         - get named info ('pids','idle','named','msgs')
	until(:Goal)               - wait until Goal succeeds
	fork/0                     - continue as child, parent fails
	wait/0                     - wait indefinitely (used by parent)
	end_wait/0                 - abort the wait (used by child)

	receive ?Term              - receive message (operator fy)
	undo ?Term                 - undo message (operator fy)

	pid(+Name,-Pid)            - get Pid of named process
	pid(-Pid)                  - get current Pid
	send(+Pid,+Term)           - send to Pid
	recv(+Pid,?Term)           - receive from Pid
	rsvp(+Pid,+Term,?Term)     - rendezvous with Pid
	abort(+Pid)                - tell Pid to halt
	procinfo(+Pid,+Atom,-V)    - get named info for Pid ('msgs')

A linked process will send the tuple {'EXIT',Code,Reason} back to its parent (where *Code*
represents the integer halt code and *Reason* the description) if it terminates abnormally. If
*exit/1* is called *Code* will be 1 and *Reason* the argument supplied. Otherwise it will be an
exception code and pre-defined reason. It can be identified as the current Pid. For example:

	?- proc:spawn_link(sys:exit('die')),proc:recv(E).
	E: {'EXIT',1,'die'}
	(0.001 s) yes

Wherever *Pid* is used *Name* can also be used for local named processes.

For example, spawn 100K child processes and rendezvous with them:

	:-using([proc]).

	parent :-
		between(1,100000,I),
			spawn(echo),
			rsvp(I,Resp),
			fail.

	echo :-
		recv(N),
		send(ok).

Note: fork/0 here is not the sames as SWI-Prolog fork/1 (which is a true Unix fork).

After a spawn or fork, both the parent and child can assume the value of the current Pid is set
(for the parent the Pid of the child, and for the child the Pid of the parent). If the parent spawns
many processes then the Pid for each child may retrieved and saved. Two child processes may
communicate by one obtaining the Pid of the other (from the parent). Alternatively spawn may
register a local named process. Named processes can be located by a call to *pid/2*. A process can
refer to itself as *self* and its parent (if linked) as *parent*.

A timeout may be set as an int in milliseconds, a value of 0 means no timeout (and so polls) and -1
means infinite (the default). After a timeout the receive call fails.

Matching may be used on the receive side to do selective message dequeueing. A receive blocks if no
message (or match) is found. After a successful receive the current Pid is set. An Erlang style case
statement is easily implemented (see testspawn.pro test6).

For example in Erlang from [LYSEFGG] (http://learnyousomeerlang.com/content):

	important() ->
		receive {Priority, Message}
		when Priority > 10 -> [Message | important()]
		after 0 -> normal()
		end.

	normal() ->
		receive
		{_, Message} -> [Message | normal()]
		after 0 -> []
		end.

becomes in Trealla:

	:-using([proc]).

	important(L, L2) :-
		tmo(0),
		receive {Priority, Message},
		after -> normal(L, L2) ;
		Priority > 10 ->
		important([Message | L], L2) ;
		undo {Priority, Message}.

	normal(L, L2) :-
		tmo(0),
		receive {_, Message},
		after -> L2 = L ;
		normal([Message | L], L2).

which returns a list of all messages with the priority ones first.

Note: *after/0* must come before any explicit pattern matches.

Note: Once the *after/0* succeeds (on a timeout) all the *undo/1* messages become available again
for reading.

A send call completes immediately, ie. asynchronously (need to limit queue sizes?). If a synchronous
send+recv is required use *rsvp/2*. The read side of an rsvp behaves just like a recv does (ie. can
yield). The send side will transfer its' scheduling slice to a recv that has a message queued (which
may be the read side).

A send/recv/rsvp fails and an exception is generated if the other process no longer exists. If the
process is linked it will notify its parent.

If a process calls *delay/1* or *sleep/1* then it yields.

The *procinfo/2* named info items may be any of *pids*, *idle*, *names*.

Each process has access to it's own private in-memory dictionary:

	lput(+Key,?Old,+New)       - set value under Key
	put(+Key,?Old,+New)        - set value under Key
	put(+Key,+Term)            - set value under Key
	lget(+Key,-Term)           - get value under Key (or [])
	get(+Key,-Term)            - get value under Key (or 0)
	get_keys(+V,-L)            - get list of keys with specified value
	get(-L)                    - get list of tuples {key,value}
	erase(+Key)                - erase value under Key
	erase/0                    - erase all

Hint: use *lput/3* to push or pop a list:

	lput(Key,Old,[V|Old])      - push V to head
	lput(Key,[V|New],New)      - pop V from head

See *samples/testspawn.pro*, *samples/skynet.pro* for further guidance.

Network Processes: namespace 'proc'
-----------------------------------

A process can be created as a network server, to which a client process connects and they can
exchange messages just as above:

	server(+BindList,-Pid)            - fork as named network process
	server(+BindList,-Pid,+Key,+Cert) - ditto & specify KEY & CERT .pem files

The *server/2* call just creates a named network server (or servers) which waits and then spawns a
light-weight process for every client connection (with TCP only). The name (if present) refers to a
named service for sharing with discovery. When multiple bindings are provided they all funnel into
the same receive queue for the process. An ephemeral port is allocated automatically by the system.
The spawned process exits on a client disconnect.

Use the *pid/2* call to connect a network client, where Name may refer to a named service to be found
using discovery. If a local process of that name is found, it will be used first, otherwise a network
process will be sought using discovery.

Both *server/* and *pid/2* may take bind attributes (as outlined in the next section). For example,
a simple echo server and client:

	:-using([proc,net]).

	echod :-
		server([';name=ECHO'],Pid),
		repeat,
			recv(Pid,Msg),
			send(Pid,Msg),
			fail.

	echo(Msg) :-
		pid(';name=ECHO',Pid),
		rsvp(Pid,Msg,Resp),
		write(Resp), nl.

or to NOT use naming but instead use a specific port (9000):

	:-using([proc,net]).

	echod :-
		server([':9000'],Pid),
		repeat,
			recv(Pid,Msg),
			send(Pid,Msg),
			fail.

	echo(Host,Msg) :-
		atomic_list_concat([Host,':',9000],Server),
		pid(Server,Pid),
		rsvp(Pid,Msg,Resp),
		write(Resp), nl.

Note: messaging between network processes must always specify the actual Pid, as the concept of
'current Pid' is only valid locally.

Note: there is no facility to directly start remote processes, run directed or arbitrary functions.

See *samples/echo.pro* for further guidance.

Socket Streams: namespace 'net'
-------------------------------

Socket streams can be managed simply by using the client/server patterns. Server bind format is
'iface:port;attr' or just ':port;attr' or even just ';attr' if discovery is being used. Client host
format is 'host:port;attr' or again just ';attr'. Multiple attributes may be included, each
seaparated by a ';' character.

Attributes can include *tcp*, *udp*, *'+tls'* or *'+ws'*. If *udp* is not present then *tcp*
is assumed. If *'+ws'* is present this indicates raw WebSockets are in use and no HTTP protocol
upgrade need be negotiated.

Optional attributes are allowed of the form 'key=value' such as 'scope=TEST' which names a scope for
discovery purposes, and 'name=QUOTES' which specifies a named service.

If no port (or port=0) is specified then an ephemeral one is assigned by the system. For named
services with discovery this is usually sufficient.

For SSL servers files *key.pem* and *cert.pem* are looked for, which contain the private key and
certificate(s). Default ones for testing are provided.

	server(+BindList,-S)            - listen for and accept connections
	server(+BindList,-S,+Key,+Cert) - ditto & specify KEY & CERT .pem files
	client(+Host,-S)                - connect to server
	start_tls(+S)                   - enable TLS switchover
	readmsg(+S,-Atom)               - read a line from stream
	stash_get(+S,+Key,?Old)         - get value for Key (or '' if non-exist)
	stash_get(+S,+Key,?Old,+Def)    - get value for Key (or default)
	stash_set(+S,+Key,+New)         - set atom value for Key
	stash_set(+S,+Key,?Old,+New)    - set atom value for Key (return Old)
	stash_clr(+S,+Key,?Old)         - clear Key
	service(+S,-Name)               - name of the stream
	local_port(+S,-Int)             - local port
	remote_port(+S,-Int)            - remote port
	local_addr(+S,-Atom)            - local address
	local_host(+S,-Atom)            - local host (resolved)
	remote_addr(+S,-Atom)           - remote address
	remote_host(+S,-Atom)           - remote host (resolved)
	tls(+S,?Boolean)                - unifies with 'true/false'
	tcp(+S,?Boolean)                - unifies with 'true/false'
	udp(+S,?Boolean)                - unifies with 'true/false'
	ipv4(+S,?Boolean)               - unifies with 'true/false'
	ipv6(+S,?Boolean)               - unifies with 'true/false'
	is_socket(+Term)                - is the arg a socket?

With *server/2* a listener is started which waits for incoming connections. A new light-weight
process is started per connection.

The attributes 'mcast6=addr' adds multicast IPV6 membership to the group-address supplied, and
'mcast4=addr' adds IPV4.

With *client/2* it either succeeds once or fails. Client can also take a URI format specifiying
either http:, https:, ws: or wss: schemes. If ws: or wss: then an immediate protocol upgrade should
be requested. The attribute 'loop=[1|0]' adds UDP multicast loopback and 'ttl=[N|0]' adds multicast
TTLs (aka hops). Clients can also specify userid and password in the URL for HTTP 'Basic'
authorization (preferably only over SSL/TLS).

With *readmsg/2* the stream is read until a trailing new-line. In other words: a complete message.

With *bread/3* an ungrounded length specification causes a non-blocking read that returns whatever
is currently available, otherwise it blocks until it can return the specified length.

Note: regular streams I/O (except character-based) can be used over sockets.

Note: writes to sockets are normally blocking but *sys:write_file/2* can yield when an operation
would cause blocking. Use *tmo/1* to set a timeout. Most (?) read operations with sockets will
yield when waiting for data that would cause blocking. Yielding surrenders the thread until more
data is available.


HTTP processing: namespace 'http'
---------------------------------

Server-side:

	parse(+S,-Ver,-Method,-Path)  - parse headers (except content)
	basic_auth(+S,-User,-Pass)    - decode Basic auth token (if present)
	www_form(+S)                  - decode form data on POST (urlencoded)

	form(+S,+Name,-Atom)          - get named form value (or '' if non-exist)
	query(+S,+Name,-Atom)         - get named query value (or '' if non-exist)
	cookie(+S,+Name,-Atom)        - get named cookie value (or '' if non-exist)

	form_list(+S,+Name,-Atom)     - get named form values (or [] if non-exist)
	query_list(+S,+Name,-Atom)    - get named query values (or [] if non-exist)
	cookie_list(+S,+Name,-Atom)   - get named cookie values (or [] if non-exist)

Client-side:

	head(+S,+Path,+Options)
	get(+S,+Path,+Options,)
	delete(+S,+Path,+Options)
	post(+S,+Path,+Options)
	put(+S,+Path,+Options)

The above HTTP method calls are available as arity 2 or 3. If the latter then *Options* is a
list of possible request modifiers:

	version(+Float)              - HTTP version 1.0 or 1.1 (the default)
	method(+Atom)                - HTTP method (send arbitrary HTTP)
	persist(+Boolean)            - Connection true/false (default as per version)
	length(+Integer)             - Content-Length in bytes (assumed zero for GET)
	type(+Atom)                  - Content-Type string (eg: 'text/html')
	agent(+String)               - User-Agent / Server string
	modified(+Atom)              - If-Modified-Since / Last-Modified RFC datetime
	cookie(+Atom)                - Cookie / Set-Cookie string
	referer(+Atom)               - Referer string
	chunked(+Boolean)            - HTTP/1.1 chunked transfer
	header(+Pair)                - a header pair of Name:Value
	debug(+Boolean)              - dump headers
	...

To include an extra HTTP header:

	http:get(S,'/index.html',[header('X-Hdr1':'o n e'), header('X-Hdr2':2)])

The *chunked* option indicates willingness to receive a chunked response. However, the server may
choose not to send such a response format.

If *post/4* or *put/4* and version is 1.1 and *length* is not specified then chunked transfer is
in play, so use *put_chunk/2* to write.

	put_chunk(+S,+Atom)
	get_chunk(+S,-Blob,-Len)

To parse the response from a request:

	parse(+S,-Status)
	parse(+S,-Status,-Options)

See *samples/http_server.pro* & *library/http_client.pro* for guidance.

HTTP2 processing: namespace 'h2'
--------------------------------

Under consideration.

WebSocket: namespace 'ws'
-------------------------

	request(+S,+Path,-Status,+Prots,-Prot) - request HTTP upgrade to WS
	request(+S,+Path,-Status)              - request HTTP upgrade to WS
	upgrade(+S,+Prot)                      - do HTTP upgrade to WS

	parse(+S,-Op,-Atom)                    - parse and read message
	msg(+S,+Op,+Atom)                      - write message

	is_ws(+S,?Boolean)                     - WebSocket? unifies with 'true/false'

With *msg/3* *Op* can be the atom *more*, *data*, *ping* or *close*. Note *parse/3* will
return a *more*, *data* or *close*, anything else should be discarded.

Also note data atom will be a BLOB for a binary message and an atom for a text (UTF-8) message.

The *request/5* client call takes an atom or list of atoms of wanted protocols and returns the one
selected by the server on success. A *Status* of 101 indicates success.

Also, *parse/3* and *msg/3* can be used with raw WebSockets, ie. both client and server end-points
were created with 'ws:' or '+ws' attribute (and so no upgrade was needed).

For example:

  tpl -l samples/testws.pro -g "echo('ws://echo.websocket.org','Hello, world')"
  tpl -l samples/testws.pro -g "ping('ws://echo.websocket.org','Hello, world')"

See *samples/ws_server.pro* & *samples/testws.pro* for guidance.

Lists library module: namespace 'lists'
---------------------------------------

This is an auto-loaded module:

	:-import(library(lists)).

and can be used for compatability with other Prologs:

	maplist(:Goal,+L)           - call goal with every element of list L
	maplist(:Goal,+L1,+L2)      - call goal with every element of L1 & L2
	member(?Term,+List)         - does atom occur in the list?
	memberchk(?Term,+List)      - same as once(member(...))
	select(+Term,+L1,-L)        - one occurrance of term is removed
	subtract(+L1,+L2,?L)        - remove all L2 elements from L1
	union(+L1,+L2,?L)           - union of L1 & L2
	intersection(+L1,+L2,?L)    - intersection of L1 & L2
	reverse(+L1,-L2)            - reverse a list
	append(+L1,+L2,-List)       - make a joined list
	find(+N,+L,-Term)           - find nth arg of list

Dictionary library module: namespace  'dict'
--------------------------------------------

This is a compiled-in module that must be imported:

	:-import(library(dict)).

Provides control over name-value pairs in a list:

	get(+Dict,+Name,-Value)
	lget(+Dict,+Name,-Value)
	set(+Dict,+Name,+Value,-NewDict)
	del(+Dict,+Name,-NewDict)

If name not found 'get' returns 0, 'lget' returns [].

Auth library module: namespace 'auth'
-------------------------------------

This is a compiled-in module that must be imported:

	:-import(library(auth)).

The following provide user control:

	adduser(+User,+Passwd)
	deluser(+User)
	setuser_email(+User,+Email)
	getuser_email(+User,-Email)
	setuser_nick(+User,+Nick)
	getuser_nick(+User,-Nick)
	setuser_locked(+User,+Code)
	getuser_locked(+User,-Code)
	setuser_pass(+User,+Passwd)

The following provide session control:

	login(+User,+Passwd,-SessId,+Keep,-Expires)
	checkin(+SessId,-User,-Expires)
	session_set(+SessId,+Name,+Value)		% name-value pair
	session_get(+SessId,+Name,-Value)		% name-value pair
	logout(+SessId)

Blog library module: namespace 'blog'
-------------------------------------

This is a compiled-in module that must be imported:

	:-import(library(blog)).

The following provide for posting:

	addpost(+BlogName,-Id,+User,+Head,+Body)
	lockpost(+Id)
	unlockpost(+Id)
	delpost(+Id)
	undelpost(+Id)
	getpost(+Id,-Deleted,-Locked,-Created,-Modified,-User,-Head,-Body)
	getposts(+BlogName,-List)

The following provide for commenting:

	addcomment(+PostId,-Id,+User,+Body)
	replycomment(+ReplyId,+Id)
	delcomment(+Id)
	undelcomment(+Id)
	getcomment(+Id,-Created,-Modified,-User,-Body,-ReplyTo,-ReplyList)
	getcomments(+PostId,-List)

SMTP client library module: namespace 'smtp_client'
---------------------------------------------------

This is a compiled-in module that must be imported:

	:-import(library(smtp_client)).

Provides basic mail sending functionality:

	send_plain(+From,+To,+Subject,+Body)
	send_html(+From,+To,+Subject,+Body)
	send_base64(+From,+To,+Subject,+Body)

More functionality to added, eg. attachments and inline HTML objects.

HTTP client library module: namespace 'http_client'
---------------------------------------------------

This is a compiled-in module that must be imported:

	:-import(library(http_client)).

Provides basic HTTP/1.0 client functionality:

	get10_data(+Host,+Path,-Data)
	get10_file(+Host,+Path,+Filename)
	post10_data(+Host,+Path,+MimeType,+Data)
	post10_file(+Host,+Path,+Filename)

Provides basic HTTP/1.1 client functionality:

	get11_data(+Host,+Path,-Data)
	get11_file(+Host,+Path,+Filename)
	post11_data(+Host,+Path,+MimeType,+Data)
	post11_file(+Host,+Path,+Filename)
	put11_data(+Host,+Path,+MimeType,+Data)
	put11_file(+Host,+Path,+Filename)

STOMP library module: namespace 'stomp'
---------------------------------------

This is a compiled-in module that must be imported:

	:-import(library(stomp_client)).

	parse(+S,-Method,-Len)       - parse headers (except content)
	msg(+S,+Method,+Hdrs,+Atom)  - send ANY request ('Data' can be BLOB or atom)

While superficially similar to HTTP, STOMP is actually bidirectional, so *parse/3* and *msg/4*
are used on both the server & client sides.

See *samples/stomp_server.pro* & *samples/stomp_client.pro* for guidance.

MIME library module: namespace 'mime'
-------------------------------------

This is a compiled-in module that must be imported:

	:-import(library(mime)).

	mime_type(+Filename,-MimeType)

YAHOO library module: namespace 'yahoo'
---------------------------------------

This is a compiled-in module that must be imported:

	:-import(library(yahoo)).

	get_chart(+Symbol,-Data)                (DEPRECATED due to Yahoo changes)

Where *Symbol must be an atom such as 'GOOG' or 'IBM'. The result in *Data* is multiple CSV lines,
one per date entry.

	get_name(+Symbols,-Data)				- get_fields(Symbols,'n',Data)
	get_quote(+Symbols,-Data)				- get_fields(Symbols,'spol1vbad1t1',Data)
	get_fields(+Symbols,+Fields,-Data)		- field names supplied

Where *Symbol* must be an atom such as 'GOOG' or 'IBM' or multiples can be requested together such as
'GOOG,IBM,AAPL,MSFT'. The result in *Data* is one more CSV lines (one per requested symbol). There
is a Yahoo applied limit of 100 (?) symbols per request.
