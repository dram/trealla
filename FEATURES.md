Trealla Prolog
==============

Modules
-------

Each module specifies a separate and self-contained database for both
its static rules and dynamic assertions. It also represents a unit of
locking for updates.

	:-module(+Name).
	:-module(+Name,+PublicList).			% Specify public list
	:-export(+PublicList).					% Specify public list

The *module* directive can specify a list of exported public predicates,
or they can be specified with the dedicated *export* directive. The
public list elements take the form Functor/Arity.

The *module* directive will also look for a NAME.conf JSON file in the
current directory where 'NAME' is the module name. This config file
defines name/value pairs in the module namespace that will override
any explicit defines (that can set default values) thus:

	:-define(Name,Value).

defines a name/value pair in the module namespace.

	?Name

Will substitute during load a defined value from the namespace. The name
*?MODULE* will give the module name. This is a lexical pasting of the
value. Other pre-defined names are *?RANDOM*, *?RANDOMSTR*, *?TIME*
and *?TIMESTR*.

Use the module (by loading if necessary) and add the export-list to the
current namespace:

	:-use_module(+Name).

The *include/1* directive loads a file that is lexically pasted into the
source. Use the *unload_file/1* directive to remove a module from the
system.

User-defined Functions
----------------------

UDFs are ones that can be evaluated in *is/2* expressions, their
specification does not form part of standard Prolog, so there is no
standard way to define one. Trealla does this:

	:-function([fac/1]).

	fac(0.0,1).
	fac(0,1).
	fac(N,F) :- N>0, N1 is N-1, fac(N1,F1), F is N*F1.

then:

	?- F is fac(5).
	F = 120

Built-in Functions
------------------

BIFs are either compiled-in C functions or Prolog rules.

Some common and/or miscellaneous functions that are extra to the ISO
standard but are commonly found in Prolog implementations:

	consult(+Filename)          - consult file
	deconsult(+Filename)        - deconsult file
	reconsult(+Filename)        - reconsult file
	abolish(+Name,+Arity)       - abolish functor name/arity
	between(+From,+To,-Int)     - loop with start, end and index range
	term_to_atom(+Term,?Atom)   - convert term to atom
	term_to_blob(+Term,-Blob)   - convert term to BLOB
	is_list(+Term)              - is the arg a list?
	is_struct(+Term)            - is the arg a structure?
	is_tuple(+Term)             - is the arg a tuple?
	is_stream(+Term)            - is the arg a stream?
	findnsols(+N,@Term,:G,-L)   - as per SWI-Prolog
	maplist(:Goal,+L)           - call goal with every element of list L
	maplist(:Goal,+L1,+L2)      - call goal with every element of L1 & L2
	member(?Term,+List)         - does atom occur in the list?
	select(+Term,+L1,-L2)       - one occurrance of term is removed
	efface(+List,+L1,-L2)       - remove all of list (maplist on select)
	reverse(+L1,-L2)            - reverse a list
	append(+L1,+L2,-List)       - make a joined list
	find(+N,+L,-Term)           - find nth arg of list
	time(:Goal,-Float)          - run goal and return elapsed time (seconds)
	time(:Goal)                 - run goal and print elapsed time
	assert(+Clause)             - same as assertz
	asserta(+Clause,-Ref)
	assertz(+Clause,-Ref)
	erase(+Ref)
	writeln(+Term1)             - does buffered write/1 + nl/0 to stdout
	writeln(+S,+Term1)          - does buffered write/2 + nl/1 to stream

System-extras: namespace 'sys'
------------------------------

These are a random assortment of utilities:

	now(-Secs)                  - get the seconds C-epoch (also a function now())
	timestamp(-Usecs)           - get the useconds C-epoch
	sleep(+Secs)                - yield for seconds (also hsleep/1 for hard-sleep)
	msleep(+Msecs)              - yield for milli-seconds (also hmsleep/1 ...)
	concat(+Atomic,...,-Atom)   - concatenates all atomics to atom or BLOB
	getline(-Atom)              - read a line into atom (CR and/or LF removed)
	getline(+S,-Atom)           - ... same but from file stream
	exists_dir(+Path)           - check if dir exists
	exists_file(+Filename)      - check if file exists
	exists_file(+Filename,-Len,-Secs) - plus size in bytes and last-modified time C-epoch
	load_file(+Filename,-Blob)  - read named file into BLOB
	save_file(+Filename,+Atom)  - create named file
	remove_file(+Filename)      - delete named file
	append_file(+Filename,+Atom) - append to named file
	write_file(+S,+Filename)    - write contents of named file to stream
	bread(+S,?len,-Blob)        - block read from stream
	bwrite(+S,+Atom)            - block write to stream
	rand(-Int)                  - random int value >= 0 and < 2^64
	random(-Float)              - random float value >= 0.0 and <= 1.0
	uuid(-Atom)                 - return representation of a new UUID
	split(+Atom,+Sep,?L,?R)     - split atom based on separator into left & right
	split(+Atom,+Sep,-L)        - split atom based on separator into list
	jsonq(+Atom,+N,-Atom)       - quick get named value from JSON source
	jsonq(+Atom,+N,-Atom,+Def)  - quick get named value from JSON source (or default)
	xmlq(+Atom,+N,-Atom)        - quick get named value from XML source
	xmlqi(+Atom,+N,+Idx,-Atom)  - quick get I'th named value from XML source
	upper(+Atom,-Atom)          - convert to upper-case
	lower(+Atom,-Atom)          - convert to lower-case
	sha1(+Atom,-Atom)           - SHA-1 hash
	sha2(+Atom,-Atom)           - SHA-2 (256-bit) hash
	hash(+Atom,-Int)			- 32-bit non-crypto hash
	url_encode(+Atom,-Atom)
	url_decode(+Atom,-Atom)
	b64_encode(+Atom,-Atom)     - Base64 encode
	b64_decode(+Atom,-Blob)     - Base64 decode
	begins(+Atom,+List)         - does atom begin with any atom in the list?
	left(+Atom,?Len,?Sub)       - len and/or Sub must be instantiated
	right(+Atom,?Len,?Sub)      - len and/or Sub must be instantiated
	replace(+Atom,+S1,+S2,-Atom) - replace every occurrance of S1 with S2
	read_term_from_atom(+Atom,?Term,+Opts) - convert atom to term
	atom_number(+A,?V)          - convert atom to Int or Float
	atom_timestamp(+A,-Usecs)   - atom is YYYY-MM-DD!HH:MM:SS.SSS
								   (Note: the actual separators can be
								   any character at all) to usecs.

	format_rfcdate(+Int,-Atom)  - format C-epoch to RFC datetime
	parse_rfcdate(+Atom,-Int)   - parse RFC datetime to C-epoch
	exit(+Atom)                 - exit with reason string
	parse_csv(+Atom,-List)      - parse CSV into list of elements
	parse_tab(+Atom,-List)      - parse TAB into list of elements

BLOBs and atoms can often be used interchangeably, as the code checks
the type. While BLOBs can contain embedded NULs etc, atoms can't. Atoms
have a terminating NUL that is not returned as part of the length, but
BLOBs also store a length internally. BLOBS can be written and read in
back-quotes in which case the enclosed characters are base64-encoded
(eg. `SGVsbG8sIHdvcmxkIQ==` == 'Hello, world!').

Also, *'atom_length/2'* returns the number of UTF8 characters in an
atom while for BLOBs it's the stored length. Atoms are assumed UTF8.

Concurrent Processes: namespace 'proc'
--------------------------------------

The whole concept of light-weight concurrent processes and message
passing is borrowed from Erlang. Such a process in prolog is just a
free (or asynchronous) goal. Processes have little system overhead
(using just a 1K bytes of memory initially), so creating tens or even
hundreds of thousands of processes is easy (samples/skynet.pro creates
a million). They run in isolated memory and use preemptive multitasking
based on thread-pools.

A message is a term. It can be as simple as an atom or number, or as
complex as a list, tuple or compound.

A proc yields when entering a blocking recv, send, read, write or
sleep.

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
	wait/0                     - wait indefinitely
	abort_wait/0               - abort the wait

	receive ?Term              - receive message (operator fy)
	undo ?Term                 - undo message (operator fy)

	pid(+Name,-Pid)            - get Pid of named process
	pid(-Pid)                  - get current Pid
	send(+Pid,+Term)           - send to Pid
	recv(+Pid,?Term)           - receive from Pid
	rsvp(+Pid,+Term,?Term)     - rendezvous with Pid
	abort(+Pid)                - tell Pid to halt
	procinfo(+Pid,+Atom,-V)    - get named info for Pid ('msgs')

The following manipulate the per-process dictionary:

	lput(+Key,?Old,+New)       - set value under Key
	put(+Key,?Old,+New)        - set value under Key
	put(+Key,+Term)            - set value under Key
	lget(+Key,-Term)           - get value under Key (or [])
	get(+Key,-Term)            - get value under Key (or 0)
	get_keys(+V,-L)            - get list of keys with specified value
	get(-L)                    - get list of tuples (key,value)
	erase(+Key)                - erase value under Key
	erase/0                    - erase all

Hint: use *lput/3* to push or pop a list:

	lput(Key,Old,[V|Old])      - push V to head
	lput(Key,[V|New],New)      - pop V from head

A linked process will send the tuple {'EXIT',Code,Reason} back to its
parent (where *Code* represents the integer halt code and *Reason* the
description) if it terminates abnormally. If *exit/1* is called
*Code* will be 1 and *Reason* the argument supplied. Otherwise it will
be an exception code and pre-defined reason. It can be identified as
the current Pid. For example:

	?- spawn_link(exit('die')),recv(E).
	E: {'EXIT',1,'die'}
	(0.001 s) yes

Wherever *Pid* is used *Name* can also be used for local named processes.

For example, spawn 100K child processes and rendezvous with them:

	:-use_module(proc).

	parent :-
		between(1,100000,I),
			spawn(echo),
			rsvp(I,Resp),
			fail.

	echo :-
		recv(N),
		send(ok).

After a spawn or fork, both the parent and child can assume the value of
the current Pid is set (for the parent the Pid of the child, and for the
child the Pid of the parent). If the parent spawns many processes then
the Pid for each child may retrieved and saved. Two child processes may
communicate by one obtaining the Pid of the other (from the parent).
Alternatively spawn may register a local named process. Named processes
can be located by a call to *pid/2*. A process can refer to itself
as *'self'* and its parent (if linked) as *'parent'*.

A timeout may be set as an int in milliseconds, a value of 0 means no
timeout (and so polls) and -1 means infinite (the default). After a
timeout the receive call fails.

Matching may be used on the receive side to do selective message
dequeueing. A receive blocks if no message (or match) is found. After
a successful receive the current Pid is set. An Erlang style case
statement is easily implemented (see testspawn.pro test6). For example
in Erlang from [LYSEFGG] (http://learnyousomeerlang.com/content):

	important() ->
		receive
		{Priority, Message} when Priority > 10 ->
			[Message | important()]
		after 0 ->
			normal()
		end.

	normal() ->
		receive
		{_, Message} ->
			[Message | normal()]
		after 0 ->
			[]
		end.

becomes (after converting from functional to declarative style):

	:-use_module(proc).

	important(L, L2) :-
		tmo(0), receive {Priority, Message},
		after -> normal(L, L2) ;
		Priority > 10 -> important([Message | L], L2) ;
		undo {Priority, Message}.

	normal(L, L2) :-
		tmo(0), receive {_, Message},
		after -> L2 = L ;
		normal([Message | L], L2).

which returns a list of all messages with the priority ones first.

Note: *after/0* must come before any explicit pattern matches.

Note: Once the *after/0* succeeds (on a timeout) all the *undo/1*
messages become available again for reading.

A send call completes immediately, ie. asynchronously (need to limit
queue sizes?). If a synchronous rendezvous is required use *rsvp/2*.
The read side of an rsvp behaves just like a recv does (ie. can yield).
A send checks that the Pid is active and fails if it isn't.

If a process calls *sleep/1* then it yields for the specified time.

A send/recv/rsvp fails and an exception is generated if the other
process no longer exists. If the process is linked it will notify its
parent.

Each process has access to it's own private dictionary (as described
above). It is lock-free, in-memory and fast.

A process name may be any grounded term.

The *procinfo/2* named info items may be any of *'pids'*, *'idle'*,
*'names'*.

See *'samples/testspawn.pro'*, *'samples/skynet.pro'* for further guidance.

Network Processes: namespace 'proc'
-----------------------------------

A process can be created as a network server, to which a client process
connects and they can exchange messages just as above. Simple:

	server(+NameList,-Pid)            - fork as named network process
	server(+NameList,-Pid,+Key,+Cert) - ditto & specify KEY & CERT .pem files

The *netproc/2* call just creates a named network server which then
creates a process for each client connection (with TCP only). The name
refers to a named service for sharing with discovery. When multiple
bindings are provided they all funnel into the same receive queue
for the process. An ephemeral port is allocated automatically by the
system. The process terminates on a client disconnect.

Use the *pid/2* call to connect a network client, where Name may refer
to a named service to be found using discovery. If a local process of
that name is found, it will be used first, otherwise a network process
will be sought using discovery.

Both *netproc/* and *pid/2* may take bind attributes (as outlined in the
next section). For example, a simple echo server and client:

	:-use_module(proc).
	:-use_module(net).

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

	:-use_module(proc).
	:-use_module(net).

	echod :-
		server([':9000'],Pid),
		repeat,
			recv(Pid,Msg),
			send(Pid,Msg),
			fail.

	echo(Host,Msg) :-
		concat(Host,':',9000,',Server),
		pid(Server,Pid),
		rsvp(Pid,Msg,Resp),
		write(Resp), nl.

Note: messaging between network processes must always specify the
actual Pid, as the concept of 'current Pid' is only valid locally.

Note: there is no facility to directly start remote processes or run
directed or arbitrary functions.

See *'samples/echo.pro'* for further guidance.

Socket Streams: namespace 'net'
-------------------------------

Socket streams can be managed simply by using the client/server
patterns. Server bind format is 'iface:port;attr' or just ':port;attr'
or even just ';attr' if discovery is being used. Client host format is
'host:port;attr' or again just ';attr'. Multiple attributes may be
included, each seaparated by a ';' character.

Attributes can include *'tcp'*, *'udp'*, *'+tls'* or *'+ws'*. If *'udp'*
is not present then *'tcp'* is assumed. If *'+ws'* is present this
indicates raw WebSockets are in use and no HTTP protocol upgrade need
be negotiated.

Optional attributes are allowed of the form 'key=value' such as
'scope=TEST' which names a scope for discovery purposes, and 'name=QUOTES'
which specifies a named service.

If no port (or port=0) is specified then one is assigned by the
system. For named services with discovery this is usually sufficient.

For SSL servers files *key.pem* and *cert.pem* are looked for, which
contain the private key and certificate(s) (see LetEncrypt note above).
Default ones for testing are provided.

	server(+BindList,-S)            - listen for and accept connections
	server(+BindList,-S,+Key,+Cert) - ditto & specify KEY & CERT .pem files
	handler(+S,:Hello,:Bye)         - on server connection handling
	client(+Host,-S)                - connect to server
	start_tls(+S)                   - enable TLS switchover
	readmsg(+S,-Atom)               - read up thru LF from socket
	stash_get(+S,+Key,?Old)         - get value for Key (or '' if non-exist)
	stash_get(+S,+Key,?Old,+Def)    - get value for Key (or default)
	stash_set(+S,+Key,+New)         - set value for Key
	stash_set(+S,+Key,?Old,+New)    - set value for Key (return Old)
	stash_clr(+S,+Key,?Old)         - clear Key
	service(+S,-Name)               - name of the stream
	local_port(+S,-Int)             - local port
	remote_port(+S,-Int)            - remote port
	local_addr(+S,-Atom)            - local address
	local_host(+S,-Atom)            - local host (resolved)
	remote_addr(+S,-Atom)           - remote address
	remote_host(+S,-Atom)           - remote host (resolved)
	tls(+S,?V)                      - unifies with 'true/false'
	tcp(+S,?V)                      - unifies with 'true/false'
	udp(+S,?V)                      - unifies with 'true/false'
	ipv4(+S,?V)                     - unifies with 'true/false'
	ipv6(+S,?V)                     - unifies with 'true/false'
	is_socket(+Term)                - is the arg a socket?

With *server/3* a listener is started which waits for incoming connections.
Whenever an event is detected on a socket a thread continues as an
independent sub-query. The attributes 'mcast6=addr' adds multicast IPV6
membership to the group-address supplied, and 'mcast4=addr' adds IPV4.

With *client/2* it either succeeds once or fails. Client can also take
a URI format specifiying either http:, https:, ws: or wss: schemes.
If ws: or wss: then an immediate protocol upgrade should be requested.
The attribute 'loop=[1|0]' adds UDP multicast loopback and 'ttl=[N|0]'
adds multicast TTLs (aka hops). Clients can also specify userid and
password in the URL for HTTP 'Basic' authorization (preferably only over
SSL/TLS).

With *readmsg/2* the stream is read until a trailing new-line.

With *bread/3* an ungrounded length specification causes a non-blocking
read that returns whatever is currently available, otherwise it blocks
until it can return the specified length.

Note: regular streams I/O can be used over sockets.

Note: writes to sockets are normally blocking but *sys:write_file/2*
can yield internally when called by a process. Use *tmo/1* to set
a timeout.

HTTP processing: namespace 'http'
---------------------------------

Hyper-Text Transfer Protocol:

	parse(+S,-Ver,-Cmd,-Path)    - parse & decode request (except content)
	www_form(+S)                 - decode form data on POST (urlencoded)
	form(+S,+Name,-Atom)         - get named form value (or '' if non-exist)
	query(+S,+Name,-Atom)        - get named query value (or '' if non-exist)
	cookie(+S,+Name,-Atom)       - get named cookie value (or '' if non-exist)
	basic_auth(+S,-User,-Pass)   - decode Basic auth token (if present)

	get10(+S,+Path,-Status)      - GET HTTP/1.0 & parse response
	head10(+S,+Path,-Status)     - HEAD HTTP/1.0 & parse response
	del10(+S,+Path,-Status)      - DELETE HTTP/1.0 & parse response
	put10(+S,+Path,+Type,+Len,-Status) - PUT HTTP/1.0 & parse response

	get11(+S,+Path,-Status)      - GET HTTP/1.1 & parse response
	head11(+S,+Path,-Status)     - HEAD HTTP/1.1 & parse response
	del11(+S,+Path,-Status)      - DELETE HTTP/1.1 & parse response
	put11(+S,+Path,+Type,-Status) - PUT HTTP/1.1 & parse response
	get_chunk(+S,-Blob,-Len)     - read HTTP/1.1 chunk (Len=0 on final chunk)
	put_chunk(+S,+Atom,+Len)     - write HTTP/1.1 chunk (len=0 to finalize)
	put_chunk(+S,+Atom)          - write HTTP/1.1 chunk
	put_file(+S,+Filename)       - write HTTP/1.1 chunked

With *parse/4* and *get/3* header values are saved to the stash and can
be accessed by name. Ditto with cookie crumbs. With *parse/4* query args
can also be accessed by name. These functions succeed only when all
headers have been consumed and content (if any) is ready for reading.
Stash keys are case-insensitive. The path and query values are
URL-decoded. Currently only the HEAD and GET methods are supported.

Note: writes to sockets are normally blocking but *put_file/2*
can yield internally when called by a process. Use *tmo/1* to set
a timeout.

See *'samples/http_server.pro'* & *'samples/http_client.pro'* for guidance.

HTTP2 processing: namespace 'h2'
---------------------------------

Under development.

WebSocket: namespace 'ws'
-------------------------

	request(+S,+Path,-Status,+Prots,-Prot) - request HTTP upgrade to WS
	request(+S,+Path,-Status)              - request HTTP upgrade to WS
	upgrade(+S,+Prot)                      - do HTTP upgrade to WS

	parse(+S,-Op,-Atom)    - parse and read message
	msg(+S,+Op,+Atom)      - write message

	is_ws(+S,?V)               - WebSocket? unifies with 'true/false'

With *'msg/3'* *Op* can be the atom *'more'*, *'data'*, *'ping'* or
*'close'*. Note *'parse/3'* will return a *'more'*, *'data'* or *'close'*,
anything else should be discarded.

Also note data atom will be a BLOB for a binary message and an atom for
a text (UTF8) message.

The *request/5* client call takes an atom or list of atoms of wanted
protocols and returns the one selected by the server on success. A
*Status* of 101 indicates success.

Also, *parse/3* and *msg/3* can be used with raw WebSockets, ie. both
client and server end-points were created with the '+ws' attribute
(and so no upgrade was needed). This can be used to exchange BLOBs.

For example:

  ./tpl samples/testws "--goal=ping('ws://echo.websocket.org','Hello, world!')"

See *'samples/ws_server.pro'* & *'samples/testws.pro'* for guidance.

Pub/Sub processing: namespace 'stomp'
-------------------------------------

The Simple(/Streaming) Text Oriented Messaging Protocol:

	parse(+S,-Cmd,-Len)       - parse message (except content)
	msg(+S,+Cmd,+Hdrs,+Atom)  - send ANY request ('Data' can be BLOB or atom)

While superficially similar to HTTP, STOMP is actually bidirectional, so
*'parse/3'* and *'msg/4'* are used on both the server & client sides.

See *'samples/stomp_server.pro'* & *'samples/stomp_client.pro'* for guidance.

Database store: namespace 'dbs'
------------------------------

The rule database will save to the transaction log asserted items
that have first been declared with the 'persist' modifier:

	:-dynamic(Name/Arity,[persist]).

All such persistent asserta/assertz/retract items are recorded. A
retractall call is converted to individual retracts.

The alternative declaration with a 'storage' modifier:

	:-dynamic(Name/Arity,[storage]).

means that all data from the second-argument on is stored on disk
and is not in memory. Instead a filepos is stored and the data is
loaded on demand. In this way rule databases far larger than physical
memory can be accessed.

To load previously saved values:

	load/0               - load existing data

Transactions within the same database can be done via bracketing a
series of one or more updates with *begin/end* calls:

	begin/0              - start atomic transaction sequence
	end/0                - commit changes with fsync
	end(+Boolean)        - commit changes optional fsync

and occurs as a single (all-or-nothing) update to the database and
(if persistent) write to the transaction log-file. A transaction locks
out any other *dbs* transactions for the duration. The *begin/0* call
creates a choicepoint and backtracking will cause it to rollback the
transaction. Inside a transaction changes to the database are not visible.

To write to the log only, without updating the database:

	log(:Term)

where *Term* is an asserta/assertz/retract (or other) operation. This
can be useful for external adapters that need to write to the log what
to do, without actually doing it. Another program could then tail the
log and update the real database.

Periodically the log and the master are merged and a new master is
created, discarding old files. This normally happens when the '--merge'
option is specified.

Dictionary module: namespace  'dict'
------------------------------------

This is a compiled-in module that must be imported:

	:-use_module(dict).

Provides control over name-value pairs in a list:

	get(+Dict,+Name,-Value)
	lget(+Dict,+Name,-Value)
	set(+Dict,+Name,+Value,-NewDict)
	del(+Dict,+Name,-NewDict)

If name not found 'get' returns 0, 'lget' returns [].

Auth module: namespace 'auth'
-----------------------------

This is a compiled-in module that must be imported:

	:-use_module(auth).

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
	logout(+SessId)

Blog module: namespace 'blog'
-----------------------------

This is a compiled-in module that must be imported:

	:-use_module(blog).

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

SMTP client module: namespace 'smtp_client'
-------------------------------------------

This is a compiled-in module that must be imported:

	:-use_module(smtp_client).

Provides basic mail sending functionality:

	send_plain(+From,+To,+Subject,+Body)
	send_html(+From,+To,+Subject,+Body)
	send_base64(+From,+To,+Subject,+Body)

More functionality to added, eg. attachments and inline HTML objects.

HTTP client module: namespace 'http_client'
-------------------------------------------

This is a compiled-in module that must be imported:

	:-use_module(http_client).

Provides basic HTTP/1.0 & HTTP/1.1 client functionality:

	get10_data(+Host,+Path,-Data)
	get10_file(+Host,+Path,+Filename)
	put10_file(+Host,+Path,+Filename)

	get11_data(+Host,Path,-Data)
	get11_file(+Host,Path,+Filename)
	put11_file(+Host,Path,+Filename)

