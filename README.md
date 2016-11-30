Trealla Prolog
==============

Prolog with processes
---------------------

Trealla is Prolog with light-weight processes and message passing.

Trealla is an interpreter that parses to an AST. Queries are executed
by the Jela ("fir-tree") runtime running recursively over the AST while
maintaining a trail (environment) and stack (choices). It uses deep
binding and structure sharing.

It supports much of ISO-PROLOG minus (for now) exceptions and bagof,
setof, ...? With:

 - modules
 - persistent clause database (logged to disk)
 - key-value datastore (for huge storage to disk)
 - transactions on database operations
 - first-argument indexing
 - Erlang-style processes and message-passing
 - advanced built-in networking features
 - unlimited length and number of UTF8 or binary atoms
 - 128-bit integers

The rule database usage is currently immediate update view (the
traditional way). ISO-PROLOG however specifies logical update view
(ie. snapshot), so this may change in future (a flag?).

Writen in plain-old C with a permissive license.

This is early alpha-stage software and is subject to change. This is
experimental software that itself is an ongoing experiment.

Getting / Building
------------------

	git clone https://github.com/trealla-lang/trealla
	cd trealla
	make [CC=gcc|clang|tcc] [iso|iso_debug|debug]

WIN compilation is sporadic and may not support everything.

RaspberryPi compilation should probably use TCC, but TCC does not
support 128-bit integers or atomics (so no procs). GCC & Clang are
very slow.

Compiler should be C11 if using procs (to use atomics) otherwise
C99 should be adequate, eg:

	make OPT=-std=c11       # or -std=gnu11

if not the default.

Missing
-------

Current ISO predicates that are not implemented:

	bagog
	setof
	set_input
	set_output
	curr_input
	curr_output
	sub_atom

As far as I know, anyway.

Usage
-----

  ./tpl [files] [options]

	Options:

	--goal=term    - set 'term' as the top-level goal
	--start        - same as '--goal=start'
	--test         - same as '--goal=test'
	--listing      - same as '--goal=listing' (also --listing_canonical)
	--stats        - print statistics at completion of query
	--trace        - trace step-by-step of execution
	--noopt        - disable optimizations (or -O0)
	--tpool=N      - thread-pool size (default is 2)
	--daemon       - daemonize program (or -d)
	--watchdog     - set restart watchdog for daemon (or -w)
	--cd=path      - chdir for daemon
	--quiet        - no banner (or -q)
	--load         - load file (or -l, actually does nothing)
	--merge        - merge transaction log

Files can be filename[.ext] where '.ext' if not specified can be one
of the following: .pro, .prolog, .pl or .P

	tpl -l samples/validate.pro
	yap -l samples/validate.pro
	swipl -l samples/validate.pro --traditional

	./tpl samples/queens4 --test
	./tpl samples/queens12 --test
	./tpl samples/qsort --test
	./tpl samples/sieve --test
	./tpl samples/fac --test

Special Usage
-------------

	./tpl get URL [filename]       	    - download (to filename)
	./tpl install URL [filename]   	    - download & then extract

	./tpl appget name                   - download app (from GitHub)

Using the REPL
--------------

To just get a prompt do not specify a goal:

	> ./tpl
	?- Y is 12/4, Z is 12//4.
	Y: 3.0, Z: 3
	?- halt.
	>

The REPL supports LEFT/RIGHT arrow keys (move left/right) as well as
CTRL-LEFT/RIGHT (jump left/right), UP/DOWN for prev/next command in
history. Also CTRL-R for reverse-search in history. History is saved
in the *'~/.tpl_history'* file. Also HOME (CTRL-A) to go to start of
line, END (CTRL-E) for end of line, and CTRK-K to kill to end of line.

Trealla also supports command-input via input redirection.

Benchmarks
----------

Indicative only, may not be current, and depends highly on compilers
and compiler options. Made with 'make iso' option:

  ./tpl samples/hanoi '--goal=hanoiq(20)'

	yap       0.072s
	swi       0.206s
	gprolog   0.404s
	trealla   0.455s

  ./tpl samples/sieve '--goal=test4'  // generate 1st 5K primes

	yap       0.052s
	trealla   0.138s
	swi       0.212s
	gprolog   0.272s

  ./tpl samples/queens4 '--goal=test3'  // queens 4x4 10K times

	yap       0.152s
	swi       0.875s
	trealla   1.043s
	gprolog   1.180s

  ./tpl samples/queens12 '--goal=testq' // queens 12x12 1 times

	yap       1.546s
	swi       8.189s
	trealla  10.190s
	gprolog  11.204s

[Features](FEATURES.md)
