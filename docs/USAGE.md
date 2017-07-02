Trealla Prolog
==============

[Home](../README.md) | [Features](../docs/FEATURES.md)

Getting / Building
------------------

	git clone https://github.com/trealla-lang/trealla
	cd trealla
	make [CC=gcc|clang|tcc] [iso|iso_debug|iso_profile|debug|profile]

Note *tcc* does not support atomics (so no procs).

Compiler should be C11 if using procs (to use atomics) otherwise C99 should be
adequate, eg:

	make OPT=-std=c11       # or -std=gnu11
	make OPT=-std=c99       # or -std=gnu99

The Windows build has been discontinued.

Missing / Limitations
---------------------

Current ISO predicates that are (known) not yet implemented:

	sub_atom
	*exceptions*

Unbounded integers (bigints or bignums) are considered experimental at this stage. To introduce
a bignum use the 'B' suffix e.g: *X is 1B*, or *unbounded/1* function e.g: *X is unbounded(1)*.
Thereafter X will propagate as such in calculations. Integer literals will automatically set to
unbounded if needed (>INT_MAX) during parsing. Not all BIFs may handle unbounded ints as yet.
Expect numerical programs to run 2-3 times slower when making use of unbounded ints. Integer
overflow does *NOT* promote to unbounded (nor is it detected). For example:

	tpl -l samples/factorial.pro -g 'fac(200B,F),writeln(F)'               # OR
	tpl -l samples/factorial.pro -g 'fac(200,F),writeln(F)' --unbounded

The rule database usage is currently immediate update view (the traditional way). ISO-PROLOG
however specifies logical update view (ie. snapshot), so this may change in future (a flag?).

Trealla reserves predicate names beginning with *$* for internal use.

Usage
-----

  tpl [options]

	Options:

	-l file        - load (consult) file
	-g term        - set 'term' as the top-level goal
	-O0            - no optimization
	-O1            - optimizes conjunctions
	-O2            -  + last-call/last-match optimization
	-O3            -  ++ plus tail-recursion elimination (DEFAULT)
	-q             - quiet mode (no banner)
	-v             - verbose mode (show execution time)

	--stats        - print statistics of query (debug build only)
	--trace        - trace step-by-step of execution
	--tpool=N      - thread-pool size (default is 4)
	--daemon       - daemonize program (or -d)
	--watchdog     - set restart watchdog for daemon (or -w)
	--cd=path      - chdir (usually for daemon)
	--merge        - merge transaction log
	--consult      - consult from STDIN
	--ns           - non-stop (ie. don't drop to REPL) on error
	--http11       - use HTTP/1.1 (DEFAULT)
	--http10       - use HTTP/1.0
	--consult      - consult from STDIN
	--dbdir=path   - root for persistent database files
	--unbounded    - use unbounded ints by default for integers

Files can be filename[.ext] where '.ext' if not specified can be one of the following:

	.pro, .prolog, .pl, or .P extensions.

Avoid using *.pl* so as not confuse your text editor (it will think it's Perl source).

	tpl -l samples/validate.pro
	yap -l samples/validate.pro
	swipl -l samples/validate.pro --traditional

	tpl -l samples/queens4.pro -g test
	tpl -l samples/queens8.pro -g test
	tpl -l samples/qsort.pro -g test
	tpl -l samples/sieve.pro -g test
	tpl -l samples/fac.pro -g test

Consulting from STDIN works as follows:

	ARG=hello
	tpl --consult <<EOF
	:-initialization(main).
	main :- write($ARG), nl.
	EOF

and can be useful with scripting.

Special Usage
-------------

	tpl get URL [filename]       	    - download
	tpl install URL [filename]   	    - download & then extract
	tpl appget NAME                   - download app (from GitHub)

Using the REPL
--------------

To just get a prompt do not specify a goal:

	> tpl
	?- Y is 12/4, Z is 12//4.
	Y: 3.0, Z: 3
	?- 1/3.
	 0.333333333333333
	?- halt.
	>

The REPL supports LEFT/RIGHT arrow keys (move left/right) as well as
CTRL-LEFT/RIGHT (jump left/right), UP/DOWN for prev/next command in
history (also CTRL-P/CTRL-N) and CTRL-R for reverse-search in history.
History is saved in the *~/.tpl_history* file. Also HOME (CTRL-A) to
go to start of line, END (CTRL-E) for end of line, and CTRK-K to kill
to end of line.

Benchmarks
----------

Indicative only, may not be current, and depends highly on compilers
and compiler options. Made with 'make iso' option:

  tpl -l samples/hanoi.pro -g 'hanoiq(20)'

	yap       0.072s
	swipl     0.213s
	gprolog   0.388s
	trealla   0.424s

  tpl -l samples/sieve.pro -g 'test4'  // generate 1st 5K primes

	yap       0.052s
	trealla   0.151s
	swipl     0.155s   (with -O)
	swipl     0.197s
	gprolog   0.272s

  tpl -l samples/sieve.pro -g 'test6'  // generate 1st 50K primes

	yap       2.7s
	trealla   8.3s
	swipl     out of memory after 15.7 seconds
	gprolog   out of memory

  tpl -l samples/queens4.pro -g 'test3'  // queens 4x4 10K times

	yap       0.152s
	swipl     0.608s   (with -O)
	swipl     0.783s
	trealla   1.108s
	gprolog   1.180s

  tpl -l samples/queens12.pro -g 'testq' // queens 12x12 1 times

	yap       1.5s
	swipl     5.7s   (with -O)
	swipl     8.3s
	trealla  10.9s
	gprolog  11.5s

[Features](docs/FEATURES.md)
