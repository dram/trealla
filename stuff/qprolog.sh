#!/bin/sh

export COMMAND_ARGUMENTS="$*"

exec tpl -q --consult <<EOF

:- op(400, yfx, '\\\\\\\\').
:- op(1150, fx, determinate).
:- op(1150, fx, mode).

determinate(_).
mode(_).

command_line_arguments(Args) :-
        getenv('COMMAND_ARGUMENTS', ArgumentString),
        sys:split_all(ArgumentString, ' ', Args).

dbreference(_) :- fail.         % sufficient here

enable_trace(_).

current_error_output(S) :- current_output(S).

%% this must produce identical results as hash_name() in pc.h
atom_hash(A, H) :-
        atom_codes(A, AL),
        atom_hash(0, AL, 0, H).
atom_hash(I, AL, H, H) :-
        (I >= 100; AL == []), !.
atom_hash(I, [C|R], H1, H2) :-
        H is (H1 xor ((H1 << 6) + (H1 >> 2) + C)) /\\ 1073741823,
        I2 is I + 1,
        atom_hash(I2, R, H, H2).

skip_shebang :- \\+ peek_char('#').
skip_shebang :- repeat, get0(10).

append([], []).
append([List |RestLists], Concatenated) :-
        append(RestLists, RestConcatenated),
        append(List, RestConcatenated, Concatenated).

:- include('settings.pl').
:- include('lib/rdtok.pl').
:- include('lib/read.pl').
:- include('support.pl').
:- include('state.pl').
:- include('terms.pl').
:- include('index.pl').
:- include('lib/dcg.pl').
:- include('builtin.pl').
:- include('process.pl').
:- include('compile.pl').
:- include('assemble.pl').
:- include('xref.pl').
:- include('dce.pl').
:- include('main.pl').

show_version_and_exit :- display('<none>\\n'), halt.

EOF
