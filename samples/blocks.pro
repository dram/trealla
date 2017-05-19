% A blocks grammar in DCG

:-initialization(main).

%%%%%%%%%%%%%%%%% Grammar %%%%%%%%%%%%%%%%%%%

s --> vp.
s --> qest.

qest --> wh_loc, vbe, np.
qest --> wh_obj1, vbe, pp.
qest --> wh_obj2, snp, vbe, pp.
qest --> vbe, np, pp.

vp --> v, np.

np --> pn.
np --> det, snp.
np --> det, snp, pp.

snp --> noun.
snp --> ap, noun.

ap --> adj.
ap --> adj, ap.

pp --> prep, np.

noun --> [block].
noun --> [box].
noun --> [table].
noun --> [one].

pn --> [it].

v --> [put].
v --> [move].
v --> [pickup].
v --> [putdown].

vbe --> [is].

wh_loc --> [where].

wh_obj1 --> [what].

wh_obj2 --> [which].

adj --> [white].
adj --> [red].
adj --> [blue].
adj --> [green].
adj --> [big].
adj --> [small].
adj --> [large].
adj --> [little].

prep --> [on].
prep --> [onto].
prep --> [above].
prep --> [over].

det --> [each].
det --> [every].
det --> [the].
det --> [a].
det --> [some].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(S) :- phrase(s,S), write(S), write(' '), writeln(ok).
test(S) :- write(S), write(' '), writeln(nok).

main :-
	test([pickup,the,small,white,box]),
	test([paint,the,small,white,box]),
	halt.
