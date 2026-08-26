genitor(maria, joao).
genitor(jose, joao).
genitor(joao, ana).

avo(X, Y) :- genitor(X, Z), genitor(Z, Y).