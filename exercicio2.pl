/ 2.Considere o código que usa operadores customizados abaixo./

:- op(400, yfx, e).
:- op(500, xfx, amo).
:- op(600, xf, demais).

/Relacione os trechos de código com suas respectivas formas canônicas./

write_canonical(X).

/
X - eu amo prolog demais. 
→ demais(amo(eu, prolog)).

X - eu amo prolog e python e elm. 
→ amo(eu, e(e(prolog, python), elm)).

X - eu e python amo prolog. 
→ amo(e(eu, python), prolog).
/