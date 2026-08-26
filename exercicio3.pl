:- use_module(library(clpfd)).

% DOIS + DOIS = OITO: cada letra é um dígito (0-9) diferente, e as
% letras que começam um número não podem valer 0.
crypto([
    [D, O, I, S],
    [D, O, I, S],
    [O, I, T, O]
]) :-
    Vars = [D, O, I, S, T],
    Vars ins 0..9,
    all_different(Vars),
    D #\= 0, O #\= 0,
    1000 * D + 100 * O + 10 * I + S +
    1000 * D + 100 * O + 10 * I + S #=
    1000 * O + 100 * I + 10 * T + O.


:- forall((
        crypto([
            [D, O, I, S],
            [D, O, I, S],
            [O, I, T, O]
        ]),
        label([D, O, I, S, T])
    ),
    format("DOIS=~w~w~w~w + DOIS=~w~w~w~w = OITO=~w~w~w~w~n", [D, O, I, S, D, O, I, S, O, I, T, O])).