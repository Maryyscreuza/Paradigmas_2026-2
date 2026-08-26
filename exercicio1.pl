/ 1. Considere os trechos de código Prolog abaixo e julgue as afirmações. /
% ex A
father(john, X) = father(Y, mary). 

% ex B
father(john, mary) = father(X). 

% ex C
father(X, Y) = father(john, X).  

/
O exemplo B não unifica porque duas variáveis diferentes não unificam.
F
 
O exemplo A unifica com X = mary e Y = john.
V
 
No exemplo C, unificamos Y com john.
V
 
O exemplo B unifica como uma lista X=[john, mary]
F
 
Apenas o exemplo B não unifica.
V
/

% Exercício 1 - exemplos de unificação (versão simples)
% Aqui mostramos três testes de unificação e o resultado esperado.

% Teste A: father(john, X) = father(Y, mary).
% Esperado: unifica com X = mary, Y = john.
test_a :-
	(father(john, X) = father(Y, mary) ->
		format('A: unifica -> X=~w, Y=~w~n', [X, Y]) ;
		writeln('A: nao unifica'))
.

% Teste B: father(john, mary) = father(X).
% Esperado: nao unifica (diferença de aridade).
test_b :-
	(father(john, mary) = father(X) ->
		format('B: unifica -> X=~w~n', [X]) ;
		writeln('B: nao unifica'))
.

% Teste C: father(X, Y) = father(john, X).
% Esperado: unifica com X = john e Y = john.
test_c :-
	(father(X, Y) = father(john, X) ->
		format('C: unifica -> X=~w, Y=~w~n', [X, Y]) ;
		writeln('C: nao unifica'))
.

% Executa os três testes sequencialmente
run_tests :-
	test_a,
	test_b,
	test_c.

% Dica: carregar no SWI-Prolog e chamar `run_tests.`