%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados básicos %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% proceso(+P)
% acciones basicas:
proceso(computar).
proceso(escribir(B,E)).
proceso(leer(B)).

%procesos que pueden incluir varias acciones (es decir que tanto p como q sean procesos):
proceso(secuencia(P, Q)) :- proceso(P), proceso(Q).

proceso(paralelo(P, Q)) :- proceso(P), proceso(Q).

%% Ejercicio 2
%% buffersUsados(+P,-BS)
buffersUsados(escribir(B,E), [B]).
buffersUsados(leer(B), [B]).
buffersUsados(secuencia(P, Q), BS) :- buffersUsados(P, B1), buffersUsados(Q, B2), append(B1, B2, B), eliminaRepetidos(B, BS).
buffersUsados(paralelo(P, Q), BS) :- buffersUsados(P, B1), buffersUsados(Q, B2), append(B1, B2, B), eliminaRepetidos(B, BS).

%predicados extra:
%eliminaRepetidos(+E, -L)
eliminaRepetidos([], []).
eliminaRepetidos([X|XS], [X|LSR]) :- not(member(X, XS)), eliminaRepetidos(XS, LSR).
eliminaRepetidos([X|XS], LSR) :- member(X, XS), eliminaRepetidos(X, LSR).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Organización de procesos %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 3
%% intercalar(+XS,+YS,?ZS)
intercalar([], [], []).
intercalar([X|XS],YS,[X|ZS]) :- intercalar(XS, YS, ZS).
intercalar(XS,[Y|YS],[Y|ZS]) :- intercalar(XS, YS, ZS).

%% Ejercicio 4
%% serializar(+P,?XS)
serializar(_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% contenidoBuffer(+B,+ProcesoOLista,?Contenidos)
contenidoBuffer(_,_,_).


%% Ejercicio 6
%% contenidoLeido(+ProcesoOLista,?Contenidos)
contenidoLeido(_,_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 7
%% esSeguro(+P)

%% Ejercicio 8
%% ejecucionSegura( XS,+BS,+CS) - COMPLETAR LA INSTANCIACIÓN DE XS
ejecucionSegura(_,_,_).

  %% 8.1. Analizar la reversibilidad de XS, justificando adecuadamente por qué el predicado se comporta como
  %% lo hace.



%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

% Se espera que completen con las subsecciones de tests que crean necesarias, más allá de las puestas en estos ejemplos

cantidadTestsBasicos(2). % Actualizar con la cantidad de tests que entreguen
testBasico(1) :- proceso(computar).
testBasico(2) :- proceso(secuencia(escribir(1,pepe),escribir(2,pipo))).
testBasico(3) :- buffersUsados(escribir(1, hola), [1]).
% Agregar más tests

cantidadTestsProcesos(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

cantidadTestsBuffers(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

cantidadTestsSeguros(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests


tests(basico) :- cantidadTestsBasicos(M), forall(between(1,M,N), testBasico(N)).
tests(procesos) :- cantidadTestsProcesos(M), forall(between(1,M,N), testProcesos(N)).
tests(buffers) :- cantidadTestsBuffers(M), forall(between(1,M,N), testBuffers(N)).
tests(seguros) :- cantidadTestsSeguros(M), forall(between(1,M,N), testSeguros(N)).

tests(todos) :-
  tests(basico),
  tests(procesos),
  tests(buffers),
  tests(seguros).

tests :- tests(todos).
