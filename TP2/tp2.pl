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
serializar(computar, [computar]).
serializar(leer(B), [leer(B)]).
serializar(escribir(B,E),[escribir(B,E)]).
serializar(secuencia(P,Q),ZS) :- serializar(P,XS), serializar(Q, YS), append(XS, YS, ZS).
serializar(paralelo(P,Q),ZS) :- serializar(P,XS), serializar(Q, YS), intercalar(XS, YS, ZS).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% contenidoBuffer(+B, +ProcesoOLista, ?Contenidos)
contenidoBuffer(B, POL, CS) :- procesarEntrada(POL, P), contenidoBufferLista(B, P, C), procesarLecturas(B, P, C, CS). %primero convierte la entrada a lista si es proceso, despues llena con las escrituras, y despues aplica las lecturas


%procesarEntrada(+ProcesoOLista, +ListaProcesos)
%puede que necesite cut para evitar que entre a las dos ramas
procesarEntrada(POL, LPS) :- esLista(POL), LPS = POL.   % si es lista unifica ListaProcesos con ProcesoOLista (si es lista va a poder unificar)
procesarEntrada(POL, LPS) :- serializar(POL, LPS).      % si no es lista serializa y devuelve la lista. NOTA: en el caos de ser un paralelo no repite las soluciones como en los ejemplos, no se si esta bien o no

%% contenidoBufferLista(+B, +ListaProcesos, ?ContenidosLista) % escribe los contendios del buffer consultado
contenidoBufferLista(_, [], []).                                                                  % caso base, si no hya escrituras sale vacio
contenidoBufferLista(B, [computar | XS], LS) :- contenidoBufferLista(B, XS, LS).                  % computar se ignora
contenidoBufferLista(B, [escribir(B, E) | XS], [E | LS]) :- contenidoBufferLista(B, XS, LS).      % si escribe el buffer consultado se agrega a la lista de salida (COntenidosLista)
contenidoBufferLista(B, [escribir(XB, _) | XS], LS) :- B \= XB, contenidoBufferLista(B, XS, LS).  % si escribe en otro buffer se ignora
%NOTA: estoy seguro que los casos de leer pueden ser uno solo que ignore todas las lecturas
contenidoBufferLista(B, [leer(B) | XS], LS) :- contenidoBufferLista(B, XS, LS).                   % si lee el buffer consultado se ignora
contenidoBufferLista(B, [leer(XB) | XS], LS) :- B \= XB, contenidoBufferLista(B, XS, LS).         % si lee otro buffer tambien se ignora

%% procesarLecturas(+B, +ListaProcesos, +ContenidosEntrada, ?ContenidosSalida)
procesarLecturas(_, [], CE, CE).                                                            % caso base lista vacia, sale la misma
procesarLecturas(B, [computar | XS], CE, CS) :- procesarLecturas(B, XS, CE, CS).            % computar se ignora
procesarLecturas(B, [leer(B) | XS], [C | CE], CS) :- procesarLecturas(B, XS, CE, CS).       % si se lee el buffer consultado desencola
procesarLecturas(B, [leer(XB) | XS], CE, CS) :- B \= XB, procesarLecturas(B, XS, CE, CS).   % si lee otro buffer se ignora
procesarLecturas(B, [escribir(_,_) | XS], CE, CS) :- procesarLecturas(B, XS, CE, CS).       % las escrituras se ignoran

% Predicado para verificar si una entrada es una lista
esLista([]). %no se si [_|_] ya lo cubre
esLista([_]). % puede que no haga falta pq "_" puede ser vacio en el otro caso
esLista([_|_]).

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
