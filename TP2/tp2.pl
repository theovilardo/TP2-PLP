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
buffersUsados(secuencia(P, Q), BS) :- buffersUsados(P, B1), buffersUsados(Q, B2), append(B1, B2, B), setof(X, member(X, B), BS).
buffersUsados(paralelo(P, Q), BS) :- buffersUsados(P, B1), buffersUsados(Q, B2), append(B1, B2, B), setof(X, member(X, B), BS).

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
contenidoBuffer(B, POL, CS) :- procesarEntrada(POL, P), contenidoBufferLista(B, P, C), procesarLecturas(B, P, C, CS).

%procesarEntrada(+ProcesoOLista, -ListaProcesos)
%puede que necesite cut para evitar que entre a las dos ramas
procesarEntrada(POL, POL).                                       % si no es proceso devuelve la serialización
procesarEntrada(POL, PL) :- proceso(POL), serializar(POL, PL).   % si es proceso serializa y la devuelve

%% contenidoBufferLista(+B, +ListaProcesos, ?ContenidosLista) % escribe los contendios del buffer consultado
contenidoBufferLista(_, [], []).                                                                  % caso base, si no hya escrituras sale vacio
contenidoBufferLista(B, [computar | XS], LS) :- contenidoBufferLista(B, XS, LS).                  % computar se ignora
contenidoBufferLista(B, [escribir(B, E) | XS], [E | LS]) :- contenidoBufferLista(B, XS, LS).      % si escribe el buffer consultado se agrega a la lista de salida (COntenidosLista)
contenidoBufferLista(B, [escribir(XB, _) | XS], LS) :- B \= XB, contenidoBufferLista(B, XS, LS).  % si escribe en otro buffer se ignora
contenidoBufferLista(B, [leer(_) | XS], LS) :- contenidoBufferLista(B, XS, LS).                   % las lecturas se ignoran

%% procesarLecturas(+B, +ListaProcesos, +ContenidosEntrada, ?ContenidosSalida)
procesarLecturas(_, [], CE, CE).                                                            % caso base lista vacia, sale la misma
procesarLecturas(B, [computar | XS], CE, CS) :- procesarLecturas(B, XS, CE, CS).            % computar se ignora
procesarLecturas(B, [leer(B) | XS], [C | CE], CS) :- procesarLecturas(B, XS, CE, CS).       % si se lee el buffer consultado desencola
procesarLecturas(B, [leer(XB) | XS], CE, CS) :- B \= XB, procesarLecturas(B, XS, CE, CS).   % si lee otro buffer se ignora
procesarLecturas(B, [escribir(_,_) | XS], CE, CS) :- procesarLecturas(B, XS, CE, CS).       % las escrituras se ignoran

%% Version sin funciones auxiliares (consultar uso de reverse)
%% contenidoBuffer(+B,+ProcesoOLista,?Contenidos)
%contenidoBuffer(B, P, _) :- proceso(P), serializar(P, PS), contenidoBuffer(B, PS, C). %Si es proceso, lo serializo
%contenidoBuffer(B, L, []) :- reverse(L, PR), contenidoBuffer(B, LR, ). %Cuando esta serializado, lo invierto
%contenidoBuffer(_, [], []). %Caso base
%contenidoBuffer(B, [computar|XS], C) :- contenidoBuffer(B, XS, C). %Computar se ignora
%contenidoBuffer(B, [escribir(B, E)|XS], [E|CS]) :- contenidoBuffer(B, XS, CS). %La lista resultante debe tener lo escrito en el buffer
%contenidoBuffer(B, [escribir(X, _)|XS], C) :- B \= X, contenidoBuffer(B, XS, C). %Ignoro las escrituras a otros buffers
%contenidoBuffer(B, [leer(B)|XS], C) :- member(escribir(B, _), XS), contenidoBuffer(B, XS, C). %Leer
%contenidoBuffer(B, [leer(_)|XS], C) :- contenidoBuffer(B, XS, C). %Ignoro las lecturas a otros buffers

%% Ejercicio 6
%% contenidoLeido(+ProcesoOLista, ?Contenidos)
contenidoLeido(POL, CS) :- obtenerProcesos(POL, PS), reverse(PS, PSR), aplicarLecturasR(PSR, CSR), reverse(CSR, CS).     %lo doy vuelta para leer y le vuelvo a dar vuelta para devolver el contenido

% obtenerProcesos(+ProcesoOLista, -Procesos)
obtenerProcesos(POL, PS) :- proceso(POL), serializar(POL, PS).
obtenerProcesos(POL, POL).

% procesarLecturasR(+ProcesosReverse, -ContenidosReverse)
aplicarLecturasR([], []).                                                                                       % caso base
aplicarLecturasR([leer(B)|XS], [E|ES]) :- desencolarEscrituras(B, XS, XSA, E), aplicarLecturasR(XSA, ES).       % caso lectura, verifica que haya contenido para leer con desencolarEscrituras y se agrega a los contenidos leidos
aplicarLecturasR([X|XS], ES) :- X \= leer(_), aplicarLecturasR(XS, ES).                                         % si no es lectura ignoro el proceso y sigo buscando 

% desencolarEscrituras(+Buffer, +Procesos, -ProcesosActualizados, -Escritura)
desencolarEscrituras(B, P, PA, E) :- select(escribir(B, E), P, PA).                  % una vez leida la escritura la saco de la lista para que no se vuelva a leer en caso de haber otra lectura anterior, si no hay nada select tira false?


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 7
%% esSeguro(+P)

esSeguro(secuencia(P,Q)) :-  esSeguro(P), esSeguro(Q).
esSeguro(paralelo(P,Q)) :- esUnParaleloSeguro(paralelo(P,Q)), esSeguro(P), esSeguro(Q).

esSeguro(P) :- P \= paralelo(_,_), serializar(P, LP), reverse(LP,LPR), esSeguro(LPR).
esSeguro([]).
esSeguro([computar|LP]) :- esSeguro(LP).
esSeguro([escribir(_,_)|LP]) :- esSeguro(LP).
esSeguro([leer(B)|LP]) :- member(escribir(B, _), LP).

%Aux:
%esUnPareleloSeguro(+P)
esUnParaleloSeguro(paralelo(P,Q)):- buffersUsados(P,PS), buffersUsados(Q, QS), interseccionVacia(PS, QS).

% interseccionVacia(+L1, +L2) es verdadero si L1 y L2 no tienen elementos en común
interseccionVacia([], _).
interseccionVacia([X|XS], YS) :- not(member(X, YS)), interseccionVacia(XS, YS).


%% Ejercicio 8
%% ejecucionSegura(-XS,+BS,+CS) - COMPLETAR LA INSTANCIACIÓN DE XS
ejecucionSegura(XS, BS, CS) :- generarEjecuciones(XS, BS, CS), esSeguro(XS).

% generarSecuencia(?Procesos, +Buffers, +Contenidos)
generarEjecuciones([], _, _).                                                                 % caso base 
generarEjecuciones([P|PS], BS, CS) :- generarEjecuciones(PS, BS, CS), generarOp(BS, CS, P).   % lista de procesos, recursion

% accionValida(?Proceso, +Buffers, +Contenidos)
generarOp(_, _, computar).                                              % ignorar computar como siempre
generarOp(BS, CS, escribir(B, C)) :- member(B, BS), member(C, CS).      % como es una escritura me fijos si esrtan en la lista de buffers y lo mismo con los contenidos
generarOp(BS, _, leer(B)) :- member(B, BS).                             % si es lectura chequeo que el buffer este en la lista de buffers

  %% 8.1. Analizar la reversibilidad de XS, justificando adecuadamente por qué el predicado se comporta como
  %% lo hace.

  %%El predicado es completamente reversible ya que puede no tener ninguna variable instanciada e igualmente genera resultados,
  %% esto lo podemos ver ya que puede resolver cualquier combinacion de valores,  instanciados o no y cubre todas las combinaciones
  %% ninguna variable depende de otra ni tampoco el orden de evaluacion afecta entre las variables, por lo que tampoco depende su instanciación de las otras.


%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

% Se espera que completen con las subsecciones de tests que crean necesarias, más allá de las puestas en estos ejemplos

cantidadTestsBasicos(10). % Actualizar con la cantidad de tests que entreguen
% Ejercicio 1
testBasico(1) :- proceso(computar).
testBasico(2) :- proceso(secuencia(escribir(1,pepe),escribir(2,pipo))).
testBasico(3) :- proceso(paralelo(escribir(1,hola),leer(2))).
testBasico(4) :- proceso(secuencia(paralelo(computar,leer(1)),escribir(2,fin))).
testBasico(5) :- not(proceso(invalido)).
% Ejerccio 2
testBasico(6) :- buffersUsados(escribir(1, hola), [1]).
testBasico(7) :- buffersUsados(paralelo(escribir(1,hola),escribir(2,chau)), [1,2]).
testBasico(8) :- buffersUsados(secuencia(escribir(1,hola),escribir(1,chau)), [1]).
testBasico(9) :- buffersUsados(paralelo(secuencia(escribir(1,hola),leer(2)),escribir(2,chau)), [1,2]).
testBasico(10) :- buffersUsados(secuencia(paralelo(escribir(1,a),escribir(2,b)),leer(1)), [1,2]).

% Agregar más tests
cantidadTestsProcesos(8). % Actualizar con la cantidad de tests que entreguen
% Ejercicio 3
testProcesos(1) :- intercalar([1,2],[3,4],TS), member(TS,[[1,2,3,4], [1,3,2,4], [1,3,4,2], [3,1,2,4], [3,1,4,2], [3,4,1,2]]).
testProcesos(2) :- intercalar([],[1,2],[1,2]).
testProcesos(3) :- intercalar([1,2],[3,4,5],TS), member(TS,[[1,2,3,4,5], [1,3,2,4,5], [1,3,4,2,5], [1,3,4,5,2], [3,1,2,4,5], [3,1,4,2,5], [3,1,4,5,2], [3,4,1,2,5], [3,4,1,5,2], [3,4,5,1,2]]).
% Ejercicio 4
testProcesos(4) :- serializar(computar, [computar]).
testProcesos(5) :- serializar(secuencia(escribir(1,a),leer(1)), [escribir(1,a),leer(1)]).
testProcesos(6) :- serializar(paralelo(escribir(1,a),escribir(2,b)),TS), member(TS,[[escribir(1,a),escribir(2,b)],[escribir(2,b),escribir(1,a)]]).
testProcesos(7) :- serializar(secuencia(paralelo(escribir(1,a),escribir(2,b)),leer(1)),TS), member(TS,[[escribir(1,a),escribir(2,b),leer(1)],[escribir(2,b),escribir(1,a),leer(1)]]).
testProcesos(8) :- serializar(paralelo(leer(1),leer(2)),TS), member(TS,[[leer(1),leer(2)],[leer(2),leer(1)]]).

% Agregar más tests

cantidadTestsBuffers(10). % Actualizar con la cantidad de tests que entreguen
% Ejercicio 5
testBuffers(1) :- contenidoBuffer(1,[escribir(1,hola),escribir(1,mundo)],[hola,mundo]).
testBuffers(2) :- contenidoBuffer(1,[escribir(1,hola),leer(1)],[]).
testBuffers(3) :- contenidoBuffer(2,[escribir(1,hola),escribir(2,mundo)],[mundo]).
testBuffers(4) :- contenidoBuffer(1,secuencia(escribir(1,hola),leer(1)),[]).
testBuffers(5) :- contenidoBuffer(1,paralelo(escribir(1,hola),escribir(1,mundo)),TS), member(TS,[[hola,mundo],[mundo,hola]]).
% Ejercicio 6
testBuffers(6) :- contenidoLeido([escribir(1,hola),leer(1)],[hola]).
testBuffers(7) :- contenidoLeido(paralelo(secuencia(escribir(2,sol),leer(2)), secuencia(escribir(1,agua),leer(1))), TS), member(TS, [[sol,agua], [sol,agua], [agua,sol], [sol,agua], [agua,sol], [agua,sol]]).
testBuffers(8) :- contenidoLeido(secuencia(escribir(1,hola),leer(1)),[hola]).
testBuffers(9) :- contenidoLeido(paralelo(secuencia(escribir(1,a),leer(1)), secuencia(escribir(2,b),leer(2))),TS), member(TS,[[a,b],[b,a]]).
testBuffers(10) :- contenidoLeido(paralelo(escribir(1,a), secuencia(escribir(1,b),leer(1))),TS), member(TS,[[a],[b]]).

% Agregar más tests

cantidadTestsSeguros(6). % Actualizar con la cantidad de tests que entreguen
% Ejercicio 7
testSeguros(1) :- esSeguro(secuencia(escribir(1,hola),leer(1))).
testSeguros(2) :- not(esSeguro(secuencia(leer(1),escribir(1,hola)))).
testSeguros(3) :- esSeguro(paralelo(escribir(1,hola),escribir(2,mundo))).
testSeguros(4) :- not(esSeguro(paralelo(escribir(1,hola),leer(1)))).
testSeguros(5) :- not(esSeguro(secuencia(escribir(1, hola), leer(2)))).
testSeguros(6) :- esSeguro(secuencia(paralelo(escribir(1,a),escribir(2,b)), paralelo(leer(1),leer(2)))).
% Ejercicio 8
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
