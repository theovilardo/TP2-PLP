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
%% contenidoLeido(+ProcesoOLista,?Contenidos)
contenidoLeido(POL, CS) :- proceso(POL), serializar(POL, LPS), obtenerLeidos(LPS, [], [], CS). %si es proceso, se serializa
contenidoLeido(POL, CS) :- obtenerLeidos(POL, [], [], CS). % si no es proceso es lista y sigue

% obtenerLeidos(+ListaProcesos, +Buffers, +Leidos, ?ContenidosSalida)
% BS = Buffers ---> [(BufferID, Escritura)]
% BID: Banco interamericano de Desarrollo... mentira es BufferID, 
obtenerLeidos([], _, L, L).                                                                                                   % caso base (termina de dvolver los leidos post recursion)
obtenerLeidos([computar|LPS], BS, L, CS) :- obtenerLeidos(LPS, BS, L, CS).                                                    % ignora computar
obtenerLeidos([escribir(BID, E)|LPS], BS, L, CS) :- agregarBuffer(BID, E, BS, NB), obtenerLeidos(LPS, NB, L, CS).             % si es una escritura agrego el escrito al contenido asociando con su buffer respectivo
obtenerLeidos([leer(BID)|LPS], BS, L, CS) :- leerBuffer(BID, BS, E, NB), append(L, [E], NL), obtenerLeidos(LPS, NB, NL, CS).  % si es una lectura, se lee el contenido con leerBuffer y appendeo a los leidos dando los nuevos leidos y despues se llama a recursion de obtenerLeidos con los nuevos leidos

% agregarBuffer(+BufferID, +Escritura, +BufferReg, -NuevosBuffers)
% si pido BufferContenido = (ID, Escritura) ---> IDs de los buffers con esa Escritura
% XBC: XBufferContenido, CE: ContenidoEscrito, NCE: NuevoContenidoEscrito, REG: Registro (contnido que sigue), NR: NuevoRegistro
agregarBuffer(BID, E, [(BID,CE)|REG], [(BID,NCE)|REG]) :- append(CE, [E], NCE).                           % si el ID del buffer coincide con la escritura, se agrega al contenido de ese buffer
agregarBuffer(BID, E, [XBC|REG], [XBC|NR]) :- XBC = (XID,_), BID \= XID, agregarBuffer(BID, E, REG, NR).  % si el ID del buffer no coincide (no unifica) con el ID de la escritura no modifica el contenido
agregarBuffer(BID, E, [], [(BID,[E])]).                                                                   % si el REG esta vacio es que todavia no fue escrito y se inserta el buffer con su ID y Contenido (no esoty seguro si deberia ir primero)

% leerBuffer(+BufferID, +Buffers, -Escritura, -NuevosBuffers)
leerBuffer(BID, [(BID,[E|REG])|BS], E, [(BID,REG)|BS]).                                               % si el ID del buffer del registro coincide con el de la lectura, se desencola un elemento dle contenido de ese buffer
leerBuffer(BID, [XBC|REG], E, [XBC|NB]) :- XBC = (XID,_), BID \= XID, leerBuffer(BID, REG, E, NB).    % si el ID no coincide, no modifico el contenido


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
ejecucionSegura(XS,BS,CS) :- generarEjecuciones(XS,BS,CS), esSeguro(XS).


%aux:
generarEjecuciones([], [], _).
generarEjecuciones([], _, []).
generarEjecuciones([O|OS], [B|BF], [C|CS]) :-  generarOp(B, C, O), generarEjecuciones(OS, BF, CS). 

generarOp(_, _, computar).
generarOp(B, _, leer(B)).
generarOp(B, C, escribir(B, C)).

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
