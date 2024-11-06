# Trabajo Práctico 2: Programación Lógica - Paradigmas de Lenguajes de Programación

## Introducción

Este repositorio contiene la resolución del Trabajo Práctico 2 para la materia Paradigmas de Lenguajes de Programación del 2do cuatrimestre de 2024. El objetivo de este trabajo práctico es modelar el funcionamiento de ciertos procesos que escriben en buffers, utilizando el lenguaje de programación lógica Prolog.

## Representación Utilizada

### Acciones Básicas

- `computar`: Acción que no realiza cambios en ningún buffer.
- `escribir(B, E)`: Acción que escribe el elemento `E` en el buffer `B`.
  - Ejemplo: `escribir(b1, 'casa')`.
- `leer(B)`: Acción que lee (y elimina) el primer elemento del buffer `B`.
  - Ejemplo: `leer(b1)`.

### Procesos

- `secuencia(P, Q)`: Proceso que ejecuta los subprocesos `P` y `Q` en secuencia.
  - Ejemplo: `secuencia(escribir(a8, hola), escribir(a8, chau))`.
- `paralelo(P, Q)`: Proceso que ejecuta los subprocesos `P` y `Q` en paralelo, intercalando sus acciones.
  - Ejemplo: `paralelo(secuencia(escribir(1, 'hola'), escribir(1, 'chau')), secuencia(escribir(2, 'hallo'), escribir(2, 'tchüss')))`.

## Ejercicios

### Predicados Básicos

1. **proceso(+P)**
   - Identifica si un término es un proceso.
   - Ejemplo:
     ```prolog
     ?- proceso(computar).
     true.
     ```

2. **buffersUsados(+P, -BS)**
   - Instancia en `BS` los buffers utilizados por el proceso `P`, en una lista ordenada y sin repetidos.
   - Ejemplo:
     ```prolog
     ?- buffersUsados(escribir(1, hola), BS).
     BS = [1] ;
     false.
     ```

### Organización de Procesos

3. **intercalar(+XS, +YS, ?ZS)**
   - Intercala dos listas en una tercera, de todas las maneras posibles, manteniendo el orden relativo de los elementos.
   - Ejemplo:
     ```prolog
     ?- intercalar([1, 2, 3], [4, 5, 6], I).
     I = [1, 2, 3, 4, 5, 6] ;
     I = [4, 5, 6, 1, 2, 3] ;
     I = [1, 4, 2, 5, 3, 6] ;
     ...
     false.
     ```

4. **serializar(+P, ?XS)**
   - Instancia en `XS` una lista de acciones básicas que puede realizar el proceso en cualquiera de sus posibles ejecuciones.
   - Ejemplo:
     ```prolog
     ?- serializar(secuencia(computar, leer(2)), XS).
     XS = [computar, leer(2)] ;
     false.
     ```

### Contenido de los Buffers

5. **contenidoBuffer(+B, +ProcesoOLista, ?L)**
   - Instancia en `L` el contenido del buffer `B` después de realizar las acciones indicadas por un proceso o una serialización.
   - Ejemplo:
     ```prolog
     ?- contenidoBuffer(1, [escribir(1, pa), escribir(2, ma), escribir(1, hola), computar, escribir(1, mundo), leer(1)], C).
     C = [hola, mundo] ;
     false.
     ```

6. **contenidoLeido(+ProcesoOLista, ?Contenidos)**
   - Instancia en `Contenidos` la lista de contenidos que se van leyendo, en el orden en que se leen.
   - Ejemplo:
     ```prolog
     ?- contenidoLeido(paralelo(secuencia(escribir(2, sol), leer(2)), secuencia(escribir(1, agua), leer(1))), CS).
     CS = [sol, agua] ;
     CS = [sol, agua] ;
     CS = [agua, sol] ;
     ...
     false.
     ```

### Secuencias y Procesos Seguros

7. **esSeguro(+P)**
   - Determina si un proceso es seguro.
   - Ejemplo:
     ```prolog
     ?- esSeguro(secuencia(leer(1), escribir(1, agua))).
     false.
     ```

8. **ejecucionSegura(-XS, +BS, +CS)**
   - Instancia en `XS` todas las posibles ejecuciones seguras con buffers de `BS` y contenidos de `CS` utilizando Generate & Test.
   - Ejemplo:
     ```prolog
     ?- ejecucionSegura(L, [1, 2], [a, b]).
     L = [] ;
     L = [computar] ;
     L = [escribir(1, a)] ;
     ...
     false.
     ```

## Pautas de Entrega

- La elaboración de este trabajo debe ser 100% de los estudiantes.
- Cada predicado debe contar con ejemplos (tests) que muestren su funcionalidad.
- Subir el código fuente a la tarea respectiva en el Campus.
- El código debe poder ser ejecutado en SWI-Prolog.
- Asegurarse de que el código esté adecuadamente comentado.

## Referencias y Sugerencias

- Utilizar los predicados ISO y los de SWI-Prolog ya disponibles.
- Consultar la bibliografía y documentación online de SWI-Prolog: [Documentación SWI-Prolog](http://www.swi-prolog.org/pldoc/doc_for?object=manual).

