%herramientasRequeridas(Tarea,Requeridas).
%aspiradora(PotenciaMinimaRequerida).
herramientasRequeridas(ordenarCuarto, [aspiradora(100), trapeador, plumero]).
herramientasRequeridas(ordenarCuarto, [escoba, trapeador, plumero]).
herramientasRequeridas(limpiarTecho, [escoba, pala]).
herramientasRequeridas(cortarPasto, [bordedadora]).
herramientasRequeridas(limpiarBanio, [sopapa, trapeador]).
herramientasRequeridas(encerarPisos, [lustradpesora, cera, aspiradora(300)]).
herramientasRequeridas(serUnaG, [aspiradora(50), trapeador]).

% ****** Punto 1 ******

%dustbuster(Cazador).
dustbuster(egon).
dustbuster(peter).
dustbuster(ray).
dustbuster(winston).

%tieneHerramienta(Cazador,Herramienta que posee).
tieneHerramienta(egon,aspiradora(400)).
tieneHerramienta(egon,trapeador).
tieneHerramienta(peter,trapeador).
tieneHerramienta(winston,varitaDeNeutrones).

% ****** Punto 2 ******

tieneHerramientaRequerida(Cazador,Herramienta):-
    tieneHerramienta(Cazador,Herramienta).

tieneHerramientaRequerida(Cazador,aspiradora(Requerida)):-
    tieneHerramienta(Cazador,aspiradora(DeCazador)),
%   DeCazador >= Requerida.           <----      NO ES INVERSIBLE
    between(0, DeCazador, Requerida).% <----      SI ES INVERSIBLE

% ****** Punto 3 ******
puedeRealizar(Cazador,Tarea):-
    tieneHerramienta(Cazador,varitaDeNeutrones),
    herramientasRequeridas(Tarea,_).
/*
puedeRealizar(Cazador,Tarea):-
    herramientasRequeridas(Tarea,Herramientas),
    tieneTodasLasHerramientas(Cazador,Herramientas).

tieneTodasLasHerramientas(Cazador,[PrimeraHerramienta|Cola]):-
    tieneHerramientaRequerida(Cazador,PrimeraHerramienta),
    tieneTodasLasHerramientas(Cazador,Cola).
tieneTodasLasHerramientas(Cazador,[]):-
    tieneHerramienta(Cazador,_).
*/
puedeHacerTarea(Persona, Tarea):-
	tieneHerramienta(Persona, _),
	requiereHerramienta(Tarea, _),
	forall(requiereHerramienta(Tarea, Herramienta), tieneHerramientaRequerida(Persona, Herramienta)).

requiereHerramienta(Tarea, Herramienta):- % <--- EN VEZ DE RECORRER LA LISTA, ESTO INDICA TODOS LOS MIEMBROS DE LA LISTA.
	herramientasRequeridas(Tarea, ListaDeHerramientas),
	member(Herramienta, ListaDeHerramientas).

% ****** Punto 4 ******

%tareaPedida(Cliente,Tarea,MetrosCuadrados).
tareaPedida(frenchie,serUnaG,5).
tareaPedida(frenchie,serUnaG,7).
tareaPedida(frenchie,serUnaG,2).

%precio(Tarea,PrecioPorMetroCuadrado).
precio(serUnaG,25).
precio(cortarPasto,10).

cuantoCobrar(Cliente,Precio):-
    tareaPedida(Cliente,_,_),
    findall(Precio,precioPorTarea(Cliente,_,Precio),ListaDePrecios),
    sumlist(ListaDePrecios,Precio).

precioPorTarea(Cliente,Tarea,Precio):-
    tareaPedida(Cliente,Tarea,MetrosCuadrados),
    precio(Tarea,PrecioPorMetroCuadrado),
    Precio is PrecioPorMetroCuadrado*MetrosCuadrados.

% ****** Punto 5 ******

aceptaElPedido(Cazador,Cliente):-

/*  tareaPedida(Cliente, _, _),
	tieneHerramienta(Cazador, _),
    forall(tareaPedida(Cliente,Tarea,_),puedeRealizar(Cazador,Tarea)),
*/
    puedeHacerPedido(Cazador,Cliente),
    aceptaTarea(Cazador,Cliente).

puedeHacerPedido(Cazador,Cliente):-
    tareaPedida(Cliente, _, _),
	tieneHerramienta(Cazador, _),
    forall(tareaPedida(Cliente,Tarea,_),puedeRealizar(Cazador,Tarea)).

aceptaTarea(ray,Cliente):-
    tareaPedida(Cliente,_,_),
    not(tareaPedida(Cliente,limpiarTecho,_)).
aceptaTarea(winston,Cliente):-
    cuantoCobrar(Cliente,Precio),
    Precio > 500.
aceptaTarea(egon,Cliente):-
    forall(tareaPedida(Cliente,Tarea,_), not(esTareaCompleja(Tarea))).
/*  herramientasRequeridas(Tarea,Herramientas),
    length(Herramientas,Cantidad),
    Cantidad =< 2.
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Se puede delegar. */
aceptaTarea(winston,Cliente):-
    tareaPedida(Cliente,_,_).

esTareaCompleja(Tarea):-
    herramientasRequeridas(Tarea,Herramientas),
    length(Herramientas,Cantidad),
    Cantidad > 2.

% ****** Punto 6 ******
/*
a)
herramientasRequeridas(ordenarCuarto, [aspiradora(100), trapeador, plumero]).
herramientasRequeridas(ordenarCuarto, [escoba, trapeador, plumero]).

b) No hay nada que modificar, todo funciona de igual forma.

c) Esto se debe al backtracking de Prolog, que permite que por si solo evalúe todas las posiblidades, al encontrarse en un camino falso, retrocede y prueba con otra clausula hasta que de verdadero (si es que es posible).
*/

esLinda(guadi).