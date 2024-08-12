%accion(NombreDelJuego).
%mmorpg(NombreDelJuego, CantidadDeUsuarios).
%puzzle(NombreDelJuego, CantidadDeNiveles, Dificultad).

%juego(Juego,Precio).
juego(accion(callOfDuty),5).
juego(accion(batmanAA),10).
juego(mmorpg(wow,5000000),30).
juego(mmorpg(lineage2,6000000),15).
juego(puzzle(plantsVsZombies,40,media),10).
juego(puzzle(tetris,10,facil),0).

oferta(callOfDuty,10).
oferta(plantsVsZombies,51).

usuario(nico,[batmanAA,plantsVsZombies,tetris],[compra(lineage2),regalo(lineage2,fede)]).
usuario(fede,[],[regalo(callOfDuty,nico),regalo(wow,nico)]).
usuario(rasta,[lineage2],[compra(plantsVsZombies)]).
usuario(agus,[],[]).
usuario(felipe,[plantsVsZombies],[compra(tetris)]).

% **************************************************

cuantoSale(Juego,Valor):-
    juego(Juego,Valor),
    nombreDelJuego(Juego,Nombre),
    not(oferta(Nombre,_)).
cuantoSale(Juego,Valor):-
    juego(Juego,Precio),
    nombreDelJuego(Juego,Nombre),
    oferta(Nombre,Descuento),
    Valor is Precio - (Precio*Descuento/100).

existeJuego(Juego):-
    juego(Juego,_).

nombreDelJuego(accion(Nombre),Nombre):-
    existeJuego(accion(Nombre)).
nombreDelJuego(mmorpg(Nombre,Usuarios),Nombre):-
    existeJuego(mmorpg(Nombre,Usuarios)).
nombreDelJuego(puzzle(Nombre,CantidadDeNiveles,Dificultad),Nombre):-
    existeJuego(puzzle(Nombre,CantidadDeNiveles,Dificultad)).

% **************************************************

juegoPopular(accion(Nombre)):-
    existeJuego(accion(Nombre)).
juegoPopular(mmorpg(Nombre,Usuarios)):-
    existeJuego(mmorpg(Nombre,Usuarios)),
    Usuarios > 1000000.
% aca pregunto si hace falta que le transfiera los niveles o lo dejo
% como variable anonima
juegoPopular(puzzle(Nombre,CantidadDeNiveles,facil)):-
    existeJuego(puzzle(Nombre,CantidadDeNiveles,facil)).
juegoPopular(puzzle(Nombre,25,Dificultad)):-
    existeJuego(puzzle(Nombre,25,Dificultad)).

% **************************************************

tieneUnBuenDescuento(Juego):-
    existeJuego(Juego),
    nombreDelJuego(Juego,Nombre),
    oferta(Nombre,Descuento),
    Descuento > 50.

% **************************************************

adictoALosDescuentos(Usuario):-
    adquisicionesDe(Usuario,Adquisiciones),
    forall(juegoAAdquirir(Adquisiciones,Juego),tieneUnBuenDescuento(Juego)).

adquisicionesDe(Usuario,Adquisiciones):-
    usuario(Usuario,_,Adquisiciones),
    Adquisiciones \= [].

juegoAAdquirir(Adquisiciones,Juego):-
    member(compra(Nombre),Adquisiciones),
    nombreDelJuego(Juego,Nombre).
juegoAAdquirir(Adquisiciones,Juego):-
    member(regalo(Nombre,_),Adquisiciones),
    nombreDelJuego(Juego,Nombre).

% **************************************************

fanaticoDe(Usuario,Genero):-
    bibliotecaDe(Usuario,Biblioteca),
    juegosDe(UnJuego,Biblioteca),
    juegosDe(OtroJuego,Biblioteca),
    UnJuego \= OtroJuego,
    esDe(UnJuego,Genero),
    esDe(OtroJuego,Genero).

bibliotecaDe(Usuario,Biblioteca):-
    usuario(Usuario,Biblioteca,_),
    Biblioteca \= [].

juegosDe(Juego,Biblioteca):-
    member(Nombre,Biblioteca),
    nombreDelJuego(Juego,Nombre).

esDe(accion(Nombre),accion):-
    existeJuego(accion(Nombre)).
esDe(mmorpg(Nombre,Usuarios),mmorpg):-
    existeJuego(mmorpg(Nombre,Usuarios)).
esDe(puzzle(Nombre,CantidadDeNiveles,Dificultad),puzzle):-
    existeJuego(puzzle(Nombre,CantidadDeNiveles,Dificultad)).

% **************************************************

monotematico(Usuario,Genero):-
    bibliotecaDe(Usuario,Biblioteca),
    todosLosJuegosDe(Biblioteca,Genero).

todosLosJuegosDe(Biblioteca,Genero):-
    esDe(_,Genero),
    forall(juegosDe(Juego,Biblioteca),esDe(Juego,Genero)).

% **************************************************

buenosAmigos(UnUsuario,OtroUsuario):-
    regalaJuegoPopular(UnUsuario,OtroUsuario),
    regalaJuegoPopular(OtroUsuario,UnUsuario).

regalaA(Juego,UnUsuario,OtroUsuario):-
    adquisicionesDe(UnUsuario,UnasAdquisiciones),
    member(regalo(Nombre,OtroUsuario),UnasAdquisiciones),
    nombreDelJuego(Juego,Nombre).

regalaJuegoPopular(UnUsuario,OtroUsuario):-
    regalaA(Juego,UnUsuario,OtroUsuario),
    juegoPopular(Juego).

% **************************************************

cuantoGastara(Usuario,Dinero):-
    adquisicionesDe(Usuario,Adquisiciones),
    findall(Precio,(juegoAAdquirir(Adquisiciones,Juego),cuantoSale(Juego,Precio)),ListaDePrecios),
    sumlist(ListaDePrecios,Dinero).