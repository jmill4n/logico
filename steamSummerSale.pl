%juego(genero,Precio).
%juego(accion(NombreDelJuego),Precio).
%juego(mmorpg(NombreDelJuego,CantidadDeUsuarios),Precio).
%juego(puzzle(NombreDelJuego,CantidadDeNiveles,Dificultad),Precio).
juego(accion(callOfDuty),5).
juego(accion(batmanAA),10).
juego(mmorpg(wow,5000000),30).
juego(mmorpg(lineage2,6000000),15).
juego(puzzle(plantsVsZombies,40,media),10).
juego(puzzle(tetris,10,facil),0).

%oferta(NombreDelJuego,PorcentajeDeDescuento).
oferta(callOfDuty,10).
oferta(plantsVsZombies,50).

%usuario(Usuario,NombresJuegosPosee,TipoDeAdquisicion).
%usuario(Usuario,NombresJuegosPosee,compra(Juego)).
%usuario(Usuario,NombresJuegosPosee,regalo(Juego,OtroUsuario)).
usuario(nico,[batmanAA,plantsVsZombies,tetris],[compra(lineage2)]).
usuario(fede,[],[regalo(callOfDuty,nico),regalo(wow,nico)]).
usuario(rasta,[lineage2],[]).
usuario(agus,[],[]).
usuario(felipe,[plantsVsZombies],[compra(tetris)]).

% TOTALMENTE INVERSIBLES

% ****** Punto 1 ******

cuantoSale(Juego,Precio):-
    existeJuego(Juego),
    oferta(Juego,Descuento),
    precioJuego(Juego,PrecioOriginal),
    Precio is PrecioOriginal-((PrecioOriginal*Descuento)/100).

cuantoSale(Juego,Precio):-
    existeJuego(Juego),
    not(oferta(Juego,_)),
    precioJuego(Juego,Precio).

existeJuego(Juego):-
    esDe(Juego,accion).
existeJuego(Juego):-
    esDe(Juego,mmorpg).
existeJuego(Juego):-
    esDe(Juego,puzzle).

esDe(Juego,accion):-
    juego(accion(Juego),_).
esDe(Juego,mmorpg):-
    juego(mmorpg(Juego,_),_).
esDe(Juego,puzzle):-
    juego(puzzle(Juego,_,_),_).

precioJuego(Juego,Precio):-
    juego(accion(Juego),Precio).
precioJuego(Juego,Precio):-
    juego(mmorpg(Juego,_),Precio).
precioJuego(Juego,Precio):-
    juego(puzzle(Juego,_,_),Precio).

% ****** Punto 2 ******

juegoPopular(Juego):-
    esDe(Juego,accion).
juegoPopular(Juego):-
    juego(mmorpg(Juego,Usuarios),_),
    Usuarios > 1000000.
juegoPopular(Juego):-
    juego(puzzle(Juego,25,_),_).
juegoPopular(Juego):-
    juego(puzzle(Juego,_,facil),_).

% ****** Punto 3 ******

tieneUnBuenDescuento(Juego):-
    oferta(Juego,Descuento),
    Descuento > 50.

% ****** Punto 4 ******

adictoALosDescuentos(Usuario):-
    adquisiciones(Usuario,Adquisiciones),
    forall(vaAAdquirir(Juego,Adquisiciones),tieneUnBuenDescuento(Juego)).

adquisiciones(Usuario,Adquisiciones):-
    usuario(Usuario,_,Adquisiciones),
    Adquisiciones \= [].

vaAAdquirir(Juego,Adquisiciones):-
    member(compra(Juego),Adquisiciones).
vaAAdquirir(Juego,Adquisiciones):-
    member(regalo(Juego,_),Adquisiciones).

% ****** Punto 5 ******

fanaticoDe(Usuario,Genero):-
    bibliotecaDe(Usuario,Biblioteca),
    alMenos2JuegosDe(Biblioteca,Genero).

alMenos2JuegosDe(Biblioteca,Genero):-
    member(Juego,Biblioteca),
    member(OtroJuego,Biblioteca),
    Juego \= OtroJuego,
    esDe(Juego,Genero),
    esDe(OtroJuego,Genero).

% ****** Punto 6 ******

monotematico(Usuario,Genero):-
    bibliotecaDe(Usuario,Biblioteca),
    todosJuegosDe(Biblioteca,Genero).

todosJuegosDe(Biblioteca,Genero):-
    forall(member(Juego,Biblioteca),esDe(Juego,Genero)).

bibliotecaDe(Usuario,Biblioteca):- 
    usuario(Usuario,Biblioteca,_),
    Biblioteca \= [].

% ****** Punto 7 ******

buenosAmigos(Usuario,OtroUsuario):-
    regalaJuegoPopular(Usuario,OtroUsuario),
    regalaJuegoPopular(OtroUsuario,Usuario).

regalaA(Usuario,OtroUsuario,Juego):-
    usuario(Usuario,_,Adquisiciones),
    member(regalo(Juego,OtroUsuario),Adquisiciones).

regalaJuegoPopular(Usuario,OtroUsuario):-
    regalaA(Usuario,OtroUsuario,Juego),
    juegoPopular(Juego).

% ****** Punto 8 ******

cuantoGastara(Usuario,Dinero):-
    adquisiciones(Usuario,Adquisiciones),
    findall(Precio,(precioPorAdquisicion(Adquisiciones,Precio)),Precios),
    sumlist(Precios,Dinero).

precioPorAdquisicion(Adquisiciones,Precio):-
    vaAAdquirir(Juego,Adquisiciones),
    cuantoSale(Juego,Precio).