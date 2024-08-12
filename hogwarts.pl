%sombreroSeleccionador(caracter,prefiere,statusSangre,casa).

%magos(statusSangre,odiariaCasa)

mago(harry,mestiza,[corajudo,amistoso,orgullo,inteligencia]).
odiariaCasa(harry,slytherin).

casa(gryffindor).
casa(slytherin).
casa(ravenclaw).

condicionCasa(gryffindor,coraje).
condicionCasa(slytherin,orgullo).
condicionCasa(slytherin,inteligencia).
condicionCasa(ravenclaw,inteligencia).
condicionCasa(ravenclaw,responsable).
condicionCasa(hufflepuff,amistoso).

mago(Mago):-
    sangre(Mago,_).
mago(ron).
/*mago(harry).
mago(draco).
mago(hermione).*/

sangre(harry,mestiza).
sangre(draco,pura).
sangre(hermione,impura).

%No hay ningun motivo para pensar en las caracteristicas todas juntas, pueden ir separadas (incluso se recomienda).
caracteristicas(harry,[coraje,amistoso,orgullo,inteligencia]).
caracteristicas(draco,[inteligencia,orgullo]).
caracteristicas(hermione,[inteligencia,orgullo,responsable,amistoso]).
%tieneCaracteristica(harry,coraje).
%tieneCaracteristica(harry,amistoso).

odiariaEntrar(harry,slytherin).
odiariaEntrar(draco,hufflepuff).

% PUNTO 1

permiteEntrar(slytherin,Mago):-
    sangre(Mago,Sangre),
    Sangre \= impura.
/*permiteEntrar(slytherin,Mago):-
    mago(Mago),
    not(sangre(Mago,impura)).*/
permiteEntrar(Casa,Mago):-
    casa(Casa),
    mago(Mago),
    Casa \= slytherin.
/*
permiteEntrar(not(slytherin),Mago):-
    mago(Mago).

permiteEntrar(gryffindor,Mago):-
    mago(Mago).
permiteEntrar(ravenclaw,Mago):-
    mago(Mago).
permiteEntrar(hufflepuff,Mago):-
    mago(Mago).*/

% PUNTO 2

/*
tieneCaracterApropiado(Mago,Casa):-
    casa(Casa),
    caracteristicas(Mago,Caracteristicas),
    forall(condicionCasa(Casa,Condicion),member(Condicion,Caracteristicas)).
*/

%Esta solucion delega y es mucho mas declarativa, mas como el enunciado.
tieneCaracterApropiado(Mago,Casa):-
    mago(Mago),
    casa(Casa),
    forall(condicionCasa(Casa,Condicion),tieneCaracteristica(Mago,Condicion)).

tieneCaracteristica(Mago,Caracteristica):-
    caracteristicas(Mago,Caracteristicas),
    member(Caracteristica,Caracteristicas).

% PUNTO 3

quedaSeleccionado(Mago,Casa):-
    tieneCaracterApropiado(Mago,Casa),
    permiteEntrar(Casa,Mago),
    not(odiariaEntrar(Mago,Casa)).
quedaSeleccionado(hermione,gryffindor).

% PUNTO 5 
cadenaDeAmistades([Cabeza,Segundo|Cola]):- % lista de magos
    tieneCaracteristica(Cabeza,amistoso), %por cada mago
    quedaSeleccionado(Cabeza,Casa),
    quedaSeleccionado(Segundo,Casa),
    cadenaDeAmistades([Segundo|Cola]).
cadenaDeAmistades([Mago]):-
    tieneCaracteristica(Mago,amistoso).
cadenaDeAmistades([]).


%% PARTE 2
/*
%malaAccion(Mago,Donde fue).
malaAccion(harry,fueraDeCama).
malaAccion(harry,bosque).
malaAccion(harry,tercerPiso).
malaAccion(hermione,tercerPiso).
malaAccion(hermione,seccionRestringida).

lugarProhibido(fueraDeCama,50).
lugarProhibido(bosque,50).
lugarProhibido(seccionRestringida,10).
lugarProhibido(tercerPiso,75).

%buenaAccion(Mago,Puntaje a favor).
buenaAccion(ron,50).
buenaAccion(hermione,50).
buenaAccion(harry,60).

accion(mala,harry,fueraDeCama).
accion(mala,harry,bosque).
accion(mala,harry,tercerPiso).
accion(mala,hermione,tercerPiso).
accion(mala,hermione,seccionRestringida).
accion(buena,ron,50).
accion(buena,hermione,50).
accion(buena,harry,60).

%esDe(Mago,Su Casa Seleccionada).
esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

esBuenAlumno(Mago):-
    accion(_,Mago,_),
    not(accion(mala,Mago,_)).

 esBuenAlumno(Mago):-
    existeAccion(Mago),
    not(malaAccion(Mago,_)).

existeAccion(Mago):-
    buenaAccion(Mago,_).
existeAccion(Mago):-
    malaAccion(Mago,_).

*/
%% Mejor solución.
hizo(harry,   fueraDeCama).
hizo(harry,   irA(bosque)).
hizo(harry,   irA(tercerPiso)).
hizo(hermione,irA(tercerPiso)).
hizo(hermione,irA(seccionRestringida)).
hizo(draco,   irA(mazmorra)).
hizo(ron,     buenaAccion(50,ganarAlAjedrezMagico)).
hizo(hermione,buenaAccion(50,salvarASusAmigos)).
hizo(harry,   buenaAccion(60,ganarleAVoldermort)).
hizo(cedric,  buenaAccion(100,ganarAlQuiddich)).

hizo(hermione,respondio(20,snape,dondeSeEncuentraUnBozar)).
hizo(hermione,respondio(25,flitwick,comoHacerLevitarUnaPluma)).


esBuenAlumno(Mago):-
    hizo(Mago,_),
    not(hizoUnaMalaAccion(Mago)).

hizoUnaMalaAccion(Mago):-
    hizo(Mago,Accion),
    puntajeQueGenera(Accion,Puntaje),
    Puntaje < 0.

lugarProhibido(bosque,-50).
lugarProhibido(seccionRestringida,-10).
lugarProhibido(tercerPiso,-75).

puntajeQueGenera(fueraDeCama,-50).
puntajeQueGenera(irA(Lugar),Puntaje):-
    lugarProhibido(Lugar, Puntaje).
puntajeQueGenera(buenaAccion(Puntaje,_),Puntaje).

puntajeQueGenera(respondio(Dificultad,snape,_),Puntaje):-
    Puntaje is Dificultad/2.
puntajeQueGenera(respondio(Dificultad,Profesor,_),Dificultad):-
%   profesor(Profesor).
    Profesor \= snape.

esRecurrente(Accion):-
    hizo(Mago,Accion),
    hizo(OtroMago,Accion),
    Mago \= OtroMago.

% Punto 2

esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).
esDe(cedric,hufflepuff).

/*puntajeDe(Mago,Puntos):-
    esDe(Mago,_),
    findall(Punto,(hizo(Mago,Accion),puntajeQueGenera(Accion,Punto)),ListaPuntos),
    sumlist(ListaPuntos,Puntos).

puntajeTotalCasa(Casa,Puntaje):-
    esDe(_,Casa),
    findall(Puntos,(esDe(Mago,Casa),puntajeDe(Mago,Puntos)),ListaPuntos),
    sumlist(ListaPuntos,Puntaje).*/

%Solucion de los videos:
puntajeTotalCasa(Casa,Puntaje):-
    esDe(_,Casa),
    findall(Puntos,(esDe(Mago,Casa),puntosQueObtuvo(Mago,_,Puntos)),ListaPuntos),
    sumlist(ListaPuntos,Puntaje).

puntosQueObtuvo(Mago,Accion,Puntos):-
    hizo(Mago,Accion),
    puntajeQueGenera(Accion,Puntos).

% Punto 3

casaGanadora(Casa):-
    esDe(_,Casa),
    puntajeTotalCasa(Casa,PuntajeCasaOriginal),
    forall(esDe(_,OtraCasa),(puntajeTotalCasa(OtraCasa,PuntajeOtraCasa),PuntajeCasaOriginal >= PuntajeOtraCasa)).
%Solucion de los videos:
/*casaGanadora(Casa):-
    puntajeTotalCasa(Casa,PuntajeCasaOriginal),
    forall((puntajeTotalCasa(OtraCasa,PuntajeOtraCasa),Casa \= OtraCasa), PuntajeCasaOriginal > PuntajeOtraCasa).
*/

% Punto 4
/*
%hizo(Mago,respondio(dificultad,profesor,pregunta)).
hizo(hermione,respondio(20,snape,dondeSeEncuentraUnBozar)).
hizo(hermione,respondio(25,flitwick,comoHacerLevitarUnaPluma)).

puntajeQueGenera(respondio(Dificultad,Profesor,_),Puntaje):-
    profesor(Profesor),
    Dificultad is Puntaje.
puntajeQueGenera(respondio(Dificultad,snape,_),Puntaje):-
    Dificultad is Puntaje/2.
*/
esLinda(guada).

profesor(snape).
profesor(flitwick).