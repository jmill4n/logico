%vocaloid(Cantante)
vocaloid(megurineLuka).
vocaloid(hatsuneMiku).
vocaloid(gumi).
vocaloid(seeU).
vocaloid(kaito).

%sabeCantar(Cantante,Cancion,Duracion).
%sabeCantar(Cantante,cancion(Cancion,Duracion)).
%sabeCantar(megurineLuka,cancion(nightFever,4)).
sabeCantar(megurineLuka,nightFever,4).
sabeCantar(megurineLuka,foreverYoung,5).
sabeCantar(megurineLuka,tellYourWorld,5).
sabeCantar(megurineLuka,novemberRain,5).
sabeCantar(hatsuneMiku,tellYourWorld,4).
sabeCantar(gumi,foreverYoung,4).
sabeCantar(gumi,tellYourWorld,5).
sabeCantar(seeU,novemberRain,6).
sabeCantar(seeU,nightFever,5).

% ********* Parte 1 *********

% ****** Punto 1 ******

esNovedoso(Cantante):-
    vocaloid(Cantante),
    sabe2Canciones(Cantante),
    duracionDeCanciones(Cantante,Duracion),
    Duracion < 15.

sabe2Canciones(Cantante):-
    sabeCantar(Cantante,UnaCancion,_),
    sabeCantar(Cantante,OtraCancion,_),
    UnaCancion \= OtraCancion.

duracionDeCanciones(Cantante,DuracionTotal):-
    findall(Duracion,sabeCantar(Cantante,_,Duracion),ListaDuraciones),
    sumlist(ListaDuraciones,DuracionTotal),
    DuracionTotal \= 0.

% ****** Punto 2 ******

esAcelerado(Cantante):-
    sabeCantar(Cantante,_,_),
    not((sabeCantar(Cantante,_,Duracion),not(Duracion =< 4))).


esAcelerado1(Cantante):-
    sabeCantar(Cantante,_,_),
    forall(sabeCantar(Cantante,_,Duracion),Duracion =< 4).

% ********* Parte 2 *********

% ****** Punto 1 ******

%concierto(nombre,pais,fama,tipo).
%concierto(nombre,pais,fama,gigante(cancionesMinimas,duracionTotalMinima)).
%concierto(nombre,pais,fama,mediano(duracionTotalMenorA)).
%concierto(nombre,pais,fama,pequenio(algunaCancionDuracionMayorA)).

concierto(mikuExpo,eeuu,2000,gigante(2,6)).
concierto(magicalMirai,japon,3000,gigante(3,10)).
concierto(vocalektVisions,eeuu,1000,mediano(9)).
concierto(mikuFest,argentina,100,pequenio(4)).

% ****** Punto 2 ******

puedeParticipar(Vocaloid,Concierto):-
    vocaloid(Vocaloid),
   %Vocaloid \= hatsuneMiku,
    concierto(Concierto,_,_,Requisitos),
    cumpleRequisitos(Vocaloid,Requisitos).
puedeParticipar(hatsuneMiku,_).

cumpleRequisitos(Vocaloid,gigante(CancionesMinimas,DuracionTotalMinima)):-
    sabeCanciones(Vocaloid,Cantidad),
    duracionDeCanciones(Vocaloid,DuracionTotal),
    Cantidad > CancionesMinimas,
    DuracionTotal >= DuracionTotalMinima.

cumpleRequisitos(Vocaloid,mediano(DuracionTotalMaxima)):-
    duracionDeCanciones(Vocaloid,DuracionTotal),
    DuracionTotal =< DuracionTotalMaxima.

cumpleRequisitos(Vocaloid,pequenio(DuracionMinima)):-
    sabeCantar(Vocaloid,_,Duracion),
    Duracion > DuracionMinima.

sabeCanciones(Vocaloid,Cantidad):-
    findall(Cancion,sabeCantar(Vocaloid,Cancion,_),ListaDeCanciones),
    length(ListaDeCanciones,Cantidad).

% ****** Punto 3 ******

vocaloidMasFamoso(Vocaloid):-
    fama(Vocaloid,Nivel),
    forall((vocaloid(OtroVocaloid),OtroVocaloid \= Vocaloid),(fama(OtroVocaloid,NivelOtro),Nivel > NivelOtro)).

fama(Vocaloid,Nivel):-
    vocaloid(Vocaloid),
    findall(Fama,ganaFamaParticipando(Vocaloid,Fama),ListaDeFama),
    limpiarYSumar(ListaDeFama, Fama),
    sabeCanciones(Vocaloid,Cantidad),
    Nivel is Fama*Cantidad.

ganaFamaParticipando(Vocaloid,Fama):-
    puedeParticipar(Vocaloid,Concierto),
    concierto(Concierto,_,Fama,_).

limpiarYSumar(ListaDeFama,Fama):-
    list_to_set(ListaDeFama,ListaDeFamaLimpia),
    sumlist(ListaDeFamaLimpia,Fama).

% ****** Punto 4 ******

conoce(megurineLuka,hatsuneMiku).
conoce(megurineLuka,gumi).
conoce(gumi,seeU).
conoce(seeU,kaito).

conocido(Vocaloid,OtroVocaloid):-
    conoce(Vocaloid,OtroVocaloid).
conocido(Vocaloid,OtroVocaloid):-
    conoce(Vocaloid,Tercero),
    conoce(Tercero,OtroVocaloid).

unicoQueParticipa(Vocaloid,Concierto):-
    puedeParticipar(Vocaloid,Concierto),
   %forall(conocido(Vocaloid,OtroVocaloid),not(puedeParticipar(OtroVocaloid,Concierto))).
    not((conocido(Vocaloid,OtroVocaloid),puedeParticipar(OtroVocaloid,Concierto))).

unicoParticipanteEntreConocidos(Cantante,Concierto):- 
    puedeParticipar(Cantante, Concierto),
    not((conocido(Cantante, OtroCantante),puedeParticipar(OtroCantante, Concierto))).
    
/* ****** Punto 5 ******
En la solución planteada habría que agregar una claúsula en el predicado cumpleRequisitos/2  que tenga en cuenta el nuevo functor con sus respectivos requisitos 

El concepto que facilita los cambios para el nuevo requerimiento es el polimorfismo, que nos permite dar un tratamiento en particular a cada uno de los conciertos en la cabeza de la cláusula.
*/
esLinda(guadi).