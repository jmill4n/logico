esPersonaje(aang).
esPersonaje(katara).
esPersonaje(zoka).
esPersonaje(appa).
esPersonaje(momo).
esPersonaje(toph).
esPersonaje(tayLee).
esPersonaje(zuko).
esPersonaje(azula).
esPersonaje(iroh).

esPersonaje(bumi).
esPersonaje(suki).

esElementoBasico(fuego).
esElementoBasico(agua).
esElementoBasico(tierra).
esElementoBasico(aire).

elementoAvanzadoDe(fuego, rayo).
elementoAvanzadoDe(agua, sangre).
elementoAvanzadoDe(tierra, metal).


controla(zuko, rayo).
controla(toph, metal).
controla(katara, sangre).
controla(aang, aire).
controla(aang, agua).
controla(aang, tierra).
controla(aang, fuego).
controla(azula, rayo).
controla(iroh, rayo).

controla(bumi,tierra).

% reinoTierra(nombreDelLugar, estructura)
% nacionDelFuego(nombreDelLugar, soldadosQueLoDefienden)
% tribuAgua(puntoCardinalDondeSeUbica)
% temploAire(puntoCardinalDondeSeUbica)

visito(aang, reinoTierra(baSingSe, [muro, zonaAgraria, sectorBajo, sectorMedio])).
visito(iroh, reinoTierra(baSingSe, [muro, zonaAgraria, sectorBajo, sectorMedio])).
visito(zuko, reinoTierra(baSingSe, [muro, zonaAgraria, sectorBajo, sectorMedio])).
visito(toph, reinoTierra(fortalezaDeGralFong, [cuartel, dormitorios, enfermeria, salaDeGuerra, templo, zonaDeRecreo])).
visito(aang, nacionDelFuego(palacioReal, 1000)).
visito(katara, tribuAgua(norte)).
visito(katara, tribuAgua(sur)).
visito(aang, temploAire(norte)).
visito(aang, temploAire(oeste)).
visito(aang, temploAire(este)).
visito(aang, temploAire(sur)).

% 1
esElAvatar(Personaje):-
    esPersonaje(Personaje),
    controla(Personaje,_),
    forall(esElementoBasico(Elemento),controla(Personaje,Elemento)).

% 2
/*Aca ser podria haber hecho una división, es vez de un predicado por cada uno, haber hecho una relacion con el tipo de maestro.
clasificacion(Personaje,Clasificacion):-
    personaje(Personaje),
    clasificacionPersonaje(Personaje,Clasificacion)

clasificacionPersonaje(Personaje,noEsMaestro):-...
clasificacionPersonaje(Personaje,esMaestroPrincipiante):-...
clasificacionPersonaje(Personaje,esMaestroAvanzado):-...
*/
noEsMaestro(Personaje):-
    esPersonaje(Personaje),
    not(controla(Personaje,_)).
esMaestroPrincipiante(Personaje):-
    controla(Personaje,_),
    not(esElAvatar(Personaje)),
    forall(controla(Personaje,Elemento),esElementoBasico(Elemento)).
esMaestroAvanzado(Personaje):-
    controla(Personaje,Elemento),
    elementoAvanzadoDe(_,Elemento).
esMaestroAvanzado(aang).

% 3

sigueA(Personaje,OtroPersonaje):-
    visito(Personaje,_),
    visito(OtroPersonaje,_),
    Personaje \= OtroPersonaje,
    forall(visito(Personaje,Lugar),visito(OtroPersonaje,Lugar)).
sigueA(zuko,aang).

% 4
/*
esDignoDeConocer(temploAire(_)).
esDignoDeConocer(tribuAgua(norte)).
esDignoDeConocer(Lugar):-
    visito(_,Lugar),
    noTieneMuros(Lugar).

noTieneMuros(reinoTierra(_,Estructura)):-
    not(member(muro,Estructura)).
*/
esDignoDeConocer(UnLugar) :-
    lugar(UnLugar),
    esDignoDeConocerSegunTipo(UnLugar).
      
lugar(UnLugar) :-
    visito(_, UnLugar).
      
esDignoDeConocerSegunTipo(temploAire(_)).
esDignoDeConocerSegunTipo(tribuAgua(norte)).
esDignoDeConocerSegunTipo(reinoTierra(_, Estructura)) :-
    not(member(muro, Estructura)).

% 5

esPopular(Lugar):-
    visito(_,Lugar),
    findall(Personaje,visito(Personaje,Lugar),Personajes),
    length(Personajes,Cantidad),
    Cantidad > 4.

% 6
/*
esPersonaje(bumi).
controla(bumi,tierra).
visito(bumi, reinoTierra(baSingSe, [muro, zonaAgraria, sectorBajo, sectorMedio])).

esPersonaje(suki).
visito(suki,nacionDelFuego(prisionDeMaximaSeguridad,200)).
*/
simulacro(simulacro).
