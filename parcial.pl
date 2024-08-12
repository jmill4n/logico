% receta(Plato, Duración, Ingredientes)
receta(empanadaDeCarneFrita, 20, [harina, carne, cebolla, picante, aceite]).
receta(empanadaDeCarneAlHorno, 20, [harina, carne, cebolla, picante]).
receta(lomoALaWellington, 125, [lomo, hojaldre, huevo, mostaza]).
receta(pastaTrufada, 40, [spaghetti, crema, trufa]).
receta(souffleDeQueso, 35, [harina, manteca, leche, queso]).
receta(tiramisu, 30, [vainillas, cafe, mascarpone]).
receta(rabas, 20, [calamar, harina, sal]).
receta(parrilladaDelMar, 40, [salmon, langostinos, mejillones]).
receta(sushi, 30, [arroz, salmon, sesamo, algaNori]).
receta(hamburguesa, 15, [carne, pan, cheddar, huevo, panceta, trufa]).
receta(padThai, 40, [fideos, langostinos, vegetales]).

% elabora(Chef, Plato)
elabora(guille, empanadaDeCarneFrita).
elabora(guille, empanadaDeCarneAlHorno).
elabora(vale, rabas).
elabora(vale, tiramisu).
elabora(vale, parrilladaDelMar).
elabora(ale, hamburguesa).
elabora(lu, sushi).
elabora(mar, padThai).

% cocinaEn(Restaurante, Chef)
cocinaEn(pinpun, guille).
cocinaEn(laPececita, vale).
cocinaEn(laParolacha, vale).
cocinaEn(sushiRock, lu).
cocinaEn(olakease, lu).
cocinaEn(guendis, ale).
cocinaEn(cantin, mar).

% tieneEstilo(Restaurante, Estilo)
tieneEstilo(pinpun, bodegon(parqueChas, 6000)).
tieneEstilo(laPececita, bodegon(palermo, 20000)).
tieneEstilo(laParolacha, italiano(15)).
tieneEstilo(sushiRock, oriental(japon)).
tieneEstilo(olakease, oriental(japon)).
tieneEstilo(cantin, oriental(tailandia)).
tieneEstilo(cajaTaco, mexicano([habanero, rocoto])).
tieneEstilo(guendis, comidaRapida(5)).

% italiano(CantidadDePastas)
% oriental(País)
% bodegon(Barrio, PrecioPromedio)
% mexicano(VariedadDeAjies)
% comidaRapida(cantidadDeCombos)

% ********** Punto 1 ********** 

esCrack(Chef):-
    cocinaEn(Restaurante,Chef),
    cocinaEn(OtroRestaurante,Chef),
    Restaurante \= OtroRestaurante.
esCrack(Chef):-
    elabora(Chef,padThai).

% ********** Punto 2 ********** 

esOtaku(Chef):-
    trabaja(Chef),
    forall(cocinaEn(Restaurante,Chef),tieneEstilo(Restaurante,oriental(japon))).

trabaja(Chef):-
    cocinaEn(_,Chef).

% ********** Punto 3 ********** 

esTop(Plato):-
    esElaborado(Plato),
    forall(elabora(Chef,Plato),esCrack(Chef)).

esElaborado(Plato):-
    elabora(_,Plato).

% ********** Punto 4 ********** 

esDificil(Plato):-
    receta(Plato,Duracion,_),
    Duracion > 120.
esDificil(Plato):-
    tieneTrufa(Plato).
esDificil(souffleDeQueso).

tieneTrufa(Plato):-
    receta(Plato,_,Ingredientes),
    member(trufa,Ingredientes).

% ********** Punto 5 ********** 

seMereceLaMichelin(Restaurante):-
    tieneChefCrack(Restaurante),
    tieneEstiloMichelinero(Restaurante).

tieneChefCrack(Restaurante):-
    cocinaEn(Restaurante,Chef),
    esCrack(Chef).

tieneEstiloMichelinero(Restaurante):-
    tieneEstilo(Restaurante,Estilo),
    esEstiloMichelinero(Estilo).

esEstiloMichelinero(oriental(tailandia)).
esEstiloMichelinero(bodegon(palermo,_)).
esEstiloMichelinero(italiano(CantidadDePastas)):-
    CantidadDePastas > 5.
esEstiloMichelinero(mexicano(Ajies)):-
    member(habanero,Ajies),
    member(rocoto,Ajies).

% ********** Punto 6 **********

tieneMayorRepertorio(UnRestaurante,OtroRestaurante):-
    cantidadDePlatosDeRestaurante(UnRestaurante,UnaCantidad),
    cantidadDePlatosDeRestaurante(OtroRestaurante,OtraCantidad),
    UnaCantidad > OtraCantidad.

cantidadDePlatosDeRestaurante(Restaurante,Cantidad):-
    cocinaEn(Restaurante,Chef),
    cantidadDePlatos(Chef,Cantidad).

cantidadDePlatos(Chef,Cantidad):-
    elabora(Chef,_),
    findall(Plato,elabora(Chef,Plato),ListaDePlatos),
    length(ListaDePlatos,Cantidad).

% ********** Punto 7 ********** 

calificacionGastronomica(Restaurante,Calificacion):-
    cantidadDePlatosDeRestaurante(Restaurante,Cantidad),
    Calificacion is Cantidad*5.