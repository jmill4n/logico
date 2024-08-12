% ******* Punto 1 *******
%atiende(persona,dia,horainicial,horafinal).

atiende(dodain,lunes,9,15).
atiende(dodain,miercoles,9,15).
atiende(dodain,viernes,9,15).
atiende(lucas,martes,10,20).
atiende(juanC,sabados,18,22).
atiende(juanC,domingo,18,22).
atiende(juanFdS,jueves,10,20).
atiende(juanFdS,viernes,12,20).
atiende(leoC,lunes,14,18).
atiende(leoC,miercoles,14,18).
atiende(martu,miercoles,23,24).

atiende(vale,Dia,HorarioInicial,HorarioFinal):-
    atiende(dodain,Dia,HorarioInicial,HorarioFinal).
atiende(vale,Dia,HorarioInicial,HorarioFinal):-
    atiende(juanC,Dia,HorarioInicial,HorarioFinal).

% "Nadie hace el mismo horario que leoC", "maiu está pesando si hace el horario..." no hace falta agregarlos por el principio del universo cerrado.
% Si no está en nuestra base de conocimientos entonces es falso.

% ******* Punto 2 *******

quienAtiende(Dia,Hora,Persona):-
    atiende(Persona,Dia,HoraInicial,HoraFinal),
    between(HoraInicial,HoraFinal,Hora).

% ******* Punto 4 *******

foreverAlone(Dia,Hora,Persona):-
    quienAtiende(Dia,Hora,Persona),
    quienAtiende(Dia,Hora,OtraPersona),
    not(Persona \= OtraPersona).

% ******* Punto 4 *******
/*
quienesPodrianAtender(Dia,Personas):-
    %findall(Persona,quienAtiende(Dia,Hora,Persona),Personas),
    findall(Persona,atiendeEseDia(Dia,Persona),ListaDePersonas),
    list_to_set(ListaDePersonas,Personas).


quienesEstanEnElKiosco(Dia,Personas):-
    findall(Persona,quienesPodrianAtender(Dia,Persona),ListaDeListasPersonas),
    list_to_set(ListaDeListasPersonas,Personas).

**********************

podriaAtender(Dia,Personas):-
    atiendeEseDia(Dia,Personas),
    findall(Atendedores,puedeAtender(Dia,Atendedores),ListaDePersonas),
    list_to_set(ListaDePersonas,Personas).

puedeAtender(Dia,Persona):-
    atiendeEseDia(Dia,Persona).
puedeAtender(Dia,Persona):-
    findall(Personas,atiendeEseDia(Dia,Personas),ListaDePersonas),
    list_to_set(ListaDePersonas,Persona).

atiendeEseDia(Dia,Persona):-
    quienAtiende(Dia,_,Persona).
*/
posibilidadesAtencion(Dia, Personas):-
    findall(Persona,quienAtiende(Dia,_,Persona),ListaDePersonas),
    list_to_set(ListaDePersonas,PersonasPosibles),
    combinar(PersonasPosibles, Personas).

combinar([Persona|PersonasPosibles], [Persona|Personas]):-
    combinar(PersonasPosibles, Personas).
combinar([_|PersonasPosibles], Personas):-
    combinar(PersonasPosibles, Personas).
combinar([], []).

% ******* Punto 5 *******

%venta(Persona,Dia,Fecha,golosinas(Precio)).
%venta(Persona,Dia,Fecha,cigarillos([Cuantos])).
%venta(Persona,Dia,Fecha,bebidas(tipo,cantidad)).
%                                ^^^^ como son unicamente alcoholica o no, se podria usar bool.

venta(dodain,lunes,agosto10,[golosinas(1200),cigarillos([jockey])]).
venta(dodain,miercoles,agosto12,[bebidas(alcoholica,8),bebidas(sinAlcohol,1),golosinas(10)]).
venta(martu,miercoles,agosto12,[golosinas(1000),cigarillos([chesterfield,colorado,carisiennes])]).
venta(lucas,martes,agosto11,[golosinas(600)]).
venta(lucas,martes,agosto18,[bebidas(sinAlcohol,2),cigarillos([derby])]).

/*
vendedorSuertudo(Persona):-
    venta(Persona,_,_,_),
    forall(venta(Persona,_,_,ListaDeVentas),primeraVentaImportante(ListaDeVentas)).

primeraVentaImportante(ListaDeVentas):-
    nth1(1,ListaDeVentas,Venta),
    esVentaImportante(Venta).

esVentaImportante(golosinas(Precio)):-
    Precio > 100.
esVentaImportante(cigarillos(ListaCigarillos)):-
    length(ListaCigarillos,Marcas),
    Marcas > 2.
esVentaImportante(bebidas(alcoholica,_)).
esVentaImportante(bebidas(sinAlcohol,Cantidad)):-
    Cantidad > 5.
*/

%Solucion mejorada

vendedorSuertudo(Persona):-
    vendedora(Persona),
    forall(venta(Persona,_,_,[Venta|_]),esVentaImportante(Venta)).

vendedora(Persona):-
    venta(Persona,_,_,_).

esVentaImportante(golosinas(Precio)):-
    Precio > 100.
esVentaImportante(cigarillos(ListaCigarillos)):-
    length(ListaCigarillos,Marcas),
    Marcas > 2.
esVentaImportante(bebidas(alcoholica,_)).
esVentaImportante(bebidas(sinAlcohol,Cantidad)):-
    Cantidad > 5.