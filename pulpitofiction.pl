personaje(pumkin,     ladron([licorerias, estacionesDeServicio])).
personaje(honeyBunny, ladron([licorerias, estacionesDeServicio])).
personaje(vincent,    mafioso(maton)).
personaje(jules,      mafioso(maton)).
personaje(marsellus,  mafioso(capo)).
personaje(winston,    mafioso(resuelveProblemas)).
personaje(mia,        actriz([foxForceFive])).
personaje(butch,      boxeador).

pareja(marsellus, mia).
pareja(pumkin,    honeyBunny).

%trabajaPara(Empleador, Empleado)
trabajaPara(marsellus, vincent).
trabajaPara(marsellus, jules).
trabajaPara(marsellus, winston).

% **************************************************

esPeligroso(Personaje):-
    personaje(Personaje,Actividad),
    esActividadPeligrosa(Actividad).
esPeligroso(Personaje):-
    trabajaPara(Personaje,Empleado),
    esPeligroso(Empleado).

esActividadPeligrosa(mafioso(maton)).
esActividadPeligrosa(ladron(Objetivos)):-
    member(licorerias,Objetivos).

% **************************************************

amigo(vincent, jules).
amigo(jules, jimmie).
amigo(vincent, elVendedor).


duoTemible(Personaje,OtroPersonaje):-
    sonPeligrosos(Personaje,OtroPersonaje),
    tienenRelacion(Personaje,OtroPersonaje).

sonPeligrosos(Personaje,OtroPersonaje):-
    esPeligroso(Personaje),
    esPeligroso(OtroPersonaje).

tienenRelacion(Personaje,OtroPersonaje):-
    sonAmigos(Personaje,OtroPersonaje).
tienenRelacion(Personaje,OtroPersonaje):-
    sonPareja(Personaje,OtroPersonaje).

sonAmigos(Personaje,OtroPersonaje):-
    amigo(Personaje,OtroPersonaje).
sonAmigos(Personaje,OtroPersonaje):-
    amigo(OtroPersonaje,Personaje).

sonPareja(Personaje,OtroPersonaje):-
    pareja(Personaje,OtroPersonaje).
sonPareja(Personaje,OtroPersonaje):-
    pareja(OtroPersonaje,Personaje).

% **************************************************

%encargo(Solicitante, Encargado, Tarea). 
%las tareas pueden ser cuidar(Protegido), ayudar(Ayudado), buscar(Buscado, Lugar)
encargo(marsellus, vincent,   cuidar(mia)).
encargo(vincent,  elVendedor, cuidar(mia)).
encargo(marsellus, winston, ayudar(jules)).
encargo(marsellus, winston, ayudar(vincent)).
encargo(marsellus, vincent, buscar(butch, losAngeles)).


estaEnProblemas(Personaje):-
    trabajaPara(Jefe,Personaje),
    esPeligroso(Jefe),
    encargo(Jefe,Personaje,cuidar(Pareja)),
    sonPareja(Jefe,Pareja).

estaEnProblemas(Personaje):-
    encargo(_,Personaje,buscar(Objetivo,_)),
    esBoxeador(Objetivo).

esBoxeador(Personaje):-
    personaje(Personaje,boxeador).

% **************************************************

sanCayetano(Personaje):-
    hizoEncargo(Personaje),
    forall(tieneCerca(Personaje,OtroPersonaje),encargo(Personaje,OtroPersonaje,_)).

hizoEncargo(Personaje):-
    encargo(Personaje,_,_).

tieneCerca(Personaje,OtroPersonaje):-
    sonAmigos(Personaje,OtroPersonaje).
tieneCerca(Personaje,OtroPersonaje):-
    trabajaPara(Personaje,OtroPersonaje).

% **************************************************

masAtareado(Personaje):-
    cantidadDeEncargos(Personaje,Cantidad),
    forall((cantidadDeEncargos(OtroPersonaje,OtraCantidad),Personaje\=OtroPersonaje), Cantidad > OtraCantidad).

masAtareado2(Personaje):-
    cantidadDeEncargos(Personaje,Cantidad),
    forall(cantidadDeEncargos(_,OtraCantidad), Cantidad >= OtraCantidad).

cantidadDeEncargos(Personaje,Cantidad):-
    tieneEncargo(Personaje),
    findall(Encargo,encargo(_,Personaje,Encargo),ListaDeEncargos),
    length(ListaDeEncargos,Cantidad).

tieneEncargo(Personaje):-
    encargo(_,Personaje,_).

% **************************************************

personajesRespetables(Personajes):-
    findall(Personaje,suficientementeRespetable(Personaje),Personajes).

suficientementeRespetable(Personaje):-
    nivelDeRespeto(Personaje,Nivel),
    Nivel > 9.

nivelDeRespeto(Personaje,Nivel):-
    personaje(Personaje,Actividad),
    respetoDeActividad(Actividad,Nivel).

respetoDeActividad(actriz(Peliculas),Nivel):-
    length(Peliculas,CantidadDePelis),
    Nivel is CantidadDePelis*(1/10).
respetoDeActividad(mafioso(resuelveProblemas),10).
respetoDeActividad(mafioso(maton),1).
respetoDeActividad(mafioso(capo),20).

% **************************************************

hartoDe(UnPersonaje,OtroPersonaje):-
    tieneEncargo(UnPersonaje),
    relacionadoCercanoA(_,OtroPersonaje),
    forall(encargoDe(UnPersonaje,Encargo),relacionadoCercanoA(Encargo,OtroPersonaje)).

encargoDe(UnPersonaje,Encargo):-
    encargo(_,UnPersonaje,Encargo).

relacionadoCercanoA(Encargo,Personaje):-
    existeEncargo(Encargo),
    interactuaCon(Encargo,Personaje).
relacionadoCercanoA(Encargo,Personaje):-
    existeEncargo(Encargo),
    sonAmigos(Personaje,Amigo),
    interactuaCon(Encargo,Amigo).

existeEncargo(Encargo):-
    encargo(_,_,Encargo).

interactuaCon(cuidar(Personaje),Personaje).
interactuaCon(ayudar(Personaje),Personaje).
interactuaCon(buscar(Personaje,_),Personaje).

hartoDeVicky(Personaje, OtroPersonaje):-
    tieneEncargo(Personaje),
    esPersonaje(OtroPersonaje),
    forall(encargoDe(Personaje,Encargo), requiereInteractuarCon(OtroPersonaje, Encargo)).

esPersonaje(Personaje):-
    personaje(Personaje,_).

requiereInteractuarCon(Personaje, cuidar(Personaje)).
requiereInteractuarCon(Personaje, ayudar(Personaje)).
requiereInteractuarCon(Personaje, buscar(Personaje,_)).
requiereInteractuarCon(Personaje, cuidar(Amigo)):- 
    sonAmigos(Personaje, Amigo).
requiereInteractuarCon(Personaje,  buscar(Amigo, _)):- 
    sonAmigos(Personaje, Amigo).
requiereInteractuarCon(Personaje, ayudar(Amigo)):- 
    sonAmigos(Personaje, Amigo).

% **************************************************

caracteristicas(vincent,  [negro, muchoPelo, tieneCabeza]).
caracteristicas(jules,    [tieneCabeza, muchoPelo]).
caracteristicas(marvin,   [negro]).

duoDiferenciable(UnPersonaje,OtroPersonaje):-
    tienenRelacion(UnPersonaje,OtroPersonaje),
    tienenCaracteristicaDiferenciable(UnPersonaje,OtroPersonaje).

tienenCaracteristicaDiferenciable(UnPersonaje,OtroPersonaje):-
    caracteristicas(UnPersonaje,UnasCaracteristicas),
    caracteristicas(OtroPersonaje,OtrasCaracteristicas),
    member(Caracteristica,UnasCaracteristicas),
    not(member(Caracteristica,OtrasCaracteristicas)).