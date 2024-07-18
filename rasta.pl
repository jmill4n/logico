%loAyuda(Persona,Persona)

loAyuda(UnaPersona,OtraPersona):-
    loQuiere(UnaPersona,OtraPersona),
    estaEnElAula(UnaPersona).

estaEnElAula(rasta).
estaEnElAula(polito).
estaEnElAula(santi).

loQuiere(santi,OtraPersona):-
    estaEnElAula(OtraPersona),
    not(loQuiere(rasta,OtraPersona)).

loQuiere(rasta,santi).
loQuiere(rasta,rasta).

tieneMenosSuerte(UnaPersona, rasta):-
    estaEnElAula(UnaPersona),
    not(loQuiere(santi,UnaPersona)).

loQuiere(polito,santi).
loQuiere(polito,rasta).

/*
?- loAyuda(Alguien,rasta).
Alguien = rasta ;
Alguien = polito.

?- loQuiere(_,milhouse).
false.

?- loQuiere(santi,Quien).
Quien = polito ;

?- loQuiere(Quien,rasta).   
Quien = rasta ;
Quien = polito.

?- loQuiere(UnaPersona, OtraPersona).
UnaPersona = santi,
OtraPersona = polito ;
UnaPersona = rasta,
OtraPersona = santi ;
UnaPersona = rasta, 
OtraPersona = rasta ;
UnaPersona = polito,
OtraPersona = santi ;
UnaPersona = polito,
OtraPersona = rasta.

?- loQuiere(polito,_).       
true ;