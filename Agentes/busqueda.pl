:-consult(extras_for_agents).

:-dynamic abierto/1.
:-dynamic cerrado/1.

nodo_eti([E,_H,_C,_F],E).
nodo_heu([_E,H,_C,_F],H).
nodo_cam([_E,_H,C,_F],C).
nodo_fun([_E,_H,_C,F],F).

mostrar_abiertos:-forall(abierto([E,_,_,_]),(write(E),write('\n'))).
mostrar_cerrados:-forall(cerrado([E,_,_,_]),(write(E),write('\n'))).

resetear_abiertos:-retractall(abierto(_)).
resetear_cerrados:-retractall(cerrado(_)).

%--MODIFICACIONES

distancia_directa(NI,NF,Dist):-
	NI=[FI,CI],
	NF=[FF,CF],
	
	FDif is (FF-FI),
	CDif is (CF-CI),
	
	abs(FDif,FDist),
	abs(CDif,CDist),
	
	Dist is (FDist+CDist).

%--Funcion heuristica
h(Nodo,Val):-
	esMeta(NodoM),
	distancia_directa(Nodo,NodoM,Dist),
	Val is Dist+1.

dir_posible(e).
dir_posible(w).
dir_posible(s).
dir_posible(n).

adyacente(Pos,PosV,P):-
	estado_grilla(Grilla),
	
	dir_posible(Dir),
	ady_at_cardinal(Pos,Dir,PosV),
	
	member([PosV,Land,_],Grilla),
	Land\=water,
	Land\=forest,
	
	((Land=mountain,P=2);(Land=plain,P=1)).

:-dynamic esMeta/1.
esMeta([0,0]).
%--============================================================================

generar_vecinos(Nodo):-
	nodo_eti(Nodo,E),nodo_cam(Nodo,C),nodo_fun(Nodo,F),
	forall(
		(
			adyacente(E,NodoV,P),
			not(abierto([NodoV,_,_,_])),
			not(cerrado([NodoV,_,_,_]))
		),
		(
			h(NodoV,HNV),
			(FNV is F+P),
			assert(abierto([NodoV,HNV,[E|C],FNV]))
		)
	),
	forall(
		(
			adyacente(E,NodoV,P),
			abierto([NodoV,_,_,NodoVF]),
			(FNV is F+P),
			(FNV<NodoVF)
		),
		(
			retract(abierto([NodoV,_,_,NodoVF])),
			h(NodoV,HNV),
			assert(abierto([NodoV,HNV,[E|C],FNV]))
		)
	),
	forall(
		(
			adyacente(E,NodoV,P),
			cerrado([NodoV,_,_,NodoVF]),
			(FNV is F+P),
			(FNV<NodoVF)
		),
		(
			retract(cerrado([NodoV,_,_,NodoVF])),
			h(NodoV,HNV),
			assert(abierto([NodoV,HNV,[E|C],FNV]))
		)
	),
	retract(abierto(Nodo)),assert(cerrado(Nodo)).

seleccionarA(Nodo):-
	abierto(Nodo),
	Nodo=[_,_,_,NF],	
	forall(
		(abierto(NodoF),NodoF=[_,_,_,NFF]),
		(NF<NFF) | (NF=NFF)
	).

buscarA([NodoE|SSol]):-seleccionarA(Nodo),(Nodo=[NodoE,_,SSol,_]),esMeta(NodoE).
buscarA(Sol):-seleccionarA(Nodo),generar_vecinos(Nodo),buscarA(Sol).

empezar(NodoE,Meta,Sol):-
	h(NodoE,HN),
	(Nodo=[NodoE,HN,[],0]),
	
	assert(abierto(Nodo)),
	
	retractall(esMeta(_)),
	assertz(esMeta(Meta)),
	
	buscarA(Sol),
	resetear_abiertos,
	resetear_cerrados.