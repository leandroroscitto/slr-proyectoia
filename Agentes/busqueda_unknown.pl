:-consult(extras_for_agents).

:-dynamic abierto/1.
:-dynamic cerrado/1.

:-dynamic esMeta/1.

nodo_eti([E,_H,_C,_F],E).
nodo_heu([_E,H,_C,_F],H).
nodo_cam([_E,_H,C,_F],C).
nodo_cos([_E,_H,_C,G],G).

esMeta([[0,0],null]).

mostrar_abiertos:-write('Abiertos='),forall(abierto([E,_,_,_]),(write(E),write(','))).
mostrar_cerrados:-write('Cerrados='),forall(cerrado([E,_,_,_]),(write(E),write(','))).

resetear_abiertos:-retractall(abierto(_)).
resetear_cerrados:-retractall(cerrado(_)).

resetear_metas:-retractall(esMeta(_)).

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
h(Nodo,Dist):-
	findall(
		D,
		(
			esMeta(NodoM),
			Nodo=[Pos,_Dir],
			NodoM=[PosM,_DirM],
			distancia_directa(Pos,PosM,D)
		),
		LDist
	),
	min_list(LDist,Dist).

dir_posible(e).
dir_posible(w).
dir_posible(s).
dir_posible(n).

adyacente(PosDir,PosDirV,P):-
	estado_grilla(Grilla),
	
	dir_posible(DirV),
	PosDir=[Pos,Dir],
	
	ady_at_cardinal(Pos,DirV,PosV),
	(member([PosV,Land,_],Grilla);
	(
		not(member([PosV,_,_],Grilla)),
		%--PosV=[FV,CV],
		%--maxFila(Grilla,MF),
		%--maxCol(Grilla,MC),
		%--FV<MF,FV>1,
		%--CV<MC,CV>1,
		Land=unknown
		)
	),
	Land\=water,Land\=forest,
	
	PosDirV=[PosV,DirV],
	
	((Land=mountain,P1=2);(Land=plain,P1=1);(Land=unknown,P1=3)),
	%--Si la dirección es distinta el costo aumenta en 1
	((DirV=Dir,P2=0);(DirV\=Dir,P2=1)),
	
	P is P1+P2.

%--============================================================================
generar_vecinos(Nodo):-
	nodo_eti(Nodo,E),nodo_cam(Nodo,C),nodo_cos(Nodo,G),
	forall(
		(
			adyacente(E,NodoV,P),
			NodoV=[PosV,_],
			%--celda_libre(PosV,G),
			
			E=[PosE,_],
			not(abierto([NodoV,_,_,_])),
			not(cerrado([NodoV,_,_,_]))
		),
		(
			h(NodoV,HNV),
			(GNV is G+P),
			assert(abierto([NodoV,HNV,[PosE|C],GNV]))
		)
	),
	forall(
		(
			adyacente(E,NodoV,P),
			NodoV=[PosV,_],
			%--celda_libre(PosV,G),
			
			E=[PosE,_],
			abierto([NodoV,_,_,NodoVG]),
			(GNV is G+P),
			(GNV<NodoVG)
		),
		(
			retract(abierto([NodoV,_,_,NodoVG])),
			h(NodoV,HNV),
			assert(abierto([NodoV,HNV,[PosE|C],GNV]))
		)
	),
	forall(
		(
			adyacente(E,NodoV,P),
			NodoV=[PosV,_],
			%--celda_libre(PosV,G),
			
			E=[PosE,_],
			cerrado([NodoV,_,_,NodoVG]),
			(GNV is G+P),
			(GNV<NodoVG)
		),
		(
			retract(cerrado([NodoV,_,_,NodoVG])),
			h(NodoV,HNV),
			assert(abierto([NodoV,HNV,[PosE|C],GNV]))
		)
	),
	retract(abierto(Nodo)),assert(cerrado(Nodo)).

seleccionarA(Nodo):-
	abierto(Nodo),
	Nodo=[_,NH,_,NG],
	NF is NH+NG,
	forall(
		(abierto(NodoF),NodoF=[_,NFH,_,NFG]),
		(
			NFF is NFH+NFG,
			((NF<NFF);(NF=NFF))
		)
	).

buscarA([PosE|SSol]):-
	seleccionarA(Nodo),
	Nodo=[NodoE,_,SSol,_],
	NodoE=[PosE,_],
	
	%--No importa la dirección VER
	esMeta([PosE,_]).
buscarA(Sol):-
	seleccionarA(Nodo),
	generar_vecinos(Nodo),
	
	buscarA(Sol).

empezar(NodoE,Metas,Sol):-
	resetear_abiertos,
	resetear_cerrados,
	resetear_metas,
	
	forall(
		member(Meta,Metas),		
		assert(esMeta(Meta))
	),
	
	h(NodoE,HN),
	Nodo=[NodoE,HN,[],0],
	
	assert(abierto(Nodo)),
	
	buscarA(Sol).