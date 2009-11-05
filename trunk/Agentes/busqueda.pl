:-consult(extras_for_agents).

:-dynamic abierto/1.
:-dynamic cerrado/1.

:-dynamic esMeta/1.
:-dynamic cota_prof/1.

nodo_eti([E,_H,_C,_F],E).
nodo_heu([_E,H,_C,_F],H).
nodo_cam([_E,_H,C,_F],C).
nodo_cos([_E,_H,_C,G],G).

esMeta([[0,0],null]).

cota_prof(-1).

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

dir_opuesta(e,o).
dir_opuesta(o,e).
dir_opuesta(s,n).
dir_opuesta(n,s).

adyacente_pos_ini(PosDir,PosDirV,P):-
	estado_grilla(Grilla),
	
	dir_posible(DirV),
	PosDir=[Pos,Dir],
	
	ady_at_cardinal(Pos,DirV,PosV),
	member([PosV,Land,_],Grilla),
	Land\=water,Land\=forest,
	
	PosDirV=[PosV,DirV],
	
	((Land=mountain,P1=2);(Land=plain,P1=1)),
	%--Si la direcci�n es distinta el costo aumenta en 1
	((DirV=Dir,P2=0);(DirV\=Dir,P2=1)),
	
	P is P1+P2.

adyacente(PosDir,PosDirV,P):-
	estado_grilla(Grilla),
	
	dir_posible(DirV),
	PosDir=[Pos,Dir],
	not(dir_opuesta(DirV,Dir)),
	
	ady_at_cardinal(Pos,DirV,PosV),
	member([PosV,Land,_],Grilla),
	Land\=water,Land\=forest,
	
	PosDirV=[PosV,DirV],
	
	((Land=mountain,P1=2);(Land=plain,P1=1)),
	%--Si la direcci�n es distinta el costo aumenta en 1
	((DirV=Dir,P2=0);(DirV\=Dir,P2=1)),
	
	P is P1+P2.

%--============================================================================
generar_vecinos(Nodo):-
	nodo_eti(Nodo,E),nodo_cam(Nodo,C),nodo_cos(Nodo,G),
	E=[PosE,_],
	forall(
		adyacente(E,NodoV,P),
		(
			GNV is G+P,
			(
				(
					not(abierto([NodoV,_,_,_])),
					not(cerrado([NodoV,_,_,_])),
					h(NodoV,HNV),
					
					assert(abierto([NodoV,HNV,[PosE|C],GNV]))
				);
				(
					abierto([NodoV,NodoVH,_,NodoVG]),
					(GNV<NodoVG),
					
					retract(abierto([NodoV,_,_,NodoVG])),
					assert(abierto([NodoV,NodoVH,[PosE|C],GNV]))
				);
				(
					cerrado([NodoV,NodoVH,_,NodoVG]),
					(GNV<NodoVG),
					
					retract(cerrado([NodoV,_,_,NodoVG])),
					assert(abierto([NodoV,NodoVH,[PosE|C],GNV]))
				);
				(
					(
						abierto([NodoV,NodoVH,_,NodoVG]);
						cerrado([NodoV,NodoVH,_,NodoVG])
					),
					(
						(GNV>NodoVG);
						(GNV=NodoVG)
					)
				)
			)
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

buscarA([PosE|SSol],Prof):-
	seleccionarA(Nodo),
	Nodo=[NodoE,_,SSol,_],
	NodoE=[PosE,DirE],
	
	%--No importa la direcci�n VER
	esMeta([PosE,DirE]).
	
	%--write('################################Profundidad='),write(Prof),nl
	
buscarA(Sol,Prof):-
	cota_prof(CotaP),
	Prof<CotaP,
	
/*	write('Profundidad #'),write(Prof),nl,*/

	seleccionarA(Nodo),
/*	write('Nodo seleccionado |='),write(Nodo),nl,
	mostrar_abiertos,nl,
	mostrar_cerrados,nl,nl,*/
	generar_vecinos(Nodo),
	
	ProfSig is Prof+1,
	buscarA(Sol,ProfSig).

empezar(NodoE,Metas,Sol,CotaP):-
	resetear_abiertos,
	resetear_cerrados,
	resetear_metas,
	
	retractall(cota_prof(_)),
	assert(cota_prof(CotaP)),
	
	forall(
		member(Meta,Metas),		
		assert(esMeta(Meta))
	),
	
	h(NodoE,HN),
	Nodo=[NodoE,HN,[],0],
	
	%--assert(abierto(Nodo)),
	
	%--Permite utilizar la versi�n mas eficiente de adyacente, que no genera
	%--caminos para atras
	(
		(
			esMeta(NodoE),
			Sol=[]
		);
		(
			not(esMeta(NodoE)),
			
			NodoE=[PosE,_],
			forall(
				adyacente_pos_ini(NodoE,NodoV,P),
				(
					h(NodoV,HNV),
					assert(abierto([NodoV,HNV,[PosE],P]))
				)
			),
			
			buscarA(Sol,0)
		)
	).