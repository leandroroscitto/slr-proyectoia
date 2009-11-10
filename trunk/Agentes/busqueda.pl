%--IMPLEMENTACION DEL ALGORITMO DE BUSQUEDA A*
:-consult(extras_for_agents).

%--abierto(+Nodo) determina que Nodo se encuentra en la frontera de busqueda
%--cerrado(+Nodo) determina que Nodo ya fue visitado y expandido
:-dynamic abierto/1.
:-dynamic cerrado/1.

%--esMeta(+Meta) determina si Meta es una meta de la búsqueda
:-dynamic esMeta/1.
%--cota_prof(+Cota), Cota determina la máxima profundidad permitada
%--antes de cortar la busqueda
:-dynamic cota_prof/1.
%--Indica que no se cortó la busqueda por superar la máxima profundidad
:-dynamic continuar_busqueda/0.

%--tiempo_inicio(+Momento), Momento indica el tiempo en segundos en el que
%--se inició la busqueda
:-dynamic tiempo_inicio/1.

%--AUXILIARES
%--Funciones auxiliares que permiten extraer las componentes de un nodo
%--E: representa los elementos que integran la solución de la busqueda y
%--la meta, en este caso E=[Pos,Dir]
%--H: valor de la evaluación heurística del elemento E
%--C: mejor camino encontrado desde la posición inicial hasta E
%--G: costo del camino C
nodo_eti([E,_H,_C,_F],E).
nodo_heu([_E,H,_C,_F],H).
nodo_cam([_E,_H,C,_F],C).
nodo_cos([_E,_H,_C,G],G).

%--dir_posible(-Dir), indica una de las cuatro direcciones posibles en Dir
dir_posible(e).
dir_posible(w).
dir_posible(s).
dir_posible(n).

%--dir_opuesta(+Dir,-DirOp), determina dada una dirección Dir, su dirección
%--opuesta (180 grados) en DirOp
dir_opuesta(e,o).
dir_opuesta(o,e).
dir_opuesta(s,n).
dir_opuesta(n,s).

%--Muestra los elementos abiertos (frontera)
mostrar_abiertos:-write('Abiertos='),forall(abierto([E,_,_,_]),(write(E),write(','))).
%--Muestra los elementos cerrados (expandidos)
mostrar_cerrados:-write('Cerrados='),forall(cerrado([E,_,_,_]),(write(E),write(','))).

%--Permiten realizar una busqueda desde cero
resetear_abiertos:-retractall(abierto(_)).
resetear_cerrados:-retractall(cerrado(_)).
resetear_metas:-retractall(esMeta(_)).
resetear_cota:-retractall(cota_prof(_)).

%--es_transitable(+PosV), si PosV existe en la grilla, no es terreno de agua o bosque
%--y si no está temporalmente bloqueado
es_transitable(PosV):-
	estado_grilla(Grilla),
	member([PosV,Land,_],Grilla),
	Land\=water,Land\=forest,
	not(no_transitable(PosV)).

%--IMPLEMENTACION
%--dist_manhattan(+NI,+NF,-Dist), dado la celda de inicio NI y de fin NF,
%--determina la distancia de Manhattan Dist entre las dos
dist_manhattan(NI,NF,Dist):-
	NI=[FI,CI],
	NF=[FF,CF],
	
	FDif is (FF-FI),
	CDif is (CF-CI),
	
	abs(FDif,FDist),
	abs(CDif,CDist),
	
	Dist is (FDist+CDist).

%--h(+Nodo,-Dist), dada una celda Nodo, determina su evaluación heurística como
%--la menor distancia de Manhattan entre el Nodo y alguna de las metas
h(Nodo,Dist):-
	findall(
		%--Busca todas las distancias de Manhattan desde el Nodo hasta una meta
		D,
		(
			esMeta(NodoM),
			Nodo=[Pos,_Dir],
			NodoM=[PosM,_DirM],
			dist_manhattan(Pos,PosM,D)
		),
		LDist
	),
	%--Determina la menor de ellas
	min_list(LDist,Dist).

%--adyacente_pos_ini(+PosDir,-PosDirV,-P), dada un par PosDir=[Pos,Dir], determina una posición y dirección
%--adyacente válida PosDirV=[PosV,DirV] y el costo P necesario para trasladarse a ella
%--Con válida se refiere que deba ser transitable
%--Este es el caso de la posición inicial y por lo tanto si se calculan las posiciones y direcciónes hacia atrás
adyacente_pos_ini(PosDir,PosDirV,P):-
	%--Obtiene una dirección posible
	dir_posible(DirV),
	PosDir=[Pos,Dir],
	
	%--Obtiene una posición adyacente a la posición y dirección actual
	ady_at_cardinal(Pos,DirV,PosV),
	%--Verifica que la posición adyacente obtenida exista en la grilla
	%--y que sea transitable
	es_transitable(PosV),
	
	PosDirV=[PosV,DirV],
	
	%--Dos consideraciones para el costo:
	%--1)Si el terreno es montaña el costo es 2, si es llanura es 1
	%--2)Si la dirección en la que está la celda adyacente es
	%--distinta de la actual, el costo es 1 (el de girar), sino es 0
	((Land=mountain,P1=2);(Land=plain,P1=1)),
	((DirV=Dir,P2=0);(DirV\=Dir,P2=1)),

	%--El costo total es la suma de las dos consideraciones
	P is P1+P2.

%--adyacente(+PosDir,-PosDirV,-P), dada un par PosDir=[Pos,Dir], determina una posición y dirección
%--adyacente válida PosDirV=[PosV,DirV] y el costo P necesario para trasladarse a ella
%--Con válida se refiere que deba ser transitable
%--Por eficiencia no calcula posiciones y direcciones hacia atrás:
%--			[ 1] [ 2] [ 3]
%--			[ 4] [<-] [ 5]
%--			[ 6] [ 7] [ 8]
%--obtendrá solo las posiciones 2,4 y 7 (en el caso de que sean transitables)
adyacente(PosDir,PosDirV,P):-
	%--Obtiene una dirección posible, que no sea la opuesta a la de entrada
	dir_posible(DirV),
	PosDir=[Pos,Dir],
	not(dir_opuesta(DirV,Dir)),

	%--Obtiene una posición adyacente a la posición y dirección actual
	ady_at_cardinal(Pos,DirV,PosV),
	%--Verifica que la posición adyacente obtenida exista en la grilla
	%--y que sea transitable
	es_transitable(PosV),
	
	PosDirV=[PosV,DirV],
	
	%--Dos consideraciones para el costo:
	%--1)Si el terreno es montaña el costo es 2, si es llanura es 1
	%--2)Si la dirección en la que está la celda adyacente es
	%--distinta de la actual, el costo es 1 (el de girar), sino es 0
	((Land=mountain,P1=2);(Land=plain,P1=1)),
	((DirV=Dir,P2=0);(DirV\=Dir,P2=1)),
	
	%--El costo total es la suma de las dos consideraciones
	P is P1+P2.

%--============================================================================
%--actualizar_ramas_por(+NodoE,+NuevoC,+DifCosto), actualiza el camino NuevoC y el
%--costo (ajustando la diferencia DifCosto) de las ramas del árbol de busqueda,
%--en los casos de que se encuentre un mejor camino para una celda NodoE ya
%--expandida y cerrada
actualizar_ramas_por(NodoE,NuevoC,DifCosto):-
	NodoE=[PosE,_DirE],
	forall(
		(
			%--Para todos los nodos abiertos que pasen por el nodo NodoE
			abierto([ANodoE,ANodoH,ANodoC,ANodoG]),
			member(PosE,ANodoC)
		),
		(
			%--Se calcula el nuevo costo
			ANuevoG is (ANodoG-DifCosto),
			
			%--Se arma el nuevo camino
			append([S1,[PosE],_],ANodoC),
			append([S1,[PosE],NuevoC],ANuevoC),
			
			retract(abierto([ANodoE,ANodoH,ANodoC,ANodoG])),
			assert(abierto([ANodoE,ANodoH,ANuevoC,ANuevoG]))
		)
	),
	forall(
		(
			%--Para todos los nodos cerrados que pasen por el nodo NodoE
			cerrado([ANodoE,ANodoH,ANodoC,ANodoG]),
			member(PosE,ANodoC)
		),
		(
			%--Se calcula el nuevo costo
			ANuevoG is (ANodoG-DifCosto),
			
			%--Se arma el nuevo camino
			append([S1,[PosE],_],ANodoC),
			append([S1,[PosE],NuevoC],ANuevoC),
			
			retract(cerrado([ANodoE,ANodoH,ANodoC,ANodoG])),
			assert(cerrado([ANodoE,ANodoH,ANuevoC,ANuevoG]))
		)
	).
			
		

%--generar_vecinos(+Nodo), expande un nodo seleccionado Nodo, buscando todos sus
%--adyacentes
generar_vecinos(Nodo):-
	%--Obtiene las componentes del nodo
	nodo_eti(Nodo,E),nodo_cam(Nodo,C),nodo_cos(Nodo,G),
	E=[PosE,_],
	forall(
		%--Para todos los adyacentes a la celda de entrada
		adyacente(E,NodoV,P),
		(
			%--Calcula el costo de llegar desde al celda de inicio de la busqueda
			%--al adyacente considerado como el costo de llegar hasta la celda
			%--de entreda mas el costo de trasladarse desde la ésta hasta la celda
			%--adyacente
			GNV is G+P,
			(
				(
					%--Si el nodo adyacente no esta abierto ni cerrado,
					%--calcula su heuristica y lo ingresa a la frontera
					not(abierto([NodoV,_,_,_])),
					not(cerrado([NodoV,_,_,_])),
					h(NodoV,HNV),
					
					assert(abierto([NodoV,HNV,[PosE|C],GNV]))
				);
				(
					%--Si el nodo adyacente ya está en la frontera,
					%--pero su costo es mayor al obtenido pasando
					%--por el nodo de entrada, entonces lo reemplaza
					%--por la nueva instancia
					abierto([NodoV,NodoVH,_,NodoVG]),
					(GNV<NodoVG),
					
					retract(abierto([NodoV,_,_,NodoVG])),
					assert(abierto([NodoV,NodoVH,[PosE|C],GNV]))
				);
				(
					%--Si el nodo adyacente ya está cerrado,
					%--pero su costo es mayor al obtenido pasando
					%--por el nodo de entrada, entonces lo reemplaza
					%--por la nueva instancia, actualizando primero
					%--el costo de todos los nodos abiertos o 
					%--cerrados que pasen por él
					cerrado([NodoV,NodoVH,_,NodoVG]),
					(GNV<NodoVG),
					
					%--Se actualizan todos los costos y caminos de
					%--los nodos que tengan un camino a través de 
					%--NodoV
					Diff is (NodoVG-GNV),
					actualizar_ramas_por(NodoV,C,Diff),
					
					retract(cerrado([NodoV,_,_,NodoVG])),
					%--assert(abierto([NodoV,NodoVH,[PosE|C],GNV])) VER
					assert(cerrado([NodoV,NodoVH,[PosE|C],GNV]))
				);
				(
					(
						%--En los casos de que este abierto o cerrado,
						%--pero el nuevo costo sea mayor o igual,
						%--no hago nada
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

%--seleccionarA(-Nodo), devuelve un Nodo seleccionado para ser expandido
%--El nodo devuelto es el de menor valor de la suma entre el costo de su
%--camino y el valor de su función heurística
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

%--buscarA(-Sol,+Prof), determina el camino solución Sol para la busqueda,
%--si Prof no supera la cota máxima de profundidad
buscarA([PosE|SSol],_Prof):-
%--Este es el caso base, si no se detuvo la busqueda por un corte en la profundidad
	continuar_busqueda,

%--Se selecciona un nodo candidato
	seleccionarA(Nodo),
	Nodo=[NodoE,_,SSol,_],
	NodoE=[PosE,DirE],

%--Y se verifica si es meta
	esMeta([PosE,DirE]),
%--En caso de serlo, la solución devuelta se compone de la solución para alcanzar el 
%--nodo Nodo mas la posición del mismo nodo

%--Por control se imprime el tiempo que tomó encontrar la solución
	get_time(TiempoAct),
	tiempo_inicio(TiempoIni),
	DifT is (TiempoAct-TiempoIni),
	write('Tiempo en encontrar la solución: '),write(DifT),nl.
	
buscarA(Sol,Prof):-
%--Si la busqueda no se cortó por la cota máxima
	continuar_busqueda,
	
%--Se determina cual esta cota máxima, y se controla
%--que no se la esté pasando
	cota_prof(CotaP),
	Prof<CotaP,

%--Se selecciona un nodo a expandir y se genera la nueva
%--frontera
	seleccionarA(Nodo),
	generar_vecinos(Nodo),

%--Se busca una solución con el nuevo estado, considerando que
%--tendrá una profundidad igual a la actual + 1
	ProfSig is Prof+1,
	buscarA(Sol,ProfSig).

%--Corta la busqueda
buscarA(Sol,Prof):-
%--Si la busqueda no se cortó por la cota máxima
	continuar_busqueda,
%--Obtengo que se superó la cota máxima
	cota_prof(CotaP),
	(Prof>CotaP;Prof=CotaP),

%--La mejor opcion es devolver un valor nulo, asi evita el backtracking
	Sol=null,
	
	write('Corté la busqueda...'),nl,

%--Evita que se siga busando una solución
	retractall(continuar_busqueda).

%--empezar(+NodoE,+Metas,-Sol,+CotaP), dado un nodo de inicio NodoE, un conjunto de
%--metas Metas compuesto por pares [Pos,Dir] y una cota máxima CotaP, intenta buscar
%--el mejor camino desde NodoE a la meta más cercana
empezar(NodoE,Metas,Sol,CotaP):-
%--Deja el estado de la busqueda en 0 para realizar una nueva
	resetear_abiertos,
	resetear_cerrados,
	resetear_metas,
	resetear_cota,
	
%--Actualiza el tiempo de inicio de la búsqueda
	get_time(TIni),
	retractall(tiempo_inicio(_)),
	assert(tiempo_inicio(TIni)),
	retractall(continuar_busqueda),
	assert(continuar_busqueda),

%--Actualiza la cota de profundidad
	retractall(cota_prof(_)),
	assert(cota_prof(CotaP)),

%--Indica todas las metas posibles
	forall(
		member(Meta,Metas),		
		assert(esMeta(Meta))
	),
	
%--Permite utilizar la versión mas eficiente de adyacente, que no genera
%--caminos para atras, calculando la frontera a partir del nodo inicial
	(
		(
			%--Si es meta el nodo inicial, la solución es un camino vacío
			esMeta(NodoE),
			Sol=[]
		);
		(
			%--Si no es meta el nodo inicial
			not(esMeta(NodoE)),
			
			NodoE=[PosE,_],
			forall(
				%--calculando la frontera a partir del nodo inicial
				adyacente_pos_ini(NodoE,NodoV,P),
				(
					h(NodoV,HNV),
					assert(abierto([NodoV,HNV,[PosE],P]))
				)
			),
			
			%--Y busca una solución desde la profundidad incial 0
			buscarA(Sol,0)
		)
	).