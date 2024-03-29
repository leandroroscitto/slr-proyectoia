%%-- Player-Agent Alfa

%-- IMPORTACION================================================================
:-consult(ag_primitives),consult(extras_for_agents).
:-consult(aux_imp).
:-consult(busqueda).

%-- PREDICADOS DINAMICOS=======================================================
:-dynamic estado_objetos/1.
:-dynamic estado_grilla/1.

:-dynamic turno_act/1.
:-dynamic pos_act/1.
:-dynamic dir_act/1.
:-dynamic sta_act/1.
:-dynamic msta_act/1.

:-dynamic camino_meta/1.

:-dynamic ag_name/1.

%-- HECHOS=====================================================================
estado_grilla([]).
estado_objetos([]).

turno_act(-1).
pos_act([-1,-1]).
sta_act(-1).
msta_act(-1).
dir_act(-1).

camino_meta([]).

%-- AUXILIARES=================================================================
%--TEMPORAL
eliminar_rep([],[]).
eliminar_rep([Ele|SL],LNoR):-
	eliminar_rep(SL,SLNoR),
	member(Ele,SLNoR),
	LNoR=SLNoR.
eliminar_rep([Ele|SL],LNoR):-
	eliminar_rep(SL,SLNoR),
	not(member(Ele,SLNoR)),
	LNoR=[Ele|SLNoR].

%--Determina si una cosa es un tesoro
es_tesoro([treasure,_,_]).

%--Determina si una cosa es un agente
es_agente([agent,_,_]).

%--Determina si una cosa es un hostel
es_hostel([hostel,_,_]).

%--Determina el nombre de una cosa
nombre_cosa([_,Nombre,_],Nombre).

%--Determina la posicion actual del agente a partir de sus atributos
get_pos_attr(Attr,PosAct):-
	member([pos,PosAct],Attr).

%--Determina la direccion actual del agente a partir de sus atributos
get_dir_attr(Attr,DirAct):-
	member([dir,DirAct],Attr).

%--Determina la stamina actual del agente a partir de sus atributos
get_stamina_attr(Attr,StaAct):-
	member([stamina,StaAct],Attr).

%--Determina la maxima stamina actual del agente a partir de sus atributos
get_mstamina_attr(Attr,MStaAct):-
	member([max_stamina,MStaAct],Attr).

%--Determina la habilidad de pelea actual del agente a partir de sus atributos
get_fightskill_attr(Attr,FSAct):-
	member([fight_skill,FSAct],Attr).

%--Determina si una cosa no es un agente, y en el caso de serlo que no sea �l mismo
no_es_si_mismo(Cosa):-not(es_agente(Cosa)),!.
no_es_si_mismo(Cosa):-
	es_agente(Cosa),
	nombre_cosa(Cosa,NCosa),
	ag_name(NAgente),
	NCosa\=NAgente.

%--Imprime la grilla interna de las celdas conocidas
imp_grilla:-
	estado_grilla(Grilla),
	write('Grilla interna='),write(Grilla),nl,nl.

%--Imprime los objetos percibidos 
imp_objetos:-
	estado_objetos(Objs),
	write('Objetos internos='),write(Objs),nl,nl.

%--reemplazar(Ele1,Ele2,List1,List2) reemplaza la primera ocurrencia de Ele2 en
%--List1 por Ele1, da como resultado List2
reemplazar(Ele1,Ele2,List1,List2):-
	append([SList11,[Ele2],SList12],List1),
	append([SList11,[Ele1],SList12],List2).

%--Devuelve todos los objetos registrados en el estado interno dada una posicion
objetos_en_pos(Pos,Objs):-
	estado_objetos(Objetos),
	findall(
			Obj,
			(
				member(Cosa,Objetos),
				Cosa=[Pos,Obj,_Tur]
			),
			Objs
		).

%--Convierte la estructura de la vision de una percepcion en una lista
%--de objetos [Pos,Cosa,TurAct]
procesar_vis(Vis,TurAct,VObjetos):-
	findall(
		[Pos,Cosa,TurAct],
		(
			member(Item,Vis),
			Item=[Pos,_,Cosas],
			member(Cosa,Cosas)
		),
		VObjetos
	).

%--Determina la accion necesaria para avanzar a la posicion destino
ir_a_pos_ady(PosDest,Accion):-
	pos_act(PosAct),
	dir_act(DirAct),
	
	ady_at_cardinal(PosAct,Dir,PosDest),
	DirAct=Dir,
	
	Accion=move_fwd.
ir_a_pos_ady(PosDest,Accion):-
	pos_act(PosAct),
	dir_act(DirAct),
	
	ady_at_cardinal(PosAct,Dir,PosDest),
	DirAct\=Dir,
	
	Accion=turn(Dir).

%--Determina la accion para seguir un camino a una meta
seguir_camino(_Accion):-
	camino_meta(Camino),
	Camino=[],
	fail.
seguir_camino(Accion):-
	camino_meta(Camino),
	Camino=[NextPos|SCamino],
	
	pos_act(PosAct),
	NextPos=PosAct,

	retract(camino_meta(Camino)),
	assert(camino_meta(SCamino)),
	
	ir_a_pos_ady(NextPos,Accion).
seguir_camino(Accion):-
	camino_meta(Camino),
	Camino=[NextPos|_SCamino],
	
	pos_act(PosAct),
	NextPos\=PosAct,

	ir_a_pos_ady(NextPos,Accion).

%-- ACTUALIZACION DE ESTADOS===================================================
%--Actualiza la grilla con las nuevas celdas descubiertas en la percepcion y
%--la posicion que fue visitada
act_estado_grilla(Vis,PosAct):-
	estado_grilla(Grilla),
	findall(
			%--Tercer elemento si fue o no transitado
			[Pos,Land,0],
			(
				member(Ele,Vis),
				Ele=[Pos,Land,_Cosas],
				not(member([Pos,Land,_],Grilla))
			),
			NCeldas
		),
	append(Grilla,NCeldas,NGrilla),
	
	member([PosAct,Land,Vist],NGrilla),
	reemplazar([PosAct,Land,1],[PosAct,Land,Vist],NGrilla,NGrillaV),
	
	retract(estado_grilla(Grilla)),
	assert(estado_grilla(NGrillaV)).

%--Actualiza la grilla determinando la posici�n que fue visitada
act_estado_visita(Pos):-
	estado_grilla(Grilla),
	member([Pos,Land,Vist],Grilla),
	reemplazar([Pos,Land,1],[Pos,Land,Vist],Grilla,NGrilla),
	retract(estado_grilla(Grilla)),
	assert(estado_grilla(NGrilla)).

%--Actualiza la representacion interna de los objetos del entorno
act_estado_objetos(TurAct,Vis):-

	procesar_vis(Vis,TurAct,VObjetos),
	
	estado_objetos(EIObjetos),
	
	%--Extrae los objetos de la vision que no se encuentran en el estado
	%--interno, no importa la posici�n
	%--Me quedo con la ultima posicion y turno conocido
	findall(
		[Pos,Cosa,Tur],
		(
			member(Obj,VObjetos),
			Obj=[Pos,Cosa,Tur],
			no_es_si_mismo(Cosa),
			not(member([_,Cosa,_],EIObjetos))
		),
		NObjetosNoRep
	),
	%--Extrae los objetos de la vision que se encuentran en el estado interno
	%--en la misma posicion y me quedo con el ultimo turno conocido
	findall(
		[Pos,Cosa,Tur],
		(
			member(Obj,VObjetos),
			Obj=[Pos,Cosa,Tur],
			no_es_si_mismo(Cosa),
			member([Pos,Cosa,_],EIObjetos)
		),
		NObjetosRep
	),
	%--Extrae los objetos del estado interno que no se encuentran en la
	%--vision, y que no deberian encontrarse tampoco (no mira la pos en la
	%--que estarian)
	%--En caso de que la posicion es observada en la vision y los objetos
	%--no estan no los agrega
	findall(
		[Pos,Cosa,Tur],
		(
			member(Obj,EIObjetos),
			Obj=[Pos,Cosa,Tur],
			no_es_si_mismo(Cosa),
			not(member([_,Cosa,_],VObjetos)),
			not(member([Pos,_,_],Vis))
		),
		NObjetosDif
	),

	append([NObjetosNoRep,NObjetosRep,NObjetosDif],NObjetos_P),
	
	%--PARCHE
	eliminar_rep(NObjetos_P,NObjetos),
	
	/*
	write('Objetos no repetidos='),write(NObjetosNoRep),nl,
	write('Objetos repetidos='),write(NObjetosRep),nl,
	write('Objetos viejos='),write(NObjetosDif),nl,nl,
	*/
	
	write(NObjetos),nl,nl,
	
	retract(estado_objetos(EIObjetos)),
	assert(estado_objetos(NObjetos)).

%--Actualiza la representacion interna del entorno
act_estado_ent(Perc):-
	Perc=[Tur,Vis,Attr,_Inv],
	
	retract(turno_act(_)),
	assert(turno_act(Tur)),
	
	get_pos_attr(Attr,PosAct),
	act_estado_grilla(Vis,PosAct),
	act_estado_objetos(Tur,Vis).

%--Actualiza el estado interno de la informacion del agente
act_estado_ag(Perc):-
	Perc=[_Tur,_Vis,Att,_Inv],
	
	%--Actualiza la posicion del agente
	get_pos_attr(Att,Pos),
	retract(pos_act(_)),
	assert(pos_act(Pos)),
	
	%--Actualiza la direccion del agente
	get_dir_attr(Att,Dir),
	retract(dir_act(_)),
	assert(dir_act(Dir)),
	
	%--Actualiza la stamina actual del agente
	get_stamina_attr(Att,Sta),
	retract(sta_act(_)),
	assert(sta_act(Sta)),
	
	%--Actualiza la maxima stamina actual del agente
	get_mstamina_attr(Att,MSta),
	retract(msta_act(_)),
	assert(msta_act(MSta)).

%-- SELECCION DE ACCION========================================================

%-- CAMINATA POR EL BORDE DERECHO==============================================
%--Si hay tesoro en el piso lo lenvanta
walkabout_accion(Action):-
	pos_act(Pos),
	
	objetos_en_pos(Pos,Objetos),
	
	member(Obj,Objetos),
	Obj=[Tipo,Nombre,_Descrip],
	Tipo=treasure,
	
	Action=pickup(Nombre).
	
%--Avanza si es posible o si todabia no lo visito
walkabout_accion(Action):-
	%--Obtiene posicion de la celda de enfrente
	pos_act(Pos),
	dir_act(Dir),
	ady_at_cardinal(Pos,Dir,PosDir),
	
	%--Se fija que sea transitable
	estado_grilla(Grilla),
	member([PosDir,Land,Vist],Grilla),
	Land\=forest,Land\=water,Vist=0,
	
	%--Si es transitable avanza
	Action=move_fwd.

%--Si no fue posible avanzar dobla a la izquierda
walkabout_accion(Action):-
	%--Obtiene posicion de la celda de la izquierda
	pos_act(Pos),
	dir_act(Dir),
	next_90_clockwise(DirIzq,Dir),
	ady_at_cardinal(Pos,DirIzq,PosDir),
	
	%--Se fija que sea transitable
	estado_grilla(Grilla),
	member([PosDir,Land,Vist],Grilla),
	Land\=forest,Land\=water,Vist=0,
	
	%--No es transitable, gira a la izquierda
	Action=turn(DirIzq).

%--Si no fue posible doblar a la izquierda, dobla a la derecha
walkabout_accion(Action):-
	%--Obtiene posicion de la celda de la derecha
	pos_act(Pos),
	dir_act(Dir),
	next_90_clockwise(Dir,DirDer),
	ady_at_cardinal(Pos,DirDer,PosDir),
	
	%--Se fija que sea transitable
	estado_grilla(Grilla),
	member([PosDir,Land,Vist],Grilla),
	Land\=forest,Land\=water,Vist=0,
	
	%--No es transitable, gira a la derecha
	Action=turn(DirDer).

%--Avanza si es posible
walkabout_accion(Action):-
	%--Obtiene posicion de la celda de enfrente
	pos_act(Pos),
	dir_act(Dir),
	ady_at_cardinal(Pos,Dir,PosDir),
	
	%--Se fija que sea transitable
	estado_grilla(Grilla),
	member([PosDir,Land,_Vist],Grilla),
	Land\=forest,Land\=water,
	
	%--Si es transitable avanza
	Action=move_fwd.

%--Si no fue posible avanzar dobla a la izquierda
walkabout_accion(Action):-
	%--Obtiene posicion de la celda de la izquierda
	pos_act(Pos),
	dir_act(Dir),
	next_90_clockwise(DirIzq,Dir),
	ady_at_cardinal(Pos,DirIzq,PosDir),
	
	%--Se fija que sea transitable
	estado_grilla(Grilla),
	member([PosDir,Land,_Vist],Grilla),
	Land\=forest,Land\=water,
	
	%--No es transitable, gira a la izquierda
	Action=turn(DirIzq).

%--Si no fue posible doblar a la izquierda, dobla a la derecha
walkabout_accion(Action):-
	%--Obtiene posicion de la celda de la derecha
	pos_act(Pos),
	dir_act(Dir),
	next_90_clockwise(Dir,DirDer),
	ady_at_cardinal(Pos,DirDer,PosDir),
	
	%--Se fija que sea transitable
	estado_grilla(Grilla),
	member([PosDir,Land,_Vist],Grilla),
	Land\=forest,Land\=water,
	
	%--No es transitable, gira a la derecha
	Action=turn(DirDer).

%--Obtiene las celdas al rededor de la celda Pos
adyacente(Pos,PosV):-
	dir_posible(Dir),
	ady_at_cardinal(Pos,Dir,PosV).

%--Caminata al azar que prioriza lugares que no visito todabia
walkabout2_accion(Action):-
	pos_act(PosAg),
	
	estado_grilla(Grilla),
	member(Celda,Grilla),
	(Celda=[Pos,Land,0];Celda=[Pos,Land,1]),
	
	Land\=forest,Land\=water,
	
	adyacente(Pos,PosAdy),
	CeldaAdy=[PosAdy,_,_],
	not(member(CeldaAdy,Grilla)),
	
	empezar(PosAg,Pos,SolR),
	reverse(SolR,Sol),

	retract(camino_meta(_)),
	assert(camino_meta(Sol)),
	
	seguir_camino(Action).
walkabout2_accion(Action):-
	write('Pero siguio por el WALK1'),nl,nl,
	walkabout_accion(Action).
	
%-- SELECCION==================================================================	
%--Si hay un tesoro en el piso lo levanta
sel_accion(Action):-
	pos_act(Pos),
	
	objetos_en_pos(Pos,Objetos),
	
	member(Obj,Objetos),
	Obj=[Tipo,Nombre,_Descrip],
	Tipo=treasure,
	
	Action=pickup(Nombre).
%--Si no esta a full de stamina, se queda esperando en un hostel
sel_accion(Action):-
	sta_act(Sta),
	msta_act(MSta),
	(Sta<MSta),
	
	pos_act(Pos),
	
	objetos_en_pos(Pos,Objetos),
	
	member(Obj,Objetos),
	Obj=[Tipo,_Nombre,_Descrip],
	Tipo=hostel,
	
	Action=null_action.

sel_accion(Action):-
	seguir_camino(Action),!.

sel_accion(Action):-
	write('Entro al WALK2'),nl,nl,
	walkabout2_accion(Action),!.
	
sel_accion(null_action).

%-- ACTUALIZAR LAS METAS DEL AGENTE============================================
act_metas:-
	sta_act(StaAg),
	msta_act(MStaAg),
	
	estado_grilla(Grilla),
	maxFila(Grilla,MF),
	maxCol(Grilla,MC),
	Dist is MF+MC,
	Dist34 is (Dist*3/4),
	
	%--LMSta is MStaAg/3+2,
	LMSta is Dist34,
	StaAg<LMSta,
	
	pos_act(PosAg),
	
	estado_objetos(Objetos),
	member(Obj,Objetos),
	Obj=[Pos,Cosa,_Tur],
	
	es_hostel(Cosa),
	
	empezar(PosAg,Pos,SolR),
	reverse(SolR,Sol),
	
	retract(camino_meta(_)),
	assert(camino_meta(Sol)).
act_metas:-
	pos_act(PosAg),
	
	estado_objetos(Objetos),
	member(Obj,Objetos),
	Obj=[Pos,Cosa,_Tur],
	
	es_tesoro(Cosa),
	
	empezar(PosAg,Pos,SolR),
	reverse(SolR,Sol),
	
	retract(camino_meta(_)),
	assert(camino_meta(Sol)).
act_metas:-		
	estado_objetos(Objetos),
	member(Obj,Objetos),
	Obj=[_Pos,Cosa,_Tur],
	
	not(es_tesoro(Cosa)).
act_metas:-
	estado_objetos(Objetos),
	Objetos=[].

%-- CICLO======================================================================
cantidad_ele([],0).
cantidad_ele([_Ele|SList],Cant):-
	cantidad_ele(SList,SCant),
	Cant is SCant+1.

maxFila([],0).
maxFila([Celda|SGrilla],MCF):-
	maxFila(SGrilla,CF),
	Celda=[Pos,_,_],
	Pos=[F,_C],
	
	MCF is max(CF,F).

maxCol([],0).
maxCol([Celda|SGrilla],MCC):-
	maxCol(SGrilla,CC),
	Celda=[Pos,_,_],
	Pos=[_F,C],
	
	MCC is max(CC,C).

imp_porcentaje_grilla:-
	estado_grilla(Grilla),
	
	maxFila(Grilla,MF),
	maxCol(Grilla,MC),
	
	cantidad_ele(Grilla,CantE),
	TamGrilla is MF*MC,
	Porc is ((CantE/TamGrilla)*100),
	write('Porcentaje='),write(Porc),nl,nl.

run:-
	get_percept(Perc),
	
	act_estado_ent(Perc),
	act_estado_ag(Perc),
	
	act_metas,!,
	
	ag_name(AgName),
	display_ag(AgName,Perc),
	imp_porcentaje_grilla,
	
	camino_meta(Cam),write('Camino='),write(Cam),nl,nl,
	
	sel_accion(Action),
    do_action(Action),
    run.

%-- INICIALIZACION ============================================================
start_ag:-
	AgName=alfa,
    register_me(AgName, Status),
    !,
    write('REGISTRATION STATUS: '),
    write(Status),nl,nl,
    Status=connected,
    assert(ag_name(AgName)),
    run.
   
s:-start_ag.

start_ag_instance(InstanceID):-
	AgClassName=alfa,
    AgInstanceName=..[AgClassName, InstanceID],
    register_me(AgInstanceName, Status),
    !,
    write('REGISTRATION STATUS: '),
    write(Status),nl,nl,
    Status=connected,
    assert(ag_name(AgInstanceName)),
    run.

si(InstanceID):-start_ag_instance(InstanceID).