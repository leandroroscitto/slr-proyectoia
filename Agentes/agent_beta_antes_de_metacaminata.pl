%%-- Player-Agent Beta

%-- IMPORTACION================================================================
%--Primitivas para interacción con el entorno
:-consult(ag_primitives).
%--Predicados auxiliares
:-consult(extras_for_agents).
%--Predicados auxiliares para impresión en pantalla
:-consult(aux_imp).
%--Implementación de la busqueda A*
:-consult(busqueda).

%-- PREDICADOS DINAMICOS=======================================================
%--estado_objetos(Obs), con Obs es una lista de objetos del tipo
%--[Posicion,Objeto,UltimoTurnoVisto], con Objeto=[Tipo,Nombre,Descripción]
%--Representación interna de los objetos que fueron percibidos por el agente,
%--y cual fue el último turno en el que se los vió
:-dynamic estado_objetos/1.
%--estado_grilla(Grilla), con Grilla una lista de celdas del tipo
%--[Posicion,TipoTierra,Visitado]
%--Representación interna de la geografía del entorno percibida por el agente
:-dynamic estado_grilla/1.

%--Turno actual
:-dynamic turno_act/1.
%--Posicion actual del agente
:-dynamic pos_act/1.
%--Dirección actual del agente
:-dynamic dir_act/1.
%--Stamina actual del agente
:-dynamic sta_act/1.
%--Máxima Stamina actual del agente
:-dynamic msta_act/1.
%--Inventario actual del agente
:-dynamic inv_act/1.
%--Habilidad de pelea actual del agente
:-dynamic fs_act/1.

%--camino_meta(Camino), Camino es una lista de posiciones que seguirá el agente
%--con destino a una meta, de no presentarse alguna situación de mayor prioridad
:-dynamic camino_meta/1.
%--meta_act(Meta), Meta es una posición (o celda) de la grilla al que el agente
%--intentará llegar, siempre que no se presente una situación de mayor prioridad
:-dynamic meta_act/1.
%--tipo_meta(Tipo), Tipo pertenece {treasure,hostel,unknown}, indica el tipo
%--de meta que indica la posición
:-dynamic tipo_meta/1.
%--Representa el descubrimiento de nuevos objetos desde la última percepción
:-dynamic nuevos_objetos/1.
%--Representa el descubrimiento de nuevas celdas de la grilla
:-dynamic nuevas_celdas/0.

%--ag_name(Nombre), representa el Nombre del agente
:-dynamic ag_name/1.

%--ultimo_descanso(Turno), representa el último Turno en el que el agente
%--descansó en un hostel
:-dynamic ultimo_descanso/1.

%-- HECHOS=====================================================================
%--En un principio no existe ninguna percepción de la geografia del entorno
estado_grilla([]).
%--En un principio no se percibieron ningunos objetos
estado_objetos([]).

%--Inicialización nula de los atributos del agente (se actualizan luego de la
%--primera percepción)
turno_act(-1).
pos_act([-1,-1]).
sta_act(-1).
msta_act(-1).
dir_act(-1).
inv_act([]).
fs_act(-1).

%--En un principio no existe ninguna meta ni un camino hacia alguna
meta_act(null).
tipo_meta(null).
camino_meta([]).

%-- AUXILIARES=================================================================
%--Determina si una cosa es un tesoro
es_tesoro([treasure,_,_]).

%--Determina si una cosa es un agente
es_agente([agent,_,_]).

%--Determina si una cosa es un hostel
es_hostel([hostel,_,_]).

%--Determina el nombre de una cosa
nombre_cosa([_,Nombre,_],Nombre).

%--Determina el tipo de una cosa
tipo_cosa([Tipo,_,_],Tipo).

%--Determina la descripción de una cosa
desc_cosa([_,_,Desc],Desc).

%--get_pos_attr(+Attr,-PosAct), determina la posicion actual del agente a partir
%--de sus atributos
get_pos_attr(Attr,PosAct):-
	member([pos,PosAct],Attr).

%--get_dir_attr(+Attr,-DirAct), determina la direccion actual del agente a partir
%--de sus atributos
get_dir_attr(Attr,DirAct):-
	member([dir,DirAct],Attr).

%--get_stamina_attr(+Attr,-StaAct), determina la stamina actual del agente a partir
%--de sus atributos
get_stamina_attr(Attr,StaAct):-
	member([stamina,StaAct],Attr).

%--get_mstamina_attr(+Attr,-MStaAct), determina la maxima stamina actual del agente
%--a partir de sus atributos
get_mstamina_attr(Attr,MStaAct):-
	member([max_stamina,MStaAct],Attr).

%--get_fightskill_attr(+Attr,-FSAct), determina la habilidad de pelea actual del
%--agente a partir de sus atributos
get_fightskill_attr(Attr,FSAct):-
	member([fight_skill,FSAct],Attr).
	
%--puede_pasar_hostel(+TurnoFut), determina si es posible trasladarse a través
%--de un hostel en un turno futuro
puede_pasar_hostel(TurnoFut):-
	ultimo_descanso(UTurno),!,
	turno_act(TurnoAct),
	
	TurnosPasados is (TurnoAct-UTurno),
	DifTurnos is (TurnosPasados+TurnoFut),
	
	%--La cantidad de turnos pasados deben ser más que la cantidad de turnos
	%--necesarios para poder volver a pasar por un hostel
	msta_act(MSta),
	FET is round(MSta/2),
	FET<DifTurnos,
	
	%--También la stamina en ese turno debe ser menor que la máxima stamina
	sta_act(Sta),
	msta_act(MSta),
	(Sta-TurnoFut)<MSta.
puede_pasar_hostel(_TurnoFut):-
	%--También puede pasar por un hostel si nunca descansó en uno
	not(ultimo_descanso(_UTurno)).

%--celda_libre(+Pos,+TurnoFut), determina si una celda de la grilla esta o estará
%--libre en un turno futuro
celda_libre(Pos,TurnoFut):-
	estado_grilla(Grilla),
	member([Pos,Land,_],Grilla),
	
	%--El terreno debe ser transitable
	Land\=forest,
	Land\=water,
	
	estado_objetos(Objetos),
	%--Si hay un hostel en la posición, debe poder ser pasable en el turno ingresado
	(
		(
			member([Pos,[hostel,_,_],_],Objetos),
			puede_pasar_hostel(TurnoFut)
		);
		(
			not(member([Pos,[hostel,_,_],_],Objetos))
		)
	).

%--no_es_si_mismo(+Cosa), determina si una cosa no es un agente, y en el caso
%--de serlo que no sea él mismo
no_es_si_mismo(Cosa):-not(es_agente(Cosa)),!.
no_es_si_mismo(Cosa):-
	es_agente(Cosa),
	nombre_cosa(Cosa,NCosa),
	ag_name(NAgente),
	NCosa\=NAgente.

%--eliminar(+Elemento,+List,-ListR), elimina un elemento de una lista, pertenezca
%--o no a esta
eliminar(_Ele,[],[]).
eliminar(Ele,List,ListR):-
	append([SL1,[Ele],SL2],List),
	append([SL1,SL2],ListR).

%--mismos_objetos(+Conj1,+Conj2), determina si dos conjuntos de objetos tienen
%--las mismas cosas, sin importar el turno en que fueron vistas (si la posición)
mismos_objetos([],[]).
mismos_objetos([Obj1|SS1],S2):-
	Obj1=[Pos,Cosa,_],
	member([Pos,Cosa,_],S2),
	eliminar([Pos,Cosa,_],S2,SS2),
	mismos_objetos(SS1,SS2).

%--cantidad_ele(+Lista,-Cant), determina la cantidad de elementos de una lista
cantidad_ele([],0).
cantidad_ele([_Ele|SList],Cant):-
	cantidad_ele(SList,SCant),
	Cant is SCant+1.

%--maxFila(+Grilla,-Cant), máxima cantidad de filas de la grilla percibida
%--hasta el momento
maxFila([],0).
maxFila([Celda|SGrilla],MCF):-
	maxFila(SGrilla,CF),
	Celda=[Pos,_,_],
	Pos=[F,_C],
	
	MCF is max(CF,F).

%--maxCol(+Grilla,-Cant), máxima cantidad de columnas de la grilla percibida
%--hasta el momento
maxCol([],0).
maxCol([Celda|SGrilla],MCC):-
	maxCol(SGrilla,CC),
	Celda=[Pos,_,_],
	Pos=[_F,C],
	
	MCC is max(CC,C).

%--reemplazar(+Ele1,+Ele2,+List1,-List2) reemplaza la primera ocurrencia de Ele2 en
%--List1 por Ele1, da como resultado List2
reemplazar(Ele1,Ele2,List1,List2):-
	append([SList11,[Ele2],SList12],List1),
	append([SList11,[Ele1],SList12],List2).

%--objetos_en_pos(+Pos,-Objs), devuelve una lista de todos los objetos
%--[Tipo,Objeto,Descr] registrados en el estado interno dada una posicion
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

%--procesar_vis(+Vis,+TurAct,-VObjetos), convierte la estructura de la vision 
%--de una percepcion en una lista de objetos [Pos,Cosa,TurAct]
%--Representa la lista de objetos percibidos durante el turno actual
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

%--ir_a_pos_ady(+PosDest,-Accion), determina la acción necesaria para avanzar
%--a la posición destino a partir de la posición actual
ir_a_pos_ady(PosDest,Accion):-
	pos_act(PosAct),
	dir_act(DirAct),
	
	ady_at_cardinal(PosAct,Dir,PosDest),
	%--Si la dirección en la que se encuentra la posición es la misma que
	%--la dirección actual, la acción es avanzar
	DirAct=Dir,
	
	Accion=move_fwd.
ir_a_pos_ady(PosDest,Accion):-
	pos_act(PosAct),
	dir_act(DirAct),
	
	ady_at_cardinal(PosAct,Dir,PosDest),
	%--Si la dirección en la que se encuentra la posición no es la misma que
	%--la dirección actual, la acción es girar hacia esa dirección
	DirAct\=Dir,
	
	Accion=turn(Dir).

%--seguir_camino(-Accion), determina la accion para seguir un camino a una meta
seguir_camino(_Accion):-
	%--Si no hay camino a seguir, se retrae la meta actual y falla
	camino_meta(Camino),
	Camino=[],
	retractall(meta_act(_)),
	assert(meta_act(null)),
	retractall(tipo_meta(_)),
	assert(tipo_meta(null)),
	fail.
seguir_camino(Accion):-
	%--Si la próxima posición es igual a la posición actual del agente,
	%--la acción necesaria es la que se determina de seguir el resto del camino
	camino_meta(Camino),
	Camino=[NextPos|SCamino],
	
	pos_act(PosAct),
	NextPos=PosAct,

	retractall(camino_meta(Camino)),
	assert(camino_meta(SCamino)),
	
	seguir_camino(Accion).
seguir_camino(Accion):-
	%--Si la próxima posición es distinta a la posición actual del agente,
	%--determina la acción necesaria para desplazarse a esa posición
	camino_meta(Camino),
	Camino=[NextPos|_SCamino],
	
	pos_act(PosAct),
	NextPos\=PosAct,

	ir_a_pos_ady(NextPos,Accion).

%-- ACTUALIZACION DE ESTADOS===================================================
%--act_estado_grilla(+Vis,+PosAct), actualiza la grilla interno con las nuevas
%--celdas descubiertas en la percepcion y la posicion que fue visitada
act_estado_grilla(Vis,PosAct):-
	estado_grilla(Grilla),
	%--NCeldas representa todas las celdas de la percepción (visión) actual
	%--que no están incluidas actualmente en el estado interno del agente
	findall(
			%--Si ni siquiera está en el estado interno, no fue visitida todabía
			[Pos,Land,0],
			(
				member(Ele,Vis),
				Ele=[Pos,Land,_Cosas],
				not(member([Pos,Land,_],Grilla))
			),
			NCeldas
		),
	%--Agrega las nuevas celdas a la grilla
	append(Grilla,NCeldas,NGrilla),
	
	%--Si hay nuevas celdas representa el hecho, en otro caso también
	(
		(
			NCeldas\=[],
			assert(nuevas_celdas)
		);
		(
			NCeldas=[],
			retractall(nuevas_celdas)
		)
	),

	%--Y reemplaza la celda de la posición actual para representar que fue
	%--visitada
	member([PosAct,Land,Vist],NGrilla),
	reemplazar([PosAct,Land,1],[PosAct,Land,Vist],NGrilla,NGrillaV),
	
	retractall(estado_grilla(Grilla)),
	assert(estado_grilla(NGrillaV)).

%--VER: Actualizar solo los visitados una vez que toda la grilla fue descubierta
%--act_estado_visita(+Pos), actualiza la grilla determinando la posición
%--que fue visitada
act_estado_visita(Pos):-
	estado_grilla(Grilla),
	member([Pos,Land,Vist],Grilla),
	reemplazar([Pos,Land,1],[Pos,Land,Vist],Grilla,NGrilla),
	retractall(estado_grilla(Grilla)),
	assert(estado_grilla(NGrilla)).

%--act_estado_objetos(+TurAct,+Vis), actualiza la representación interna
%--de los objetos del entorno, agregando o modificando los objetos a partir
%--de la visión de la percepción actual
act_estado_objetos(TurAct,Vis):-
	%--Obtiene la lista de objetos observados en la percepción
	procesar_vis(Vis,TurAct,VObjetos),
	%--Obtiene la representación interna de los objetos percibidos por el agente
	estado_objetos(EIObjetos),
	
	%--Extrae los objetos de la visión que no se encuentran en el estado interno,
	%--no importa la posición
	%--Se queda con la ultima posicion y turno conocido del objeto
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
	%--Extrae los objetos de la visión que se encuentran en el estado interno
	%--en la misma posición y me quedo con el último turno conocido (Turno actual)
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
	%--Extrae los objetos del estado interno que no contradicen la visualización,
	%--es decir no aparecen en la visualización y no deberían encontrarse según el
	%--rango de visión
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
	
	%--La nueva representación de los objetos percibidos es la unión de los
	%--tres conjuntos construidos
	append([NObjetosNoRep,NObjetosRep,NObjetosDif],NObjetos),
	
	%--Indica el hecho de que se modificó el conjunto de objetos percibidos
	%--Solo considera los objetos y la posición, no el turno en que fueron
	%--vistos
	/*
	(
		(
			mismos_objetos(EIObjetos,NObjetos),
			retractall(nuevos_objetos(_)),
			assert(nuevos_objetos(0))
		);
		(
			retractall(nuevos_objetos(_)),
			assert(nuevos_objetos(1)),
			write('Encontre nuevos objetos...'),nl)
	),
	*/
	%--VER
	(
		(
			member([_,Cosa,_],NObjetos),
			es_tesoro(Cosa),
			retractall(nuevos_objetos(_)),
			assert(nuevos_objetos(1)),
			write('Encontre nuevos objetos...')
		);
		(
			retractall(nuevos_objetos(_)),
			assert(nuevos_objetos(0))
		)
	),
	
	retractall(estado_objetos(EIObjetos)),
	assert(estado_objetos(NObjetos)).

%--act_estado_ent(+Perc), actualiza la representación interna del entorno
act_estado_ent(Perc):-
	Perc=[Tur,Vis,Attr,_Inv],
	
	%--Actualiza el turno actual
	retractall(turno_act(_)),
	assert(turno_act(Tur)),
	
	%--Actualiza la grilla y la representación de los objetos internas
	get_pos_attr(Attr,PosAct),
	act_estado_grilla(Vis,PosAct),
	act_estado_objetos(Tur,Vis).

%--act_estado_ag(+Perc), actualiza el estado interno de los atribtutos del agente
act_estado_ag(Perc):-
	Perc=[_Tur,_Vis,Att,Inv],
	
	%--Actualiza la posición del agente
	get_pos_attr(Att,Pos),
	retractall(pos_act(_)),
	assert(pos_act(Pos)),
	
	%--Actualiza la dirección del agente
	get_dir_attr(Att,Dir),
	retractall(dir_act(_)),
	assert(dir_act(Dir)),
	
	%--Actualiza la stamina actual del agente
	get_stamina_attr(Att,Sta),
	retractall(sta_act(_)),
	assert(sta_act(Sta)),
	
	%--Actualiza la máxima stamina actual del agente
	get_mstamina_attr(Att,MSta),
	retractall(msta_act(_)),
	assert(msta_act(MSta)),
	
	%--Actualiza el inventario actual del agente
	retractall(inv_act(_)),
	assert(inv_act(Inv)).

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
	member([PosDir,_Land,Vist],Grilla),
	%--Land\=forest,Land\=water,
	celda_libre(PosDir,0),
	Vist=0,
	
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
	member([PosDir,_Land,Vist],Grilla),
	%--Land\=forest,Land\=water,
	celda_libre(PosDir,0),
	Vist=0,
	
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
	member([PosDir,_Land,Vist],Grilla),
	%--Land\=forest,Land\=water,
	celda_libre(PosDir,0),
	Vist=0,
	
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
	member([PosDir,_Land,_Vist],Grilla),
	%--Land\=forest,Land\=water,
	celda_libre(PosDir,0),
	
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
	member([PosDir,_Land,_Vist],Grilla),
	%--Land\=forest,Land\=water,
	celda_libre(PosDir,0),
	
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
	member([PosDir,_Land,_Vist],Grilla),
	%--Land\=forest,Land\=water,
	celda_libre(PosDir,0),
	
	%--No es transitable, gira a la derecha
	Action=turn(DirDer).

%--adyacente(+Pos,-PosV), obtiene las celdas continuas de la celda Pos
adyacente(Pos,PosV):-
	dir_posible(Dir),
	ady_at_cardinal(Pos,Dir,PosV).

%--Determina el porcentaje de la grilla descubierto (visto, no necesariamente
%--visitado)
porcentaje_grilla(Porc):-
	estado_grilla(Grilla),
	
	maxFila(Grilla,MF),
	maxCol(Grilla,MC),
	
	cantidad_ele(Grilla,CantE),
	TamGrilla is MF*MC,
	Porc is ((CantE/TamGrilla)*100).

%--caminata(+Action), caminata al azar que prioriza lugares que no
%--visito todabia
caminata(Action):-
	pos_act(PosAg),
	dir_act(DirAg),
	
	estado_grilla(Grilla),
	
	write('Ya descubrí toda la grilla? '),
	%--Si no descubrió toda la grilla, busca explorar
	porcentaje_grilla(Porc),
	Porc<100,
	
	%--Busca todos las celdas adyacentes a celdas conocidas, que no fueron
	%--percibidas todabía
	findall(
		[Pos,Dir],
		(
			member(Celda,Grilla),
			Celda=[Pos,Land,_],

			Land\=forest,Land\=water,

			%--adyacente(Pos,PosAdy),
			ady_at_cardinal(Pos,Dir,PosAdy),
			CeldaAdy=[PosAdy,_,_],
			not(member(CeldaAdy,Grilla))
		),
		Inexplorados
	),
	
	write('No, y creo que hay terreno por explorar cerca de '),
	write(Inexplorados),nl,
	write('Voy a ver si puedo llegar hasta allí'),nl,
	
	%--Busca el camino mas corto a una de las celdas inexploradas
	empezar([PosAg,DirAg],Inexplorados,SolR,100),
	reverse(SolR,Sol),
	
	%--Actualiza la meta actual
	SolR=[Pos|_],
	retractall(meta_act(_)),
	assert(meta_act(Pos)),
	retractall(tipo_meta(_)),
	assert(tipo_meta(unknown)),
	
	%--Actualiza el camino a seguir
	retractall(camino_meta(_)),
	assert(camino_meta(Sol)),
	
	%--Determina la acción necesaria para avanzar en el camino obtenido
	seguir_camino(Action),
	
	write('Si puedo llegar, ya me pongo en marcha'),nl.
caminata(Action):-
	porcentaje_grilla(Porc),
	(
		(
			%--Si ya visitó todas las celdas observables, camina al azar
			Porc=100,
			write('Si, no se donde ir, voy a caminar sin rumbo'),nl
		);
		(
			%--Si no visitó todas las celdas observables, pero no encontro un camino
			Porc<100,
			write('No, no se donde ir, voy a caminar sin rumbo'),nl
		)
	),
	walkabout_accion(Action).
	
	
%-- SELECCION==================================================================	
%--Si hay un agente enfrente lo ataca
sel_accion(Action):-
	pos_act(Pos),
	dir_act(Dir),
	
	ady_at_cardinal(Pos,Dir,PosAdy),
	objetos_en_pos(PosAdy,Objetos),
	
	member(Obj,Objetos),
	Obj=[Tipo,Nombre,_Desrip],
	Tipo=agent,
	
	write('Vi al agente '),write(Nombre),write(', lo voy a atacar'),nl,
	
	Action=attack(Nombre).
%--Si hay un tesoro en el piso lo levanta
sel_accion(Action):-
	pos_act(Pos),
	
	objetos_en_pos(Pos,Objetos),
	
	member(Obj,Objetos),
	Obj=[Tipo,Nombre,_Descrip],
	Tipo=treasure,
	
	write('Esta el tesoro '),write(Nombre),
	write(' en esta posición, voy intentar levantarlo'),nl,
	
	Action=pickup(Nombre).
%--Si no esta a full de stamina, se queda esperando en un hostel
sel_accion(Action):-
	sta_act(Sta),
	msta_act(MSta),
	(Sta<MSta),
	
	pos_act(Pos),
	
	objetos_en_pos(Pos,Objetos),
	
	member(Obj,Objetos),
	Obj=[Tipo,Nombre,_Descrip],
	Tipo=hostel,
	
	write('No estoy a full de stamina, voy a descanzar en el hostel '),
	write(Nombre),nl,
	
	turno_act(Turn),
	retractall(ultimo_descanso(_)),
	assert(ultimo_descanso(Turn)),
	retractall(camino_meta(_)),%--PARCHE
	assert(camino_meta([])),%--PARCHE
	
	Action=null_action.

sel_accion(Action):-
	write('Estaba llendo a algún lado?'),nl,
	seguir_camino(Action),!,
	write('Si estaba siguiendo el camino'),nl.

sel_accion(Action):-
	write('No, mejor voy a explorar un toque a ver que hay'),nl,
	caminata(Action),!.
	
sel_accion(null_action):-
	write('Como que no voy a hacer nada'),nl.

%-- ACTUALIZAR LAS METAS DEL AGENTE============================================
eliminar_inaccesibles(Tesoros,TesAcc):-
	findall(
		Tes,
		(
			member(Tes,Tesoros),
			Tes=[Pos,_],
			adyacente(Pos,PosA),
			celda_libre(PosA,0),
			adyacente(PosA,PosAA),
			PosAA\=Pos,
			celda_libre(PosAA,0)
		),
		TesR
	),
	list_to_set(TesR,TesAcc).

act_metas:-
	write('Estoy suficientemente cansado para buscar refujio? '),
	sta_act(StaAg),
	%--msta_act(MStaAg),
	
	estado_grilla(Grilla),
	maxFila(Grilla,MF),
	maxCol(Grilla,MC),
	Dist is MF+MC,
	Dist34 is (Dist*3/4),
	
	%--LMSta is MStaAg/3+2,
	LMSta is Dist34,
	StaAg<LMSta,
	
	estado_objetos(Objetos),
	member(Obj,Objetos),
	Obj=[Pos,Cosa,_Tur],
	
	es_hostel(Cosa),
	
	meta_act(Pos),
	tipo_meta(hostel), %--VER:eliminar lo de arriba
	
	not(nuevas_celdas),
	
	nl,write('Si lo estoy, pero ya estoy en camino a un hostel'),nl.
act_metas:-
	sta_act(StaAg),
	%--msta_act(MStaAg),
	
	estado_grilla(Grilla),
	maxFila(Grilla,MF),
	maxCol(Grilla,MC),
	Dist is MF+MC,
	Dist34 is (Dist*3/4),
	
	%--LMSta is MStaAg/3+2,
	LMSta is Dist34,
	StaAg<LMSta,
	
	nl,write('Si, estoy algo cansado y voy buscar refujio...'),nl,
	
	pos_act(PosAg),
	dir_act(DirAg),
	
	findall(
		[Pos,_Dir],
		(
			estado_objetos(Objetos),
			member(Obj,Objetos),
			Obj=[Pos,Cosa,_Tur],
			
			es_hostel(Cosa)
		),
		Hosteles
	),
	
	write('Conozco algún hostel?'),
	Hosteles\=[],
	
	nl,write('Estos son los que encontre:'),write(Hosteles),nl,
	
	empezar([PosAg,DirAg],Hosteles,SolR,150),!,
	reverse(SolR,Sol),
	
	write('Siguiendo este nuevo camino:'),write(Sol),nl,
	
	SolR=[Meta|_],
	retractall(meta_act(_)),
	assert(meta_act(Meta)),
	retractall(tipo_meta(_)),
	assert(tipo_meta(hostel)),
	
	retractall(camino_meta(_)),
	assert(camino_meta(Sol)).
act_metas:-
	write('No'),nl,
	write('Habrá algun tesoro nuevo, o un mejor camino por conseguir? '),
	%--camino_meta(VMeta),
	
	nuevos_objetos(NObjs),
	NObjs=1,

	/*
	(
		nuevos_objetos;
		(nuevas_celdas,tipo_meta(treasure));
		VMeta=[];
		(
			%--PARCHEPARCHEPARCHEPARCHE
			msta_act(MSta),
			sta_act(Sta),
			(Sta=MSta)
		)
	),
	*/
		
	pos_act(PosAg),
	dir_act(DirAg),	

	findall(
		[Pos,Dir],
		(
			estado_objetos(Objetos),
			member(Obj,Objetos),
			Obj=[Pos,Cosa,_Tur],
			
			dir_posible(Dir),%--VER
			
			es_tesoro(Cosa)
		),
		Tesoros
	),
	
	%--eliminar_inaccesibles(TesorosR,Tesoros),
	
	nl,write('Conozco algún tesoro?'),
	Tesoros\=[],
	
	nl,write('Estos son los que encontre:'),write(Tesoros),nl,
	
	empezar([PosAg,DirAg],Tesoros,SolR,30),!,
	reverse(SolR,Sol),
	
	write('Voy a actualizar mis metas de riquezas...'),nl,
	write('En base a este nuevo camino a seguir:'),write(Sol),nl,
	
	SolR=[Meta|_],
	retractall(meta_act(_)),
	assert(meta_act(Meta)),
	retractall(tipo_meta(_)),
	assert(tipo_meta(treasure)),
	
	retractall(camino_meta(_)),
	assert(camino_meta(Sol)).
act_metas:-
	write('No'),nl,
	write('No hay algo nuevo que buscar?, no tengo ningun camino nuevo a seguir? '),
	camino_meta(VMeta),
	nuevos_objetos(NObjs),
	NObjs\=1,
	
	estado_objetos(Objetos),
	member(Obj,Objetos),
	Obj=[_Pos,Cosa,_Tur],
	
	not(es_tesoro(Cosa)),
	nl,write('Y lo que hay no me convence...'),nl,
	
	write('No actualizo mis metas'),nl,
	write('Continuo con el camino:'),write(VMeta),nl.
act_metas:-
	write('No'),nl,
	write('No vi nada en mis viajes? '),
	estado_objetos(Objetos),
	Objetos=[],
	nl,write('Tampoco actualizo mis metas'),nl,
	write('Quizas seguiré el camino anterior'),nl.
act_metas:-
	write('No'),nl,
	camino_meta(VMeta),
	VMeta\=[],
	write('Nada nuevo a que aspirar...'),nl,
	write('Seguiré por el camino en que venia:'),write(VMeta),nl.
act_metas:-
	write('No tengo ningún camino que seguir?'),nl,
	camino_meta(VMeta),
	VMeta=[],
	
	write('No, no hay ninguna meta'),nl,
	retractall(meta_act(_)),
	assert(meta_act(null)),
	retractall(tipo_meta(_)),
	assert(tipo_meta(null)).

%-- CICLO======================================================================
run:-
	get_percept(Perc),
	
	Perc=[TurnAct,_,_,_],
	nl,write('Turno:'),write(TurnAct),write(' --------'),nl,
	
	act_estado_ent(Perc),
	act_estado_ag(Perc),
	
	imp_info_act_ag,
	
	act_metas,!,
	
	sel_accion(Action),
    do_action(Action),
    run.

%-- INICIALIZACION ============================================================
start_ag:-
	AgName=beta,
    register_me(AgName, Status),
    !,
    write('REGISTRATION STATUS: '),
    write(Status),nl,nl,
    Status=connected,
    assert(ag_name(AgName)),
    run.
   
s:-start_ag.

start_ag_instance(InstanceID):-
	AgClassName=beta,
    AgInstanceName=..[AgClassName, InstanceID],
    register_me(AgInstanceName, Status),
    !,
    write('REGISTRATION STATUS: '),
    write(Status),nl,nl,
    Status=connected,
    assert(ag_name(AgInstanceName)),
    run.

si(InstanceID):-start_ag_instance(InstanceID).