%-- SELECCION DE ACCION A REALIZAR
%--Supone que se consulta desde el agente.pl

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

%--caminata(+Action), caminata al azar que prioriza lugares que no
%--visito todabia
caminata(Action):-
	%--Determina la acción necesaria para avanzar en el camino obtenido
	seguir_camino(Action),
	
	write('Si puedo llegar, ya me pongo en marcha'),nl.
caminata(Action):-
	camino_meta(Camino),
	Camino=[],
	write('No hay ninguna meta, me pongo a caminar'),nl,
	walkabout_accion(Action).

%--sel_accion_supervivencia(+Action), si hay un agente alcanzable y él está en condiciones de pelear, lo ataca
sel_accion_supervivencia(Action):-
	pos_act(Pos),
	dir_act(Dir),
	turno_act(TurnAct),
	
	%--Si ya sabe que está en peligro, no calcula todo nuevamente
	not(peligro_agente),
	
	%--Busca agentes en todas las posiciones atacables (enfrente,
	%--diagonal derecha e izquierda) en el turno actual. Se podría implementar que ataque
	%--a alguno en particular de acuerdo a alguna condición,
	%--por ahora elige un cualquiera
	findall(
		Ag,
		(
			estado_objetos(Objetos),
			member(Obj,Objetos),
			Obj=[PosAg,Ag,TurnAct],
			Ag=[agent,_,_],
			(
				ady_at_cardinal(Pos,Dir,PosAg);
				diagd_at_cardinal(Pos,Dir,PosAg);
				diagi_at_cardinal(Pos,Dir,PosAg)
			)
		),
		Agentes
	),
	
	member(Agente,Agentes),
	Agente=[_,Nombre,_],
	
	%--Indica que hay peligro, exiten agentes en posición atacable
	%--(posiblemente ellos pueden atacar también)
	retractall(peligro_agente),
	assert(peligro_agente),
	
	%--Verifica que este en condiciones de pelear
	estoy_condiciones_pelear,
	
	write('Vi al agente '),write(Nombre),write(', lo voy a atacar'),nl,
	
	Action=attack(Nombre).

%--sel_accion_supervivencia(+Action), hay agentes demasiado cerca, y no esta en condiciones de pelear, 
%--busca refujio en un hostel
sel_accion_supervivencia(Action):-
	peligro_agente,
	not(estoy_condiciones_pelear),
	
	%--Verifica si ya está camino a un hostel
	tipo_meta(hostel),
	
	pos_act(PosAg),
	dir_act(DirAg),
	
	%--Busca todas las celdas donde exita un hostel
	findall(
		[Pos,Dir],
		(
			estado_objetos(Objetos),
			member(Obj,Objetos),
			Obj=[Pos,Cosa,_Tur],
			
			dir_posible(Dir),
			
			es_hostel(Cosa)
		),
		Hosteles
	),
	
	(
		(	
			%--Si encontró hosteles, busca un camino a alguno
			Hosteles\=[],
			empezar([PosAg,DirAg],Hosteles,SolR,40),
			(
				(
					%--Si encontró un camino, lo sigue
					SolR=[Meta|_SSolR],
					reverse(SolR,Sol),%--DEVERIA FALLAR ACA SI DEVUELVE null
					retractall(meta_act(_)),
					assert(meta_act(Meta)),
					retractall(camino_meta(_)),
					assert(camino_meta(Sol)),
					seguir_camino(Action)
				);
				(
					%--Si no encontró un camino VER
					(SolR=[];Sol=null)
				)
			)
		);
		(
			%--Si no encontró hosteles VER IDEM SolR=[]
			Hosteles=[]
		)
	).

%--sel_accion_supervivencia(+Action), si no esta a full de stamina, se queda esperando en un hostel
/* VER: REALMENTE PUEDE JODER
sel_accion_supervivencia(Action):-
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
*/
	
%--sel_accion_tesoros(+Action), si hay un tesoro en el piso lo levanta
sel_accion_tesoros(Action):-
	pos_act(Pos),
	
	objetos_en_pos(Pos,Objetos),
	
	member(Obj,Objetos),
	Obj=[Tipo,Nombre,_Descrip],
	Tipo=treasure,
	
	write('Esta el tesoro '),write(Nombre),
	write(' en esta posición, voy intentar levantarlo'),nl,
	
	Action=pickup(Nombre).

%--sel_accion_metas(+Action), si no necesita realizar ninguna acción mas prioritaria, sigue por el camino
%--a una meta
sel_accion_metas(Action):-
	write('Estaba llendo a algún lado?'),nl,
	seguir_camino(Action), %--VER:SAQUE EL !
	write('Si estaba siguiendo el camino'),nl.

%--sel_accion_exploracion(+Action), si no hay camino a seguir, busca celdas inexploradas
sel_accion_exploracion(Action):-
	write('No, mejor voy a explorar un toque a ver que hay'),nl,
	caminata(Action),!.

%--sel_accion_exploracion(+Action), no hay camino a seguir, no hay celdas inexploradas, y no tiene nada que hacer
%--Nunca debería ocurrir VER
sel_accion_exploracion(null_action):-
	write('Look at you, hacker. A pathetic creature of meat and bone, panting and sweating as you run through my corridors. How can you challenge a perfect, immortal machine?'),nl.
