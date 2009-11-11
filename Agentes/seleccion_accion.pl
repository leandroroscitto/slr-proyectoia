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
	
	objetos_en_pos(Pos,treasure,Objetos),
	
	member(Obj,Objetos),
	Obj=[treasure,Nombre,_Descrip],
	
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

%--sel_accion_supervivencia(+Action), si hay un agente alcanzable y él está en condiciones de pelear, lo ataca
sel_accion_supervivencia(Action):-
	%--Hay agentes cerca?
	peligro_agente,
	
	%--Verifica que este en condiciones de pelear
	estoy_condiciones_pelear,
	
	%--Elige un agente cercano al azar, que no estea inconciente
	agentes_cerca(Agentes),
	member(Agente,Agentes),
	Agente=[[_,Nombre,_Desc],Pos],
	%--Verifica que el agente no está en un hostel
	objetos_en_pos(Pos,hostel,Hosteles),
	(
		(
			%--No hay hosteles (debería haber solo uno en todo caso)
			Hosteles=[],
			
			write('Vi al agente '),write(Nombre),
			write(' en la posición '),write(Pos),
			write(', lo voy a atacar'),nl,
			
			%--Ataca al agente
			Action=attack(Nombre)
		);
		(
			%--El agente está en un hostel, no lo puede atacar
			Hosteles\=[],
			
			write('El cobarde de '),write(Nombre),
			write(' se esconde en un hostel'),nl,
			
			fail
		)
	).
%--sel_accion_supervivencia(+Action), si no esta a full de stamina, se queda esperando en un hostel
sel_accion_supervivencia(Action):-
	sta_act(Sta),
	msta_act(MSta),
	(Sta<MSta),
	
	pos_act(Pos),
	
	objetos_en_pos(Pos,hostel,Objetos),
	
	member(Obj,Objetos),
	Obj=[hostel,Nombre,_Descrip],
	
	write('No estoy a full de stamina, voy a descanzar en el hostel '),
	write(Nombre),nl,
	
	turno_act(Turn),
	retractall(ultimo_descanso(_)),
	assert(ultimo_descanso(Turn)),
	retractall(camino_meta(_)),%--PARCHE
	assert(camino_meta([])),%--PARCHE
	
	Action=null_action.
	
%--sel_accion_tesoros(+Action), si hay un tesoro en el piso lo levanta
sel_accion_tesoros(Action):-
	pos_act(Pos),
	
	objetos_en_pos(Pos,_Tipo,Objetos),
	write('Objetos en la posición: '),write(Pos),write(', '),
	write(Objetos),nl,
	
	member(Obj,Objetos),
	Obj=[Tipo,Nombre,_Descrip],
	Tipo=treasure,
	
	%--Busca que no halla ningún agente en la misma posición
	%--que esté conciente, de manera que se satisfaga la
	%--precondición del pickup
	findall(
		Nombre2,
		(
			member(Obj2,Objetos),
			Obj2=[Tipo2,Nombre2,Desc],
			Tipo2=agent,
			member([unconscious,false],Desc)
		),
		Agentes
	),
	write('Agentes: '),write(Agentes),nl,
	(
		(	
			%--No hay ningún agente, puedo levantarlo
			Agentes=[],
			
			write('Esta el tesoro '),write(Nombre),
			write(' en esta posición, voy intentar levantarlo'),nl,
			
			Action=pickup(Nombre)
		);
		(
			%--Hay otro agentes en la misma posición
			Agentes\=[],
			
			%--Lo ataca
			member(NAgente,Agentes),
			Action=attack(NAgente)
			
			/*
			Opcion de escapar, no funciona demasiado bien
			write('Hay otros agentes, en esta misma posición,'),
			write(' mejor me voy'),
			
			%--Busca regresar por donde venia
			dir_act(Dir),write('Direccion actual'),write(Dir),nl,
			dir_opuesta(Dir,DirO),write('Direccion opuesta'),write(DirO),nl,
			ady_at_cardinal(Pos,DirO,PosP),write('Posicion meta'),write(PosP),nl,
			write(' a la posición '),write(PosP),nl,

			
			%--Y sigue el camino a la posición continua
			retractall(meta_act(_)),
			assert(meta_act(PosP)),
			retractall(tipo_meta(_)),
			assert(tipo_meta(moverse)),
			
			retractall(camino_meta(_)),
			assert(camino_meta([PosP])),
			seguir_camino(Action)
			*/
		)
	).

%--sel_accion_metas(+Action), si no necesita realizar ninguna acción mas prioritaria, sigue por el camino
%--a una meta
sel_accion_metas(Action):-
	write('Estaba llendo a algún lado?'),nl,
	seguir_camino(Action), %--VER:SAQUE EL !
	write('Si estaba siguiendo el camino'),nl.

%--sel_accion_exploracion(+Action), si no hay camino a seguir, camina al azar
sel_accion_exploracion(Action):-
	write('No, mejor voy a explorar un toque a ver que hay'),nl,
	seguir_camino(Action),!.
sel_accion_exploracion(Action):-
	camino_meta(Camino),
	Camino=[],
	write('No hay ninguna meta, me pongo a caminar'),nl,
	walkabout_accion(Action).

%--sel_accion_exploracion(+Action), no hay camino a seguir, no hay celdas inexploradas, y no tiene nada que hacer
%--Nunca debería ocurrir VER
sel_accion_exploracion(null_action):-
	write('Look at you, hacker. A pathetic creature of meat and bone, panting and sweating as you run through my corridors. How can you challenge a perfect, immortal machine?'),nl.
