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
	%--Si la pr�xima posici�n es igual a la posici�n actual del agente,
	%--la acci�n necesaria es la que se determina de seguir el resto del camino
	camino_meta(Camino),
	Camino=[NextPos|SCamino],
	
	pos_act(PosAct),
	NextPos=PosAct,

	retractall(camino_meta(Camino)),
	assert(camino_meta(SCamino)),
	
	seguir_camino(Accion).
seguir_camino(Accion):-
	%--Si la pr�xima posici�n es distinta a la posici�n actual del agente,
	%--determina la acci�n necesaria para desplazarse a esa posici�n
	camino_meta(Camino),
	Camino=[NextPos|_SCamino],
	
	pos_act(PosAct),
	NextPos\=PosAct,

	ir_a_pos_ady(NextPos,Accion).

%-- SELECCION DE ACCION========================================================

%-- CAMINATA POR EL BORDE DERECHO==============================================
%--Si hay tesoro en el piso lo lenvanta
caminata_azar(Action):-
	pos_act(Pos),
	
	objetos_en_pos(Pos,treasure,Objetos),
	
	member(Obj,Objetos),
	Obj=[treasure,Nombre,_Descrip],
	
	Action=pickup(Nombre).	
%--Avanza si es posible o si todabia no lo visito
caminata_azar(Action):-
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
caminata_azar(Action):-
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
caminata_azar(Action):-
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
caminata_azar(Action):-
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
caminata_azar(Action):-
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
caminata_azar(Action):-
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
%--Si todo lo dem�s falla, girar en sentido horario,
%--en el peor de los casos el agente se cansa e intenta
%--buscar refujio
caminata_azar(Action):-
	dir_act(Dir),
	next_90_clockwise(Dir,DirDer),
	Action=turn(DirDer).

%--sel_accion_supervivencia(+Action), si es atacado y no tiene agentes alcanzable,
%--gira para encontrar a su agresor
sel_accion_supervivencia(Action):-
	%--Hay agentes atacandole
	atacantes(AgentesHost),
	AgentesHost\=[],
	
	%--Y est� en condiciones de pelear
	estoy_condiciones_pelear,
	%--Si no est� en condiciones de pelear
	%--se supone que no se quedar� parado
	%--a menos que est� en un hostel,
	%--en cuyo caso se queda hasta recuperar
	%--stamina
	
	%--Pero �l no los tiene al alcanze
	agentes_cerca(AgentesAtacables),
	AgentesAtacables=[],
	%--Si los tiene al alcanze, falla y se analiza la pr�xima
	%--situaci�n y determina si los atacar�
	
	%--Entonces girar 180 grados para ver si ve el agresor
	dir_act(Dir),
	next_90_clockwise(Dir,DirR1),
	next_90_clockwise(DirR1,DirR),
	Action=turn(DirR).
%--sel_accion_supervivencia(+Action), si hay un agente alcanzable y este agente est� en condiciones de pelear, lo ataca
sel_accion_supervivencia(Action):-
	%--Hay agentes cerca?
	peligro_agente,
	
	%--Verifica que este en condiciones de pelear
	estoy_condiciones_pelear,
	
	%--Elige un agente cercano al azar, que no estea inconciente
	agentes_cerca(Agentes),
	member(Agente,Agentes),
	Agente=[[_,Nombre,_Desc],Pos],
	%--Verifica que el agente no est� en un hostel
	objetos_en_pos(Pos,hostel,Hosteles),
	(
		(
			%--No hay hosteles (deber�a haber solo uno en todo caso)
			Hosteles=[],
			
			write('Vi al agente '),write(Nombre),
			write(' en la posici�n '),write(Pos),
			write(', lo voy a atacar'),nl,
			
			%--Ataca al agente
			Action=attack(Nombre)
		);
		(
			%--El agente est� en un hostel, no lo puede atacar
			Hosteles\=[],
			
			write('El cobarde de '),write(Nombre),
			write(' se esconde en un hostel'),nl,
			
			fail
		)
	).
%--sel_accion_supervivencia(+Action), si no esta a full de stamina, se queda esperando en un hostel,
%--a menos que halla un tesoro, en cuyo caso espera levantarlo en la selecci�n de acciones relacionadas
%--tesoros
sel_accion_supervivencia(Action):-
	sta_act(Sta),
	msta_act(MSta),
	(Sta<MSta),
	
	pos_act(Pos),
	
	objetos_en_pos(Pos,treasure,Tesoros),
	(
		%--Si no hay tesoros, continua con el predicado
		Tesoros=[];
		(
			%--En el caso de haber tesoros, falla para permitir
			%--considerar levantarlo en las selecciones de acciones
			%--relacionadas con tesoros.
			%--De todos maneras, al levantar el tesoro recuera
			%--stamina si est� detro de un hostel (es asi?)
			Tesoros=[],
			fail
		)
	),
	
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
	write('Objetos en la posici�n: '),write(Pos),write(', '),
	write(Objetos),nl,
	
	member(Obj,Objetos),
	Obj=[Tipo,Nombre,_Descrip],
	Tipo=treasure,
	
	%--Busca que no halla ning�n agente en la misma posici�n
	%--que est� conciente, de manera que se satisfaga la
	%--precondici�n del pickup
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
			%--No hay ning�n agente, puedo levantarlo
			Agentes=[],
			
			write('Esta el tesoro '),write(Nombre),
			write(' en esta posici�n, voy intentar levantarlo'),nl,
			
			Action=pickup(Nombre)
		);
		(
			%--Esta posibilidad probablemente nunca se de, de existir un 
			%--agente en la misma posici�n ser� capturado por la selecci�n
			%--de supervivencia anterior
			
			%--Hay otro agentes en la misma posici�n
			Agentes\=[],
			
			%--Lo ataca
			member(NAgente,Agentes),
			Action=attack(NAgente)
			
			/*
			Opcion de escapar, no funciona demasiado bien
			write('Hay otros agentes, en esta misma posici�n,'),
			write(' mejor me voy'),
			
			%--Busca regresar por donde venia
			dir_act(Dir),write('Direccion actual'),write(Dir),nl,
			dir_opuesta(Dir,DirO),write('Direccion opuesta'),write(DirO),nl,
			ady_at_cardinal(Pos,DirO,PosP),write('Posicion meta'),write(PosP),nl,
			write(' a la posici�n '),write(PosP),nl,

			
			%--Y sigue el camino a la posici�n continua
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

%--sel_accion_metas(+Action), si no necesita realizar ninguna acci�n mas prioritaria, sigue por el camino
%--a una meta
sel_accion_metas(Action):-
	write('Estaba llendo a alg�n lado?'),nl,
	seguir_camino(Action),
	write('Si estaba siguiendo el camino'),nl.

%--sel_accion_exploracion(+Action), si no hay camino a seguir, camina al azar
sel_accion_exploracion(Action):-
	camino_meta(Camino),
	Camino=[],
	write('No hay ninguna meta, me pongo a caminar'),nl,
	caminata_azar(Action).
%--sel_accion_exploracion(+Action), no hay camino a seguir, no hay celdas inexploradas, y no tiene nada que hacer
%--Nunca deber�a ocurrir
sel_accion_exploracion(null_action):-
	write('No hago nada, nose que pas�'),nl.
