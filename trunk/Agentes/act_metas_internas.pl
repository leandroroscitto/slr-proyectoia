%--METAS DEL AGENTE
%--Supone que se consulta desde el agente.pl

%--METAS DE SUPERVIVENCIA (DESCANSO, ATAQUE Y HUIDA)
act_metas_supervivencia:-
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
act_metas_supervivencia:-
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
	
	empezar([PosAg,DirAg],Hosteles,SolR,50),
	reverse(SolR,Sol),%--DEVERIA FALLAR ACA SI DEVUELVE null
	
	write('Siguiendo este nuevo camino:'),write(Sol),nl,
	
	SolR=[Meta|_],
	retractall(meta_act(_)),
	assert(meta_act(Meta)),
	retractall(tipo_meta(_)),
	assert(tipo_meta(hostel)),
	
	retractall(camino_meta(_)),
	assert(camino_meta(Sol)).

%--METAS DE TESOROS (BUSQUEDA PRINCIPALMENTE)
act_metas_tesoros:-
	write('No estoy tan cansado'),nl,
	nl,write('No habrá un tesoro en mis pies?'),
	
	pos_act(PosAct),
	estado_objetos(Objetos),
	member([PosAct,[treasure,_,_],_],Objetos),
	
	nl,write('Si lo hay, supongo que lo levantaré, nose...'),nl.
act_metas_tesoros:-
	write('No'),nl,
	write('Habrá algun tesoro nuevo en otro lado, o un mejor camino por conseguir? '),
	
	nuevos_tesoros,
		
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
	
	nl,write('Conozco algún tesoro? '),
	Tesoros\=[],
	
	nl,write('Estos son los que encontre:'),write(Tesoros),nl,
	
	write('Voy a ver si puedo alcanzar alguno...'),nl,
	
	empezar([PosAg,DirAg],Tesoros,SolR,40),
	reverse(SolR,Sol),%--DEVERIA FALLAR ACA SI DEVUELVE null
	
	write('Puedo alcanzar uno, voy a actualizar mis metas de riquezas...'),nl,
	write('En base a este nuevo camino a seguir:'),write(Sol),nl,
	
	SolR=[Meta|_],
	retractall(meta_act(_)),
	assert(meta_act(Meta)),
	retractall(tipo_meta(_)),
	assert(tipo_meta(treasure)),
	
	retractall(camino_meta(_)),
	assert(camino_meta(Sol)).

%--METAS DE EXPLORACION DE TERRENO INEXPLORADO
act_metas_exploracion:-
	nl,write('No conozco ninguno, o no pude encontra un camino a el (corte en la busqueda)'),nl,
	write('No hay algo nuevo que buscar?, no tengo ningun camino nuevo a seguir? '),
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
	empezar([PosAg,DirAg],Inexplorados,SolR,60),
	reverse(SolR,Sol),%--DEVERIA FALLAR ACA SI DEVUELVE null
	
	write('Si puedo llegar, ya me pongo en marcha, creo...'),nl,
	write('Este sería el camino: '),write(Sol),nl,
	
	%--Actualiza la meta actual
	SolR=[Pos|_],
	retractall(meta_act(_)),
	assert(meta_act(Pos)),
	retractall(tipo_meta(_)),
	assert(tipo_meta(unknown)),
	
	%--Actualiza el camino a seguir
	retractall(camino_meta(_)),
	assert(camino_meta(Sol)).
act_metas_exploracion:-
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
	fail.

%--METAS GENERALES
%--Si la acción previa fue avanzar, y la posición no cambio,
%--supone que está en la situación del hostel bloqueado, (u otra razón
%--no permite atravezar la posición), e intenta buscar un camino por un costado
act_metas_bloqueadas:-
	pos_act(PosAct),
	dir_act(DirAct),
	pos_previa(PosPrev),
	accion_previa(AccPrev),
	
	AccPrev=move_fwd,
	PosAct=PosPrev,
	
	write('Me estoy dando la cabeza contra una pared y no puedo pasar...'),nl,
	write('Voy a intentar pasar por un costado'),nl,
	
	%--Posición frontal que parece estar bloqueada
	ady_at_cardinal(PosAct,DirAct,PosBloq),
	assert(no_transitable(PosBloq)),
	
	%--Busca un nuevo camino por un costado
	meta_act(Meta),
	empezar([PosAct,DirAct],[Meta],SolR,80),
	reverse(SolR,Sol),
	
	write('Si hay forma de pasar, voy a mandarme'),nl,
	
	%--Ya no necesito saber que está bloqueado
	retract(no_transitable(PosBloq)),
	
	%--Actualiza el camino a seguir
	retractall(camino_meta(_)),
	assert(camino_meta(Sol)).
act_metas_generales:-
	nl,write('No vi nada en mis viajes? '),
	estado_objetos(Objetos),
	Objetos=[],
	nl,write('No, tampoco actualizo mis metas'),nl,
	write('Quizas seguiré el camino anterior'),nl.
act_metas_generales:-
	write('No'),nl,
	camino_meta(VMeta),
	VMeta\=[],
	write('Nada nuevo a que aspirar...'),nl,
	write('Seguiré por el camino en que venia:'),write(VMeta),nl.
act_metas_generales:-
	write('No tengo ningún camino que seguir?'),nl,
	camino_meta(VMeta),
	VMeta=[],
	
	write('No, no hay ninguna meta'),nl,
	retractall(meta_act(_)),
	assert(meta_act(null)),
	retractall(tipo_meta(_)),
	assert(tipo_meta(null)).