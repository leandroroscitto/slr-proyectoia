%--METAS DEL AGENTE
%--Supone que se consulta desde el agente.pl

:-consult(busqueda).

%--Determina si fueron generadas las landmarks de la grilla a utilizar
:-dynamic land_generadas/0.
%--landmark(-Pos,-Num), indica si una posici�n Pos es una landmark, y que n�mero Num tiene asociado
:-dynamic landmark/2.
%--max_num_landmark(-Num), indica la cantidad Num de landmarks en el estado interno
:-dynamic max_num_landmark/1.

%--proxima_landmark(-Pos), devuelve la posici�n Pos de una landmark, de acuerdo al turno actual
proxima_landmark(Pos):-
	turno_act(TurnAct),
	max_num_landmark(MaxNL),
	N is mod(TurnAct,MaxNL),
	landmark(Pos,N).

%--pos_aprox_pasable(+Grilla,+Pos,-PosAP), dada la Grilla con el estado interno del terreno del agente y una posici�n Pos,
%--determina una posici�n PosAP. PosAP es Pos si esta �ltima es transitable, en otro caso busca alrededor de Pos
%--por una posici�n transitable. En el caso de no existir ninguna celda que cumpla con esta condici�n, falla.
pos_aprox_pasable(Grilla,[PosF,PosC],[PosF,PosC]):-
	%--Caso base: Pos es transitable
	Pos=[PosF,PosC],
	member([Pos,Land,_],Grilla),
	Land\=water,Land\=forest.
pos_aprox_pasable(Grilla,[PosF,PosC],[PosAF,PosAC]):-
	%--Si Pos no es transitable
	Pos=[PosF,PosC],
	member([Pos,Land,_],Grilla),
	(Land=water;Land=forest),
	
	findall(
		%--Busca todas las posiciones en un radio de 1 alrededor de Pos,
		%--por celdas que sean transitables
		Pos,
		(
			((PF is PosF);(PF is PosF-1);(PF is PosF+1)),
			((PC is PosC);(PC is PosC-1);(PC is PosC+1)),
			Pos=[PF,PC],
			member([Pos,Land,_],Grilla),
			Land\=water,Land\=forest
		),
		PasablesCercanas
	),
	
	%--Elije cualquiera de ellas, y en el caso de no existir ninguna
	%--falla.
	member([PosAF,PosAC],PasablesCercanas).

%--generar_landmarks(+Grilla,+Num,+MaxCF,+MaxCC,+CantF,+CantC):
%--Grilla: es la grilla con el estado interno del terreno del agente
%--Num: cantidad de landmarks designadas hasta el momento
%--MaxCF: m�xima cantidad de landmarks por 'fila'
%--MaxCC: m�xima cantidad de landmarks por 'columna'
%--CantF: cantidad de landmarks restantes en la 'fila'
%--CantC: cantidad de landmarks restantes en la 'columna'
%--En resumen, el predicado dado una grilla de N*M, determina la 
%--posici�n de una serie de landmarks en la grilla que se utilizar�n
%--para recorrer el entorno una vez completados los objetivos.
%--Intenta distribuir N/5 landmarks a lo ancho por M/5 landmarks a lo
%--alto de manera uniforme, pero no asegura que se encuentren todas las
%--posiciones.
generar_landmarks(_Grilla,Num,_MaxCF,_MaxCC,_CantF,CantC):-
	%--Caso base: ya distribui� todas las landmarks posibles
	CantC=0,
	%--Indica la cantidad de posiciones que fueron marcadas
	assert(max_num_landmark(Num)).
generar_landmarks(Grilla,Num,MaxCF,MaxCC,CantF,CantC):-
	%--Caso en que ya se distribuyeron todas las posiciones para una 
	%--columna, se comienza con la pr�xima columna.
	CantF=0,
	CantC>0,
	
	CantC1 is CantC-1,
	generar_landmarks(Grilla,Num,MaxCF,MaxCC,MaxCF,CantC1).
generar_landmarks(Grilla,Num,MaxCF,MaxCC,CantF,CantC):-
	%--Caso en el que es necesario determinar una posici�n
	CantF>0,

	PosF is ((CantF*5)-2),
	PosC is ((CantC*5)-2),
	
	%--A partir de una posici�n ideal (para que queden distribuidas de forma
	%--uniforme) determina si es transitable, y si no lo es indica una posici�n
	%--cercana que lo sea.
	pos_aprox_pasable(Grilla,[PosF,PosC],PosAp),
	
	(
		(
			%--Si la posici�n no est� marcada, lo indica
			not(landmark(PosAp,_)),
			assert(landmark(PosAp,Num))
		);
		landmark(PosAp,_)
	),
	
	%--Genera el resto de las posiciones
	CantF1 is CantF-1,
	Num1 is Num+1,
	generar_landmarks(Grilla,Num1,MaxCF,MaxCC,CantF1,CantC).
generar_landmarks(Grilla,Num,MaxCF,MaxCC,CantF,CantC):-
	%--Si no puede determinar una posici�n transitable cercana,
	%--simplemente no la agrega, y sigue con la pr�xima posici�n
	%--deseada.
	CantF>0,

	PosF is ((CantF*5)-2),
	PosC is ((CantC*5)-2),
	
	not(pos_aprox_pasable(Grilla,[PosF,PosC],_PosAp)),
	
	CantF1 is CantF-1,
	generar_landmarks(Grilla,Num,MaxCF,MaxCC,CantF1,CantC).

%--METAS DE RECORRIDO DE LA GRILLA POST-EXPLORACION
%--En un principio el agente evaluar� estas metas una vez que recorri� toda la grilla,
%--y no tiene ni tesoros ni otras prioridades que atender. En esta situaci�n se pone
%--a recorrer la grilla de manera de cubrir la mayor cantidad de espacio con su visi�n
%--y asi poder encontrar otros agentes u tesoros nuevos (que se le cayeron a otros agentes)
%--La primera vez que puede evaluarse, se generan las landmarks.Estas son posiciones que 
%--de alguna manera sirven al agente como guias para ir recorriendo la grilla de manera
%--eficiente y un poco al azar (depende del n�mero del turno actual cual ser� la landmark
%--destino).
%--Luego de generar las posiciones, se busca el mejor camino a alguna de ellas.
act_metas_landmarks:-
	write('Ya recorr� toda la grilla?'),nl,
	%--Si ya recorri� toda la grilla
	porcentaje_grilla(Porc),
	Porc=100,
	write('Si, habr� determinados puntos de referencia ya?'),nl,
	
	%--Y todab�a no gener� las posiciones
	not(land_generadas),
	
	write('No, me voy a fijar una manera de recorrer toda la grilla'),nl,
	
	%--Genera las posiciones o landmarks
	estado_grilla(Grilla),
	maxFila(Grilla,MF),
	maxCol(Grilla,MC),
	
	CantLandF is ceil(MF/5),
	CantLandC is ceil(MC/5),
	
	generar_landmarks(Grilla,0,CantLandF,CantLandC,CantLandF,CantLandC),
	
	forall(
		landmark(Pos,N),
		(
			write('landmark #'),write(N),write(': '),write(Pos)
		)
	),nl,
	write('Ya lo hice, deber�a ver cual es mi pr�ximo destino...'),nl,
	
	assert(land_generadas),
	%--Y se proceda a buscar el camino a alguna de ellas
	act_metas_landmarks.
act_metas_landmarks:-
	%--Si ya esta en camino a una meta, no modifica nada
	
	%--Si ya recorri� toda la grilla
	porcentaje_grilla(Porc),
	Porc=100,
	%--Y ya se generaron las posiciones
	land_generadas,
	write('Si ya determin� todos las landmarks'),nl,
	
	pos_act(PosAct),
	camino_meta(CMeta),
	(CMeta\=[],CMeta\=[PosAct]),
	write('Pero ya estoy en camino a un meta, mejor no hago ning�n nuevo plan'),nl.
	
act_metas_landmarks:-
	%--Si ya recorri� toda la grilla
	porcentaje_grilla(Porc),
	Porc=100,
	%--Y ya se generaron las posiciones
	land_generadas,
	
	%--No debe haber metas pendientes
	pos_act(PosAct),
	camino_meta(CMeta),
	(CMeta=[];CMeta=[PosAct]),
	
	pos_act(PosAct),
	dir_act(DirAct),
	
	%--Determina la pr�xima posici�n a visitar
	proxima_landmark(PosLM),
	write('Mi pr�xima parada es ...'),write(PosLM),nl,
	write('Voy a buscar el mejor camino'),nl,
	%--Y busca un camino a ella, con una profundidad de 150,
	%--aunque para esta altura el agente ya conoce toda la 
	%--grilla, asi que de existir un camino lo encontrar�
	empezar([PosAct,DirAct],[[PosLM,_DirLM]],SolR,150),
	reverse(SolR,Sol),
	%--De encontrar uno lo inidica como la pr�xima meta
	write('Ya lo encontr�, voy a seguirlo supongo...'),
	SolR=[Meta|_],
	retractall(meta_act(_)),
	assert(meta_act(Meta)),
	retractall(tipo_meta(_)),
	assert(tipo_meta(landmark)),
	
	retractall(camino_meta(_)),
	assert(camino_meta(Sol)).
act_metas_landmarks:-
	%--En el caso de que falle todo, no se encontr� ninguna landmark en la
	%--generaci�n o no se encontr� el camino, no se hace nada
	porcentaje_grilla(Porc),
	Porc=100,
	
	land_generadas,
	write('Pero no hab�a ninguna al final, o no encontr� un camino a alguna de ellas'),nl.

%--METAS DE SUPERVIVENCIA (DESCANSO, ATAQUE Y HUIDA)
%--Si el agente considera que est� demasiado cansado, busca refujio en el hostel conocido
%--mas cercano.
act_metas_supervivencia:-
	write('Estoy suficientemente cansado para buscar refujio? '),
	
	%--Si ya esta en camino a un hostel, posiblemente este cansado, asi que
	%--continua con el camino.
	tipo_meta(hostel),
	not(nuevas_celdas),
	
	nl,write('Debo estarlo, ya estoy en camino a un hostel'),nl.
act_metas_supervivencia:-
	%--A partir de la stamina actual del agente, y el tama�a de la grilla conocida,
	%--determina que debe buscar refujio si su stamina actual es menor que 3/4
	%--de la altura m�s lo ancho de la grilla conocida.
	%--Se podr�a haber utilizado la busqueda A* para encontrar el hoste m�s
	%--cercano y determinar si el costo de llegar a el no supera la stamina
	%--actual, pero el costo de realizar esta consulta ser�a demasiado
	%--considerando que debe realizar mas cosas en el turno.
	sta_act(StaAg),
	
	estado_grilla(Grilla),
	maxFila(Grilla,MF),
	maxCol(Grilla,MC),
	Dist is MF+MC,
	Dist34 is (Dist*3/4),

	LMSta is Dist34,
	StaAg<LMSta,
	
	nl,write('Si, estoy algo cansado y voy buscar refujio...'),nl,
	
	pos_act(PosAg),
	dir_act(DirAg),
	
	%--A partir de todos los hosteles conocidos busca el menor camino
	%--a alguno de ellos, que es lo mismo que decir el mejor camino
	%--al hostel mas cercano.
	estado_objetos(Objetos),
	findall(
		[Pos,Dir],
		(
			member(Obj,Objetos),
			Obj=[Pos,Cosa,_Tur],
			
			%--Dado que el aloritmo del A* busca metas con una posici�n
			%--y DIRECCION especifica, se determinan para cada posici�n
			%--las cuatro direcciones posibles.
			dir_posible(Dir),
			
			es_hostel(Cosa)
		),
		Hosteles
	),
	
	write('Conozco alg�n hostel?'),
	Hosteles\=[],
	
	nl,write('Estos son los que encontre:'),write(Hosteles),nl,
	
	%--Se utilza una profundidad de 50 para buscar hosteles, ya que
	%--por lo general estar�n en zonas accecibles desde el momento
	%--en que son vistos
	empezar([PosAg,DirAg],Hosteles,SolR,50),
	reverse(SolR,Sol),
	
	write('Siguiendo este nuevo camino:'),write(Sol),nl,
	%--En el caso de encontrar el camino, indica las nuevas metas
	SolR=[Meta|_],
	retractall(meta_act(_)),
	assert(meta_act(Meta)),
	retractall(tipo_meta(_)),
	assert(tipo_meta(hostel)),
	
	retractall(camino_meta(_)),
	assert(camino_meta(Sol)).

%--METAS DE TESOROS (BUSQUEDA)
%--Determina si tiene accesible cualquier tesoro, y busca caminos a ellos
act_metas_tesoros:-
	%--Caso en el que hay un tesoro en la celda actual del agente,
	%--No indica nada especial, supone que en la etapa de selecci�n
	%--de acci�n eligir� levantarlo, si no surge ninguna otra situaci�n
	%--de mayor prioridad
	write('No estoy tan cansado!'),nl,
	nl,write('No habr� un tesoro en mis pies?'),
	
	pos_act(PosAct),
	estado_objetos(Objetos),
	member([PosAct,[treasure,_,_],_],Objetos),
	
	nl,write('Si lo hay, supongo que lo levantar�, nose...'),nl.
act_metas_tesoros:-
	%--Caso en el que no hay tesoro en la misma celda del agente,
	%--va a buscar uno.
	write('No'),nl,
	write('Habr� algun tesoro nuevo en otro lado, o un mejor camino por conseguir? '),
	
	%--Si hay tesoros que fueron vistos pero no levantados todabia
	nuevos_tesoros,
		
	pos_act(PosAg),
	dir_act(DirAg),	

	%--A partir de todos los tesoros conocidos busca el menor camino
	%--a alguno de ellos, que es lo mismo que decir el mejor camino
	%--al tesoros mas cercano.
	estado_objetos(Objetos),
	findall(
		[Pos,Dir],
		(
			member(Obj,Objetos),
			Obj=[Pos,Cosa,_Tur],
			
			%--Dado que el aloritmo del A* busca metas con una posici�n
			%--y DIRECCION especifica, se determinan para cada posici�n
			%--las cuatro direcciones posibles.
			dir_posible(Dir),
			
			es_tesoro(Cosa)
		),
		Tesoros
	),
	
	nl,write('Conozco alg�n tesoro? '),
	Tesoros\=[],
	
	nl,write('Estos son los que encontre:'),write(Tesoros),nl,
	
	write('Voy a ver si puedo alcanzar alguno...'),nl,
	
	%--Busca el menor camino a un tesoro, con una profundidad de 40,
	%--que de acuerdo a varias pruebas evita que se pierdan turnos
	%--en los casos que no hay camino posible.
	empezar([PosAg,DirAg],Tesoros,SolR,40),
	reverse(SolR,Sol),
	
	write('Puedo alcanzar uno, voy a actualizar mis metas de riquezas...'),nl,
	write('En base a este nuevo camino a seguir:'),write(Sol),nl,
	
	%--Si encontr� una meta lo indica
	SolR=[Meta|_],
	retractall(meta_act(_)),
	assert(meta_act(Meta)),
	retractall(tipo_meta(_)),
	assert(tipo_meta(treasure)),
	
	retractall(camino_meta(_)),
	assert(camino_meta(Sol)).

%--METAS DE EXPLORACION DE TERRENO INEXPLORADO
%--Estas metas buscan mapear todo el terreno a la grilla interna que mantiene el agente,
%--mediante la percepci�n del entorno.
%--En una forma similar a las metas de tesoros y hosteles, busca el camino mas corto
%--a una posici�n no explorada (en realidad a una posici�n explorada que est� continua
%--a una no explorada).
act_metas_exploracion:-
	nl,write('No conozco ninguno, o no pude encontra un camino a el (corte en la busqueda)'),nl,
	write('No hay algo nuevo que buscar?, no tengo ningun camino nuevo a seguir? '),
	pos_act(PosAg),
	dir_act(DirAg),
	
	estado_grilla(Grilla),
	
	write('Ya descubr� toda la grilla? '),
	%--Si no descubri� toda la grilla, busca explorar
	porcentaje_grilla(Porc),
	Porc<100,
	
	%--Busca todos las celdas adyacentes a celdas conocidas, que no fueron
	%--percibidas todab�a
	findall(
		[Pos,Dir],
		(
			%--Busca las celdas percibidas en la grilla...
			member(Celda,Grilla),
			Celda=[Pos,Land,_],
			
			%--que sean transitables...
			Land\=forest,Land\=water,

			%--y que posean una celda adyacente que no 
			%--pertenece a la grilla conocida
			ady_at_cardinal(Pos,Dir,PosAdy),
			CeldaAdy=[PosAdy,_,_],
			not(member(CeldaAdy,Grilla))
		),
		Inexplorados
	),
	
	write('No, y creo que hay terreno por explorar cerca de '),
	write(Inexplorados),nl,
	write('Voy a ver si puedo llegar hasta all�'),nl,
	
	%--Busca el camino mas corto a una de las celdas inexploradas,
	%--Dado que muchas veces la celda puede estar en el otro extremo
	%--de la grilla, se le permite una cota de profundadid mas
	%--grande
	empezar([PosAg,DirAg],Inexplorados,SolR,60),
	reverse(SolR,Sol),
	
	write('Si puedo llegar, ya me pongo en marcha, creo...'),nl,
	write('Este ser�a el camino: '),write(Sol),nl,
	
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
			%--Si ya visit� todas las celdas observables, camina al azar
			Porc=100,
			write('Si, no se donde ir, voy a caminar por ah�'),nl
		);
		(
			%--Si no visit� todas las celdas observables, pero no encontro un camino
			Porc<100,
			write('No, no se donde ir, voy a caminar por ah�'),nl
		)
	),
	fail.

%--METAS GENERALES
%--Si la acci�n previa fue avanzar, y la posici�n no cambio,
%--supone que est� en la situaci�n del hostel bloqueado, (u otra raz�n
%--no permite atravezar la posici�n), e intenta buscar un camino por un costado
act_metas_bloqueadas:-
	%--En el caso de que este tratando de llegar a un hostel y este no me
	%--deja pasar, no tengo opci�n, tengo que esperar
	tipo_meta(hostel),
	fail. %--VER
	
act_metas_bloqueadas:-
	pos_act(PosAct),
	dir_act(DirAct),
	pos_previa(PosPrev),
	accion_previa(AccPrev),
	
	AccPrev=move_fwd,
	PosAct=PosPrev,
	
	write('Me estoy dando la cabeza contra una pared y no puedo pasar...'),nl,
	write('Voy a intentar pasar por un costado'),nl,
	
	%--Posici�n frontal que parece estar bloqueada
	ady_at_cardinal(PosAct,DirAct,PosBloq),
	assert(no_transitable(PosBloq)),
	
	%--Busca un nuevo camino por un costado
	meta_act(MetaPos),
	empezar([PosAct,DirAct],[[MetaPos,_MetaDir]],SolR,80),
	(
		(
			(SolR=null;SolR=[]),
			%--Ya no necesito saber que est� bloqueado
			retract(no_transitable(PosBloq)),
			fail
		);
		(
			SolR\=null
		)
	),
	reverse(SolR,Sol),
	
	write('Si hay forma de pasar, voy a mandarme'),nl,
	
	%--Ya no necesito saber que est� bloqueado
	retract(no_transitable(PosBloq)),
	
	%--Actualiza el camino a seguir
	retractall(camino_meta(_)),
	assert(camino_meta(Sol)).

%--Mas que nada informan por pantalla lo que sucedi�
act_metas_generales:-
	write('No tengo ning�n camino que seguir?'),nl,
	camino_meta(VMeta),
	VMeta=[],
	
	%--Limpia el estado interno del agente de metas no v�lidas
	write('No, no hay ninguna meta'),nl,
	retractall(meta_act(_)),
	assert(meta_act(null)),
	retractall(tipo_meta(_)),
	assert(tipo_meta(null)).
act_metas_generales:-
	camino_meta(VMeta),
	VMeta\=[],
	write('Si estaba siguiendo un camino'),nl.