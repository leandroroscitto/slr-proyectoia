%--FUNCIONES AUXILIARES PARA EL AGENTE
%--Supone que se consulta desde el agente.pl

%--PRINCIPALMENTE EN ESTE M�DULO PUEDEN EXISTIR PREDICADOS
%--QUE SE USARON PARA AGUNA FUNCI�N PERO QUE YA NO SON USADOS 
%--EN LA VERSI�N FINAL DEL AGENTE

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

%--Determina la descripci�n de una cosa
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

%--diagd_at_cardinal(+Pos,+Dir,-Posdiagd), devuelve la posici�n diagonal 
%--derecha a partir de la direcci�n
diagd_at_cardinal([F,C],n,[PredF,SuccC]):-next(PredF,F),next(C,SuccC).
diagd_at_cardinal([F,C],s,[SuccF,PredC]):-next(F,SuccF),next(PredC,C).
diagd_at_cardinal([F,C],e,[SuccF,SuccC]):-next(F,SuccF),next(C,SuccC).
diagd_at_cardinal([F,C],w,[PredF,PredC]):-next(PredF,F),next(PredC,C).

%--diagi_at_cardinal(+Pos,+Dir,-Posdiagi), idem para diagonal izquierda
diagi_at_cardinal([F,C],n,[PredF,SuccC]):-next(PredF,F),next(C,SuccC).
diagi_at_cardinal([F,C],s,[SuccF,PredC]):-next(F,SuccF),next(PredC,C).
diagi_at_cardinal([F,C],e,[SuccF,SuccC]):-next(F,SuccF),next(C,SuccC).
diagi_at_cardinal([F,C],w,[PredF,PredC]):-next(PredF,F),next(PredC,C).
	
%--puede_pasar_hostel(+TurnoFut), determina si es posible trasladarse a trav�s
%--de un hostel en un turno futuro
%--NO USADA
puede_pasar_hostel(TurnoFut):-
	ultimo_descanso(UTurno),!,
	turno_act(TurnoAct),
	
	TurnosPasados is (TurnoAct-UTurno),
	DifTurnos is (TurnosPasados+TurnoFut),
	
	%--La cantidad de turnos pasados deben ser m�s que la cantidad de turnos
	%--necesarios para poder volver a pasar por un hostel
	msta_act(MSta),
	FET is round(MSta/2),
	FET<DifTurnos,
	
	%--Tambi�n la stamina en ese turno debe ser menor que la m�xima stamina
	sta_act(Sta),
	msta_act(MSta),
	(Sta-TurnoFut)<MSta.
puede_pasar_hostel(_TurnoFut):-
	%--Tambi�n puede pasar por un hostel si nunca descans� en uno
	not(ultimo_descanso(_UTurno)).

%--celda_libre(+Pos,+TurnoFut), determina si una celda de la grilla esta o estar�
%--libre en un turno futuro
%--NO USADA
celda_libre(Pos,TurnoFut):-
	estado_grilla(Grilla),
	member([Pos,Land,_],Grilla),
	
	%--El terreno debe ser transitable
	Land\=forest,
	Land\=water,
	
	estado_objetos(Objetos),
	%--Si hay un hostel en la posici�n, debe poder ser pasable en el turno ingresado
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
%--de serlo que no sea �l mismo
no_es_si_mismo(Cosa):-not(es_agente(Cosa)).
no_es_si_mismo(Cosa):-
	es_agente(Cosa),
	nombre_cosa(Cosa,NCosa),
	ag_name(NAgente),
	NCosa\=NAgente.

%--estoy_condiciones_pelear, determina si esta en condiciones de pelear contra
%--otro agente (se considera cuando tiene mas de 2 tercios de la m�xima stamina
estoy_condiciones_pelear:-
	sta_act(Sta),
	msta_act(MSta),
	
	MSta32 is (MSta*2/3),
	Sta>MSta32.

%--eliminar(+Elemento,+List,-ListR), elimina un elemento de una lista, pertenezca
%--o no a esta
eliminar(_Ele,[],[]).
eliminar(Ele,List,ListR):-
	append([SL1,[Ele],SL2],List),
	append([SL1,SL2],ListR).

%--mismos_objetos(+Conj1,+Conj2), determina si dos conjuntos de objetos tienen
%--las mismas cosas, sin importar el turno en que fueron vistas (si la posici�n)
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

%--maxFila(+Grilla,-Cant), m�xima cantidad de filas de la grilla percibida
%--hasta el momento
maxFila([],0).
maxFila([Celda|SGrilla],MCF):-
	maxFila(SGrilla,CF),
	Celda=[Pos,_,_],
	Pos=[F,_C],
	
	MCF is max(CF,F).

%--maxCol(+Grilla,-Cant), m�xima cantidad de columnas de la grilla percibida
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

%--objetos_en_pos(+Pos,+Tipo,-Objs), devuelve una lista de todos los objetos
%--[Tipo,Nombre,Descr] registrados en el estado interno dada una posicion
objetos_en_pos(Pos,Tipo,Objs):-
	estado_objetos(Objetos),
	findall(
			Obj,
			(
				member(Cosa,Objetos),
				Cosa=[Pos,Obj,_Tur],
				Obj=[Tipo,_,_]
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
			Item=[Pos,_Land,Cosas],
			member(Cosa,Cosas)
		),
		VObjetos
	).

%--ir_a_pos_ady(+PosDest,-Accion), determina la acci�n necesaria para avanzar
%--a la posici�n destino a partir de la posici�n actual
ir_a_pos_ady(PosDest,Accion):-
	pos_act(PosAct),
	dir_act(DirAct),
	
	ady_at_cardinal(PosAct,Dir,PosDest),
	%--Si la direcci�n en la que se encuentra la posici�n es la misma que
	%--la direcci�n actual, la acci�n es avanzar
	DirAct=Dir,
	
	Accion=move_fwd.
ir_a_pos_ady(PosDest,Accion):-
	pos_act(PosAct),
	dir_act(DirAct),
	
	ady_at_cardinal(PosAct,Dir,PosDest),
	%--Si la direcci�n en la que se encuentra la posici�n no es la misma que
	%--la direcci�n actual, la acci�n es girar hacia esa direcci�n
	DirAct\=Dir,
	
	Accion=turn(Dir).

%--quemar_energia(-Action), determina la acci�n para quemera energia sin 
%--moverse del lugar, si la cantidad de turnos es mayor que 0
%--NO USADA
quemar_energia(Action):-
	esperar(CantTurn);
	CantTurn>0;
	dir_act(DirAct),
	dir_opuesta(DirAct,DirOp),
	Action=turn(DirOp).

%--adyacente(+Pos,-PosV), obtiene las celdas continuas de la celda Pos
%--NO USADA
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