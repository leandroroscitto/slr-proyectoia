%-- ACTUALIZACION DE ESTADO INTERNO
%--Supone que se consulta desde el agente.pl

%--act_estado_post(+Accion),actualiza el estado del agente posteriormente a seleccionar la acción
act_estado_post(Action):-
	pos_act(PosAct),
	retractall(accion_previa(_)),
	assert(accion_previa(Action)),
	retractall(pos_previa(_)),
	assert(pos_previa(PosAct)).

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
act_estado_objetos(Vis,TurAct):-
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
			
			%--Tiene que considerar si el objeto es un agente,
			%--ya que en ese caso su descripción puede variar
			Cosa=[Tipo,Nombre,_],
			(
				(
					Tipo=agent,
					not(member([_,[agent,Nombre,_],_],EIObjetos))
				);
				(
					Tipo\=agent,
					not(member([_,Cosa,_],EIObjetos))
				)
			)
			%--not(member([_,Cosa,_],EIObjetos))
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
			
			%--Tiene que considerar si el objeto es un agente,
			%--ya que en ese caso su descripción puede variar
			Cosa=[Tipo,Nombre,_],
			(
				(
					Tipo=agent,
					member([Pos,[agent,Nombre,_],_],EIObjetos)
				);
				(
					Tipo\=agent,
					member([Pos,Cosa,_],EIObjetos)
				)
			)
			%--member([Pos,Cosa,_],EIObjetos)
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
			
			%--Tiene que considerar si el objeto es un agente,
			%--ya que en ese caso su descripción puede variar
			Cosa=[Tipo,Nombre,_],
			(
				(
					Tipo=agent,
					not(member([_,[agent,Nombre,_],_],VObjetos))
				);
				(
					Tipo\=agent,
					not(member([_,Cosa,_],VObjetos))
				)
			),
			%--not(member([_,Cosa,_],VObjetos)),
			not(member([Pos,_,_],Vis))
		),
		NObjetosDif
	),
	
	%--La nueva representación de los objetos percibidos es la unión de los
	%--tres conjuntos construidos
	append([NObjetosNoRep,NObjetosRep,NObjetosDif],NObjetos),
	
	%--Indica el hecho de que se modificó el conjunto de tesoros percibidos
	%--En el caso de que existan tesoros percibidos en algún momento que no
	%--fueron recogidos por el agente o no se comprobó que no se encuentran más
	%--en el lugar, lo indica
	%--VER
	(
		(
			member([_,Cosa,_],NObjetos),
			es_tesoro(Cosa),
			retractall(nuevos_tesoros),
			assert(nuevos_tesoros),
			write('Encontre nuevos tesoros...'),nl
		);
		retractall(nuevos_tesoros)
	),
	
	retractall(estado_objetos(EIObjetos)),
	assert(estado_objetos(NObjetos)).

%--act_estado_ent(+Perc), actualiza la representación interna del entorno
act_estado_ent(Perc):-
	Perc=[Tur,Vis,Attr,_Inv],
	
	%--Actualiza el turno actual
	retractall(turno_act(_)),
	assert(turno_act(Tur)),
	
	%--Actualiza la grilla y la representación de los objetos internos
	get_pos_attr(Attr,PosAct),
	act_estado_grilla(Vis,PosAct),
	act_estado_objetos(Vis,Tur).

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