%-- IMPORTACION================================================================
%--Primitivas para interacción con el entorno
:-consult(ag_primitives).

%--Predicados auxiliares
:-consult(extras_for_agents).
%--Predicados auxilares
:-consult(aux_agente).

%--Predicados auxiliares para impresión en pantalla
:-consult(aux_imp).

%--Implementación de la busqueda A*
:-consult(busqueda).

%--Predicados dinámicos
:-consult(dinamicos).
%--Inicialización
:-consult(inicializacion).

%--Actualizacion de estado interno
:-consult(act_estado_interno).
%--Seleccion de accion a realizar
:-consult(seleccion_accion).
%--Actualizacion de metas
:-consult(act_metas_internas).

%--ACTUALIZACION DEL ESTADO INTERNO
%--Actualiza la representación interna del entorno y los atributos del agente
act_estado(Perc):-
	%--Actualiza la representación interna del entorno que lleva el agente,
	%--por medio de la nueva percepción
	act_estado_ent(Perc),
	%--Actualiza los atributos internos del agente, a partir de lo percibido
	act_estado_ag(Perc).

%--SELECCION==================================================================
%--La selección del acción depende de las prioridades del agente
sel_accion(Action):-
	%--Por prioridad, primero selecciona una acción que asegure su supervivencia,
	%--si no es necesaria ninuna se consentra en los tesoros, y en último lugar
	%--busca completar alguna meta seteada previamente
	sel_accion_supervivencia(Action);
	sel_accion_tesoros(Action);
	sel_accion_metas(Action);
	sel_accion_exploracion(Action).

%--ACTUALIZAR LAS METAS DEL AGENTE============================================
%--Actualización de metas de acuerdo con las prioridades del agente
act_metas:-
	act_metas_generales,
	(
		%--Primero verifica que no esté tratando de cruzar una posición
		%--temporalmente impasable
		act_metas_bloqueadas;
		%--Si no es el caso, busca ante todo sobrevivir (buscar refujio,
		%--escapar de enemigos o atacar si fuera necesario)
		act_metas_supervivencia;
		%--En el caso de que no requiera nada de lo anterior, busca tesoros
		act_metas_tesoros;
		%--Si no hay tesoros alcanzables, busca explorar las celdas no vistas
		act_metas_exploracion;
		%--Cuando ya exploró toda la grilla, trata de recorrerla de forma ordenada
		%--para encontrar nuevas cosas (agentes, o tesoros caidos)
		act_metas_landmarks
	).

%-- CICLO======================================================================
run:-
	%--Recibe la percepción del entorno
	get_percept(Perc),
	
	%--Se muestra el turno por pantalla
	Perc=[TurnAct,_,_,_],
	nl,write('Turno:'),write(TurnAct),write('---------------------------'),nl,
	
	%--Se actualiza la representación interna de la grilla, los obejtos y los agentes
	nl,write('ACTUALIZACION DE ESTADO INTERNO================================='),nl,
	act_estado(Perc),!, %--No hay vuelta atras
	
	%--Se imprime la información de los atributos del agente actualizada
	imp_info_act_ag,
	
	%--Se determinan las metas del agente, de existir alguna
	nl,write('ACTUALIZACION DE METAS=========================================='),nl,
	act_metas,!, %--No hay vuelta atras
	
	%--A partir de las metas y las prioridades del agente, se selecciona la
	%--próxima acción a realizar
	nl,write('SELECCION DE ACCION============================================='),nl,
	sel_accion(Action),!, %--No hay vuelta atras
	
	%--Actualiza el estado del agente posteriormente a seleccionar la acción
	act_estado_post(Action),
	
	%--Ejecuta la acción
    do_action(Action),
    run.

%-- INICIALIZACION ============================================================
start_ag:-
	AgName=rc1,
    register_me(AgName, Status),
    !,
    write('REGISTRATION STATUS: '),
    write(Status),nl,nl,
    Status=connected,
    assert(ag_name(AgName)),
    run.
   
s:-start_ag.

start_ag_instance(InstanceID):-
	AgClassName=rc1,
    AgInstanceName=..[AgClassName, InstanceID],
    register_me(AgInstanceName, Status),
    !,
    write('REGISTRATION STATUS: '),
    write(Status),nl,nl,
    Status=connected,
    assert(ag_name(AgInstanceName)),
    run.

si(InstanceID):-start_ag_instance(InstanceID).