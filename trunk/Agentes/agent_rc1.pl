%-- IMPORTACION================================================================
%--Primitivas para interacci�n con el entorno
:-consult(ag_primitives).

%--Predicados auxiliares
:-consult(extras_for_agents).
%--Predicados auxilares
:-consult(aux_agente).

%--Predicados auxiliares para impresi�n en pantalla
:-consult(aux_imp).

%--Implementaci�n de la busqueda A*
:-consult(busqueda).

%--Predicados din�micos
:-consult(dinamicos).
%--Inicializaci�n
:-consult(inicializacion).

%--Actualizacion de estado interno
:-consult(act_estado_interno).
%--Seleccion de accion a realizar
:-consult(seleccion_accion).
%--Actualizacion de metas
:-consult(act_metas_internas).

%--ACTUALIZACION DEL ESTADO INTERNO
%--Actualiza la representaci�n interna del entorno y los atributos del agente
act_estado(Perc):-
	act_estado_ent(Perc),
	act_estado_ag(Perc).

%--SELECCION==================================================================
%--La selecci�n del acci�n depende de las prioridades del agente
sel_accion(Action):-
	sel_accion_supervivencia(Action);
	sel_accion_tesoros(Action);
	sel_accion_metas(Action);
	sel_accion_exploracion(Action).

%--ACTUALIZAR LAS METAS DEL AGENTE============================================
%--Actualizaci�n de metas de acuerdo con las prioridades del agente
act_metas:-
	act_metas_bloqueadas;
	act_metas_supervivencia;
	act_metas_tesoros;
	act_metas_exploracion;
	act_metas_generales.

%-- CICLO======================================================================
run:-
	%--Recibe la percepci�n del entorno
	get_percept(Perc),
	
	%--Se muestra el turno por pantalla
	Perc=[TurnAct,_,_,_],
	nl,write('Turno:'),write(TurnAct),write('---------------------------'),nl,
	
	%--Se actualiza la representaci�n interna de la grilla, los obejtos y los agentes
	nl,write('ACTUALIZACION DE ESTADO INTERNO================================='),nl,
	act_estado(Perc),!, %--No hay vuelta atras
	
	%--Se imprime la informaci�n de los atributos del agente actualizada
	imp_info_act_ag,
	
	%--Se determinan las metas del agente, de existir alguna
	nl,write('ACTUALIZACION DE METAS=========================================='),nl,
	act_metas,!, %--No hay vuelta atras
	
	%--A partir de las metas y las prioridades del agente, se selecciona la
	%--pr�xima acci�n a realizar
	nl,write('SELECCION DE ACCION============================================='),nl,
	sel_accion(Action),!, %--No hay vuelta atras
	
	%--Actualiza el estado del agente posteriormente a seleccionar la acci�n
	act_estado_post(Action),
	
	%--Ejecuta la acci�n
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