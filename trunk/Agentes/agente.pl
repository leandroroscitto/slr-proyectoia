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

%--Actualizacion de estado interno
:-consult(act_estado_interno).
%--Seleccion de accion a realizar
:-consult(seleccion_accion).
%--Actualizacion de metas
:-consult(act_metas_internas).

%--PREDICADOS DINAMICOS
%--estado_objetos(-Objs), con Objs es una lista de objetos del tipo
%--[Posicion,Objeto,UltimoTurnoVisto], con Objeto=[Tipo,Nombre,Descripci�n]
%--Representaci�n interna de los objetos que fueron percibidos por el agente,
%--y cual fue el �ltimo turno en el que se los vi�
:-dynamic estado_objetos/1.
%--estado_grilla(-Grilla), con Grilla una lista de celdas del tipo
%--[Posicion,TipoTierra,Visitado]
%--Representaci�n interna de la geograf�a del entorno percibida por el agente
:-dynamic estado_grilla/1.

%--Turno actual
:-dynamic turno_act/1.
%--Posicion actual del agente
:-dynamic pos_act/1.
%--Direcci�n actual del agente
:-dynamic dir_act/1.
%--Stamina actual del agente
:-dynamic sta_act/1.
%--M�xima Stamina actual del agente
:-dynamic msta_act/1.
%--Inventario actual del agente
:-dynamic inv_act/1.
%--Habilidad de pelea actual del agente
:-dynamic fs_act/1.
%--Lista de nombres de agentes que lo atacaron
:-dynamic atacantes/1.
%--Lista de nombres de agentes que lo hirieron
:-dynamic hirieron/1.

%--accion_previa(-Accion), Accion realizada en el turno anterior al actual
:-dynamic accion_previa/1.
%--pos_previa(-Pos), Pos representa la posici�n en el �ltimo turno persivido
:-dynamic pos_previa/1.

%--camino_meta(Camino), Camino es una lista de posiciones que seguir� el agente
%--con destino a una meta, de no presentarse alguna situaci�n de mayor prioridad
:-dynamic camino_meta/1.
%--meta_act(Meta), Meta es una posici�n (o celda) de la grilla al que el agente
%--intentar� llegar, siempre que no se presente una situaci�n de mayor prioridad
:-dynamic meta_act/1.
%--tipo_meta(Tipo), Tipo pertenece {treasure,hostel,unknown}, indica el tipo
%--de meta que indica la posici�n
:-dynamic tipo_meta/1.

%--no_transitable(-Pos) determina que la posici�n Pos no es transitable por alguna raz�n
%--probablemente solo temporalmente
:-dynamic no_transitable/1.
%--nuevos_tesoros representa el descubrimiento de nuevos tesoros desde la �ltima percepci�n si Hay=1
:-dynamic nuevos_tesoros/0.
%--Representa el descubrimiento de nuevas celdas de la grilla
:-dynamic nuevas_celdas/0.
%--VER: Nuevos agentes(?)

%--esperar(Turnos), Turnos es la cantidad de turnos dedicados a realizar alguna acci�n especial
:-dynamic esperar/1.
%--peligro_agente determina si exiten agentes enemigos en las cercanias
:-dynamic peligro_agente/0.
%--agentes_cerca(-ListA), ListA es una lista de agentes que se encuentra en posiciones atacables
:-dynamic agentes_cerca/1.

%--ag_name(Nombre), representa el Nombre del agente
:-dynamic ag_name/1.

%--ultimo_descanso(Turno), representa el �ltimo Turno en el que el agente
%--descans� en un hostel
:-dynamic ultimo_descanso/1.

%--HECHOS INICIALES
%--En un principio no existe ninguna percepci�n de la geografia del entorno
estado_grilla([]).
%--En un principio no se percibieron ningunos objetos
estado_objetos([]).

%--Inicializaci�n nula de los atributos del agente (se actualizan luego de la
%--primera percepci�n)
turno_act(-1).
pos_act([-1,-1]).
sta_act(-1).
msta_act(-1).
dir_act(-1).
inv_act([]).
fs_act(-1).
atacantes([]).
hirieron([]).

accion_previa(-1).
pos_previa([-1,-1]).

%--En un principio no existe ninguna meta ni un camino hacia alguna
meta_act(null).
tipo_meta(null).
camino_meta([]).

%--ACTUALIZACION DEL ESTADO INTERNO
%--Actualiza la representaci�n interna del entorno y los atributos del agente
act_estado(Perc):-
	%--Actualiza la representaci�n interna del entorno que lleva el agente,
	%--por medio de la nueva percepci�n
	act_estado_ent(Perc),
	%--Actualiza los atributos internos del agente, a partir de lo percibido
	act_estado_ag(Perc).

%--SELECCION==================================================================
%--La selecci�n del acci�n depende de las prioridades del agente
sel_accion(Action):-
	%--Por prioridad, primero selecciona una acci�n que asegure su supervivencia,
	%--si no es necesaria ninuna se consentra en los tesoros, y en �ltimo lugar
	%--busca completar alguna meta seteada previamente
	sel_accion_supervivencia(Action);
	sel_accion_tesoros(Action);
	sel_accion_metas(Action);
	sel_accion_exploracion(Action).

%--ACTUALIZAR LAS METAS DEL AGENTE============================================
%--Actualizaci�n de metas de acuerdo con las prioridades del agente
act_metas:-
	act_metas_generales,
	(
		%--Primero verifica que no est� tratando de cruzar una posici�n
		%--temporalmente impasable para llegar a una meta
		act_metas_bloqueadas;
		%--Si no es el caso, busca ante todo sobrevivir (buscar refujio,
		%--escapar de enemigos o atacar si fuera necesario)
		act_metas_supervivencia;
		%--En el caso de que no requiera nada de lo anterior, busca tesoros
		act_metas_tesoros;
		%--Si no hay tesoros alcanzables, busca explorar las celdas no vistas
		act_metas_exploracion;
		%--Si adem�s de no haber tesoros,no hay nuevos lugares que explorar busca
		%--otro agente para atacar y robarle 
		act_metas_ofensiva;
		%--Cuando ya explor� toda la grilla, trata de recorrerla de forma ordenada
		%--para encontrar nuevas cosas (agentes, o tesoros caidos)
		act_metas_landmarks;
		%--Indica que no se pudo entrar ninguna otro meta y corrige algunas cuestiones
		act_metas_generales;
		%--Evita que falle si todo lo demas falla
		true
	).

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