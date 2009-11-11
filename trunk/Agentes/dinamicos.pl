%--PREDICADOS DINAMICOS
%--Supone que se consulta desde el agente.pl

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