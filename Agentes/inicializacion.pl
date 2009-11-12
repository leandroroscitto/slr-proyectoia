%-- HECHOS INICIALES
%--Supone que se consulta desde el agente.pl

%--En un principio no existe ninguna percepción de la geografia del entorno
estado_grilla([]).
%--En un principio no se percibieron ningunos objetos
estado_objetos([]).

%--Inicialización nula de los atributos del agente (se actualizan luego de la
%--primera percepción)
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