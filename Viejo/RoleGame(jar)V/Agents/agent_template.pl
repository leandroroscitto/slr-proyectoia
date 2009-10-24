%%-- Player-Agent Alfa

:-consult(ag_primitives),consult(extras_for_agents).

imp_turno(T):-
	write('--------------------------Turno='),
	write(T),nl.

imp_vision([]).
imp_vision([V1|SV]):-
	V1=[Pos,Land,Things],
	write('Posicion='),write(Pos),nl,
	write('Tierra='),write(Land),nl,
	write('Cosas='),write(Things),nl,
	imp_vision(SV).

imp_attrs([]).
imp_attrs([A1|SA]):-
	A1=[NA,VA],
	write(NA),write('='),write(VA),nl,
	imp_attrs(SA).

imp_inv(I):-
	write('Inventario='),write(I),nl.

imp_percept([Turno,Vision,Attrs,Inv]):-
	imp_turno(Turno),nl,
	imp_vision(Vision),nl,
	imp_attrs(Attrs),nl,
	imp_inv(Inv),nl.

run:-
	get_percept(Perc),
	
	imp_percept(Perc),
	
	Action=null_action,
    do_action(Action),
    run.

:-dynamic ag_name/1.

%-- INICIALIZACION ============================================================
start_ag(AgName):-
    register_me(AgName, Status),
    !,
    write('REGISTRATION STATUS: '),
    write(Status),nl,nl,
    Status=connected,
    assert(ag_name(AgName)),
    run.
   
s(AgName):-start_ag(AgName).

start_ag_instance(InstanceID):-
	AgClassName=alfa,
    AgInstanceName=..[AgClassName, InstanceID],
    register_me(AgInstanceName, Status),
    !,
    write('REGISTRATION STATUS: '),
    write(Status),nl,nl,
    Status=connected,
    assert(ag_name(AgInstanceName)),
    run.

si(InstanceID):-start_ag_instance(InstanceID).