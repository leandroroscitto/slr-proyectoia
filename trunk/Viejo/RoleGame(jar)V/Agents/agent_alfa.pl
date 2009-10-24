%%-- Player-Agent Alfa

:-consult(ag_primitives),consult(extras_for_agents).
:-consult(aux_imp).

:-dynamic ir_a/1.
:-dynamic turno_act/1.
:-dynamic atrib_act/1.
:-dynamic inv_act/1.

tesoro([treasure,_,_]).
	

act_estado_ent([_T,V,_A,_I]):-
	member(P,V),						
	P=[Pos,Tie,Cos],
	Cos\=[],
	member(C,Cos),
	tesoro(C),
	not(ir_a(Pos)),
	assertz(ir_a(Pos)).

act_estado_ag([T,_V,A,I]):-	
	retractall(turno_act(_)),
	assert(turno_act(T)),
	
	retractall(atrib_act(_)),
	assert(atrib_act(A)),
	
	retractall(inv_act(_)),
	assert(inv_act(I)).

sel_accion(Action):-
	ir_a(PosDest),!,
	atrib_act(A),
	member([pos,Pos],A),
	member([dir,Dir],A),
	PosDest=[XDest,YDest],
	Pos=[X,Y],
	XDif is (XDest-X),
	YDif is (YDest-Y),
		(
		((XDif=0),(YDif>0),Dir=e,Action=move_fwd) |
		((XDif=0),(YDif>0),Dir\=e,Action=turn(e)) |
		((XDif=0),(YDif<0),Dir=w,Action=move_fwd) |
		((XDif=0),(YDif<0),Dir\=w,Action=turn(w)) |
		((XDif>0),(YDif=0),Dir=s,Action=move_fwd) |
		((XDif>0),(YDif=0),Dir\=s,Action=turn(s)) |
		((XDif<0),(YDif=0),Dir=n,Action=move_fwd) |
		((XDif<0),(YDif=0),Dir\=n,Action=turn(n)) |
		((XDif>0),(YDif\=0),Dir=s,Action=move_fwd) |
		((XDif>0),(YDif\=0),Dir\=s,Action=turn(s)) |
		((XDif<0),(YDif\=0),Dir=n,Action=move_fwd) |
		((XDif<0),(YDif\=0),Dir\=n,Action=turn(n)) |
		((XDif=0),(YDif=0),Action=pickup(_))
		)
		write(Action).
	
sel_accion(null_action).

run:-
	get_percept(Perc),	
	imp_percept(Perc),
	
	act_estado_ent(Perc),
	act_estado_ag(Perc),
	write('LLegue'),
	
	sel_accion(Action),
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