%% Player-Agent crazy

:- consult(ag_primitives).

:- consult(extras_for_agents).

run:-
      get_percept(Perc),

      % AGENT CODE
      % (internal state update and action choice)

      ag_name(AgName),

      display_ag(AgName, Perc), nl,
      
      member([pos, Pos], Attrs),
      member([dir, Dir], Attrs),
      ady_at_cardinal(Pos, Dir, PosInFront),
      member([PosInFront, Land, _Content], Vision),
      (
       (Land = water ; Land = forest), %puedo chequear si es un agente, y entonces lo esquiva!!!
       next_90_clockwise(Dir, DesiredDir),
       Action = turn(DesiredDir)
       ;
       Action = move_fwd
      ),
      do_action(Action),
      
      run.
      

:- dynamic ag_name/1.


start_ag:- AgName = crazy,
           register_me(AgName, Status),
           !,
           write('REGISTRATION STATUS: '),
           write(Status), nl, nl,
           Status = connected,
           assert(ag_name(AgName)),
           run.

s:- start_ag.


start_ag_instance(InstanceID):-
                    AgClassName = crazy,
                    AgInstanceName =.. [AgClassName, InstanceID],
                    register_me(AgInstanceName, Status),
                    !,
                    write('REGISTRATION STATUS: '),
                    write(Status), nl, nl,
                    Status = connected,
                    assert(ag_name(AgInstanceName)),
                    run.

si(InstanceID):- start_ag_instance(InstanceID).