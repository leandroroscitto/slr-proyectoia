%% Player-Agent barb

:- consult(ag_primitives).

:- consult(extras_for_agents).

:- dynamic last_perc/1.

last_perc(none).

run:-
      get_percept(Perc),
      
      % AGENT CODE
      % (internal state update and action choice)
      
      ag_name(AgName),
      
      display_ag(AgName, Perc), nl,
      
      update_state(Perc),
      
      decide_action(Action),
      
      
      do_action(Action),
      
      run.



update_state(Perc):- retract(last_perc(_)),
                     assert(last_perc(Perc)).


decide_action(attack(Victim)):-

      last_perc([_Turn, _Vision, Attrs, _Inv]),

      member([pos, Pos], Attrs),
      member([dir, Dir], Attrs),

      attackable_agent(Victim, Pos, Dir).
      

decide_action(Action):-

      last_perc([_Turn, Vision, Attrs, _Inv]),

      member([pos, Pos], Attrs),
      member([dir, Dir], Attrs),
      ady_at_cardinal(Pos, Dir, PosInFront),
      member([PosInFront, Land, _Content], Vision),
      
      (Land = water
           ;
       Land = forest),
       
      next_90_clockwise(DesiredDir, Dir),
      Action = turn(DesiredDir).
      

decide_action(Action):-

      last_perc([_Turn, Vision, Attrs, _Inv]),
      member([pos, MyPos], Attrs),
      member([MyPos, _Land, Content], Vision),
      member([treasure, TrName, _], Content),
      write('He encontrado un tesoro conocido como '), write(TrName), write('.'),nl,
      write('voy a intentar recogerlo...'),nl,
      Action = pickup(TrName).
      
      
      
decide_action(move_fwd).


attackable_agent(Victim, Pos, _Dir):-

      last_perc([_Turn, Vision, Attrs, _Inv]),

      member([pos, Pos], Attrs),
      
      member([Pos, _Land, Content], Vision),
      
      member([agent, Victim, AgProperties], Content),
      
      member([unconscious, false], AgProperties),
      
      ag_name(MyName), %Debería llamarlo my_name!!!
      
      Victim \= MyName.


attackable_agent(Victim, Pos, Dir):-

      last_perc([_Turn, Vision, Attrs, _Inv]),

      member([pos, Pos], Attrs),
      member([dir, Dir], Attrs),

      ady_at_cardinal(Pos, Dir, FrontPos),
      
      member([FrontPos, _Land, Content], Vision),
      member([agent, Victim, AgProperties], Content),
      
      member([unconscious, false], AgProperties).


attackable_agent(Victim, Pos, Dir):-

      last_perc([_Turn, Vision, Attrs, _Inv]),

      member([pos, Pos], Attrs),
      member([dir, Dir], Attrs),

      ady_at_cardinal(Pos, Dir, FrontPos),
      next_90_clockwise(Dir, NextDir),
      ady_at_cardinal(FrontPos, NextDir, FrontRightPos),

      member([FrontRightPos, _Land, Content], Vision),
      member([agent, Victim, AgProperties], Content),

      member([unconscious, false], AgProperties).


attackable_agent(Victim, Pos, Dir):-

      last_perc([_Turn, Vision, Attrs, _Inv]),

      member([pos, Pos], Attrs),
      member([dir, Dir], Attrs),

      ady_at_cardinal(Pos, Dir, FrontPos),
      next_90_clockwise(PrevDir, Dir),
      ady_at_cardinal(FrontPos, PrevDir, FrontLeftPos),

      member([FrontLeftPos, _Land, Content], Vision),
      member([agent, Victim, AgProperties], Content),

      member([unconscious, false], AgProperties).




:- dynamic ag_name/1.


start_ag:- AgName = barb,
           register_me(AgName, Status),
           !,
           write('REGISTRATION STATUS: '),
           write(Status), nl, nl,
           Status = connected,
           assert(ag_name(AgName)),
           run.

s:- start_ag.


start_ag_instance(InstanceID):-
                    AgClassName = barb,
                    AgInstanceName =.. [AgClassName, InstanceID],
                    register_me(AgInstanceName, Status),
                    !,
                    write('REGISTRATION STATUS: '),
                    write(Status), nl, nl,
                    Status = connected,
                    assert(ag_name(AgInstanceName)),
                    run.

si(InstanceID):- start_ag_instance(InstanceID).