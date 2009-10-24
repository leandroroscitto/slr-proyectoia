%--Actualiza la representacion interna de los objetos del entorno
act_estado_objetos(TurAct,Vis):-	
	estado_objetos(Objetos),
	%--Busca todos los elementos de la percepcion que no estan en el estado interno
	findall(
			[Pos,Obj,TurAct],
			(
				member(Ele,Vis),
				Ele=[Pos,_,Cosas],
				member(Obj,Cosas),
				Obj=[Tipo,Nombre,Descrip],
				(
					(Tipo=agent,ag_name(NomAg),Nombre\=NomAg);
					(Tipo\=agent)
				),
				not(member([_,Obj,_],Objetos))
			),
			NObjetosNoRep			
		),	
	%--Busca todos los elementos de la percepcion que estan en el estado interno
	findall(
			[Pos,Obj,TurAct],
			(
				member(Ele,Vis),
				Ele=[Pos,_,Cosas],
				member(Obj,Cosas),
				Obj=[Tipo,Nombre,Descrip],
				(
					(Tipo=agent,ag_name(NomAg),Nombre\=NomAg);
					(Tipo\=agent)
				),
				member([_,Obj,_],Objetos)
			),
			NObjetosRep
		),
	%--Busca todos los elementos del estado interno que no estan en la percepcion
	%--Si un objeto estaba en una posicion en el estado interno y no se encuentra
	%--en la percepcion en la posicion no lo cuenta
	findall(
			[Pos,Obj,Tur],
			(
				member(Obj,Objetos),
				ObjC=[Pos,Obj,Tur],
				member(Ele,Vis),
				Ele=[Pos,_,Cosas],
				not(member(Obj,Cosas)),
				Obj=[Tipo,Nombre,Descrip],
				(
					(Tipo=agent,ag_name(NomAg),Nombre\=NomAg);
					(Tipo\=agent)
				)
			),
			NObjetosDif
		),
	%--NObjetos representa la posición actualizada de todos los elementos conocidos
	append([NObjetosNoRep,NObjetosRep,NObjetosDif],NObjetos_P),
	
	%--PARCHE
	eliminar_rep(NObjetos_P,NObjetos),
	
	/*
	write('Objetos no repetidos='),write(NObjetosNoRep),nl,
	write('Objetos repetidos='),write(NObjetosRep),nl,
	write('Objetos viejos='),write(NObjetosDif),nl,nl,
	*/
	
	write(NObjetos),nl,nl,
	
	retract(estado_objetos(Objetos)),
