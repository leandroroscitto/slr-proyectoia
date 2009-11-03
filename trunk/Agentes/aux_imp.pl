%--Predicados auxiliares
%--Impresion por pantalla

%--Impreción de información del agente, mostrada en cada turno percibido
imp_info_act_ag:-
	write('ATRIBUTOS =============================='),nl,
	ag_name(AgName),
	write('Nombre: '),write(AgName),nl,
	pos_act(Pos),
	write('Posición: '),write(Pos),nl,
	dir_act(Dir),
	write('Direccion: '),write(Dir),nl,
	sta_act(Sta),
	write('Stamina: '),write(Sta),nl,
	msta_act(MSta),
	write('Max Stamina: '),write(MSta),nl,nl,
	
	write('INVENTARIO ============================='),nl,
	inv_act(Inv),
	write('Inventario: '),write(Inv),nl,nl,
	
	write('GRILLA RECORRIDA ======================='),nl,
	imp_grilla,nl,nl,
	
	write('OBJETOS VISTOS ========================='),nl,
	imp_objetos,nl,nl,
	
	write('METAS ACTUALES ========================='),nl,
	meta_act(Meta),
	write('Meta: '),write(Meta),nl,
	camino_meta(CMeta),
	write('Camino a meta: '),write(CMeta),nl,nl.

%--Imprime el porcentaje de la grilla percibida que fue descubierta hasta
%--el momento
imp_porcentaje_grilla:-
	estado_grilla(Grilla),
	
	maxFila(Grilla,MF),
	maxCol(Grilla,MC),
	
	cantidad_ele(Grilla,CantE),
	TamGrilla is MF*MC,
	Porc is ((CantE/TamGrilla)*100),
	write('Porcentaje='),write(Porc),nl,nl.

%--Predicados axuliares para la impresión de la grilla en pantalla
fila(F):-estado_grilla(Grilla),maxFila(Grilla,NofAs),nnleq(F,NofAs).
col(C):-estado_grilla(Grilla),maxCol(Grilla,NofCs),nnleq(C,NofCs).
	
%--Imprime la grilla interna de las celdas conocidas
imp_grilla:-
	tab(5),
	forall(
		col(C),
		(
			write(C),
			(
				(C<10,write('  '));
				(C>9,write(' '))
			)					
		)
	),nl,
	forall(
		fila(F),
		(
			(
				(F<10,tab(3));
				(F>9,tab(2))
			),
			write(F),tab(1),
			forall(
				col(C),
				(
					mostrarCelda(F,C)
				)
			),
			nl
		)
	),
	fail.
imp_grilla.

%--Leyenda utilizada para imprimir una celda de la grilla
leyenda(plain,'.').
leyenda(mountain,'^').
leyenda(water,'~').
leyenda(forest,'*').

%--Imprime una celda en pantalla de acuerdo a la prioridad determinada por el orden
mostrarCelda(F,C):-
	pos_act(Pos),
	
	Pos=[F,C],

	write('@'),
	tab(2).
mostrarCelda(F,C):-
	meta_act(Pos),
	
	Pos=[F,C],

	write('X'),
	tab(2).
mostrarCelda(F,C):-
	pos_act(Pos),
	estado_grilla(Grilla),
	
	Pos\=[F,C],
	
	member([[F,C],Land,_],Grilla),
	leyenda(Land,Char),
	write(Char),
	tab(2).
mostrarCelda(F,C):-
	pos_act(Pos),
	estado_grilla(Grilla),
	
	Pos\=[F,C],
	not(member([[F,C],_Land,_],Grilla)),
	write('#'),
	tab(2).
mostrarCelda(_,_):-
	write('+'),
	tab(2).

%--Imprime los objetos percibidos 
imp_objetos:-
	estado_objetos(Objs),
	write('Objetos internos: '),write(Objs).

%--Imprime el turno actual
imp_turno(T):-
	write('--------------------------Turno='),
	write(T),nl.

%--Imprime la estructura de la visión de una percepción
imp_vision([]).
imp_vision([V1|SV]):-
	V1=[Pos,Land,Things],
	write('Posicion='),write(Pos),nl,
	write('Tierra='),write(Land),nl,
	write('Cosas='),write(Things),nl,
	imp_vision(SV).

%--Imprime la estrucutura de la lista de atributos
imp_attrs([]).
imp_attrs([A1|SA]):-
	A1=[NA,VA],
	write(NA),write('='),write(VA),nl,
	imp_attrs(SA).

%--Imprime el inventario
imp_inv(I):-
	write('Inventario='),write(I),nl.

%--Imprime la estructura de la percepción
imp_percept([Turno,Vision,Attrs,Inv]):-
	imp_turno(Turno),nl,
	imp_vision(Vision),nl,
	imp_attrs(Attrs),nl,
	imp_inv(Inv),nl.