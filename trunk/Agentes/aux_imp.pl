%--Predicados auxiliares
%--Impresion por pantalla

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