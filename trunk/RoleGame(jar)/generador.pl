:-dynamic nfil/1.
:-dynamic ncol/1.

:-dynamic nobjs/1.
:-dynamic nedif/1.

% cell_land(Pos, Land)
:-dynamic cell_land/2.

nfil(0).
ncol(0).

nobjs(0).
nedif(0).

land(0,plain).
land(1,water).
land(2,mountain).
land(3,forest).

porc(plain,60).
porc(water,5).
porc(mountain,30).
porc(forest,5).

fila(F):-nfil(NofAs),nnleq(F,NofAs).
col(C):-ncol(NofCs),nnleq(C,NofCs).

nn_pred(1,2).
nn_pred(PredM,M):-M>2,PredM is M-1.

nnleq(N,M):-nn_pred(PredM,M),nnleq(N,PredM).
nnleq(M,M).

generar_land(Land):-
	porc(plain,PPlain),
	porc(mountain,PMount),
	porc(forest,PForest),
	porc(water,PWater),
	
	D100 is random(100),
	
	(
		(D100<PPlain,Land=plain);
		(D100>PPlain-1,D100<PPlain+PMount,Land=mountain);
		(D100>PPlain+PMount-1,D100<PPlain+PMount+PForest,Land=forest);
		(D100>PPlain+PMount+PForest-1,Land=water)
	).

abrir_archivo(File,Stream):-
	open(File,write,Stream).

cerrar_stream(Stream):-
	close(Stream).

write_stream(Stream,List):-
	List=[Ele|SList],
	write(Stream,Ele),
	write_stream(Stream,SList).
write_stream(_Stream,[]).

borde([F,_C]):-F=1.
borde([_F,C]):-C=1.
borde([F,_C]):-nfil(NF),F=NF.
borde([_F,C]):-ncol(NC),C=NC.

imp_dimensiones(Stream):-
	nfil(NumFil),
	ncol(NumCol),

	write_stream(Stream,[n_of_arrows(NumFil),'.']),
	nl(Stream),
	write_stream(Stream,[n_of_columns(NumCol),'.']).

imp_grilla(Stream):-
	nl(Stream),
	nl(Stream),
	forall(
		col(C),
		(
			forall(
				fila(F),
				(
					Pos=[F,C],
					%--L is random(4),
					((not(borde(Pos)),generar_land(Land));(borde(Pos),Land=forest)),
					assertz(cell_land(Pos,Land)),
					write_stream(Stream,[cell_land(Pos,Land),'.']),
					nl(Stream)
				)
			)
		)
	).

generar_objetos(0,[]).
generar_objetos(NObjs,Objs):-
	NObjs>0,
	Objs=[Obj|SObjs],
	
	string_to_atom(SNObjs,NObjs),
	string_concat('t',SNObjs,S),
	
	VObj is random(400),
	Obj=[treasure,S,[[val,VObj]]],
	
	NObjs1 is NObjs-1,
	generar_objetos(NObjs1,SObjs).

generar_pos(Pos):-
	Pos=[F,C],
	
	nfil(NF),
	ncol(NC),
	
	F is random(NF)+1,
	C is random(NC)+1.

generar_pos_trans(Pos):-
	generar_pos(PosT),
	cell_land(PosT,Land),
	
	(
		((Land=plain;Land=mountain),Pos=PosT);
		((Land\=plain,Land\=mountain),generar_pos_trans(Pos))
	).

imp_objetos(Stream):-
	nl(Stream),
	write(Stream,':- dynamic object_at/2.'),
	nl(Stream),
	
	nobjs(NObjs),
	generar_objetos(NObjs,Objs),

	forall(
		member(Obj,Objs),
		(
			generar_pos_trans(Pos),
			write_stream(Stream,[object_at(Obj,Pos),'.']),
			nl(Stream)
		)
	).

generar_edificios(0,[]).
generar_edificios(NEdifs,Edifs):-
	NEdifs>0,
	Edifs=[Edif|SEdifs],
	
	string_to_atom(SNEdifs,NEdifs),
	string_concat('h',SNEdifs,S),

	Edif=[hostel,S,[]],
	
	NEdifs1 is NEdifs-1,
	generar_edificios(NEdifs1,SEdifs).

imp_edificios(Stream):-
	nl(Stream),
	
	nedif(NEdif),
	generar_edificios(NEdif,Edifs),

	forall(
		member(Edif,Edifs),
		(
			generar_pos_trans(Pos),
			Edif=[Tipo,Nombre,Attr],
			write_stream(Stream,[building(Tipo,Nombre,Pos,Attr),'.']),
			nl(Stream)
		)
	).

gen(NomArch,NumFil,NumCol,NumObj,NumEdif):-
	retract(nfil(_)),
	retract(ncol(_)),
	assert(nfil(NumFil)),
	assert(ncol(NumCol)),
	
	retract(nobjs(_)),
	assert(nobjs(NumObj)),
	retract(nedif(_)),
	assert(nedif(NumEdif)),
	
	abrir_archivo(NomArch,Stream),
	
	imp_dimensiones(Stream),
	imp_grilla(Stream),
	imp_objetos(Stream),
	imp_edificios(Stream),
	
	cerrar_stream(Stream).

s(F,C,NumObj,NumEdi):-gen('comarca.pl',F,C,NumObj,NumEdi).