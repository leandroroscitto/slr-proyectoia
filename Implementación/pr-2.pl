%--Proyecto #2 - Inteligencia Artificial
%--Leandro Roscitto - LU: 72518

:-consult(th).

/*
w(a).
w(b).
w(c->a).

d(a:e/e).
d(a:¬b/¬b).
d(e:z/z).
d(a:c/e).
d(¬c:¬e/m).
%--d(true:p/¬p).
*/

/*
w(a->¬c).
w(b->c).
w(¬b->f).
w(x).
w(y).
w(x and t -> z).

d(x:(a,¬c)/a and ¬z).
d(y:c/b).
d(a and x:true/e).
d(true:b/e).
d(b:¬e/¬x).
d(f:g/g).
*/

/*
w(x).
w(y->¬z).

d(x:y/y).
d(true:z/z).
d(y:(¬x,v)/v).
*/

/*
w(nandu(X)->ave(X)).
w(nandu(cleo)).

d(ave(X):vuela(X)/vuela(X)).
d(nandu(X):¬vuela(X)/¬vuela(X)).
*/

/*
w(irBuffet->comidaLista).
w(comprarComida->comida).
w(cocinar->comidaLista).
w(¬(irBuffet and comprarComida)).

%w(hambre).
%w(heladeraVacia).
%w(cansado).
w(pocoTiempo).

d((hambre and comidaLista):almorzar/almorzar).
d(comida:cocinar/cocinar).
d(true:comprarComida/comprarComida).
d(pocoTiempo:¬almorzar/¬almorzar).
d(true:irBuffet/irBuffet).
d(cansado and heladeraVacia:¬almorzar/¬almorzar).
*/

w(a).
w(e->c).

d(a:b/¬c).
d(a:c/q).
d(¬e:f/¬g).
d(¬e:g/¬f).

%--Acceso directo a la ejección del predicado
s(E,P):-lits_en_extension(E,P).

:-dynamic antecesor/3.

analizar_p(_V,[]).
analizar_p(V,[Default|[]]):-
	not(antecesor(V,Default,null)),
	assert(antecesor(V,Default,null)).
analizar_p(V,[Default|[]]):-
	antecesor(V,Default,null).
analizar_p(V,[DefaultA|[DefaultP|SubP]]):-
	not(antecesor(V,DefaultA,DefaultP)),
	assert(antecesor(V,DefaultA,DefaultP)),
	analizar_p(V,[DefaultP|SubP]).
analizar_p(V,[DefaultA|[DefaultP|SubP]]):-
	antecesor(V,DefaultA,DefaultP),
	analizar_p(V,SubP).

mostar_antecesores:-
	forall(
		antecesor(V,D1,D2),
		(
			write('antecesor('),write(V),write(','),write(D1),
			write(','),write(D2),write(').'),nl
		)
	).
	
generar_arbol(SubA):-
	findall(
		[DefaultP,SubP],
		(
			antecesor(V,null,DefaultP),
			retract(antecesor(V,null,DefaultP)),
			generar_sarbol(V,DefaultP,SubP)
		),
		Dfs
	),
	SubA=[null|Dfs].
generar_sarbol(V,Default,Sub):-
	findall(
		[DefaultP,SubP],
		(
			antecesor(V,Default,DefaultP),
			Default\=null,
			retract(antecesor(V,Default,DefaultP)),
			generar_sarbol(V,DefaultP,SubP)
		),
		Dfs
	),
	Sub=Dfs.

analizar(_V,[]).
analizar(V,ListP):-
	ListP=[P|SListP],
	
	P=[D|_],
	(
		(
			not(antecesor(V,null,D)),
			assert(antecesor(V,null,D))
		);
		antecesor(V,null,D)
	),
	analizar_p(V,P),
	
	V1 is V+1,
	analizar(V1,SListP).

sa:-	
	findall(
		P,
		s(_,P),
		Ps
	),
	
	write('Encontré todos los P'),nl,
	
	analizar(1,Ps),
	
	mostar_antecesores,
	
	write('Analisé todos los P'),nl,
	
	generar_arbol(APros),
	write(APros).

%--lits_en_extension(-LitsInE,-P), retorna el conjunto de literales LitsInE
%--de una extensión E asociada a un proceso P
lits_en_extension(LitsInE,P):-
	proceso([],[],[],LitsInE,PR),
	%--Invierte la lista de default al orden de aplicación
	reverse(PR,P).

%--cauta(+C), determina si un literal C es una conclusión cauta del sistema
cauta(C):-
	%--Para que un literal sea una conclusión cauta debe estar presente
	%--en todas las extensiones de la teoria.
	
	%--Primero busca todas las extensiones existentes en la teoria
	findall(
		Ext,
		lits_en_extension(Ext,_),
		ListExts
	),
	
	%--Y determina si C pertenece a todas ellas
	forall(
		member(Ext,ListExts),
		member(C,Ext)
	).

%--osada(+C), determina si un literal C es una conclusión osada del sistema
osada(C):-
	%--Para que un literal sea una conclusión osada, es suficiente que esté
	%--presente en al menos una extensión de la teoria.
	
	%--Primero busca todas las extensiones
	findall(
		Ext,
		lits_en_extension(Ext,_),
		ListExts
	),
	
	%--Y determina si C pertenece a alguna de ellas
	%--Existe una Ext tal que C pertenece a Ext <=> No es cierto que para toda
	%--extensión Ext, C no es miembro de Ext
	not(
		forall(
			member(Ext,ListExts),
			not(member(C,Ext))
		)
	).

%--verifica(+Ent,+Conj), determina si un axima Ent es válido de acuerdo
%--a la lista de literales de Conj
verifica(Ent,Conj):-
	%--Si es un litera, entonces se verifica si pertenece al conjunto
	es_literal(Ent),
	member(Ent,Conj).
verifica(¬Ent,Conj):-
	%--Si es la negación de un literal, se verifica si la negación pertenece
	%--al conjunto
	es_literal(Ent),
	member((¬Ent),Conj).
verifica(¬Ent,Conj):-
	%--Si es la negación de una fórmula que no es literal, se verifica
	%--si no se verifica la fórmula
	not(es_literal(Ent)),
	not(verifica(Ent,Conj)).
verifica((X and Y),Conj):-
	%--Si es una conjunción, se verifica si se verifican ambas componentes
	verifica(X,Conj),
	verifica(Y,Conj).
verifica((X or Y),Conj):-
	%--Si es una disyunción, se verifica si se verifica alguna de sus
	%--componentes
	verifica(X,Conj);
	verifica(Y,Conj).
verifica((X -> Y),Conj):-
	%--Si es una implicación, se verifica si no se verifica la primera
	%--o se verifica la segunda
	not(verifica(X,Conj));
	verifica(Y,Conj).

%--es_consistente(+Conj), determina si el conjunto In=Th(union(W,Conj)) es
%--consistente
es_consistente(Conj):-
	%--Verifica que para todo literal del In, su negación no pertenezca
	%--también a In
	lits_in_thWUS(Conj,ListLit),
	forall(
		member(Literal,ListLit),
		(
			negacion(Literal,NLiteral),
			not(member(NLiteral,ListLit))
		)
	),
	
	%--write('Lista de literales: '),write(ListLit),nl,
	
	%--Busca todos los elementos de In que no sean literales
	findall(
		NoLiteral,
		(
			w(NoLiteral),
			not(member(NoLiteral,ListLit))
		),
		ListNoLit
	),
	
	%--Los pasa a la forma equivalente compuesta por and,or y ¬
	/*
	findall(
		NoLitAO,
		(
			member(NoLiteral,ListNoLit),
			pasar_a_and_or(NoLiteral,NoLitAO)
		),
		ListNoLitAO
	),
	*/
	
	ListNoLitAO=ListNoLit,
	%--write('No literales: '),write(ListNoLit),nl,
	
	%--write('Entro a verificar...'),nl,
	%--Verifica que todos se cumplan dado el conjunto
	forall(
		member(NoLitAO,ListNoLitAO),
		verifica(NoLitAO,ListLit)
	).
	%--write('Verifiqué.'),nl,nl.

%--proceso(+S,+Out,+P,-Ext,-Proc), dado un conjunto de consecuentes de
%--defaults S, un conjunto Out y una lista de defaults aplicados P, devuelve
%--un proceso exitoso y cerrado Proc y la lista de literales Ext de la
%--extensión asociada al proceso.
proceso(S,Out,P,Ext,Proc):-
	%--write('Proceso: '),write(P),write('==='),nl,
	%--write('Entrada: '),write(S),nl,
	%--Selecciona un default que no pertenezca a P y determina si es aplicable
	%--a In=Th(union(W,S)).
	sel_default(Default),
	not(member(Default,P)),
	def_aplicable(S,Default),
	
	%--Si es aplicable, agrego el consecuente del default a S
	get_consec(Default,Con),
	SNuevo=[Con|S],
	
	%--Agrego la negación de todas las justificaciones a Out, sin considerar
	%--elementos repetidos
	get_justif(Default,Justif),
	findall(
		NEjust,
		(
			miembro_justif(Justif,Ejust),
			negacion(Ejust,NEjust)
		),
		ListEJust
	),
	append([Out,ListEJust],OutParcial),
	list_to_set(OutParcial,OutNuevo),
	
	%--Y agrego el default a la lista P
	PNuevo=[Default|P],
	
	%--LUGAR DEL es_consistente
	
	%--Luego verifica que la intersección entre OutNuevo y el
	%--In=Th(union(W,SNuevo)) no tengan elementos en común.
	forall(
		member(F,OutNuevo),
		not(in_thWUS(F,SNuevo))
	),
	
	%--Verifica si exiten mas defaults a aplicar
	proceso(SNuevo,OutNuevo,PNuevo,Ext,Proc).
proceso(S,_Out,P,Ext,P):-
	%--Considera la condición de corte que ninguno de los defaults de la
	%--teoria que no se encuentren en P sean aplicables al conjunto 
	%--In=Th(union(W,S)), o sea que el proceso P es cerrado
	forall(
		(d(Default),not(member(Default,P))),
		not(def_aplicable(S,Default))
	),
	
	%--Devuelve como resultado el proceso P y la lista de literales de
	%--la extensión
	lits_in_thWUS(S,Ext).

%--seleccionar_default(-Default), selecciona un default tal que d(Default).
sel_default(Default):-
	d(Default).

%--get_prerre(+Default,-PreR), dado un default Default, determina su
%--prerrequisito PreR.
get_prerre(Default,PreR):-
	Default=(PreR:_/_).

%--get_justif(+Default,-Justif), dado un default Default determina su
%--justificación Justif.
get_justif(Default,Justif):-
	Default=(_:Justif/_).

%--get_consec(+Default,-Consec), dado un default Default, determina su
%--consecuente Consec.
get_consec(Default,Consec):-
	Default=(_:_/Consec).

%--TODO: VER SI PUEDE RECONOCER PREDICADOS
%--es_literal(+Exp), determina si una expresión Exp es un literal.
es_literal(Exp):-
	atom(Exp).
es_literal(Exp):-
	not(atom(Exp)),
	functor(Exp,F,_A),
	F\=and,
	F\=or,
	F\=¬,
	F\=(->),
	F\=(',').
es_literal(Exp):-
	Exp=¬NExp,
	es_literal(NExp).

%--negacion(+Exp,-NExp), determina la negación de Exp, de manera de evitar
%--doble negaciones: por ejemplo si Exp=¬a, NExp=a.
negacion(true,false).
negacion(false,true).
negacion(Exp,NExp):-
	Exp\=true,
	Exp\=false,
	
	Exp\=¬_,
	NExp=¬Exp.
negacion(Exp,NExp):-
	Exp\=true,
	Exp\=false,
	
	Exp=¬NExp.

%--miembro_justif(+Justif,-Elejust), dado una justificación Justif de un
%--default, determina un elemento particular Elejust.
miembro_justif(Justif,Justif):-
	es_literal(Justif).
miembro_justif(Justif,Elejust):-
	not(es_literal(Justif)),
	Justif=(Elejust,_RestJustif).
miembro_justif(Justif,Elejust):-
	not(es_literal(Justif)),
	Justif=(_Elejust,RestJustif),
	miembro_justif(RestJustif,Elejust).

%--def_aplicable(+S,-Default), determina si el default Default es aplicable
%--al conjunt In=Th(union(W,S)).
def_aplicable(S,Default):-
	%--Para que sea aplicable, debe suceder que:
	%--  1) el prerrequisto de Default pertenezca a In
	%--  2) para todo w perteneciente a la justificación de Default,
	%--  ¬w no debe pertenecer a In
	
	get_prerre(Default,PreR),
	get_justif(Default,Just),
	get_consec(Default,Cons),
	
	%--(1)
	in_thWUS(PreR,S),
	
	%--(2)
	forall(
		miembro_justif(Just,Elejust),
		(
			negacion(Elejust,NElejust),
			not(in_thWUS(NElejust,S))
		)
	),
	
	%--Verifica que el nuevo proceso PNuevo es exitoso
	%--Primero ve si el conjunto In=Th(union(W,SNuevo)) es consistente
	es_consistente([Cons|S]).