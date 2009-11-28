%-- Definicion de operadores

:- dynamic w/1,d/1.

%--:- op(699,xfx,'->').
%--:- op(100,fx,'¬').

:-op(1050,xfx,user:(->)).
:-op(200,fx,user:(¬)).
:-op(201, xfy, user:(and)).
:-op(202, xfy, user:(or)).



%--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-- in_thWUS(+Formula, +S)
%--

in_thWUS(Formula, S):- entails(S, Formula).


%-- in_thWUS(a(1) and b(2) -> c(1, 2), [a(1), ¬d(1)]).
%-- in_thWUS(a(1), [a(1), ¬d(1)]).
%-- in_thWUS(x and y, [a(1), ¬d(1)]).
%-- in_thWUS(a(X) and ¬b(Y) and y, [a(1), ¬d(1)]).
%-- in_thWUS(a(X) and ¬b(3) and y, [a(1), ¬d(1)]).


%--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-- lits_in_thWUS(+S, -LitsInThS)
%--

lits_in_thWUS(S, LitsInThWUS):- 
	findall(L, entails_lit(S, L), LitsInThWUS_with_rep),
	list_to_set(LitsInThWUS_with_rep, LitsInThWUS).


%-- lits_in_thWUS([a(1), ¬d(1)], LitsInThWUS), member(X, LitsInThWUS).

%-- lits_in_th([a(X) and b(Y) -> c(X, Y), ¬d(Z) -> ¬c(Z, 2),
%-- a(1), ¬d(1), e(3), e(X) -> f(X)], LitsInThS).

%-- lits_in_thWUS([a(1), ¬d(1), w and x and y], LitsInThS),
%-- member(X, LitsInThS).

%-- lits_in_th([a->¬c, b->c, ¬b->f, x, y, true, c->¬a, ¬c->¬b,
%-- ¬f->b, a,¬c,¬b,f,g,e], LitsInThS), member(X, LitsInThS).

%-- lits_in_th([a->¬c, b->c, ¬b->f, x, y, true, c->¬a, ¬c->¬b,
%-- ¬f->b, b, c, ¬a, e], LitsInThS), member(X, LitsInThS).



%--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-- entails(+S, +Formula)
%--

entails(_S, true).

entails(_S, Ant -> L):- !,
                       ( w(Ant -> L), !
                        ;
                         w(Ant1 -> L1),
                         equivalent(Ant1 ->L1, Ant -> L)
                       ).

entails(S,¬(X and Y)):-
	entails(S,(¬X or ¬Y)).
entails(S,¬(X or Y)):-
	entails(S,(¬X and ¬Y)).
	
entails(S, X and Y):- !,
                      entails(S, X),
                      entails(S, Y).

entails(S, X or Y):- (entails(S, X);entails(S, Y)).


entails(_S, L):- w(L). %-- L es un literal

entails(_S, L):- w(X and Y),
                memberConj(L, X and Y).

entails(S, L):- member(L, S). %-- L es un literal

entails(S, L):- member(X and Y, S),
                memberConj(L, X and Y).


%--entails(S, L):- w(Ant -> L),
%--                entails(S, Ant), !
%--                ;
%--                w(Ant1, L1),
%--                equivalent(Ant1 ->L1, Ant -> L),
%--                entails(S, Ant).
                
entails(S, L):- (w(Ant -> L)
                ;
                w(Ant1 -> L1),
                equivalent(Ant1 ->L1, Ant -> L)),
                entails(S, Ant), !.



%-- entails([a and b -> c, ¬d -> ¬c, a, b], d).
%-- entails([a and b -> c, ¬d -> ¬c, a, ¬d], ¬b).
%-- entails([a(X) and b(Y) -> c(X, Y), ¬d(Z) -> ¬c(Z, 2), a(1), ¬d(1)], ¬b(R)).

                
%--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-- memberConj(?L, +Conj)
%--

memberConj(L, L and _Y). %-- Si L es el primer literal de Conj

memberConj(L, L):- L \= _X and _Y. %-- Si L es el único literal de Conj

memberConj(L, _X and Y):-
	memberConj(L, Y). %-- Si L es un literal del resto de Conj


%--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-- entails_lit(+S, ?L)
%--

entails_lit(_S, true).

entails_lit(_S, L):- w(L), literal(L).

entails_lit(_S, L):- w(X and Y),
                    memberConj(L, X and Y).

entails_lit(S, L):- member(L, S), literal(L).

entails_lit(S, L):- member(X and Y, S),
                    memberConj(L, X and Y).


entails_lit(S, L):- (w(Ant -> L)
                     ;
                     w(Ant1 -> L1),
                     equivalent(Ant1 ->L1, Ant -> L)
                     ),
                     entails(S, Ant).


%--equivalent(+Ant1 ->+L1, -Ant -> -Lcomp)

equivalent(Ant1 ->L1, Ant -> Lcomp):-
                compl(L1, L1comp),
                replaceInConj(L, L1comp, Ant1, Ant),
                compl(L, Lcomp).

%-- replaceInConj(?L1, +L2, +Conj, -Res)

replaceInConj(L1, L2, L1 and RestoConj, L2 and RestoConj).

replaceInConj(L1, L2, L and RestoConj, L and RestoConjconL2):-
              replaceInConj(L1, L2, RestoConj, RestoConjconL2).
              
replaceInConj(L1, L2, L1, L2):-
              %--literal(L1),
              L1 \= _ and _.


compl(¬A, A):- literal(A).
compl(A, ¬A):- literal(¬A).
              
literal(¬A):-
	A \= ¬_, A \= (_ and _), A \= (_ or _), A \= (_ -> _). %--atom(A).

literal(A):-
	A \= ¬_, A \= (_ and _), A \= (_ or _), A \= (_ -> _). %--atom(A).



%--w(a(X) and b(Y) -> c(X, Y)).
%--w(¬d(Z) -> ¬c(Z, 2)).
%--w(e(3)).
%--w(e(X) -> f(X)).
%--w(w and x and y).

%--d(a: ¬b/ ¬c).
