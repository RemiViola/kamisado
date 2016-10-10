/*	une case vide est représentée par une liste [ligne,colonne,couleur]
	une case occupée est représentée par une liste [ligne,colonne,couleur,tour,joueur]
	les joueurs sont a et b
	les couleurs sont orange(or), bleu(bl), violet(pu), rose(pi), jaune(ye), rouge(re), vert(gr) et marron(br)
	les directions de déplacement sont gauche(le), avant(fo) et droite(ri)
*/

/*Le plateau modifiable*/
:-dynamic(plateau/1).

plateau([
[[1,1,or,or,b],[1,2,bl,bl,b],[1,3,pu,pu,b],[1,4,pi,pi,b],[1,5,ye,ye,b],[1,6,re,re,b],[1,7,gr,gr,b],[1,8,br,br,b]],
[[2,1,re],[2,2,or],[2,3,pi],[2,4,gr],[2,5,bl],[2,6,ye],[2,7,br],[2,8,pu]],
[[3,1,gr],[3,2,pi],[3,3,or],[3,4,re],[3,5,pu],[3,6,br],[3,7,ye],[3,8,bl]],
[[4,1,pi],[4,2,pu],[4,3,bl],[4,4,or],[4,5,br],[4,6,gr],[4,7,re],[4,8,ye]],
[[5,1,ye],[5,2,re],[5,3,gr],[5,4,br],[5,5,or],[5,6,bl],[5,7,pu],[5,8,pi]],
[[6,1,bl],[6,2,ye],[6,3,br],[6,4,pu],[6,5,re],[6,6,or],[6,7,pi],[6,8,gr]],
[[7,1,pu],[7,2,br],[7,3,ye],[7,4,bl],[7,5,gr],[7,6,pi],[7,7,or],[7,8,re]],
[[8,1,br,br,a],[8,2,gr,gr,a],[8,3,re,re,a],[8,4,ye,ye,a],[8,5,pi,pi,a],[8,6,pu,pu,a],[8,7,bl,bl,a],[8,8,or,or,a]]]).

/*La gestion de la couleur courante*/
:-dynamic(couleur/1).

couleur([or,bl,pu,pi,ye,re,gr,br]).

/*Pour recharger le plateau*/
recharger:-
	retract(plateau(_)),
	assert(plateau([[[1,1,or,or,b],[1,2,bl,bl,b],[1,3,pu,pu,b],[1,4,pi,pi,b],[1,5,ye,ye,b],[1,6,re,re,b],[1,7,gr,gr,b],[1,8,br,br,b]],[[2,1,re],[2,2,or],[2,3,pi],[2,4,gr],[2,5,bl],[2,6,ye],[2,7,br],[2,8,pu]],[[3,1,gr],[3,2,pi],[3,3,or],[3,4,re],[3,5,pu],[3,6,br],[3,7,ye],[3,8,bl]],[[4,1,pi],[4,2,pu],[4,3,bl],[4,4,or],[4,5,br],[4,6,gr],[4,7,re],[4,8,ye]],[[5,1,ye],[5,2,re],[5,3,gr],[5,4,br],[5,5,or],[5,6,bl],[5,7,pu],[5,8,pi]],[[6,1,bl],[6,2,ye],[6,3,br],[6,4,pu],[6,5,re],[6,6,or],[6,7,pi],[6,8,gr]],[[7,1,pu],[7,2,br],[7,3,ye],[7,4,bl],[7,5,gr],[7,6,pi],[7,7,or],[7,8,re]],[[8,1,br,br,a],[8,2,gr,gr,a],[8,3,re,re,a],[8,4,ye,ye,a],[8,5,pi,pi,a],[8,6,pu,pu,a],[8,7,bl,bl,a],[8,8,or,or,a]]])),
	consult('k.pl'),
	dessiner,!.

case(X):-
	plateau(L),
	member(LL,L),
	member(X,LL).

my_flatten([],[]):-!.
my_flatten([H|T],L):-
	my_flatten(T,LL),
	append(H,LL,L).

/*Déplacement d'une tour du joueur a et modification du plateau
deplacer(Tour,Direction,Nb_Case)*/
deplacer_a(T,fo,N):-
	plateau(P),
	my_flatten(P,PP),
	member([A,O,C,T,a],PP),
	couleur(LC),
	member(C,LC),
	accessible_a([A,O,C,T,a],L),
	NA is A-N,
	member([NA,O,NC],L),
	retract(couleur(LC)),
	assert(couleur([NC])),
	modifier_plateau(P,A,O,NA,O,T,a,NP),
	retract(plateau(P)),
	assert(plateau(NP)),
	dessiner,!.
deplacer_a(T,le,N):-
	plateau(P),
	my_flatten(P,PP),
	member([A,O,C,T,a],PP),
	couleur(LC),
	member(C,LC),
	accessible_a([A,O,C,T,a],L),
	NA is A-N,
	NO is O-N,
	member([NA,NO,NC],L),
	retract(couleur(LC)),
	assert(couleur([NC])),
	modifier_plateau(P,A,O,NA,NO,T,a,NP),
	retract(plateau(P)),
	assert(plateau(NP)),
	dessiner,!.
deplacer_a(T,ri,N):-
	plateau(P),
	my_flatten(P,PP),
	member([A,O,C,T,a],PP),
	couleur(LC),
	member(C,LC),
	accessible_a([A,O,C,T,a],L),
	NA is A-N,
	NO is O+N,
	member([NA,NO,NC],L),
	retract(couleur(LC)),
	assert(couleur([NC])),
	modifier_plateau(P,A,O,NA,NO,T,a,NP),
	retract(plateau(P)),
	assert(plateau(NP)),
	dessiner,!.

/*Déplacement d'une tour du joueur b et modification du plateau
deplacer(Tour,Direction,Nb_Case)*/
deplacer_b(T,fo,N):-
	plateau(P),
	my_flatten(P,PP),
	member([A,O,C,T,b],PP),
	couleur(LC),
	member(C,LC),
	accessible_b([A,O,C,T,b],L),
	NA is A+N,
	member([NA,O,NC],L),
	retract(couleur(LC)),
	assert(couleur([NC])),
	modifier_plateau(P,A,O,NA,O,T,b,NP),
	retract(plateau(P)),
	assert(plateau(NP)),
	dessiner,!.
deplacer_b(T,le,N):-
	plateau(P),
	my_flatten(P,PP),
	member([A,O,C,T,b],PP),
	couleur(LC),
	member(C,LC),
	accessible_b([A,O,C,T,b],L),
	NA is A+N,
	NO is O-N,
	member([NA,NO,NC],L),
	retract(couleur(LC)),
	assert(couleur([NC])),
	modifier_plateau(P,A,O,NA,NO,T,b,NP),
	retract(plateau(P)),
	assert(plateau(NP)),
	dessiner,!.
deplacer_b(T,ri,N):-
	plateau(P),
	my_flatten(P,PP),
	member([A,O,C,T,b],PP),
	couleur(LC),
	member(C,LC),
	accessible_b([A,O,C,T,b],L),
	NA is A+N,
	NO is O+N,
	member([NA,NO,NC],L),
	retract(couleur(LC)),
	assert(couleur([NC])),
	modifier_plateau(P,A,O,NA,NO,T,b,NP),
	retract(plateau(P)),
	assert(plateau(NP)),
	dessiner,!.

/*Modification du plateau de jeu
modifier_plateau(Plateau,Abs,Ord,NewAbs,NewOrd,Tour,Joueur,NewPlateau)*/
modifier_plateau([],_A,_O,_NA,_NO,_T,_J,[]):-!.
modifier_plateau([H|TT],A,O,NA,NO,T,J,NP):-
	modifier_liste(H,A,O,NA,NO,T,J,NH),
	modifier_plateau(TT,A,O,NA,NO,T,J,NT),
	append([NH],NT,NP).

modifier_liste([],_A,_O,_NA,_NO,_T,_J,[]):-!.
modifier_liste([H|TT],A,O,NA,NO,T,J,NL):-
	H = [A,O,C,T,J],
	modifier_liste(TT,A,O,NA,NO,T,J,NT),
	append([[A,O,C]],NT,NL).
modifier_liste([H|TT],A,O,NA,NO,T,J,NL):-
	H = [NA,NO,C],
	modifier_liste(TT,A,O,NA,NO,T,J,NT),
	append([[NA,NO,C,T,J]],NT,NL).
modifier_liste([H|TT],A,O,NA,NO,T,J,NL):-
	H \= [NA,NO,C],
	H \= [A,O,C,T,J],
	modifier_liste(TT,A,O,NA,NO,T,J,NT),
	append([H],NT,NL).

/*Recherche des cases accessibles depuis la case X occupée par le joueur b*/
accessible_b(X,L):-
	X = [A,O,_C,_T,b],
	plateau(P),
	my_flatten(P,PP),
	AA is A+1,
	OG is O-1,
	OD is O+1,
	accessible_gauche_b(AA,OG,LG,PP),
	accessible_avant_b(AA,O,LA,PP),
	accessible_droite_b(AA,OD,LD,PP),
	/*write(LG),nl,write(LA),nl,write(LD),nl,*/
	append(LG,LA,LL),
	append(LL,LD,L),!.

accessible_gauche_b(_A,0,[],_PP):-!.
accessible_gauche_b(9,_O,[],_PP):-!.
accessible_gauche_b(A,O,[],PP):-
	member([A,O,_,_,a],PP),!.
accessible_gauche_b(A,O,[],PP):-
	member([A,O,_,_,b],PP),!.
accessible_gauche_b(A,O,L,PP):-
	AA is A+1,
	OO is O-1,
	accessible_gauche_b(AA,OO,LL,PP),
	member([A,O,X],PP),
	append([[A,O,X]],LL,L).

accessible_avant_b(9,_O,[],_PP):-!.
accessible_avant_b(A,O,[],PP):-
	member([A,O,_,_,a],PP),!.
accessible_avant_b(A,O,[],PP):-
	member([A,O,_,_,b],PP),!.
accessible_avant_b(A,O,L,PP):-
	AA is A+1,
	accessible_avant_b(AA,O,LL,PP),
	member([A,O,X],PP),
	append([[A,O,X]],LL,L).

accessible_droite_b(9,_O,[],_PP):-!.
accessible_droite_b(_A,9,[],_PP):-!.
accessible_droite_b(A,O,[],PP):-
	member([A,O,_,_,a],PP),!.
accessible_droite_b(A,O,[],PP):-
	member([A,O,_,_,b],PP),!.
accessible_droite_b(A,O,L,PP):-
	AA is A+1,
	OO is O+1,
	accessible_droite_b(AA,OO,LL,PP),
	member([A,O,X],PP),
	append([[A,O,X]],LL,L).

/*Recherche des cases accessibles depuis la case X occupée par le joueur a*/
accessible_a(X,L):-
	X = [A,O,_C,_T,a],
	plateau(P),
	my_flatten(P,PP),
	AA is A-1,
	OG is O-1,
	OD is O+1,
	accessible_gauche_a(AA,OG,LG,PP),
	accessible_avant_a(AA,O,LA,PP),
	accessible_droite_a(AA,OD,LD,PP),
	/*write(LG),nl,write(LA),nl,write(LD),nl,*/
	append(LG,LA,LL),
	append(LL,LD,L),!.

accessible_gauche_a(_A,0,[],_PP):-!.
accessible_gauche_a(0,_O,[],_PP):-!.
accessible_gauche_a(A,O,[],PP):-
	member([A,O,_,_,a],PP).
accessible_gauche_a(A,O,[],PP):-
	member([A,O,_,_,b],PP).
accessible_gauche_a(A,O,L,PP):-
	AA is A-1,
	OO is O-1,
	accessible_gauche_a(AA,OO,LL,PP),
	member([A,O,X],PP),
	append([[A,O,X]],LL,L).

accessible_avant_a(0,_O,[],_PP):-!.
accessible_avant_a(A,O,[],PP):-
	member([A,O,_,_,a],PP).
accessible_avant_a(A,O,[],PP):-
	member([A,O,_,_,b],PP).
accessible_avant_a(A,O,L,PP):-
	AA is A-1,
	accessible_avant_a(AA,O,LL,PP),
	member([A,O,X],PP),
	append([[A,O,X]],LL,L).

accessible_droite_a(0,_O,[],_PP):-!.
accessible_droite_a(_A,9,[],_PP):-!.
accessible_droite_a(A,O,[],PP):-
	member([A,O,_,_,a],PP).
accessible_droite_a(A,O,[],PP):-
	member([A,O,_,_,b],PP).
accessible_droite_a(A,O,L,PP):-
	AA is A-1,
	OO is O+1,
	accessible_droite_a(AA,OO,LL,PP),
	member([A,O,X],PP),
	append([[A,O,X]],LL,L).

/*dessin du plateau en mode console*/
dessiner_case([_,_,X]):-
	write(X),write('     '),!.
dessiner_case([_,_,X,Y,Z]):-
	write(X),write(' '),write(Y),write(' '),write(Z).

dessiner_ligne([]):-
	write('|'),nl,
	write('|       |       |       |       |       |       |       |       |'),nl,
	write('|       |       |       |       |       |       |       |       |'),nl,
	write('-----------------------------------------------------------------'),nl.
dessiner_ligne([H|T]):-
	write('|'),dessiner_case(H),
	dessiner_ligne(T).

dessiner_plateau([]).
dessiner_plateau([H|T]):-
	dessiner_ligne(H),
	dessiner_plateau(T).

dessiner:-
	write('-----------------------------------------------------------------'),nl,
	plateau(L),
	dessiner_plateau(L).


