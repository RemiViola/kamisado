/*	une case vide est représentée par une liste [ligne,colonne,couleur]
	une case occupée est représentée par une liste [ligne,colonne,couleur,tour,joueur]
	les joueurs sont a et b
	les couleurs sont orange(or), bleu(bl), violet(pu), rose(pi), jaune(ye), rouge(re), vert(gr) et marron(br)
	les directions de déplacement sont gauche(le), avant(fo) et droite(ri)
*/

/*Le plateau modifiable*/
:-dynamic(plateau/1).

plateau([
[1,1,or,or,b],[1,2,bl,bl,b],[1,3,pu,pu,b],[1,4,pi,pi,b],[1,5,ye,ye,b],[1,6,re,re,b],[1,7,gr,gr,b],[1,8,br,br,b],
[2,1,re],[2,2,or],[2,3,pi],[2,4,gr],[2,5,bl],[2,6,ye],[2,7,br],[2,8,pu],
[3,1,gr],[3,2,pi],[3,3,or],[3,4,re],[3,5,pu],[3,6,br],[3,7,ye],[3,8,bl],
[4,1,pi],[4,2,pu],[4,3,bl],[4,4,or],[4,5,br],[4,6,gr],[4,7,re],[4,8,ye],
[5,1,ye],[5,2,re],[5,3,gr],[5,4,br],[5,5,or],[5,6,bl],[5,7,pu],[5,8,pi],
[6,1,bl],[6,2,ye],[6,3,br],[6,4,pu],[6,5,re],[6,6,or],[6,7,pi],[6,8,gr],
[7,1,pu],[7,2,br],[7,3,ye],[7,4,bl],[7,5,gr],[7,6,pi],[7,7,or],[7,8,re],
[8,1,br,br,a],[8,2,gr,gr,a],[8,3,re,re,a],[8,4,ye,ye,a],[8,5,pi,pi,a],[8,6,pu,pu,a],[8,7,bl,bl,a],[8,8,or,or,a]]).

/*La gestion de la couleur courante*/
:-dynamic(couleur/1).

couleur([or,bl,pu,pi,ye,re,gr,br]).

/*Pour recharger le plateau*/
recharger:-
	retract(plateau(_)),
	assert(plateau([[1,1,or,or,b],[1,2,bl,bl,b],[1,3,pu,pu,b],[1,4,pi,pi,b],[1,5,ye,ye,b],[1,6,re,re,b],[1,7,gr,gr,b],[1,8,br,br,b],[2,1,re],[2,2,or],[2,3,pi],[2,4,gr],[2,5,bl],[2,6,ye],[2,7,br],[2,8,pu],[3,1,gr],[3,2,pi],[3,3,or],[3,4,re],[3,5,pu],[3,6,br],[3,7,ye],[3,8,bl],[4,1,pi],[4,2,pu],[4,3,bl],[4,4,or],[4,5,br],[4,6,gr],[4,7,re],[4,8,ye],[5,1,ye],[5,2,re],[5,3,gr],[5,4,br],[5,5,or],[5,6,bl],[5,7,pu],[5,8,pi],[6,1,bl],[6,2,ye],[6,3,br],[6,4,pu],[6,5,re],[6,6,or],[6,7,pi],[6,8,gr],[7,1,pu],[7,2,br],[7,3,ye],[7,4,bl],[7,5,gr],[7,6,pi],[7,7,or],[7,8,re],[8,1,br,br,a],[8,2,gr,gr,a],[8,3,re,re,a],[8,4,ye,ye,a],[8,5,pi,pi,a],[8,6,pu,pu,a],[8,7,bl,bl,a],[8,8,or,or,a]])),
	consult('k2.pl'),
	dessiner,!.

case(X):-
	plateau(L),
	member(X,L).

/*Déplacement d'une tour du joueur a et modification du plateau
deplacer(Tour,Direction,Nb_Case)*/
deplacer_a(T,fo,N):-
	plateau(P),
	member([A,O,C,T,a],P),
	couleur(LC),
	member(C,LC),
	accessible(a,T,L),
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
	member([A,O,C,T,a],P),
	couleur(LC),
	member(C,LC),
	accessible(a,T,L),
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
	member([A,O,C,T,a],P),
	couleur(LC),
	member(C,LC),
	accessible(a,T,L),
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
	member([A,O,C,T,b],P),
	couleur(LC),
	member(C,LC),
	accessible(b,T,L),
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
	member([A,O,C,T,b],P),
	couleur(LC),
	member(C,LC),
	accessible(b,T,L),
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
	member([A,O,C,T,b],P),
	couleur(LC),
	member(C,LC),
	accessible(b,T,L),
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
modifier_plateau([H|TT],A,O,NA,NO,T,J,NL):-
	H = [A,O,C,T,J],
	modifier_plateau(TT,A,O,NA,NO,T,J,NT),
	append([[A,O,C]],NT,NL).
modifier_plateau([H|TT],A,O,NA,NO,T,J,NL):-
	H = [NA,NO,C],
	modifier_plateau(TT,A,O,NA,NO,T,J,NT),
	append([[NA,NO,C,T,J]],NT,NL).
modifier_plateau([H|TT],A,O,NA,NO,T,J,NL):-
	H \= [NA,NO,C],
	H \= [A,O,C,T,J],
	modifier_plateau(TT,A,O,NA,NO,T,J,NT),
	append([H],NT,NL).

/*Recherche des cases accessibles pour le tour T du joueur b*/
accessible(b,T,L):-
	plateau(P),
	member([A,O,_C,T,b],P),
	AA is A+1,
	OG is O-1,
	OD is O+1,
	accessible_gauche_b(AA,OG,LG,P),
	accessible_avant_b(AA,O,LA,P),
	accessible_droite_b(AA,OD,LD,P),
	/*write(LG),nl,write(LA),nl,write(LD),nl,*/
	append(LG,LA,LL),
	append(LL,LD,L),!.

accessible_gauche_b(_A,0,[],_P):-!.
accessible_gauche_b(9,_O,[],_P):-!.
accessible_gauche_b(A,O,[],P):-
	member([A,O,_,_,a],P),!.
accessible_gauche_b(A,O,[],P):-
	member([A,O,_,_,b],P),!.
accessible_gauche_b(A,O,L,P):-
	AA is A+1,
	OO is O-1,
	accessible_gauche_b(AA,OO,LL,P),
	member([A,O,X],P),
	append([[A,O,X]],LL,L).

accessible_avant_b(9,_O,[],_P):-!.
accessible_avant_b(A,O,[],P):-
	member([A,O,_,_,a],P),!.
accessible_avant_b(A,O,[],P):-
	member([A,O,_,_,b],P),!.
accessible_avant_b(A,O,L,P):-
	AA is A+1,
	accessible_avant_b(AA,O,LL,P),
	member([A,O,X],P),
	append([[A,O,X]],LL,L).

accessible_droite_b(9,_O,[],_P):-!.
accessible_droite_b(_A,9,[],_P):-!.
accessible_droite_b(A,O,[],P):-
	member([A,O,_,_,a],P),!.
accessible_droite_b(A,O,[],P):-
	member([A,O,_,_,b],P),!.
accessible_droite_b(A,O,L,P):-
	AA is A+1,
	OO is O+1,
	accessible_droite_b(AA,OO,LL,P),
	member([A,O,X],P),
	append([[A,O,X]],LL,L).

/*Recherche des cases accessibles pour le tour T du joueur a*/
accessible(a,T,L):-
	plateau(P),
	member([A,O,_C,T,a],P),
	AA is A-1,
	OG is O-1,
	OD is O+1,
	accessible_gauche_a(AA,OG,LG,P),
	accessible_avant_a(AA,O,LA,P),
	accessible_droite_a(AA,OD,LD,P),
	/*write(LG),nl,write(LA),nl,write(LD),nl,*/
	append(LG,LA,LL),
	append(LL,LD,L),!.

accessible_gauche_a(_A,0,[],_P):-!.
accessible_gauche_a(0,_O,[],_P):-!.
accessible_gauche_a(A,O,[],P):-
	member([A,O,_,_,a],P).
accessible_gauche_a(A,O,[],P):-
	member([A,O,_,_,b],P).
accessible_gauche_a(A,O,L,P):-
	AA is A-1,
	OO is O-1,
	accessible_gauche_a(AA,OO,LL,P),
	member([A,O,X],P),
	append([[A,O,X]],LL,L).

accessible_avant_a(0,_O,[],_P):-!.
accessible_avant_a(A,O,[],P):-
	member([A,O,_,_,a],P).
accessible_avant_a(A,O,[],P):-
	member([A,O,_,_,b],P).
accessible_avant_a(A,O,L,P):-
	AA is A-1,
	accessible_avant_a(AA,O,LL,P),
	member([A,O,X],P),
	append([[A,O,X]],LL,L).

accessible_droite_a(0,_O,[],_P):-!.
accessible_droite_a(_A,9,[],_P):-!.
accessible_droite_a(A,O,[],P):-
	member([A,O,_,_,a],P).
accessible_droite_a(A,O,[],P):-
	member([A,O,_,_,b],P).
accessible_droite_a(A,O,L,P):-
	AA is A-1,
	OO is O+1,
	accessible_droite_a(AA,OO,LL,P),
	member([A,O,X],P),
	append([[A,O,X]],LL,L).

/*dessin du plateau en mode console*/
dessiner_case([_,_,X]):-
	write(X),write('     '),!.
dessiner_case([_,_,X,Y,Z]):-
	write(X),write(' '),write(Y),write(' '),write(Z).

dessiner_plateau([],_).
dessiner_plateau([C1,C2,C3,C4,C5,C6,C7,C8|T],N_ligne):-
	write(N_ligne),
	N_ligne_ is N_ligne+1,
	write('|'),dessiner_case(C1),
	write('|'),dessiner_case(C2),
	write('|'),dessiner_case(C3),
	write('|'),dessiner_case(C4),
	write('|'),dessiner_case(C5),
	write('|'),dessiner_case(C6),
	write('|'),dessiner_case(C7),
	write('|'),dessiner_case(C8),
	write('|'),nl,
	write(' |       |       |       |       |       |       |       |       |'),nl,
	write(' |       |       |       |       |       |       |       |       |'),nl,
	write('------------------------------------------------------------------'),nl,
	dessiner_plateau(T,N_ligne_).

dessiner:-
	write(' |   1   |   2   |   3   |   4   |   5   |   6   |   7   |   8   |'),nl,
	write('------------------------------------------------------------------'),nl,
	plateau(List),
	dessiner_plateau(List,1).


