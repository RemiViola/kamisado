/*	une case vide est représentée par une liste [ligne,colonne,couleur]
	une case occupée est représentée par une liste [ligne,colonne,couleur,tour,joueur]
	les joueurs sont a et b
	les couleurs sont orange(orange), bleu(blue), violet(purple), rose(pink), jaune(yellow), rouge(red), vert(green) et marron(brown)
	les directions de déplacement sont gauche(left), avant(forward) et droite(right)

*/

:- use_module(library(pce)).

/*Le plateau modifiable*/
:-dynamic(plateau/1).

plateau([
[1,1,orange,orange,b],[1,2,blue,blue,b],[1,3,purple,purple,b],[1,4,pink,pink,b],[1,5,yellow,yellow,b],[1,6,red,red,b],[1,7,green,green,b],[1,8,brown,brown,b],
[2,1,red],[2,2,orange],[2,3,pink],[2,4,green],[2,5,blue],[2,6,yellow],[2,7,brown],[2,8,purple],
[3,1,green],[3,2,pink],[3,3,orange],[3,4,red],[3,5,purple],[3,6,brown],[3,7,yellow],[3,8,blue],
[4,1,pink],[4,2,purple],[4,3,blue],[4,4,orange],[4,5,brown],[4,6,green],[4,7,red],[4,8,yellow],
[5,1,yellow],[5,2,red],[5,3,green],[5,4,brown],[5,5,orange],[5,6,blue],[5,7,purple],[5,8,pink],
[6,1,blue],[6,2,yellow],[6,3,brown],[6,4,purple],[6,5,red],[6,6,orange],[6,7,pink],[6,8,green],
[7,1,purple],[7,2,brown],[7,3,yellow],[7,4,blue],[7,5,green],[7,6,pink],[7,7,orange],[7,8,red],
[8,1,brown,brown,a],[8,2,green,green,a],[8,3,red,red,a],[8,4,yellow,yellow,a],[8,5,pink,pink,a],[8,6,purple,purple,a],[8,7,blue,blue,a],[8,8,orange,orange,a]]).

/*La gestion de la couleur courante*/
:-dynamic(couleur/1).

couleur([orange,blue,purple,pink,yellow,red,green,brown]).

/*La gestion du joueur courant*/
:-dynamic(joueur/1).

joueur([a,b]).

/*La gestion de la fenetre*/

case(X):-
	plateau(L),
	member(X,L).

/*Teste la victoire du joueur*/
tester(A):-
	A \= 1,!.

tester(A):-
	A is 1,
	retract(joueur(_)),
	retract(couleur(_)),
	send(@p, display,new(T, text('YOU WIN')), point(660, 140)),
	send(T, font, font(times, bold, 40)).

/*Déplacement d'une tour et modification du plateau connaissant les coordonnées d'arrivée
Attention ne s'occupe pas de la mise à jour des variables globales car sert à la base pour ia_remi...*/
deplacer2(J,T,NA,NO,NP,NC):-
	plateau(P),
	member([A,O,_C,T,J],P),
	couleur(LC),
	member(T,LC),
	accessible(J,T,P,L),
	member([NA,NO,NC],L),
	modifier_plateau(P,A,O,NA,NO,T,J,NP),
	/*dessiner_plateau(NP,1),*/!.

/*Déplacement d'une tour du joueur a et modification du plateau
deplacer(Tour,Direction,Nb_Case)*/
deplacer_a(T,forward,N):-
	write('je rentre '),write(T),
	joueur(LJ),
	member(a,LJ),
	plateau(P),
	member([A,O,_C,T,a],P),
	couleur(LC),
	member(T,LC),
	accessible(a,T,P,L),
	NA is A-N,
	member([NA,O,NC],L),
	retract(joueur(_)),
	assert(joueur([b])),
	retract(couleur(LC)),
	assert(couleur([NC])),
	modifier_plateau(P,A,O,NA,O,T,a,NP),
	retract(plateau(P)),
	assert(plateau(NP)),
	redessiner,
	tester(NA),!.
deplacer_a(T,left,N):-
	joueur(LJ),
	member(a,LJ),
	plateau(P),
	member([A,O,_C,T,a],P),
	couleur(LC),
	member(T,LC),
	accessible(a,T,P,L),
	NA is A-N,
	NO is O-N,
	member([NA,NO,NC],L),
	retract(joueur(_)),
	assert(joueur([b])),
	retract(couleur(LC)),
	assert(couleur([NC])),
	modifier_plateau(P,A,O,NA,NO,T,a,NP),
	retract(plateau(P)),
	assert(plateau(NP)),
	redessiner,
	tester(NA),!.
deplacer_a(T,right,N):-
	joueur(LJ),
	member(a,LJ),
	plateau(P),
	member([A,O,_C,T,a],P),
	couleur(LC),
	member(T,LC),
	accessible(a,T,P,L),
	NA is A-N,
	NO is O+N,
	member([NA,NO,NC],L),
	retract(joueur(_)),
	assert(joueur([b])),
	retract(couleur(LC)),
	assert(couleur([NC])),
	modifier_plateau(P,A,O,NA,NO,T,a,NP),
	retract(plateau(P)),
	assert(plateau(NP)),
	redessiner,
	tester(NA),!.

/*Déplacement d'une tour du joueur b et modification du plateau
deplacer(Tour,Direction,Nb_Case)*/
deplacer_b(T,forward,N):-
	joueur(LJ),
	member(b,LJ),
	plateau(P),
	member([A,O,_C,T,b],P),
	couleur(LC),
	member(T,LC),
	accessible(b,T,P,L),
	NA is A+N,
	member([NA,O,NC],L),
	retract(joueur(_)),
	assert(joueur([a])),
	retract(couleur(LC)),
	assert(couleur([NC])),
	modifier_plateau(P,A,O,NA,O,T,b,NP),
	retract(plateau(P)),
	assert(plateau(NP)),
	redessiner,!.
deplacer_b(T,left,N):-
	joueur(LJ),
	member(b,LJ),
	plateau(P),
	member([A,O,_C,T,b],P),
	couleur(LC),
	member(T,LC),
	accessible(b,T,P,L),
	NA is A+N,
	NO is O-N,
	member([NA,NO,NC],L),
	retract(joueur(_)),
	assert(joueur([a])),
	retract(couleur(LC)),
	assert(couleur([NC])),
	modifier_plateau(P,A,O,NA,NO,T,b,NP),
	retract(plateau(P)),
	assert(plateau(NP)),
	redessiner,!.
deplacer_b(T,right,N):-
	joueur(LJ),
	member(b,LJ),
	plateau(P),
	member([A,O,_C,T,b],P),
	couleur(LC),
	member(T,LC),
	accessible(b,T,P,L),
	NA is A+N,
	NO is O+N,
	member([NA,NO,NC],L),
	retract(joueur(_)),
	assert(joueur([a])),
	retract(couleur(LC)),
	assert(couleur([NC])),
	modifier_plateau(P,A,O,NA,NO,T,b,NP),
	retract(plateau(P)),
	assert(plateau(NP)),
	redessiner,!.

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
accessible(b,T,P,L):-
	member([A,O,_C,T,b],P),
	AA is A+1,
	OG is O-1,
	OD is O+1,
	accessible_avant_b(AA,O,LA,P),
	accessible_gauche_b(AA,OG,LG,P),
	accessible_droite_b(AA,OD,LD,P),
	/*write(LG),nl,write(LA),nl,write(LD),nl,*/
	append(LG,LA,LL),
	append(LL,LD,L),!.

/*Recherche des cases accessibles pour le tour T du joueur a*/
accessible(a,T,P,L):-
	member([A,O,_C,T,a],P),
	AA is A-1,
	OG is O-1,
	OD is O+1,
	accessible_avant_a(AA,O,LA,P),
	accessible_gauche_a(AA,OG,LG,P),
	accessible_droite_a(AA,OD,LD,P),
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

/*dessin du plateau avec XPCE*//*deplacer2(J,T,NA,NO,NP,NC)*/
dessiner_case(P,[A,O,C]):-
	send(P, display,new(B, box(80,80)), point(10+(O-1)*80,10+(A-1)*80)),
	send(B, radius, 5),
	send(B, fill_pattern, colour(C)),!.
dessiner_case(P,[A,O,C,T,b]):-
	send(P, display,new(B, box(80,80)), point(10+(O-1)*80,10+(A-1)*80)),
	send(B, radius, 5),
	send(B, fill_pattern, colour(C)),
	send(P, display,new(J, circle(60)), point(20+(O-1)*80,20+(A-1)*80)),
	send(J, pen, 10),
	send(J, fill_pattern, colour(T)),!.
dessiner_case(P,[A,O,C,T,a]):-
	send(P, display,new(B, box(80,80)), point(10+(O-1)*80,10+(A-1)*80)),
	send(B, radius, 5),
	send(B, fill_pattern, colour(C)),
	send(P, display,new(J, circle(60)), point(20+(O-1)*80,20+(A-1)*80)),
	send(J, pen, 10),
	send(J, fill_pattern, colour(T)),
	send(J, colour(white)),
	new(K, click_gesture(left, '', double,message(@prolog, gestion, @receiver))),
	send(J, recogniser, K),!.

dessiner_plateau([],_).
dessiner_plateau([C1,C2,C3,C4,C5,C6,C7,C8|T],P):-
	dessiner_case(P,C1),
	dessiner_case(P,C2),
	dessiner_case(P,C3),
	dessiner_case(P,C4),
	dessiner_case(P,C5),
	dessiner_case(P,C6),
	dessiner_case(P,C7),
	dessiner_case(P,C8),
	dessiner_plateau(T,P).

dessiner:-
	new(@p, picture('KAMISADO')),
	send(@p, size, size(900,640)),
	assert(fenetre(@p)),
	ecrire,
	plateau(List),
	dessiner_plateau(List,@p),
	send(@p, open).

redessiner:-
	fenetre(P),
	ecrire,
	plateau(List),
	dessiner_plateau(List,P),
	send(P, open).

ecrire:-
	joueur([a]),
	couleur([LC]),
	free(@t1),
	free(@t3),
	send(@p, display,new(@t1, text('Joueur actif : blanc')), point(660, 40)),
	send(@p, display,new(@t3, text(LC)), point(790, 80)),
	send(@t1, font, font(times, bold, 18)),
	send(@t3, font, font(times, bold, 18)).

ecrire:-
	joueur([b]),
	couleur([LC]),
	free(@t1),
	free(@t3),
	send(@p, display,new(@t1, text('Joueur actif : noir')), point(660, 40)),
	send(@p, display,new(@t3, text(LC)), point(790, 80)),
	send(@t1, font, font(times, bold, 18)),
	send(@t3, font, font(times, bold, 18)).

ecrire:-
	couleur([_,_|_]),
	joueur([a,b]),
	send(@p, display,new(@t1, text('Joueur actif : ')), point(660, 40)),
	send(@p, display,new(@t2, text('Couleur active : ')), point(660, 80)),
	send(@p, display,new(@t3, text('')), point(790, 80)),
	send(@t1, font, font(times, bold, 18)),
	send(@t2, font, font(times, bold, 18)),
	send(@t3, font, font(times, bold, 18)).

gestion(Gr):-
	couleur(C),
	new(D, dialog(string('Deplacement de la tour %s de %s :', Gr?fill_pattern,Gr?colour))),
	send(D, append, new(Coul, menu(couleur))),
	send(D, append, new(Dir, menu(direction))),
	send(D, append, new(Nb, menu(valeur))),
	send_list(Coul, append, C),
	send_list(Dir, append, [forward, left, right]),
	send_list(Nb, append, [1,2,3,4,5,6]),
	send(D, append, button(enter, and(message(@prolog,deplacer_a,
		Coul?selection,
		Dir?selection,
		Nb?selection),
		and(message(@prolog,jouer),message(D, destroy))))),
	send(D, append, button(quit, message(D, destroy))),
	send(D, default_button, enter),
	send(D, open).

















