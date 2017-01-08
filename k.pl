/*	Une case vide est représentée par une liste [ligne,colonne,couleur]
	une case occupée est représentée par une liste [ligne,colonne,couleur,tour,joueur]
	les joueurs sont a et b
	les couleurs sont orange(orange), bleu(blue), violet(purple), rose(pink), jaune(yellow), rouge(red), vert(green) et marron(brown)
	les directions de déplacement sont gauche(left), avant(forward) et droite(right)
*/

/*Chargement de la librairie xpce pour l'interface graphique*/
:- use_module(library(pce)).

/*Chargement des différentes ia*/
:-use_module(ia_remi).
:-use_module(ia_mathieu).
:-use_module(ia_mathieu2).
:-use_module(ia_mathieu3).
:-use_module(ia_mathieu4).
:-use_module(ia_mathieu_alphabeta).
:-use_module(ia_valentin).

/*La gestion du nombre de récursion pour les ia_math*/
:-dynamic(n_round/1).

n_round(1).

/*La gestion de l'IA*/
:-dynamic(ia/1).

/*La gestion de la fenetre*/
:-dynamic(fenetre/1).

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

couleur([brown,green,red,yellow,pink,purple,blue,orange]).

/*La gestion du joueur courant*/
:-dynamic(joueur/1).

joueur([a,b]).

/*Teste la victoire du joueur*/
tester(Ligne):-
	Ligne \= 1,!.

tester(Ligne):-
	Ligne is 1,
	retract(joueur(_)),
	retract(couleur(_)),
	send(@p, display,new(Text, text('YOU WIN')), point(660, 140)),
	send(Text, font, font(times, bold, 40)).

/*Passer en cas de tour bloquée*/
passer:-
	joueur([a]),
	plateau(Plateau),
	couleur([Couleur]),
	member([_,_,Case,Couleur,a],Plateau),
	retract(couleur(_)),
	assert(couleur([Case])),
	retract(joueur(_)),
	assert(joueur([b])),!.

passer:-
	joueur([b]),
	plateau(Plateau),
	couleur([Couleur]),
	member([_,_,Case,Couleur,b],Plateau),
	retract(couleur(_)),
	assert(couleur([Case])),
	retract(joueur(_)),
	assert(joueur([a])),!.

/*Déplacement d'une tour et modification du plateau connaissant les coordonnées d'arrivée
Attention ne s'occupe pas de la mise à jour des variables globales*/
deplacer2(Joueur,Tour,NLigne,NColonne,NPlateau,NCouleur):-
	plateau(Plateau),
	member([Ligne,Colonne,_,Tour,Joueur],Plateau),
	couleur(L_couleur),
	member(Tour,L_couleur),
	accessible(Joueur,Tour,Plateau,L_accessible),
	member([NLigne,NColonne,NCouleur],L_accessible),
	modifier_plateau(Plateau,Ligne,Colonne,NLigne,NColonne,Tour,Joueur,NPlateau),!.

/*Déplacement d'une tour du joueur a et modification du plateau
deplacer(Tour,Direction,Nb_case)*/
deplacer_a(Tour,forward,Nb_case):-
	joueur(L_joueur),
	member(a,L_joueur),
	plateau(Plateau),
	member([Ligne,Colonne,_,Tour,a],Plateau),
	couleur(L_couleur),
	member(Tour,L_couleur),
	accessible(a,Tour,Plateau,L_accessible),
	NLigne is Ligne-Nb_case,
	member([NLigne,Colonne,NCouleur],L_accessible),
	retract(joueur(_)),
	assert(joueur([b])),
	retract(couleur(L_couleur)),
	assert(couleur([NCouleur])),
	modifier_plateau(Plateau,Ligne,Colonne,NLigne,Colonne,Tour,a,NPlateau),
	retract(plateau(Plateau)),
	assert(plateau(NPlateau)),
	redessiner,
	tester(NLigne),!.
deplacer_a(Tour,left,Nb_case):-
	joueur(L_joueur),
	member(a,L_joueur),
	plateau(Plateau),
	member([Ligne,Colonne,_,Tour,a],Plateau),
	couleur(L_couleur),
	member(Tour,L_couleur),
	accessible(a,Tour,Plateau,L_accessible),
	NLigne is Ligne-Nb_case,
	NColonne is Colonne-Nb_case,
	member([NLigne,NColonne,NCouleur],L_accessible),
	retract(joueur(_)),
	assert(joueur([b])),
	retract(couleur(L_couleur)),
	assert(couleur([NCouleur])),
	modifier_plateau(Plateau,Ligne,Colonne,NLigne,NColonne,Tour,a,NPlateau),
	retract(plateau(Plateau)),
	assert(plateau(NPlateau)),
	redessiner,
	tester(NLigne),!.
deplacer_a(Tour,right,Nb_case):-
	joueur(L_joueur),
	member(a,L_joueur),
	plateau(Plateau),
	member([Ligne,Colonne,_,Tour,a],Plateau),
	couleur(L_couleur),
	member(Tour,L_couleur),
	accessible(a,Tour,Plateau,L_accessible),
	NLigne is Ligne-Nb_case,
	NColonne is Colonne+Nb_case,
	member([NLigne,NColonne,NCouleur],L_accessible),
	retract(joueur(_)),
	assert(joueur([b])),
	retract(couleur(L_couleur)),
	assert(couleur([NCouleur])),
	modifier_plateau(Plateau,Ligne,Colonne,NLigne,NColonne,Tour,a,NPlateau),
	retract(plateau(Plateau)),
	assert(plateau(NPlateau)),
	redessiner,
	tester(NLigne),!.

/*Déplacement d'une tour du joueur b et modification du plateau
deplacer(Tour,Direction,Nb_case)*/
deplacer_b(Tour,forward,Nb_case):-
	joueur(L_joueur),
	member(b,L_joueur),
	plateau(Plateau),
	member([Ligne,Colonne,_,Tour,b],Plateau),
	couleur(L_couleur),
	member(Tour,L_couleur),
	accessible(b,Tour,Plateau,L_accessible),
	NLigne is Ligne+Nb_case,
	member([NLigne,Colonne,NCouleur],L_accessible),
	retract(joueur(_)),
	assert(joueur([a])),
	retract(couleur(L_couleur)),
	assert(couleur([NCouleur])),
	modifier_plateau(Plateau,Ligne,Colonne,NLigne,Colonne,Tour,b,NPlateau),
	retract(plateau(Plateau)),
	assert(plateau(NPlateau)),
	redessiner,!.
deplacer_b(Tour,left,Nb_case):-
	joueur(L_joueur),
	member(b,L_joueur),
	plateau(Plateau),
	member([Ligne,Colonne,_C,Tour,b],Plateau),
	couleur(L_couleur),
	member(Tour,L_couleur),
	accessible(b,Tour,Plateau,L_accessible),
	NLigne is Ligne+Nb_case,
	NColonne is Colonne-Nb_case,
	member([NLigne,NColonne,NCouleur],L_accessible),
	retract(joueur(_)),
	assert(joueur([a])),
	retract(couleur(L_couleur)),
	assert(couleur([NCouleur])),
	modifier_plateau(Plateau,Ligne,Colonne,NLigne,NColonne,Tour,b,NPlateau),
	retract(plateau(Plateau)),
	assert(plateau(NPlateau)),
	redessiner,!.
deplacer_b(Tour,right,Nb_case):-
	joueur(L_joueur),
	member(b,L_joueur),
	plateau(Plateau),
	member([Ligne,Colonne,_C,Tour,b],Plateau),
	couleur(L_couleur),
	member(Tour,L_couleur),
	accessible(b,Tour,Plateau,L_accessible),
	NLigne is Ligne+Nb_case,
	NColonne is Colonne+Nb_case,
	member([NLigne,NColonne,NCouleur],L_accessible),
	retract(joueur(_)),
	assert(joueur([a])),
	retract(couleur(L_couleur)),
	assert(couleur([NCouleur])),
	modifier_plateau(Plateau,Ligne,Colonne,NLigne,NColonne,Tour,b,NPlateau),
	retract(plateau(Plateau)),
	assert(plateau(NPlateau)),
	redessiner,!.

/*Modification du plateau de jeu
modifier_plateau(Plateau,Ligne,Colonne,NewLigne,NewColonne,Tour,Joueur,NewPlateau)*/
modifier_plateau([],_,_,_,_,_,_,[]):-!.
modifier_plateau([Case|Tail],Ligne,Colonne,NLigne,NColonne,Tour,Joueur,NPlateau):-
	Case = [Ligne,Colonne,Couleur,Tour,Joueur],
	modifier_plateau(Tail,Ligne,Colonne,NLigne,NColonne,Tour,Joueur,NTail),
	append([[Ligne,Colonne,Couleur]],NTail,NPlateau).
modifier_plateau([Case|Tail],Ligne,Colonne,NLigne,NColonne,Tour,Joueur,NPlateau):-
	Case = [NLigne,NColonne,Couleur],
	modifier_plateau(Tail,Ligne,Colonne,NLigne,NColonne,Tour,Joueur,NTail),
	append([[NLigne,NColonne,Couleur,Tour,Joueur]],NTail,NPlateau).
modifier_plateau([Case|Tail],Ligne,Colonne,NLigne,NColonne,Tour,Joueur,NPlateau):-
	Case \= [NLigne,NColonne,Couleur],
	Case \= [Ligne,Colonne,Couleur,Tour,Joueur],
	modifier_plateau(Tail,Ligne,Colonne,NLigne,NColonne,Tour,Joueur,NTail),
	append([Case],NTail,NPlateau).

/*Recherche des cases accessibles pour le tour T du joueur b*/
accessible(b,Tour,Plateau,L_accessible):-
	member([Ligne,Colonne,_,Tour,b],Plateau),
	LigneA is Ligne+1,
	ColonneG is Colonne-1,
	ColonneD is Colonne+1,
	accessible_avant_b(LigneA,Colonne,L_avant,Plateau),
	accessible_gauche_b(LigneA,ColonneG,L_gauche,Plateau),
	accessible_droite_b(LigneA,ColonneD,L_droite,Plateau),
	append(L_gauche,L_avant,L_intermediaire),
	append(L_intermediaire,L_droite,L_accessible),!.

/*Recherche des cases accessibles pour le tour T du joueur a*/
accessible(a,Tour,Plateau,L_accessible):-
	member([Ligne,Colonne,_,Tour,a],Plateau),
	LigneA is Ligne-1,
	ColonneG is Colonne-1,
	ColonneD is Colonne+1,
	accessible_avant_a(LigneA,Colonne,L_avant,Plateau),
	accessible_gauche_a(LigneA,ColonneG,L_gauche,Plateau),
	accessible_droite_a(LigneA,ColonneD,L_droite,Plateau),
	append(L_gauche,L_avant,L_intermediaire),
	append(L_intermediaire,L_droite,L_accessible),!.

accessible_gauche_b(_,0,[],_):-!.
accessible_gauche_b(9,_,[],_):-!.
accessible_gauche_b(Ligne,Colonne,[],Plateau):-
	member([Ligne,Colonne,_,_,a],Plateau),!.
accessible_gauche_b(Ligne,Colonne,[],Plateau):-
	member([Ligne,Colonne,_,_,b],Plateau),!.
accessible_gauche_b(Ligne,Colonne,L_accessible,Plateau):-
	LigneA is Ligne+1,
	ColonneG is Colonne-1,
	accessible_gauche_b(LigneA,ColonneG,L_intermediaire,Plateau),
	member([Ligne,Colonne,X],Plateau),
	append([[Ligne,Colonne,X]],L_intermediaire,L_accessible).

accessible_avant_b(9,_,[],_):-!.
accessible_avant_b(Ligne,Colonne,[],Plateau):-
	member([Ligne,Colonne,_,_,a],Plateau),!.
accessible_avant_b(Ligne,Colonne,[],Plateau):-
	member([Ligne,Colonne,_,_,b],Plateau),!.
accessible_avant_b(Ligne,Colonne,L_accessible,Plateau):-
	LigneA is Ligne+1,
	accessible_avant_b(LigneA,Colonne,L_intermediaire,Plateau),
	member([Ligne,Colonne,X],Plateau),
	append([[Ligne,Colonne,X]],L_intermediaire,L_accessible).

accessible_droite_b(9,_,[],_):-!.
accessible_droite_b(_,9,[],_):-!.
accessible_droite_b(Ligne,Colonne,[],Plateau):-
	member([Ligne,Colonne,_,_,a],Plateau),!.
accessible_droite_b(Ligne,Colonne,[],Plateau):-
	member([Ligne,Colonne,_,_,b],Plateau),!.
accessible_droite_b(Ligne,Colonne,L_accessible,Plateau):-
	LigneA is Ligne+1,
	ColonneD is Colonne+1,
	accessible_droite_b(LigneA,ColonneD,L_intermediaire,Plateau),
	member([Ligne,Colonne,X],Plateau),
	append([[Ligne,Colonne,X]],L_intermediaire,L_accessible).

accessible_gauche_a(_,0,[],_):-!.
accessible_gauche_a(0,_,[],_):-!.
accessible_gauche_a(Ligne,Colonne,[],Plateau):-
	member([Ligne,Colonne,_,_,a],Plateau).
accessible_gauche_a(Ligne,Colonne,[],Plateau):-
	member([Ligne,Colonne,_,_,b],Plateau).
accessible_gauche_a(Ligne,Colonne,L_accessible,Plateau):-
	LigneA is Ligne-1,
	ColonneG is Colonne-1,
	accessible_gauche_a(LigneA,ColonneG,L_intermediaire,Plateau),
	member([Ligne,Colonne,X],Plateau),
	append([[Ligne,Colonne,X]],L_intermediaire,L_accessible).

accessible_avant_a(0,_,[],_):-!.
accessible_avant_a(Ligne,Colonne,[],Plateau):-
	member([Ligne,Colonne,_,_,a],Plateau).
accessible_avant_a(Ligne,Colonne,[],Plateau):-
	member([Ligne,Colonne,_,_,b],Plateau).
accessible_avant_a(Ligne,Colonne,L_accessible,Plateau):-
	LigneA is Ligne-1,
	accessible_avant_a(LigneA,Colonne,L_intermediaire,Plateau),
	member([Ligne,Colonne,X],Plateau),
	append([[Ligne,Colonne,X]],L_intermediaire,L_accessible).

accessible_droite_a(0,_,[],_):-!.
accessible_droite_a(_,9,[],_):-!.
accessible_droite_a(Ligne,Colonne,[],Plateau):-
	member([Ligne,Colonne,_,_,a],Plateau).
accessible_droite_a(Ligne,Colonne,[],Plateau):-
	member([Ligne,Colonne,_,_,b],Plateau).
accessible_droite_a(Ligne,Colonne,L_accessible,Plateau):-
	LigneA is Ligne-1,
	ColonneD is Colonne+1,
	accessible_droite_a(LigneA,ColonneD,L_intermediaire,Plateau),
	member([Ligne,Colonne,X],Plateau),
	append([[Ligne,Colonne,X]],L_intermediaire,L_accessible).

/*dessin du plateau avec XPCE*/
dessiner_case(Plateau,[Ligne,Colonne,Couleur]):-
	send(Plateau, display,new(Box, box(80,80)), point(10+(Colonne-1)*80,10+(Ligne-1)*80)),
	send(Box, radius, 5),
	send(Box, fill_pattern, colour(Couleur)),!.
dessiner_case(Plateau,[Ligne,Colonne,Couleur,Tour,b]):-
	send(Plateau, display,new(Box, box(80,80)), point(10+(Colonne-1)*80,10+(Ligne-1)*80)),
	send(Box, radius, 5),
	send(Box, fill_pattern, colour(Couleur)),
	send(Plateau, display,new(Cercle, circle(60)), point(20+(Colonne-1)*80,20+(Ligne-1)*80)),
	send(Cercle, pen, 10),
	send(Cercle, fill_pattern, colour(Tour)),!.
dessiner_case(Plateau,[Ligne,Colonne,Couleur,Tour,a]):-
	send(Plateau, display,new(Box, box(80,80)), point(10+(Colonne-1)*80,10+(Ligne-1)*80)),
	send(Box, radius, 5),
	send(Box, fill_pattern, colour(Couleur)),
	send(Plateau, display,new(Cercle, circle(60)), point(20+(Colonne-1)*80,20+(Ligne-1)*80)),
	send(Cercle, pen, 10),
	send(Cercle, fill_pattern, colour(Tour)),
	send(Cercle, colour(white)),
	new(Click, click_gesture(left, '', single,message(@prolog, gestion, @receiver))),
	send(Cercle, recogniser, Click),!.

dessiner_plateau([],_).
dessiner_plateau([C1,C2,C3,C4,C5,C6,C7,C8|T],Plateau):-
	dessiner_case(Plateau,C1),
	dessiner_case(Plateau,C2),
	dessiner_case(Plateau,C3),
	dessiner_case(Plateau,C4),
	dessiner_case(Plateau,C5),
	dessiner_case(Plateau,C6),
	dessiner_case(Plateau,C7),
	dessiner_case(Plateau,C8),
	dessiner_plateau(T,Plateau).

/*Pour commencer*/
commencer(IA):-
	assert(ia(IA)),
	new(@p, picture('KAMISADO')),
	send(@p, size, size(900,640)),
	assert(fenetre(@p)),
	ecrire,
	plateau(Plateau),
	dessiner_plateau(Plateau,@p),
	send(@p, open).

/*Pour faire commencer l'ia choisie*/
ia_commencer(IA):-
	assert(ia(IA)),
	new(@p, picture('KAMISADO')),
	send(@p, size, size(900,640)),
	assert(fenetre(@p)),
	ecrire,
	plateau(Plateau),
	dessiner_plateau(Plateau,@p),
	send(@p, open),
	jouer.

/*Pour le raffraichissement de l'affichage plateau*/
redessiner:-
	fenetre(Plateau),
	ecrire,
	plateau(List),
	dessiner_plateau(List,Plateau),
	send(Plateau, open).

/*Pour l'affichage du panneau latéral*/
ecrire:-
	joueur([a]),
	couleur([L_couleur]),
	free(@t1),
	free(@t3),
	send(@p, display,new(@t1, text('Active player : white')), point(660, 40)),
	send(@p, display,new(@t3, text(L_couleur)), point(770, 80)),
	send(@t1, font, font(times, bold, 18)),
	send(@t3, font, font(times, bold, 18)).

ecrire:-
	joueur([b]),
	couleur([L_couleur]),
	free(@t1),
	free(@t3),
	send(@p, display,new(@t1, text('Active player : black')), point(660, 40)),
	send(@p, display,new(@t3, text(L_couleur)), point(770, 80)),
	send(@t1, font, font(times, bold, 18)),
	send(@t3, font, font(times, bold, 18)).

ecrire:-
	couleur([_,_|_]),
	joueur([a,b]),
	send(@p, display,new(@t1, text('Active player : ')), point(660, 40)),
	send(@p, display,new(@t2, text('Active color : ')), point(660, 80)),
	send(@p, display,new(@t3, text('')), point(790, 80)),
	send(@t1, font, font(times, bold, 18)),
	send(@t2, font, font(times, bold, 18)),
	send(@t3, font, font(times, bold, 18)).

/*Gestionnaire d'événements souris par ouverture d'une popup*/
gestion(Conteneur):-
	couleur(Couleur),
	plateau(Plateau),
	couleur([C1|_]),
	accessible(a,C1,Plateau,L_accessible),
	length(L_accessible,0),
	new(Dialog, dialog(string('Move of the %s tower of the %s player', Conteneur?fill_pattern,Conteneur?colour))),
	send(Dialog, append, new(MCouleur, menu(couleur))),
	send(MCouleur, layout, vertical),
	send_list(MCouleur, append, Couleur),
	send(Dialog, append, button(pass, 
		and(
			and(
				message(@prolog,passer),
				message(Dialog, destroy)
			),
			message(@prolog,jouer)
		)
	)),
	send(Dialog, append, button(quit, message(Dialog, destroy))),
	send(Dialog, default_button, pass),
	send(Dialog, open),!.
	
gestion(Conteneur):-
	couleur(Couleur),
	couleur([C1|_]),
	plateau(Plateau),
	accessible(a,C1,Plateau,L_accessible),
	length(L_accessible,X),
	X > 0,
	new(Dialog, dialog(string('Move of the %s tower of the %s player', Conteneur?fill_pattern,Conteneur?colour))),
	send(Dialog, append, new(MCouleur, menu(couleur))),
	send(Dialog, append, new(Direction, menu(direction))),
	send(Dialog, append, new(Nb_case, menu(valeur))),
	send(MCouleur, layout, vertical),/*
	S = string('%s',Conteneur?fill_pattern),write(S),nl,
	atom_string(C,S),
	send(MCouleur, default, C),*/
	send(Direction, layout, vertical),
	send(Nb_case, layout, vertical),
	send_list(MCouleur, append, Couleur),
	send_list(Direction, append, [forward, left, right]),
	send_list(Nb_case, append, [1,2,3,4,5,6,7]),
	send(Dialog, append, button(enter, 
		and(
			and(
				message(@prolog,deplacer_a,MCouleur?selection,Direction?selection,Nb_case?selection),
				message(Dialog, destroy)
			),
			message(@prolog,jouer)
		)
	)),
	send(Dialog, append, button(quit, message(Dialog, destroy))),
	send(Dialog, default_button, enter),
	send(Dialog, open),!.

/*Aiguillage de jouer*/
jouer:-
	ia(remi),
	jouer_remi,!.

jouer:-
	ia(mathieu),
	jouer_mat,!.

jouer:-
	ia(mathieu2),
	jouer_mat2,!.

jouer:-
	ia(mathieu3),
	jouer_mat3,!.

jouer:-
	ia(mathieu4),
	jouer_mat4,!.

jouer:-
	ia(mathieu_ab),
	jouer_mat_ab,!.

jouer:-
	ia(valentin),
	jouer_val,!.

/*Pour stoper une partie ou en refaire une avec la libération de la mémoire
et remise en place du plateau et des variables*/
recommencer(IA):-
	couleur(_),
	retract(ia(_)),
	retract(n_round(_)),
	assert(n_round(1)),
	free(@p),
	free(@t1),
	free(@t2),
	free(@t3),
	retract(plateau(_)),
	assert(plateau([
[1,1,orange,orange,b],[1,2,blue,blue,b],[1,3,purple,purple,b],[1,4,pink,pink,b],[1,5,yellow,yellow,b],[1,6,red,red,b],[1,7,green,green,b],[1,8,brown,brown,b],
[2,1,red],[2,2,orange],[2,3,pink],[2,4,green],[2,5,blue],[2,6,yellow],[2,7,brown],[2,8,purple],
[3,1,green],[3,2,pink],[3,3,orange],[3,4,red],[3,5,purple],[3,6,brown],[3,7,yellow],[3,8,blue],
[4,1,pink],[4,2,purple],[4,3,blue],[4,4,orange],[4,5,brown],[4,6,green],[4,7,red],[4,8,yellow],
[5,1,yellow],[5,2,red],[5,3,green],[5,4,brown],[5,5,orange],[5,6,blue],[5,7,purple],[5,8,pink],
[6,1,blue],[6,2,yellow],[6,3,brown],[6,4,purple],[6,5,red],[6,6,orange],[6,7,pink],[6,8,green],
[7,1,purple],[7,2,brown],[7,3,yellow],[7,4,blue],[7,5,green],[7,6,pink],[7,7,orange],[7,8,red],
[8,1,brown,brown,a],[8,2,green,green,a],[8,3,red,red,a],[8,4,yellow,yellow,a],[8,5,pink,pink,a],[8,6,purple,purple,a],[8,7,blue,blue,a],[8,8,orange,orange,a]])),
	retract(couleur(_)),
	assert(couleur([brown,green,red,yellow,pink,purple,blue,orange])),
	retract(joueur(_)),
	assert(joueur([a,b])),
	commencer(IA),!.

recommencer(IA):-
	not(couleur(_)),
	retract(ia(_)),
	retract(n_round(_)),
	assert(n_round(1)),
	free(@p),
	free(@t1),
	free(@t2),
	free(@t3),
	retract(plateau(_)),
	assert(plateau([
[1,1,orange,orange,b],[1,2,blue,blue,b],[1,3,purple,purple,b],[1,4,pink,pink,b],[1,5,yellow,yellow,b],[1,6,red,red,b],[1,7,green,green,b],[1,8,brown,brown,b],
[2,1,red],[2,2,orange],[2,3,pink],[2,4,green],[2,5,blue],[2,6,yellow],[2,7,brown],[2,8,purple],
[3,1,green],[3,2,pink],[3,3,orange],[3,4,red],[3,5,purple],[3,6,brown],[3,7,yellow],[3,8,blue],
[4,1,pink],[4,2,purple],[4,3,blue],[4,4,orange],[4,5,brown],[4,6,green],[4,7,red],[4,8,yellow],
[5,1,yellow],[5,2,red],[5,3,green],[5,4,brown],[5,5,orange],[5,6,blue],[5,7,purple],[5,8,pink],
[6,1,blue],[6,2,yellow],[6,3,brown],[6,4,purple],[6,5,red],[6,6,orange],[6,7,pink],[6,8,green],
[7,1,purple],[7,2,brown],[7,3,yellow],[7,4,blue],[7,5,green],[7,6,pink],[7,7,orange],[7,8,red],
[8,1,brown,brown,a],[8,2,green,green,a],[8,3,red,red,a],[8,4,yellow,yellow,a],[8,5,pink,pink,a],[8,6,purple,purple,a],[8,7,blue,blue,a],[8,8,orange,orange,a]])),
	assert(couleur([brown,green,red,yellow,pink,purple,blue,orange])),
	assert(joueur([a,b])),
	commencer(IA),!.

ia_recommencer(IA):-
	couleur(_),
	retract(ia(_)),
	free(@p),
	free(@t1),
	free(@t2),
	free(@t3),
	retract(plateau(_)),
	assert(plateau([
[1,1,orange,orange,b],[1,2,blue,blue,b],[1,3,purple,purple,b],[1,4,pink,pink,b],[1,5,yellow,yellow,b],[1,6,red,red,b],[1,7,green,green,b],[1,8,brown,brown,b],
[2,1,red],[2,2,orange],[2,3,pink],[2,4,green],[2,5,blue],[2,6,yellow],[2,7,brown],[2,8,purple],
[3,1,green],[3,2,pink],[3,3,orange],[3,4,red],[3,5,purple],[3,6,brown],[3,7,yellow],[3,8,blue],
[4,1,pink],[4,2,purple],[4,3,blue],[4,4,orange],[4,5,brown],[4,6,green],[4,7,red],[4,8,yellow],
[5,1,yellow],[5,2,red],[5,3,green],[5,4,brown],[5,5,orange],[5,6,blue],[5,7,purple],[5,8,pink],
[6,1,blue],[6,2,yellow],[6,3,brown],[6,4,purple],[6,5,red],[6,6,orange],[6,7,pink],[6,8,green],
[7,1,purple],[7,2,brown],[7,3,yellow],[7,4,blue],[7,5,green],[7,6,pink],[7,7,orange],[7,8,red],
[8,1,brown,brown,a],[8,2,green,green,a],[8,3,red,red,a],[8,4,yellow,yellow,a],[8,5,pink,pink,a],[8,6,purple,purple,a],[8,7,blue,blue,a],[8,8,orange,orange,a]])),
	retract(couleur(_)),
	assert(couleur([brown,green,red,yellow,pink,purple,blue,orange])),
	retract(joueur(_)),
	assert(joueur([a,b])),
	ia_commencer(IA),!.

ia_recommencer(IA):-
	not(couleur(_)),
	retract(ia(_)),
	free(@p),
	free(@t1),
	free(@t2),
	free(@t3),
	retract(plateau(_)),
	assert(plateau([
[1,1,orange,orange,b],[1,2,blue,blue,b],[1,3,purple,purple,b],[1,4,pink,pink,b],[1,5,yellow,yellow,b],[1,6,red,red,b],[1,7,green,green,b],[1,8,brown,brown,b],
[2,1,red],[2,2,orange],[2,3,pink],[2,4,green],[2,5,blue],[2,6,yellow],[2,7,brown],[2,8,purple],
[3,1,green],[3,2,pink],[3,3,orange],[3,4,red],[3,5,purple],[3,6,brown],[3,7,yellow],[3,8,blue],
[4,1,pink],[4,2,purple],[4,3,blue],[4,4,orange],[4,5,brown],[4,6,green],[4,7,red],[4,8,yellow],
[5,1,yellow],[5,2,red],[5,3,green],[5,4,brown],[5,5,orange],[5,6,blue],[5,7,purple],[5,8,pink],
[6,1,blue],[6,2,yellow],[6,3,brown],[6,4,purple],[6,5,red],[6,6,orange],[6,7,pink],[6,8,green],
[7,1,purple],[7,2,brown],[7,3,yellow],[7,4,blue],[7,5,green],[7,6,pink],[7,7,orange],[7,8,red],
[8,1,brown,brown,a],[8,2,green,green,a],[8,3,red,red,a],[8,4,yellow,yellow,a],[8,5,pink,pink,a],[8,6,purple,purple,a],[8,7,blue,blue,a],[8,8,orange,orange,a]])),
	assert(couleur([brown,green,red,yellow,pink,purple,blue,orange])),
	assert(joueur([a,b])),
	ia_commencer(IA),!.

