/*Hors premier coup 				ne fait rien pour le moment...*/
jouer:-
	plateau(P),
	liste_couleur_autorisee(b,P,L_autorisee),
	couleur([X]),
	accessible(b,X,P,L_accessible),
	jouable(b,X,L_jouable),
	write('Liste des couleurs autorisées : '),write(L_autorisee),nl,
	write('Couleur active : '),write(X),nl,
	write('Liste des cases accessibles : '),write(L_accessible),nl,
	write('Liste des cases jouables sans risque : '),write(L_jouable),!.
	
jouable(b,Tour,L):-
	plateau(P),
	accessible(b,Tour,P,LL),
	tester_liste(Tour,LL,L),!.

tester_liste(_,[],[]):-!.
tester_liste(Tour,[H|T],L):-
	tester_case(Tour,H,C),
	tester_liste(Tour,T,LL),
	append(C,LL,L).

tester_case(Tour,X,[]):-
	X = [A,O,_],
	deplacer2(Tour,A,O,NP,NC),
	accessible(a,NC,NP,LL),
	member([1,_,_],LL)/*,
	write(X),write(' pas bon'),nl*/.
tester_case(Tour,X,L):-
	X = [A,O,_],
	deplacer2(Tour,A,O,NP,NC),
	accessible(a,NC,NP,LL),
	not(member([1,_,_],LL)),
	/*write(X),write(' bon'),nl,*/
	append([X],[],L).

/*simule un déplacement et retourne un plateau provisoire et la couleur correspondante*/
deplacer2(T,NA,NO,NP,NC):-
	plateau(P),
	member([A,O,C,T,b],P),
	couleur(LC),
	member(C,LC),
	accessible(b,T,P,L),
	member([NA,NO,NC],L),
	modifier_plateau(P,A,O,NA,NO,T,b,NP),
	/*dessiner_plateau(NP,1),*/!.
	



















