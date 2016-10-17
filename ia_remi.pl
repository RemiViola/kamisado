/*Il manque la gestion de la victoire du joueur...

Chercher un moyen d'évaluer le coup optimum pour l'IA...
peut être simuler toutes les parties possibles et remonter le nombre de victoires et de défaites correspondantes.
Choisir dans ce cas le meilleur ratio ?*/

/*Prédicat de gestion de l'IA, premier coup, suivants et victoire...*/
jouer:-
	joueur(LJ),
	member(b,LJ),
	couleur([C,CC|T]),
	plateau(P),
	member([A,O,_,C,b],P),
	NA is A+1,
	deplacer2(C,NA,O,NP,NC),
	retract(joueur(_)),
	assert(joueur([a])),
	retract(couleur([C,CC|T])),
	assert(couleur([NC])),
	retract(plateau(P)),
	assert(plateau(NP)),
	dessiner,!.

jouer:-
	joueur(LJ),
	member(b,LJ),
	couleur([C]),
	plateau(P),
	accessible(b,C,P,L_accessible),
	member([8,O,NC],L_accessible),
	deplacer2(C,8,O,NP,NC),
	retract(joueur(_)),
	assert(joueur([a])),
	retract(couleur([C])),
	assert(couleur([NC])),
	plateau(P),
	retract(plateau(P)),
	assert(plateau(NP)),
	dessiner,
	nl,nl,nl,
	write('                    YY    YY  OOOO  UU  UU      LL      OOOO   OOOO   SSSS  EEEEEE'),nl,
	write('                    YY    YY OOOOOO UU  UU      LL     OOOOOO OOOOOO SSSSSS EEEEEE'),nl,
	write('                     YY  YY  OO  OO UU  UU      LL     OO  OO OO  OO SS     EE'),nl,
	write('                      YYYY   OO  OO UU  UU      LL     OO  OO OO  OO   SS   EEEE'),nl,
	write('                       YY    OO  OO UU  UU      LL     OO  OO OO  OO     SS EE'),nl,
	write('                       YY    OOOOOO UUUUUU      LLLLLL OOOOOO OOOOOO SSSSSS EEEEEE'),nl,
	write('                       YY     OOOO   UUUU       LLLLLL  OOOO   OOOO   SSSS  EEEEEE'),nl,!.

jouer:-
	joueur(LJ),
	member(b,LJ),
	couleur([C]),
	jouable(b,C,L_jouable),
	/*write('Couleur active : '),write(C),nl,
	write('Liste des cases jouables sans risque : '),write(L_jouable),nl,*/
	my_sort(L_jouable,L_sort),
	/*write('Liste des cases jouables triée : '),write(L_sort),nl,*/
	choix(L_sort,L_sort,C,X),
	/*write('Choix fait'),write(X),nl,*/
	X = [A,O,NC],
	deplacer2(C,A,O,NP,NC),
	retract(joueur(_)),
	assert(joueur([a])),
	retract(couleur([C])),
	assert(couleur([NC])),
	plateau(P),
	retract(plateau(P)),
	assert(plateau(NP)),
	dessiner,!.

/*Choix de la case à jouer dans la liste des jouables ordonnées*/
choix([],[H|_],_,H):-
	/*write('arrivé à la fin de liste jouable'),nl,*/!.
choix([H|_],_,Tour,H):-
	/*write('test de '),write(H),nl,*/
	H = [A,O,_],
	deplacer2(Tour,A,O,NP,_),
	accessible(b,Tour,NP,Liste_accessible),write(Liste_accessible),nl,nl,
	member([8,_,_],Liste_accessible),!.
choix([H|T],L,Tour,HH):-
	/*write('test2 de '),write(H),nl,*/
	H = [A,O,_],
	deplacer2(Tour,A,O,NP,_),
	accessible(b,Tour,NP,Liste_accessible),write(Liste_accessible),nl,nl,
	not(member([8,_,_],Liste_accessible)),
	choix(T,L,Tour,HH),!.

/*Liste des cases où b peut jouer sans risque direct*/	
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

/*Tri de la liste de cases*/
my_sort([],[]):-!.
my_sort([X],[X]):-!.
my_sort(L,LS):-
	/*write('split de : '),write(L),nl,*/
	my_split(L,L1,L2),
	/*write('split fait : '),write(L1),write(' et '),write(L2),nl,*/
	my_sort(L1,LS1),
	my_sort(L2,LS2),
	/*write('merge de : '),write(LS1),write(' et '),write(LS2),nl,*/
	my_merge(LS1,LS2,LS)/*,
	write('merge fait : '),write(LS),nl*/.

my_split([],[],[]):-!.
my_split([X],[X],[]):-!.
my_split([H1,H2|T],L1,L2):-
	my_split(T,LL1,LL2),
	append([H1],LL1,L1),
	append([H2],LL2,L2).

my_merge([],[],[]):-!.
my_merge([],L,L):-!.
my_merge(L,[],L):-!.
/*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx*/
my_merge([H1],[H2],[H1,H2]):-
	H1 = [A1,_,_],
	H2 = [A2,_,_],
	A1 < A2,!.
my_merge([H1],[H2],[H2,H1]):-
	H1 = [A1,_,_],
	H2 = [A2,_,_],
	A1 > A2,!.
my_merge([H1],[H2],[H1,H2]):-
	H1 = [A1,O1,_],
	H2 = [A2,O2,_],
	A1 = A2,
	O1 < O2,!.
my_merge([H1],[H2],[H2,H1]):-
	H1 = [A1,O1,_],
	H2 = [A2,O2,_],
	A1 = A2,
	O1 > O2,!.
/*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx*/
my_merge([H1,H2|T1],[H3],L):-
	H2 = [A2,_,_],
	H3 = [A3,_,_],
	A2 < A3,
	my_merge(T1,[H3],LL),
	append([H1,H2],LL,L),!.
my_merge([H1,H2|T1],[H3],L):-
	H2 = [A2,O2,_],
	H3 = [A3,O3,_],
	A2 = A3,
	O2 < O3,
	my_merge(T1,[H3],LL),
	append([H1,H2],LL,L),!.
/*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx*/
my_merge([H1,H2|T1],[H3],[H1,H3,H2|T1]):-
	H1 = [A1,_,_],
	H2 = [A2,_,_],
	H3 = [A3,_,_],
	A1 < A3,
	A3 < A2,!.
my_merge([H1,H2|T1],[H3],[H1,H3,H2|T1]):-
	H1 = [A1,O1,_],
	H2 = [A2,_,_],
	H3 = [A3,O3,_],
	A1 = A3,
	A2 > A3,
	O1 < O3,!.
my_merge([H1,H2|T1],[H3],[H1,H3,H2|T1]):-
	H1 = [A1,_,_],
	H2 = [A2,O2,_],
	H3 = [A3,O3,_],
	A1 < A3,
	A2 = A3,
	O3 < O2,!.
my_merge([H1,H2|T1],[H3],[H1,H3,H2|T1]):-
	H1 = [A1,O1,_],
	H2 = [A2,O2,_],
	H3 = [A3,O3,_],
	A1 = A3,
	A2 = A3,
	O1 < O3,
	O3 < O2,!.
/*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx*/
my_merge([H1,H2|T1],[H3],[H3,H1,H2|T1]):-
	H1 = [A1,_,_],
	H3 = [A3,_,_],
	A3 < A1,!.
my_merge([H1,H2|T1],[H3],[H3,H1,H2|T1]):-
	H1 = [A1,O1,_],
	H3 = [A3,O3,_],
	A3 = A1,
	O3 < O1,!.
/*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx*/
my_merge([H3],[H1,H2|T1],L):-
	my_merge([H1,H2|T1],[H3],L),!.
/*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx*/
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [A1,_,_],
	H2 = [A2,_,_],
	A1 < A2,
	my_merge([H3|T1],[H2,H4|T2],LL),
	append([H1],LL,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [A1,_,_],
	H2 = [A2,_,_],
	A1 > A2,
	my_merge([H1,H3|T1],[H4|T2],LL),
	append([H2],LL,L),!.
/*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx*/
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [A1,O1,_],
	H2 = [A2,O2,_],
	H3 = [A3,_,_],
	H4 = [A4,_,_],
	A1 = A2,
	A1 \= A3,
	A1 \= A4,
	my_merge([H3|T1],[H4|T2],LL),
	O1 < O2,
	append([H1,H2],LL,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [A1,O1,_],
	H2 = [A2,O2,_],
	H3 = [A3,_,_],
	H4 = [A4,_,_],
	A1 = A2,
	A1 \= A3,
	A1 \= A4,
	my_merge([H3|T1],[H4|T2],LL),
	O1 > O2,
	append([H2,H1],LL,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [A1,O1,_],
	H2 = [A2,O2,_],
	H3 = [A3,O3,_],
	A1 = A2,
	A1 = A3,
	my_merge(T1,[H4|T2],LL),
	O1 < O2,
	O2 < O3,
	append([H2,H1,H3],LL,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [A1,O1,_],
	H2 = [A2,O2,_],
	H3 = [A3,O3,_],
	A1 = A2,
	A1 = A3,
	my_merge(T1,[H4|T2],LL),
	O1 < O3,
	O3 < O2,
	append([H3,H1,H2],LL,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [A1,O1,_],
	H2 = [A2,O2,_],
	H3 = [A3,O3,_],
	A1 = A2,
	A1 = A3,
	my_merge(T1,[H4|T2],LL),
	O1 < O3,
	O2 < O1,
	append([H1,H2,H3],LL,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [A1,O1,_],
	H2 = [A2,O2,_],
	H3 = [A3,O3,_],
	A1 = A2,
	A1 = A3,
	my_merge(T1,[H4|T2],LL),
	O3 < O1,
	O2 < O3,
	append([H3,H2,H1],LL,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [A1,O1,_],
	H2 = [A2,O2,_],
	H3 = [A3,O3,_],
	A1 = A2,
	A1 = A3,
	my_merge(T1,[H4|T2],LL),
	O3 < O1,
	O1 < O2,
	append([H1,H3,H2],LL,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [A1,O1,_],
	H2 = [A2,O2,_],
	H3 = [A3,O3,_],
	A1 = A2,
	A1 = A3,
	my_merge(T1,[H4|T2],LL),
	O3 < O2,
	O2 < O1,
	append([H2,H3,H1],LL,L),!.
/*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx*/
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [A1,O1,_],
	H2 = [A2,O2,_],
	H4 = [A4,O4,_],
	A1 = A2,
	A1 = A4,
	my_merge([H3|T1],T2,LL),
	O1 < O2,
	O2 < O4,
	append([H2,H1,H4],LL,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [A1,O1,_],
	H2 = [A2,O2,_],
	H4 = [A4,O4,_],
	A1 = A2,
	A1 = A4,
	my_merge([H3|T1],T2,LL),
	O1 < O4,
	O4 < O2,
	append([H4,H1,H2],LL,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [A1,O1,_],
	H2 = [A2,O2,_],
	H4 = [A4,O4,_],
	A1 = A2,
	A1 = A4,
	my_merge([H3|T1],T2,LL),
	O1 < O4,
	O2 < O1,
	append([H1,H2,H4],LL,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [A1,O1,_],
	H2 = [A2,O2,_],
	H4 = [A4,O4,_],
	A1 = A2,
	A1 = A4,
	my_merge([H3|T1],T2,LL),
	O4 < O1,
	O2 < O4,
	append([H4,H2,H1],LL,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [A1,O1,_],
	H2 = [A2,O2,_],
	H4 = [A4,O4,_],
	A1 = A2,
	A1 = A4,
	my_merge([H3|T1],T2,LL),
	O4 < O1,
	O1 < O2,
	append([H1,H4,H2],LL,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [A1,O1,_],
	H2 = [A2,O2,_],
	H4 = [A4,O4,_],
	A1 = A2,
	A1 = A4,
	my_merge([H3|T1],T2,LL),
	O4 < O2,
	O2 < O1,
	append([H2,H4,H1],LL,L),!.

/*simule un déplacement et retourne un plateau provisoire et la couleur correspondante*/
deplacer2(T,NA,NO,NP,NC):-
	plateau(P),
	member([A,O,_C,T,b],P),
	couleur(LC),
	member(T,LC),
	accessible(b,T,P,L),
	member([NA,NO,NC],L),
	modifier_plateau(P,A,O,NA,NO,T,b,NP),
	/*dessiner_plateau(NP,1),*/!.
	



















