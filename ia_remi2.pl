/*	Chercher un moyen d'évaluer le coup optimum pour l'IA...
	peut être simuler toutes les parties possibles et remonter le nombre de victoires et de défaites correspondantes.
	Choisir dans ce cas le meilleur ratio ?
*/

:- use_module(library(random)).

/*Gestion du premier coup de l'ia si elle commence*/
/*Si brown ou orange */
jouer:-
	joueur(L_joueur),
	member(b,L_joueur),
	couleur([Couleur,Couleur2|Tail]),
	plateau(Plateau),
	random_member(X, [Couleur,Couleur2|Tail]),
	member(X,[brown,orange]),
	member([_,Colonne,_,X,b],Plateau),	
	random_member(NLigne, [4,5]),
	deplacer2(b,X,NLigne,Colonne,NPlateau,NCouleur),
	retract(joueur(_)),
	assert(joueur([a])),
	retract(couleur([Couleur,Couleur2|Tail])),
	assert(couleur([NCouleur])),
	retract(plateau(Plateau)),
	assert(plateau(NPlateau)),
	redessiner,!.

/*Si green ou blue */
jouer:-
	joueur(L_joueur),
	member(b,L_joueur),
	couleur([Couleur,Couleur2|Tail]),
	plateau(Plateau),
	random_member(X, [Couleur,Couleur2|Tail]),
	member(X,[green,blue]),
	member([_,Colonne,_,X,b],Plateau),	
	random_member(NLigne, [2,4,6,7]),
	deplacer2(b,X,NLigne,Colonne,NPlateau,NCouleur),
	retract(joueur(_)),
	assert(joueur([a])),
	retract(couleur([Couleur,Couleur2|Tail])),
	assert(couleur([NCouleur])),
	retract(plateau(Plateau)),
	assert(plateau(NPlateau)),
	redessiner,!.

/*Si purple ou red */
jouer:-
	joueur(L_joueur),
	member(b,L_joueur),
	couleur([Couleur,Couleur2|Tail]),
	plateau(Plateau),
	random_member(X, [Couleur,Couleur2|Tail]),
	member(X,[purple,red]),
	member([_,Colonne,_,X,b],Plateau),	
	random_member(NLigne, [3,4,6,7]),
	deplacer2(b,X,NLigne,Colonne,NPlateau,NCouleur),
	retract(joueur(_)),
	assert(joueur([a])),
	retract(couleur([Couleur,Couleur2|Tail])),
	assert(couleur([NCouleur])),
	retract(plateau(Plateau)),
	assert(plateau(NPlateau)),
	redessiner,!.

/*Si pink ou yellow */
jouer:-
	joueur(L_joueur),
	member(b,L_joueur),
	couleur([Couleur,Couleur2|Tail]),
	plateau(Plateau),
	random_member(X, [Couleur,Couleur2|Tail]),
	member(X,[pink,yellow]),
	member([_,Colonne,_,X,b],Plateau),	
	random_member(NLigne, [4,5,6]),
	deplacer2(b,X,NLigne,Colonne,NPlateau,NCouleur),
	retract(joueur(_)),
	assert(joueur([a])),
	retract(couleur([Couleur,Couleur2|Tail])),
	assert(couleur([NCouleur])),
	retract(plateau(Plateau)),
	assert(plateau(NPlateau)),
	redessiner,!.

/*Quand une case gagnante est dans la liste des accessibles*/
jouer:-
	joueur(L_joueur),
	member(b,L_joueur),
	couleur([Couleur]),
	plateau(Plateau),
	accessible(b,Couleur,Plateau,L_accessible),
	member([8,Colonne,NCouleur],L_accessible),
	deplacer2(b,Couleur,8,Colonne,NPlateau,NCouleur),
	plateau(Plateau),
	retract(plateau(Plateau)),
	assert(plateau(NPlateau)),
	redessiner,
	send(@p, display,new(Text, text('YOU LOOSE')), point(660, 140)),
	send(Text, font, font(times, bold, 40)),!.

/*L'ia choisit le meilleur coup dans la liste des jouables*/
jouer:-
	joueur(L_joueur),
	member(b,L_joueur),
	couleur([Couleur]),
	plateau(Plateau),
	accessible(b,Couleur,Plateau,L_accessible),
	jouable(b,L_accessible,Couleur,L_jouable),
	my_sort(L_jouable,L_sort),
	choix(L_sort,L_sort,Couleur,Case),
	Case = [Ligne,Colonne,NCouleur],
	deplacer2(b,Couleur,Ligne,Colonne,NPlateau,NCouleur),
	retract(joueur(_)),
	assert(joueur([a])),
	retract(couleur([Couleur])),
	assert(couleur([NCouleur])),
	retract(plateau(Plateau)),
	assert(plateau(NPlateau)),
	redessiner,!.

/*Quand l'ia n'a pas de coup jouable sans risque et qu'elle sait qu'elle a perdu*/
jouer:-
	joueur(L_joueur),
	member(b,L_joueur),
	couleur([Couleur]),
	plateau(Plateau),
	accessible(b,Couleur,Plateau,L_accessible),
	jouable(b,L_accessible,Couleur,L_jouable),
	L_jouable = [],
	member(Case,L_accessible),
	Case = [Ligne,Colonne,NCouleur],
	deplacer2(b,Couleur,Ligne,Colonne,NPlateau,NCouleur),
	retract(joueur(_)),
	assert(joueur([a])),
	retract(couleur([Couleur])),
	assert(couleur([NCouleur])),
	retract(plateau(Plateau)),
	assert(plateau(NPlateau)),
	redessiner,!.

/*Quand l'ia est bloquée et ne peut pas se déplacer*/
jouer:-
	joueur(L_joueur),
	member(b,L_joueur),
	couleur([Couleur]),
	plateau(Plateau),
	accessible(b,Couleur,Plateau,L_accessible),
	L_accessible = [],
	member([_,_,NCouleur,Couleur,b],Plateau),
	retract(joueur(_)),
	assert(joueur([a])),
	retract(couleur([Couleur])),
	assert(couleur([NCouleur])),
	redessiner,!.

/*Choix de la case à jouer dans la liste des jouables ordonnées*/
choix([],L_save,Tour,Case):-
	write('entrer dans choix2'),nl,
	choix2(L_save,L_save,Tour,Case),!.
choix([Case|_],_,Tour,Case):-
	Case = [Ligne,Colonne,_],
	deplacer2(b,Tour,Ligne,Colonne,NPlateau,_),
	accessible(b,Tour,NPlateau,L_accessible),
	member([8,_,_],L_accessible),!.
choix([Case|Tail],L_save,Tour,Case2):-
	Case = [Ligne,Colonne,_],
	deplacer2(b,Tour,Ligne,Colonne,NPlateau,_),
	accessible(b,Tour,NPlateau,L_accessible),
	not(member([8,_,_],L_accessible)),
	choix(Tail,L_save,Tour,Case2),!.

/*Comme choix ne permet pas d'accéder à une case agressive, 
on essaie de forcer le joueur a à libérer une case gagnante, 
sinon on prend le premier de la liste*/
choix2([],[Case|_],_,Case):-!.
choix2([Case|_],_,_,Case):-
	Case = [_,Colonne,Couleur],
	plateau(NPlateau),
	member([8,Colonne,Couleur,Couleur,a],NPlateau),!.
choix2([Case|_],_,_,Case):-
	Case = [Ligne,Colonne,Couleur],
	plateau(NPlateau),
	NColonne is Colonne+8-Ligne,
	NColonne > 0, NColonne < 9,
	member([8,NColonne,Couleur,Couleur,a],NPlateau),!.
choix2([Case|_],_,_,Case):-
	Case = [Ligne,Colonne,Couleur],
	plateau(NPlateau),
	NColonne is Colonne-8+Ligne,
	NColonne > 0, NColonne < 9,
	member([8,NColonne,Couleur,Couleur,a],NPlateau),!.
choix2([Case|Tail],L_save,Tour,Case2):-
	Case = [Ligne,Colonne,Couleur],
	plateau(NPlateau),
	not(member([8,Colonne,Couleur,Couleur,a],NPlateau)),
	NColonne is Colonne+8-Ligne,
	not(member([8,NColonne,Couleur,Couleur,a],NPlateau)),
	NColonne_ is Colonne-8+Ligne,
	not(member([8,NColonne_,Couleur,Couleur,a],NPlateau)),
	choix2(Tail,L_save,Tour,Case2),!.

/*Liste des cases où b peut jouer sans risque direct*/	
jouable(b,L_accessible,Tour,L_jouable):-
	tester_liste(Tour,L_accessible,L_jouable),!.

tester_liste(_,[],[]):-!.
tester_liste(Tour,[H|T],L):-
	tester_case(Tour,H,C),
	tester_liste(Tour,T,L_intermediaire),
	append(C,L_intermediaire,L).

tester_case(Tour,Case,[]):-
	Case = [Ligne,Colonne,_],
	deplacer2(b,Tour,Ligne,Colonne,NPlateau,NCouleur),
	accessible(a,NCouleur,NPlateau,L_accessible),
	member([1,_,_],L_accessible).
tester_case(Tour,Case,L_jouable):-
	Case = [Ligne,Colonne,_],
	deplacer2(b,Tour,Ligne,Colonne,NPlateau,NCouleur),
	accessible(a,NCouleur,NPlateau,L_accessible),
	not(member([1,_,_],L_accessible)),
	append([Case],[],L_jouable).

/*Tri de la liste de cases*/
my_sort([],[]):-!.
my_sort([X],[X]):-!.
my_sort(L,L_sort):-
	my_split(L,L_split1,L_split2),
	my_sort(L_split1,L_sort1),
	my_sort(L_split2,L_sort2),
	my_merge(L_sort1,L_sort2,L_sort).

my_split([],[],[]):-!.
my_split([X],[X],[]):-!.
my_split([H1,H2|T],L_split1,L_split2):-
	my_split(T,L_intermediaire1,L_intermediaire2),
	append([H1],L_intermediaire1,L_split1),
	append([H2],L_intermediaire2,L_split2).

my_merge([],[],[]):-!.
my_merge([],L,L):-!.
my_merge(L,[],L):-!.
/*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx*/
my_merge([H1],[H2],[H1,H2]):-
	H1 = [Ligne1,_,_],
	H2 = [Ligne2,_,_],
	Ligne1 < Ligne2,!.
my_merge([H1],[H2],[H2,H1]):-
	H1 = [Ligne1,_,_],
	H2 = [Ligne2,_,_],
	Ligne1 > Ligne2,!.
my_merge([H1],[H2],[H1,H2]):-
	H1 = [Ligne1,Colonne1,_],
	H2 = [Ligne2,Colonne2,_],
	Ligne1 = Ligne2,
	Colonne1 < Colonne2,!.
my_merge([H1],[H2],[H2,H1]):-
	H1 = [Ligne1,Colonne1,_],
	H2 = [Ligne2,Colonne2,_],
	Ligne1 = Ligne2,
	Colonne1 > Colonne2,!.
/*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx*/
my_merge([H1,H2|T1],[H3],L):-
	H2 = [Ligne2,_,_],
	H3 = [Ligne3,_,_],
	Ligne2 < Ligne3,
	my_merge(T1,[H3],L_intermediaire),
	append([H1,H2],L_intermediaire,L),!.
my_merge([H1,H2|T1],[H3],L):-
	H2 = [Ligne2,Colonne2,_],
	H3 = [Ligne3,Colonne3,_],
	Ligne2 = Ligne3,
	Colonne2 < Colonne3,
	my_merge(T1,[H3],L_intermediaire),
	append([H1,H2],L_intermediaire,L),!.
/*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx*/
my_merge([H1,H2|T1],[H3],[H1,H3,H2|T1]):-
	H1 = [Ligne1,_,_],
	H2 = [Ligne2,_,_],
	H3 = [Ligne3,_,_],
	Ligne1 < Ligne3,
	Ligne3 < Ligne2,!.
my_merge([H1,H2|T1],[H3],[H1,H3,H2|T1]):-
	H1 = [Ligne1,Colonne1,_],
	H2 = [Ligne2,_,_],
	H3 = [Ligne3,Colonne3,_],
	Ligne1 = Ligne3,
	Ligne2 > Ligne3,
	Colonne1 < Colonne3,!.
my_merge([H1,H2|T1],[H3],[H1,H3,H2|T1]):-
	H1 = [Ligne1,_,_],
	H2 = [Ligne2,Colonne2,_],
	H3 = [Ligne3,Colonne3,_],
	Ligne1 < Ligne3,
	Ligne2 = Ligne3,
	Colonne3 < Colonne2,!.
my_merge([H1,H2|T1],[H3],[H1,H3,H2|T1]):-
	H1 = [Ligne1,Colonne1,_],
	H2 = [Ligne2,Colonne2,_],
	H3 = [Ligne3,Colonne3,_],
	Ligne1 = Ligne3,
	Ligne2 = Ligne3,
	Colonne1 < Colonne3,
	Colonne3 < Colonne2,!.
/*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx*/
my_merge([H1,H2|T1],[H3],[H3,H1,H2|T1]):-
	H1 = [Ligne1,_,_],
	H3 = [Ligne3,_,_],
	Ligne3 < Ligne1,!.
my_merge([H1,H2|T1],[H3],[H3,H1,H2|T1]):-
	H1 = [Ligne1,Colonne1,_],
	H3 = [Ligne3,Colonne3,_],
	Ligne3 = Ligne1,
	Colonne3 < Colonne1,!.
/*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx*/
my_merge([H3],[H1,H2|T1],L):-
	my_merge([H1,H2|T1],[H3],L),!.
/*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx*/
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [Ligne1,_,_],
	H2 = [Ligne2,_,_],
	Ligne1 < Ligne2,
	my_merge([H3|T1],[H2,H4|T2],L_intermediaire),
	append([H1],L_intermediaire,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [Ligne1,_,_],
	H2 = [Ligne2,_,_],
	Ligne1 > Ligne2,
	my_merge([H1,H3|T1],[H4|T2],L_intermediaire),
	append([H2],L_intermediaire,L),!.
/*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx*/
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [Ligne1,Colonne1,_],
	H2 = [Ligne2,Colonne2,_],
	H3 = [Ligne3,_,_],
	H4 = [Ligne4,_,_],
	Ligne1 = Ligne2,
	Ligne1 \= Ligne3,
	Ligne1 \= Ligne4,
	my_merge([H3|T1],[H4|T2],L_intermediaire),
	Colonne1 < Colonne2,
	append([H1,H2],L_intermediaire,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [Ligne1,Colonne1,_],
	H2 = [Ligne2,Colonne2,_],
	H3 = [Ligne3,_,_],
	H4 = [Ligne4,_,_],
	Ligne1 = Ligne2,
	Ligne1 \= Ligne3,
	Ligne1 \= Ligne4,
	my_merge([H3|T1],[H4|T2],L_intermediaire),
	Colonne1 > Colonne2,
	append([H2,H1],L_intermediaire,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [Ligne1,Colonne1,_],
	H2 = [Ligne2,Colonne2,_],
	H3 = [Ligne3,Colonne3,_],
	Ligne1 = Ligne2,
	Ligne1 = Ligne3,
	my_merge(T1,[H4|T2],L_intermediaire),
	Colonne1 < Colonne2,
	Colonne2 < Colonne3,
	append([H2,H1,H3],L_intermediaire,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [Ligne1,Colonne1,_],
	H2 = [Ligne2,Colonne2,_],
	H3 = [Ligne3,Colonne3,_],
	Ligne1 = Ligne2,
	Ligne1 = Ligne3,
	my_merge(T1,[H4|T2],L_intermediaire),
	Colonne1 < Colonne3,
	Colonne3 < Colonne2,
	append([H3,H1,H2],L_intermediaire,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [Ligne1,Colonne1,_],
	H2 = [Ligne2,Colonne2,_],
	H3 = [Ligne3,Colonne3,_],
	Ligne1 = Ligne2,
	Ligne1 = Ligne3,
	my_merge(T1,[H4|T2],L_intermediaire),
	Colonne1 < Colonne3,
	Colonne2 < Colonne1,
	append([H1,H2,H3],L_intermediaire,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [Ligne1,Colonne1,_],
	H2 = [Ligne2,Colonne2,_],
	H3 = [Ligne3,Colonne3,_],
	Ligne1 = Ligne2,
	Ligne1 = Ligne3,
	my_merge(T1,[H4|T2],L_intermediaire),
	Colonne3 < Colonne1,
	Colonne2 < Colonne3,
	append([H3,H2,H1],L_intermediaire,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [Ligne1,Colonne1,_],
	H2 = [Ligne2,Colonne2,_],
	H3 = [Ligne3,Colonne3,_],
	Ligne1 = Ligne2,
	Ligne1 = Ligne3,
	my_merge(T1,[H4|T2],L_intermediaire),
	Colonne3 < Colonne1,
	Colonne1 < Colonne2,
	append([H1,H3,H2],L_intermediaire,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [Ligne1,Colonne1,_],
	H2 = [Ligne2,Colonne2,_],
	H3 = [Ligne3,Colonne3,_],
	Ligne1 = Ligne2,
	Ligne1 = Ligne3,
	my_merge(T1,[H4|T2],L_intermediaire),
	Colonne3 < Colonne2,
	Colonne2 < Colonne1,
	append([H2,H3,H1],L_intermediaire,L),!.
/*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx*/
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [Ligne1,Colonne1,_],
	H2 = [Ligne2,Colonne2,_],
	H4 = [Ligne4,Colonne4,_],
	Ligne1 = Ligne2,
	Ligne1 = Ligne4,
	my_merge([H3|T1],T2,L_intermediaire),
	Colonne1 < Colonne2,
	Colonne2 < Colonne4,
	append([H2,H1,H4],L_intermediaire,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [Ligne1,Colonne1,_],
	H2 = [Ligne2,Colonne2,_],
	H4 = [Ligne4,Colonne4,_],
	Ligne1 = Ligne2,
	Ligne1 = Ligne4,
	my_merge([H3|T1],T2,L_intermediaire),
	Colonne1 < Colonne4,
	Colonne4 < Colonne2,
	append([H4,H1,H2],L_intermediaire,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [Ligne1,Colonne1,_],
	H2 = [Ligne2,Colonne2,_],
	H4 = [Ligne4,Colonne4,_],
	Ligne1 = Ligne2,
	Ligne1 = Ligne4,
	my_merge([H3|T1],T2,L_intermediaire),
	Colonne1 < Colonne4,
	Colonne2 < Colonne1,
	append([H1,H2,H4],L_intermediaire,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [Ligne1,Colonne1,_],
	H2 = [Ligne2,Colonne2,_],
	H4 = [Ligne4,Colonne4,_],
	Ligne1 = Ligne2,
	Ligne1 = Ligne4,
	my_merge([H3|T1],T2,L_intermediaire),
	Colonne4 < Colonne1,
	Colonne2 < Colonne4,
	append([H4,H2,H1],L_intermediaire,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [Ligne1,Colonne1,_],
	H2 = [Ligne2,Colonne2,_],
	H4 = [Ligne4,Colonne4,_],
	Ligne1 = Ligne2,
	Ligne1 = Ligne4,
	my_merge([H3|T1],T2,L_intermediaire),
	Colonne4 < Colonne1,
	Colonne1 < Colonne2,
	append([H1,H4,H2],L_intermediaire,L),!.
my_merge([H1,H3|T1],[H2,H4|T2],L):-
	H1 = [Ligne1,Colonne1,_],
	H2 = [Ligne2,Colonne2,_],
	H4 = [Ligne4,Colonne4,_],
	Ligne1 = Ligne2,
	Ligne1 = Ligne4,
	my_merge([H3|T1],T2,L_intermediaire),
	Colonne4 < Colonne2,
	Colonne2 < Colonne1,
	append([H2,H4,H1],L_intermediaire,L),!.


	



















