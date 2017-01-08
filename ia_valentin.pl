:-module(ia_valentin,[jouer_val/0]).
:- use_module(library(random)).
    
/*** prédicat permetant de choisir le meilleur coup pour l'ia ***/
/****   1: Couleur de la tour a jouer ****/
/****   2: Plateau sûr le quel simuler le coup ****/
/****   3: Liste de coup a simuler dont les élément sont de la forme : [Abscice de la futur case, Ordonné de la future case, Couleur de la future case] ****/
/****   4: Liste de coup simuler dont les élément sont de la forme : [A,O,C,Score] ****/
/****   5: Nombre de coup restant à simuler ****/
meilleur_coup(_,_,[],[],_).

/** le coup est gagnant : une case de la ligne adverse est accessible, on donne le score maximal**/
meilleur_coup(_,_,[[8,_,_]|_],[[8,_,_,9]],_).

/** le coup doit être étudié **/
meilleur_coup(C,P,[[A,O,NC]|L],L_,NB_COUP):-

    /* simulation du coup */
    member([AC,OC,_,C,b],P),
    modifier_plateau(P,AC,OC,A,O,C,b,NP),
    member([A,O,NC,C,b],NP),

    /* simulation du coup adverse */
    coup_utilisateur(NC,NP,S,NB_COUP),

    /* simulation des autres coups */
    meilleur_coup(C,P,L,L2,NB_COUP),
    
    L_ = [[A,O,NC,S]|L2]. /*on transmet ce coup*/


/*** predicat intermediaire entre la prédiction du coup de l'ia et celui de l'utilisateur ***/
/****    1: Couleur à jouer    ****/
/****    2: Plateau sûr le quel simuler le coup   ****/
/****    3: Score du coup    ****/
/****    4: Nombre de coup restant à simuler   ****/

/** le coup est perdant : la première ligne de l'ia est accessible, on donne le score minimal **/
coup_utilisateur(C,P,-1,_):-
    accessible(a,C,P,L_accessible),
    member([1,_,_],L_accessible).

/** la profondeur souhaité à été atteinte : on calcule le score**/
coup_utilisateur(_,P,S,0):-
    liste_couleur_bloquee(a,P,LCB), /* LCB = Liste de couleurs bloquées */
    length(LCB,S). /*S = Nombre de couleurs bloquées */

/** l'utilisateur et l'ia sont bloqué : on essai d'éviter cette situation **/
coup_utilisateur(C,P,-1,_):-
    accessible(a,C,P,[]),
    member([_,_,NC,C,a],P),

    accessible(b,NC,P,[]).      
    

/** l'utilisateur est bloqué : on simule le prochain coup de l'ia **/
coup_utilisateur(C,P,S,NB_COUP):-
    accessible(a,C,P,[]),
    member([_,_,NC,C,a],P),

    NB_COUP2 is NB_COUP - 1,
    
    /* On simule les prochains coups de l'ia */
    accessible(b,NC,P,L_coup_possible),
    meilleur_coup(NC,P,L_coup_possible,L2,NB_COUP2),

    /* On maximise */
    my_sort2(L2,[[_,_,_,S]|_]).

/** RAS : on simule les coups possible de l'utilisateur **/
coup_utilisateur(C,P,S,NB_COUP):-
    /* L = tout les coup possible */
    accessible(a,C,P,L),

    /* simulation de tout les coups */
    meilleur_coup_utilisateur(C,L,L2,P,NB_COUP),

    /* On minimise */
    my_sort3(L2,[[_,_,_,S]|_]).



/*** Prédicat permetant de simuler les coups de l'utilisateur ***/
/****   1: Couleur de la tour a jouer ****/
/****   2: Liste de coup a simuler dont les élément sont de la forme : [Abscice de la futur case, Ordonné de la future case, Couleur de la future case] ****/
/****   3: Liste de coup simuler dont les élément sont de la forme : [A,O,C,Score] ****/
/****   4: Plateau sûr le quel simuler le coup ****/
/****   5: Nombre de coup restant à simuler ****/

meilleur_coup_utilisateur(_,[],[],_,_).

/** la profondeur souhaité à été atteinte : on calcule le score après avoir simulé le coup**/
meilleur_coup_utilisateur(C,[[A,O,NC]|L],L_,P,0):-
    /* On simule le coup */
    member([AC,OC,_,C,a],P),
    modifier_plateau(P,AC,OC,A,O,C,a,NP),
    member([A,O,NC,C,a],NP),

    /* On calcule le score */
    liste_couleur_bloquee(a,NP,LCB),  /* LCB = Liste de couleurs bloquées */
    length(LCB,S), /*S = Nombre de couleurs bloquées */

    /* On simule les autres coups */
    meilleur_coup_utilisateur(C,L,L_2,P,0),
    L_ = [[A,O,NC,S]|L_2],!.

/** l'utilisateur et l'ia sont bloqué, on essai d'éviter cette situation : Score = -1**/
meilleur_coup_utilisateur(C,[[A,O,NC]|L],L_,P,NB_COUP):-
    /* On simule le coup */
    member([AC,OC,_,C,a],P),
    modifier_plateau(P,AC,OC,A,O,C,a,NP),
    member([A,O,NC,C,a],NP),

    /* l'ia ne peut pas jouer */
    accessible(b,NC,NP,[]),

    /* l'utilisateur ne peut pas jouer */
    accessible(a,NC,NP,[]),

    /* on simule les autres coup */
    meilleur_coup_utilisateur(C,L,L_2,P,NB_COUP),
    L_ = [[A,O,NC,-1]|L_2].

/* l'ia ne peut pas jouer : on re fait jouer l'utilisateur*/
meilleur_coup_utilisateur(C,[[A,O,NC]|L],L_,P,NB_COUP):-
    /* on simule le coup */
    member([AC,OC,_,C,a],P),
    modifier_plateau(P,AC,OC,A,O,C,a,NP),
    member([A,O,NC,C,a],NP),
    
    /* l'ia ne peut pas jouer */
    accessible(b,NC,NP,[]),

    NB_COUP2 is NB_COUP - 1,

    /* L_coup_possible = tout les coup possible pour l'utilisateur*/
    accessible(a,NC,NP,L_coup_possible),

    /* on simule tout les coups possibles */ 
    meilleur_coup_utilisateur(C,L_coup_possible,L2,NP,NB_COUP2),

    /* on minimise */
    my_sort3(L2,[[_,_,_,S]|_]),
    
    /* on simule les autres coups */
    meilleur_coup_utilisateur(C,L,L_2,P,NB_COUP),
    L_ = [[A,O,NC,S]|L_2].

/*il faut approfondir*/
meilleur_coup_utilisateur(C,[[A,O,NC]|L],L_,P,NB_COUP):-
    /* on simule le coup */
    member([AC,OC,_,C,a],P),
    modifier_plateau(P,AC,OC,A,O,C,a,NP),
    member([A,O,NC,C,a],NP),

    NB_COUP2 is NB_COUP - 1,
    
    /* L_coup_possible = tout les coup possible pour l'utilisateur*/
    accessible(b,NC,NP,L_coup_possible),
    /* on simule tout les coups possibles */
    meilleur_coup(NC,NP,L_coup_possible,L2,NB_COUP2),

    /* on maximise */
    my_sort2(L2,[[_,_,_,S]|_]),

    /* on simule les autres coups */
    meilleur_coup_utilisateur(C,L,L_2,P,NB_COUP),
    L_ = [[A,O,NC,S]|L_2]. /*on transmet ce coup*/


/**** Prédicat permetant de faire jouer l'ia ****/

/** L'ia commence : on choisit un coup aux hasards **/
jouer_val:-
    joueur(LJ),
    member(b,LJ),
    couleur(L),
    length(L,Nb_elem),
    Nb_elem > 1,
    /* plusieurs couleurs sont jouables -> c'est le premier coup */

    /* choix de la tour aux hasards */
    random_member(C,L),
    plateau(P),

    /* choix du déplacement aux hasards */
    accessible(b,C,P,L_accessible),
    random_member([A,O,NC],L_accessible),

    /* routine de déplacement le la tour */
    deplacer2(b,C,A,O,NP,NC),nl,!,
    retract(joueur(_)),
    assert(joueur([a])),
    retract(couleur(_)),
    assert(couleur([NC])),
    retract(plateau(P)),
    assert(plateau(NP)),
    redessiner,!.
    

/*coup gagnant*/
jouer_val:-
	joueur(LJ),
	member(b,LJ),
	couleur([C]),
	plateau(P),
	
	accessible(b,C,P,L_accessible),
	member([8,O,NC],L_accessible),
	/* la dernière ligne adverse est accessible on a gagné*/
	
	deplacer2(b,C,8,O,NP,NC),
	retract(plateau(P)),
	assert(plateau(NP)),
	redessiner,
	send(@p, display,new(Text, text('YOU LOOSE')), point(660, 140)),
	send(Text, font, font(times, bold, 40)),!.


/*Coup quelquonque*/
jouer_val:-
    couleur([C|_]),
    plateau(P),
    
    accessible(b,C,P,L_coup_possible),
    my_sort(L_coup_possible,L_trie), /* on trie les coups dans l'ordre du plus loin aux plus près */

    /* on simule à un coup à l'avance */
    meilleur_coup(C,P,L_trie,L,1),

    /* on maximise */
    my_sort2(L,[[A,O,NC,_]|_]),
    
    /*on joue le meuilleur coup*/
    deplacer2(b,C,A,O,NP,NC),
    retract(joueur(_)),
    assert(joueur([a])),
    retract(couleur([C])),
    assert(couleur([NC])),
    plateau(P),
    retract(plateau(P)),
    assert(plateau(NP)),
    redessiner,!.

/*IA ne peut pas jouer*/
jouer_val:-
	joueur(LJ),
	member(b,LJ),
	couleur([C]),
	plateau(P),
	accessible(b,C,P,L_accessible),
	L_accessible = [],
	member([_,_,NC,C,b],P),
	retract(joueur(_)),
	assert(joueur([a])),
	retract(couleur([C])),
	assert(couleur([NC])),
	write('Noir ne peut pas jouer car il est bloqué'),nl,
	redessiner,!.


/*prédicat de tri dont les éléments sont composés de 3 éléments, tri croissant en fonction du 1er*/
switch_unsorted([], []).
switch_unsorted([X|[]], [X]).

switch_unsorted([[X,C,V]|[[Y,U,I]|L]], L_):-
    X >= Y,
    switch_unsorted([[Y,U,I]|L], L_2),
    L_ = [[X,C,V]|L_2],!.

switch_unsorted([[X,C,V]|[[Y,U,I]|L]], L_):-
    X < Y,
    L_ = [[Y,U,I]|[[X,C,V]|L]],!.

is_sorted([]).
is_sorted([_|[]]).
is_sorted([[X,_,_]|[[Y,U,I]|L]]):-
    X >= Y,
    is_sorted([[Y,U,I]|L]),!.

my_sort(L,L):-
    is_sorted(L),!.

my_sort(L,L_):-
    switch_unsorted(L,L_2),
    my_sort(L_2,L_3),
    L_ = L_3,!.


/*prédicat de tri dont les éléments sont composés de 4 éléments, tri croissant en fonction du 4ème*/
switch_unsorted2([], []).
switch_unsorted2([X|[]], [X]).

switch_unsorted2([[X,C,V,B]|[[Y,U,I,O]|L]], L_):-
    B >= O,
    switch_unsorted2([[Y,U,I,O]|L], L_2),
    L_ = [[X,C,V,B]|L_2],!.

switch_unsorted2([[X,C,V,B]|[[Y,U,I,O]|L]], L_):-
    B < O,
    L_ = [[Y,U,I,O]|[[X,C,V,B]|L]],!.

is_sorted2([]).
is_sorted2([_|[]]).
is_sorted2([[_,_,_,B]|[[Y,U,I,O]|L]]):-
    B >= O,
    is_sorted2([[Y,U,I,O]|L]),!.

my_sort2(L,L):-
    is_sorted2(L),!.

my_sort2(L,L_):-
    switch_unsorted2(L,L_2),
    my_sort2(L_2,L_3),
    L_ = L_3,!.

/*prédicat de tri dont les éléments sont composés de 4 éléments, tri décroissant en fonction du 4ème*/
switch_unsorted3([], []).
switch_unsorted3([X|[]], [X]).

switch_unsorted3([[X,C,V,B]|[[Y,U,I,O]|L]], L_):-
    B =< O,
    switch_unsorted3([[Y,U,I,O]|L], L_2),
    L_ = [[X,C,V,B]|L_2],!.

switch_unsorted3([[X,C,V,B]|[[Y,U,I,O]|L]], L_):-
    B > O,
    L_ = [[Y,U,I,O]|[[X,C,V,B]|L]],!.

is_sorted3([]).
is_sorted3([_|[]]).
is_sorted3([[_,_,_,B]|[[Y,U,I,O]|L]]):-
    B =< O,
    is_sorted3([[Y,U,I,O]|L]),!.

my_sort3(L,L):-
    is_sorted3(L),!.

my_sort3(L,L_):-
    switch_unsorted3(L,L_2),
    my_sort3(L_2,L_3),
    L_ = L_3,!.


/*** prédicats permetant de savoir les couleurs bloquées ***/

/** la couleur Coul du joueur b peut atteindre la ligne victorieuse, le joueur a ne peut s'arrêter sûr cette couleur -> cette couleur est "bloquée" pour le joueur a **/
test_couleur_bloquee(a,Coul,P,L,L_):-
    accessible(b,Coul,P,L_coup_possible),
    memberchk([8,_,_],L_coup_possible),
    append([Coul],L,L_),!.

/** sinon elle ne l'est pas **/
test_couleur_bloquee(a,Coul,P,L,L_):-
    accessible(b,Coul,P,L_coup_possible),
    not(memberchk([8,_,_],L_coup_possible)),
    append([],L,L_),!.

/** la couleur Coul du joueur a peut atteindre la ligne victorieuse, le joueur b ne peut s'arrêter sûr cette couleur -> cette couleur est "bloquée" pour le joueur b **/
test_couleur_bloquee(b,Coul,P,L,L_):-
    accessible(a,Coul,P,L_coup_possible),
    memberchk([1,_,_],L_coup_possible),
    append([Coul],L,L_),!.

/** sinon elle ne l'est pas **/
test_couleur_bloquee(b,Coul,P,L,L_):-
    accessible(a,Coul,P,L_coup_possible),
    not(memberchk([1,_,_],L_coup_possible)),
    append([],L,L_),!.

/* Liste des cases où le joueur J ne peut pas s'arreter */
liste_couleur_bloquee(J,P,L):-
    test_couleur_bloquee(J,brown,P,[],L1),
    test_couleur_bloquee(J,green,P,L1,L2),
    test_couleur_bloquee(J,red,P,L2,L3),
    test_couleur_bloquee(J,yellow,P,L3,L4),
    test_couleur_bloquee(J,pink,P,L4,L5),
    test_couleur_bloquee(J,purple,P,L5,L6),
    test_couleur_bloquee(J,blue,P,L6,L7),
    test_couleur_bloquee(J,orange,P,L7,L),!.


