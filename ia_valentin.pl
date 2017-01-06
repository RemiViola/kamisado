:-module(ia_valentin,[jouer_val/0]).

/*Premier coup ...*/
/*jouer:-
    couleur(L),
    length(L,nb_elem),
    nb_elem > 1.
*/
/*Liste coup sans interdit*/
liste_coup([],[]).

liste_coup([[_,_,X]|L],L_):-
    liste_couleur_bloque(b,L_coul_bloq),
    memberchk(X,L_coul_bloq),
    liste_coup(L,L_2),
    L_ = L_2.

liste_coup([[A,O,X]|L],L_):-
    liste_couleur_bloque(b,L_coul_bloq),
    not(memberchk(X,L_coul_bloq)),
    liste_coup(L,L_2),
    L_ = [[A,O,X]|L_2].
    

meilleur_coup(_,_,[],[],_).

/*le coup est gagnant*/
meilleur_coup(_,_,[[1,O,NC]|_],L_,_):-
    /*write('---etude de la position GG: '),
    write(A), write(' '), write(O), nl,*/
 
    L_ = [[1,O,NC,9]]. /*on transmet ce coup*/

/*le coup étudié est bon*/
meilleur_coup(C,P,[[A,O,NC]|L],L_,NB_COUP):-
    write(NB_COUP),write(' '),
    write('etude de la position GG: '),
    write(A), write(' '), write(O), nl,

    member([AC,OC,_,C,b],P),
    modifier_plateau(P,AC,OC,A,O,C,b,NP),
    member([A,O,NC,C,b],NP),

    coup_utilisateur(NC,NP,S,NB_COUP),
    meilleur_coup(C,P,L,L2,NB_COUP),
    
    L_ = [[A,O,NC,S]|L2]. /*on transmet ce coup*/

coup_utilisateur(C,P,-1,_):-
    /*write(C),nl,*/
    accessible(a,C,P,L_accessible),
    member([1,_,_],L_accessible).

coup_utilisateur(C,P,S,0):-
    accessible(a,C,P,L_accessible),
    length(L_accessible,0),
    liste_couleur_bloquee(a,P,NLCB), /* NLCB = nouveau LCB, après déplacement virtuel*/
    length(NLCB,S). /*NNCB = nouveau nombre de couleurs bloquées, après déplacement virtuel*/

coup_utilisateur(C,P,S,NB_COUP):-
    accessible(a,C,P,L_accessible),
    length(L_accessible,0),

    NB_COUP2 is NB_COUP - 1,
    accessible(b,NC,NP,L_coup_possible),
    meilleur_coup(NC,NP,L_coup_possible,L2,NB_COUP2),

    my_sort2(L2,[[_,_,_,S]|_]).
    

coup_utilisateur(C,P,S,NB_COUP):-
    write(NB_COUP),write('cu1'),nl,
    accessible(a,C,P,L_accessible),
    my_sort(L_accessible,L_trie1),
    reverse(L_trie1,L_trie),

    write('------'),nl,
    meilleur_coup_utilisateur(C,L_trie,L,P,NB_COUP),
    write('------'),nl,
    my_sort3(L,[[_,_,_,S]|_]).
/*
coup_utilisateur(C,P,N,NB_COUP):-
    write(NB_COUP),write('cu2'),nl,
    accessible(a,C,P,L_accessible),
    my_sort(L_accessible,L_trie1),
    reverse(L_trie1,L_trie),

    liste_couleur_bloquee(a,P,LCB),
    length(LCB,N),
    not(meilleur_coup_utilisateur(C,L_trie,_,P,NB_COUP))
    .*/

    
meilleur_coup_utilisateur(_,[],[],_,_).

/*le coup étudié est bon*/
meilleur_coup_utilisateur(C,[[A,O,NC]|L],L_,P,0):-
    write('etude de la position u : '),
    write(A), write(' '), write(O), nl,
    
    member([AC,OC,_,C,a],P),
    nl,
    modifier_plateau(P,AC,OC,A,O,C,a,NP),
    nl,
    member([A,O,NC,C,a],NP),
    nl,
    
    liste_couleur_bloquee(a,NP,NLCB), /* NLCB = nouveau LCB, après déplacement virtuel*/
    length(NLCB,S), /*NNCB = nouveau nombre de couleurs bloquées, après déplacement virtuel*/
    nl,
    meilleur_coup_utilisateur(C,L,L_2,P,0),
    L_ = [[A,O,NC,S]|L_2],!. /*on transmet ce coup*/

/*il faut approfondir*/
meilleur_coup_utilisateur(C,[[A,O,NC]|L],L_,P,NB_COUP):-
    write(NB_COUP),write(' '),
    write('etude de la position u : '),
    write(A), write(' '), write(O), write(' '), nl,

    NB_COUP > 0,
    member([AC,OC,_,C,a],P),
    modifier_plateau(P,AC,OC,A,O,C,a,NP),
    member([A,O,NC,C,a],NP),

    NB_COUP2 is NB_COUP - 1,
    accessible(b,NC,NP,L_coup_possible),
    length(L_coup_possible,0),
    meilleur_coup_utilisateur(NC,L_coup_possible,L2,NP,NB_COUP2),

    my_sort2(L2,[[_,_,_,S]|_]),

    meilleur_coup_utilisateur(C,L,L_2,P,NB_COUP),
    L_ = [[A,O,NC,S]|L_2]. /*on transmet ce coup*/


/*il faut approfondir*/
meilleur_coup_utilisateur(C,[[A,O,NC]|L],L_,P,NB_COUP):-
    write(NB_COUP),write(' '),
    write('etude de la position u : '),
    write(A), write(' '), write(O), write(' '), nl,

    NB_COUP > 0,
    member([AC,OC,_,C,a],P),
    modifier_plateau(P,AC,OC,A,O,C,a,NP),
    member([A,O,NC,C,a],NP),

    NB_COUP2 is NB_COUP - 1,
    accessible(b,NC,NP,L_coup_possible),
    meilleur_coup(NC,NP,L_coup_possible,L2,NB_COUP2),

    my_sort2(L2,[[_,_,_,S]|_]),

    meilleur_coup_utilisateur(C,L,L_2,P,NB_COUP),
    L_ = [[A,O,NC,S]|L_2]. /*on transmet ce coup*/


/*coup gagnant*/
jouer_val:-
	joueur(LJ),
	member(b,LJ),
	couleur([C]),
	plateau(P),
	accessible(b,C,P,L_accessible),
	member([8,O,NC],L_accessible),
	deplacer2(b,C,8,O,NP,NC),
	retract(plateau(P)),
	assert(plateau(NP)),
	redessiner,
	send(@p, display,new(Text, text('YOU LOOSE')), point(660, 140)),
	send(Text, font, font(times, bold, 40)),!.


/*Coup quelquonque*/
jouer_val:-
    couleur([C|_]),

    /* length(L,nb_elem),
    nb_elem = 0,*/
    
    plateau(P),
    accessible(b,C,P,L_coup_possible),
    my_sort(L_coup_possible,L_trie),
    write(L_trie),nl,

    length(L_coup_possible,N),
    N < 4,
    nl,nl,nl,nl,nl,
    meilleur_coup(C,P,L_trie,L,1),
    write('polp'),nl,
    my_sort2(L,L4),
    write(L4),nl,
    
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

jouer_val:-
    couleur([C|_]),

    /* length(L,nb_elem),
    nb_elem = 0,*/
    
    plateau(P),
    accessible(b,C,P,L_coup_possible),
    my_sort(L_coup_possible,L_trie),
    write(L_trie),nl,
    
    meilleur_coup(C,P,L_trie,L,0),
    my_sort2(L,L4),
    write(L4),nl,
    
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

/*Pas de meilleur coup, on va le plus loin possible*/
/*Option 1 : Aller le plus loin san perdre*/
jouer_val:-
    write('pas top'),nl,
    couleur([C|_]),

    /* length(L,nb_elem),
    nb_elem = 0,*/
    
    plateau(P),
    accessible(b,C,P,L_coup_possible),
    my_sort(L_coup_possible,L_trie),

    /*on joue le coup le plus loin*/
    le_plus_loin_sans_perde(C,L_trie,[A,O,NC]),
    deplacer2(b,C,A,O,NP,NC),
    retract(joueur(_)),
    assert(joueur([a])),
    retract(couleur([C])),
    assert(couleur([NC])),
    plateau(P),
    retract(plateau(P)),
    assert(plateau(NP)),
    redessiner,!.


/*Option 2 : L'ia est condamnée à perdre*/
jouer_val:-
    write('comdamnée'),nl,
    couleur([C|_]),

    /* length(L,nb_elem),
    nb_elem = 0,*/
    
    plateau(P),
    accessible(b,C,P,[[A,O,NC]|_]),

    /*on joue le coup le plus loin*/
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

jouer_val:-
    couleur(L),
    length(L,nb_elem),
    nb_elem > 1,
    


/*fonctions de tri*/
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


/*fonctions de tri*/
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

/*fonctions de tri*/
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


/*predicat sans perte*/
le_plus_loin_sans_perde(_,[],[]).
le_plus_loin_sans_perde(C,[[A,O,NC]|_],[A,O,NC]):-
    deplacer2(b,C,A,O,P2,C2),
    accessible(a,C2,P2,L_accessible),
    not(member([1,_,_],L_accessible)).
le_plus_loin_sans_perde(C,[[A,O,NC]|_],[A,O,NC]):-
    deplacer2(b,C,A,O,P2,C2),
    accessible(a,C2,P2,L_accessible),
    member([1,_,_],L_accessible).


/******************************************/

test_couleur_bloquee(a,Coul,P,L,L_):-
    accessible(b,Coul,P,L_coup_possible),
    memberchk([8,_,_],L_coup_possible),
    append([Coul],L,L_),!.

test_couleur_bloquee(a,Coul,P,L,L_):-
    accessible(b,Coul,P,L_coup_possible),
    not(memberchk([8,_,_],L_coup_possible)),
    append([],L,L_),!.

test_couleur_bloquee(b,Coul,P,L,L_):-
    accessible(a,Coul,P,L_coup_possible),
    memberchk([1,_,_],L_coup_possible),
    append([Coul],L,L_),!.

test_couleur_bloquee(b,Coul,P,L,L_):-
    accessible(a,Coul,P,L_coup_possible),
    not(memberchk([1,_,_],L_coup_possible)),
    append([],L,L_),!.

/*Liste des cases où le joueur ne peut pas s'arreter                  /!\problème si le déplacement change cette liste*/
liste_couleur_bloquee(J,P,L):-
    test_couleur_bloquee(J,brown,P,[],L1),
    test_couleur_bloquee(J,green,P,L1,L2),
    test_couleur_bloquee(J,red,P,L2,L3),
    test_couleur_bloquee(J,yellow,P,L3,L4),
    test_couleur_bloquee(J,pink,P,L4,L5),
    test_couleur_bloquee(J,purple,P,L5,L6),
    test_couleur_bloquee(J,blue,P,L6,L7),
    test_couleur_bloquee(J,orange,P,L7,L),!.

/*Liste des cases où le joueur peut s'arreter                         idem*/
liste_couleur_autorisee(J,P,L):-
    liste_couleur_bloquee(J,P,L_),
    subtract([brown,green,red,yellow,pink,purple,blue,orange],L_,L),!.

