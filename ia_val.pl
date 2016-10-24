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
    

meilleur_coup(_,[],[],_).
meilleur_coup(C,[[A,O,NC]|_],L_,N):-
    /*write('etude de la position GG: '),
    write(A), write(' '), write(O), nl,*/
	    
    deplacer2(b,C,A,O,NP,NC), /* NP = nouveau plateau, après déplacement virtuel*/
    liste_couleur_bloquee(a,NP,NLCB), /* NLCB = nouveau LCB, après déplacement virtuel*/
    length(NLCB,NNBCB), /*NNCB = nouveau nombre de couleurs bloquées, après déplacement virtuel*/

    NNBCB > N,
    L_ = [A,O,NC],!. /*on transmet ce coup*/

meilleur_coup(C,[[A,O,NC]|L],L_,N):-
    /*write('etude de la position : '),
    write(A), write(' '), write(O), nl,*/
    
    deplacer2(b,C,A,O,NP,NC), /* NP = nouveau plateau, après déplacement virtuel*/
    liste_couleur_bloquee(a,NP,NLCB), /* NLCB = nouveau LCB, après déplacement virtuel*/
    length(NLCB,NNBCB), /*NNCB = nouveau nombre de couleurs bloquées, après déplacement virtuel*/


    N >= NNBCB, /*coup en cour moins bon que le coup actuel*/
    meilleur_coup(C,L,L_2,N), /*test de tout les autres déplacements*/
    L_ = L_2,!. /*on transmet ce coup*/


/*coup gagnant*/

jouer:-
	joueur(LJ),
	member(b,LJ),
	couleur([C]),
	plateau(P),
	accessible(b,C,P,L_accessible),
	member([8,O,NC],L_accessible),
	deplacer2(b,C,8,O,NP,NC),
	retract(joueur(_)),
	assert(joueur([])),
	retract(couleur([C])),
	assert(couleur([])),
	plateau(P),
	retract(plateau(P)),
	assert(plateau(NP)),
	redessiner,
	send(@p, display,new(T, text('YOU LOOSE')), point(660, 140)),
	send(T, font, font(times, bold, 40)),!.


/*Coup quelquonque*/
jouer:-
    couleur([C|_]),

    /* length(L,nb_elem),
    nb_elem = 0,*/
    
    plateau(P),
    accessible(b,C,P,L_coup_possible),
    my_sort(L_coup_possible,L_trie),

    liste_couleur_bloquee(a,P,LCB), /* LCB = liste couleurs bloquées*/
    length(LCB,NBCB),

    
    meilleur_coup(C,L_trie,[A,O,NC],NBCB),
    
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

/*pas de meilleur coup*/

jouer:-
    couleur([C|_]),

    /* length(L,nb_elem),
    nb_elem = 0,*/
    
    plateau(P),
    accessible(b,C,P,L_coup_possible),
    my_sort(L_coup_possible,L_trie),

    liste_couleur_bloquee(a,P,LCB), /* LCB = liste couleurs bloquées*/
    length(LCB,NBCB),

    meilleur_coup(C,L_trie,L,NBCB),
    length(L,0),
    
    /*on joue le coup le plus loin*/
    L_trie = [[A,O,NC]|_],
    deplacer2(b,C,A,O,NP,NC),
    retract(joueur(_)),
    assert(joueur([a])),
    retract(couleur([C])),
    assert(couleur([NC])),
    plateau(P),
    retract(plateau(P)),
    assert(plateau(NP)),
    redessiner,!.

jouer:-
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
