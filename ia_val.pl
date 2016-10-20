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
    

meilleur_coup(_,[],[],0).
meilleur_coup(C,[[A,O,NC]|L],L_,N):-
    deplacer2(b,C,A,O,NP,NC), /* NP = nouveau plateau, après déplacement virtuel*/
    liste_couleur_bloquee(a,NP,NLCB), /* NLCB = nouveau LCB, après déplacement virtuel*/
    length(NLCB,NNBCB), /*NNCB = nouveau nombre de couleurs bloquées, après déplacement virtuel*/

    meilleur_coup(C,L,_,N_), /*test de tout les autres déplacements*/

    N_ < NNBCB, /*coup en cour meuilleur que le coup actuel*/
    N = NNBCB,
    L_ = [A,O,NC]. /*on transmet ce coup*/

meilleur_coup(C,[[A,O,NC]|L],L_,N):-

    deplacer2(b,C,A,O,NP,NC), /* NP = nouveau plateau, après déplacement virtuel*/
    liste_couleur_bloquee(a,NP,NLCB), /* NLCB = nouveau LCB, après déplacement virtuel*/
    length(NLCB,NNBCB), /*NNCB = nouveau nombre de couleurs bloquées, après déplacement virtuel*/

    meilleur_coup(C,L,L_2,N_), /*test de tout les autres déplacements*/

    N_ >= NNBCB, /*coup en cour moins bon que le coup actuel*/
    N = N_,
    L_ = L_2. /*on transmet ce coup*/
    
/*Coup quelquonque*/
jouer:-
    couleur([C|_]),

    /* length(L,nb_elem),
    nb_elem = 0,*/
    
    plateau(P),
    accessible(b,C,P,L_coup_possible),
    meilleur_coup(C,L_coup_possible,[A,O,NC],_),
    
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
