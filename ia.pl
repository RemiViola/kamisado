test_couleur_bloquee(a,Coul,L,L_):-
    accessible(b,Coul,L_coup_possible),
    memberchk([8,_,_],L_coup_possible),
    append([Coul],L,L_).

test_couleur_bloquee(a,Coul,L,L_):-
    accessible(b,Coul,L_coup_possible),
    not(memberchk([8,_,_],L_coup_possible)),
    append([],L,L_).

test_couleur_bloquee(b,Coul,L,L_):-
    accessible(a,Coul,L_coup_possible),
    memberchk([1,_,_],L_coup_possible),
    append([Coul],L,L_).

test_couleur_bloquee(b,Coul,L,L_):-
    accessible(a,Coul,L_coup_possible),
    not(memberchk([1,_,_],L_coup_possible)),
    append([],L,L_).

/*Liste des cases où le joueur ne peut pas s'arreter                  /!\problème si le déplacement change cette liste*/
liste_couleur_bloquee(J,L):-
    test_couleur_bloquee(J,br,[],L1),
    test_couleur_bloquee(J,gr,L1,L2),
    test_couleur_bloquee(J,re,L2,L3),
    test_couleur_bloquee(J,ye,L3,L4),
    test_couleur_bloquee(J,pi,L4,L5),
    test_couleur_bloquee(J,pu,L5,L6),
    test_couleur_bloquee(J,bl,L6,L7),
    test_couleur_bloquee(J,or,L7,L).

/*Liste des cases où le joueur peut s'arreter                         idem*/
liste_couleur_autorisee(J,L):-
    liste_couleur_bloquee(J,L_),
    subtract([br,gr,re,ye,pi,pu,bl,or],L_,L).

