test_couleur_bloquee(a,Coul,P,L,L_):-
    accessible(b,Coul,P,L_coup_possible),
    memberchk([8,_,_],L_coup_possible),
    append([Coul],L,L_).

test_couleur_bloquee(a,Coul,P,L,L_):-
    accessible(b,Coul,P,L_coup_possible),
    not(memberchk([8,_,_],L_coup_possible)),
    append([],L,L_).

test_couleur_bloquee(b,Coul,P,L,L_):-
    accessible(a,Coul,P,L_coup_possible),
    memberchk([1,_,_],L_coup_possible),
    append([Coul],L,L_).

test_couleur_bloquee(b,Coul,P,L,L_):-
    accessible(a,Coul,P,L_coup_possible),
    not(memberchk([1,_,_],L_coup_possible)),
    append([],L,L_).

/*Liste des cases où le joueur ne peut pas s'arreter                  /!\problème si le déplacement change cette liste*/
liste_couleur_bloquee(J,P,L):-
    test_couleur_bloquee(J,br,P,[],L1),
    test_couleur_bloquee(J,gr,P,L1,L2),
    test_couleur_bloquee(J,re,P,L2,L3),
    test_couleur_bloquee(J,ye,P,L3,L4),
    test_couleur_bloquee(J,pi,P,L4,L5),
    test_couleur_bloquee(J,pu,P,L5,L6),
    test_couleur_bloquee(J,bl,P,L6,L7),
    test_couleur_bloquee(J,or,P,L7,L).

/*Liste des cases où le joueur peut s'arreter                         idem*/
liste_couleur_autorisee(J,P,L):-
    liste_couleur_bloquee(J,P,L_),
    subtract([br,gr,re,ye,pi,pu,bl,or],L_,L).

