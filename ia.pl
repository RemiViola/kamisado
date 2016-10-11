test_couleur_bloque(a,Coul,L,L_):-
    accessible(b,Coul,L_coup_possible),
    memberchk([1,_,_],L_coup_possible),
    append([Coul],L,L_).

test_couleur_bloque(a,Coul,L,L_):-
    accessible(b,Coul,L_coup_possible),
    not(memberchk([1,_,_],L_coup_possible)),
    append([],L,L_).

test_couleur_bloque(b,Coul,L,L_):-
    accessible(a,Coul,L_coup_possible),
    memberchk([1,_,_],L_coup_possible),
    append([Coul],L,L_).

test_couleur_bloque(b,Coul,L,L_):-
    accessible(a,Coul,L_coup_possible),
    not(memberchk([1,_,_],L_coup_possible)),
    append([],L,L_).

liste_couleur_bloque(J,L):-
    test_couleur_bloque(J,br,[],L1),
    test_couleur_bloque(J,gr,L1,L2),
    test_couleur_bloque(J,re,L2,L3),
    test_couleur_bloque(J,ye,L3,L4),
    test_couleur_bloque(J,pi,L4,L5),
    test_couleur_bloque(J,pu,L5,L6),
    test_couleur_bloque(J,bl,L6,L7),
    test_couleur_bloque(J,or,L7,L).

liste_couleur_autorise(J,L):-
    liste_couleur_bloque(J,L_),
    subtract([br,gr,re,ye,pi,pu,bl,or],L_,L).

