/*	une case vide est représentée par une liste [couleur]
	une case occupéeest représentée par une liste [couleur,tour,joueur]
	les joueurs sont a et b
	les couleurs sont orange(or), bleu(bl), violet(pu), rose(pi), jaune(ye), rouge(re), vert(gr) et marron(br)
	les directions de déplacement sont gauche(le), avant(fo) et droite(ri)
*/

/*plateau([
[[or,or,b],[bl,bl,b],[pu,pu,b],[pi,pi,b],[ye,ye,b],[re,re,b],[gr,gr,b],[br,br,b]],
[[re],[or],[pi],[gr],[bl],[ye],[br],[pu]],
[[gr],[pi],[or],[re],[pu],[br],[ye],[bl]],
[[pi],[pu],[bl],[or],[br],[gr],[re],[ye]],
[[ye],[re],[gr],[br],[or],[bl],[pu],[pi]],
[[bl],[ye],[br],[pu],[re],[or],[pi],[gr]],
[[pu],[br],[ye],[bl],[gr],[pi],[or],[re]],
[[br,br,a],[gr,gr,a],[re,re,a],[ye,ye,a],[pi,pi,a],[pu,pu,a],[bl,bl,a],[or,or,a]]]).*/

plateau([
[[1,1,or,or,b],[1,2,bl,bl,b],[1,3,pu,pu,b],[1,4,pi,pi,b],[1,5,ye,ye,b],[1,6,re,re,b],[1,7,gr,gr,b],[1,8,br,br,b]],
[[2,1,re],[2,2,or],[2,3,pi],[2,4,gr],[2,5,bl],[2,6,ye],[2,7,br],[2,8,pu]],
[[3,1,gr],[3,2,pi],[3,3,or],[3,4,re],[3,5,pu],[3,6,br],[3,7,ye],[3,8,bl]],
[[4,1,pi],[4,2,pu],[4,3,bl],[4,4,or],[4,5,br],[4,6,gr],[4,7,re],[4,8,ye]],
[[5,1,ye],[5,2,re],[5,3,gr],[5,4,br],[5,5,or],[5,6,bl],[5,7,pu],[5,8,pi]],
[[6,1,bl],[6,2,ye],[6,3,br],[6,4,pu],[6,5,re],[6,6,or],[6,7,pi],[6,8,gr]],
[[7,1,pu],[7,2,br],[7,3,ye],[7,4,bl],[7,5,gr],[7,6,pi],[7,7,or],[7,8,re]],
[[8,1,br,br,a],[8,2,gr,gr,a],[8,3,re,re,a],[8,4,ye,ye,a],[8,5,pi,pi,a],[8,6,pu,pu,a],[8,7,bl,bl,a],[8,8,or,or,a]]]).

case(X):-
	plateau(L),
	member(LL,L),
	member(X,LL).


/*Recherche des cases accessibles depuis la case occupée X*/
accessible_a(X,L):-
	X = [A,O,_C,_T,a],
	AA is A-1,
	OG is O-1,
	OD is O+1,
	accessible_gauche_a(AA,OG,LG),
	accessible_avant_a(AA,O,LA),
	accessible_droite_a(AA,OD,LD),
	append(LG,LA,LL),
	append(LL,LD,L).

accessible_gauche_a(_A,0,[]):-!.
accessible_gauche_a(0,_O,[]):-!.
accessible_gauche_a(A,O,[]):-
	case([A,O,_,_,a]).
accessible_gauche_a(A,O,[]):-
	case([A,O,_,_,b]).
accessible_gauche_a(A,O,L):-
	AA is A-1,
	OO is O-1,
	accessible_droite_a(AA,OO,LL),
	append(case([A,O,X]),LL,L).

accessible_avant_a(0,_O,[]):-!.
accessible_avant_a(A,O,[]):-
	case([A,O,_,_,a]).
accessible_avant_a(A,O,[]):-
	case([A,O,_,_,b]).
accessible_avant_a(A,O,L):-
	AA is A-1,
	accessible_droite_a(AA,O,LL),
	append(case([A,O,X]),LL,L).

accessible_droite_a(0,_O,[]):-!.
accessible_droite_a(_A,9,[]):-!.
accessible_droite_a(A,O,[]):-
	case([A,O,_,_,a]).
accessible_droite_a(A,O,[]):-
	case([A,O,_,_,b]).
accessible_droite_a(A,O,L):-
	AA is A-1,
	OO is O+1,
	accessible_droite_a(AA,OO,LL),
	append(case([A,O,X]),LL,L).

dessiner_case([_,_,X]):-
	write(X),write('     '),!.
dessiner_case([_,_,X,Y,Z]):-
	write(X),write(' '),write(Y),write(' '),write(Z).

dessiner_ligne([]):-
	write('|'),nl,
	write('|       |       |       |       |       |       |       |       |'),nl,
	write('|       |       |       |       |       |       |       |       |'),nl,
	write('-----------------------------------------------------------------'),nl.
dessiner_ligne([H|T]):-
	write('|'),dessiner_case(H),
	dessiner_ligne(T).

dessiner_plateau([]).
dessiner_plateau([H|T]):-
	dessiner_ligne(H),
	dessiner_plateau(T).

dessiner:-
	write('-----------------------------------------------------------------'),nl,
	plateau(L),
	dessiner_plateau(L).











