/*	une case vide est représentée par une liste [couleur]
	une case occupéeest représentée par une liste [couleur,tour,joueur]
	les joueurs sont a et b
	les couleurs sont orange(or), bleu(bl), violet(pu), rose(pi), jaune(ye), rouge(re), vert(gr) et marron(br)
*/

plateau([
[[or,or,b],[bl,bl,b],[pu,pu,b],[pi,pi,b],[ye,ye,b],[re,re,b],[gr,gr,b],[br,br,b]],
[[re],[or],[pi],[gr],[bl],[ye],[br],[pu]],
[[gr],[pi],[or],[re],[pu],[br],[ye],[bl]],
[[pi],[pu],[bl],[or],[br],[gr],[re],[ye]],
[[ye],[re],[gr],[br],[or],[bl],[pu],[pi]],
[[bl],[ye],[br],[pu],[re],[or],[pi],[gr]],
[[pu],[br],[ye],[bl],[gr],[pi],[or],[re]],
[[br,br,a],[gr,gr,a],[re,re,a],[ye,ye,a],[pi,pi,a],[pu,pu,a],[bl,bl,a],[or,or,a]]]).

case(X):-
	plateau(L),
	member(LL,L),
	member(X,LL).

dessiner_case([X]):-
	write(X),write('     '),!.
dessiner_case([X,Y,Z]):-
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











