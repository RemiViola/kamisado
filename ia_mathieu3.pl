:-module(ia_mathieu,[jouer_mat/0]).

:- use_module(library(random)).
:-dynamic(n_tour/1).

n_tour(1).

jouer_mat:-
	joueur(L_joueur),
	member(b,L_joueur),
	couleur([Couleur,Couleur2|Tail]),
	plateau(Plateau),
	random_member(X, [Couleur,Couleur2|Tail]),
	retract(n_tour(_)),
	assert(n_tour(1)),
	bestMove([b, X, play, Plateau], [_NextPlayer, NCouleur, State, NPlateau]),
	n_tour(NT),
	NT2 is NT+1,
	retract(n_tour(_)),
	assert(n_tour(NT2)),
	retract(joueur(_)),
	assert(joueur([a])),
	retract(couleur([Couleur,Couleur2|Tail])),
	assert(couleur([NCouleur])),
	retract(plateau(Plateau)),
	assert(plateau(NPlateau)),
	redessiner,
	fin(State),!.

jouer_mat:-
	joueur(L_joueur),
	member(b,L_joueur),
	couleur([Couleur]),
	plateau(Plateau),
	write(Couleur),nl,
	bestMove([b, Couleur, play, Plateau], [_NextPlayer, NCouleur, State, NPlateau]),
	n_tour(NT),
	NT2 is NT+1,
	retract(n_tour(_)),
	assert(n_tour(NT2)),
	retract(joueur(_)),
	assert(joueur([a])),
	retract(couleur([Couleur])),
	assert(couleur([NCouleur])),
	retract(plateau(Plateau)),
	assert(plateau(NPlateau)),
	redessiner,
	fin(State),!.

fin(State):-
	State \= win,!.

fin(State):-
	State = win,
	retract(joueur(_)),
	retract(couleur(_)),
	send(@p, display,new(Text, text('YOU LOOSE')), point(660, 140)),
	send(Text, font, font(times, bold, 40)).


bestMove(Pos, NextPos) :-
    minimax(Pos, NextPos, _, 0).


winPos(b, Couleur, Plateau):-
	%accessible(b, Couleur, Plateau, L_accessible),
	member([8,_,_,_,b],Plateau).

winPos(a, Couleur, Plateau):-
	%accessible(a, Couleur, Plateau, L_accessible),
	member([1,_,_,_,a],Plateau).

nextPlayer(a, b).
nextPlayer(b, a).


% minimax(Pos, BestNextPos, Val)
% Pos is a position, Val is its minimax value.
% Best move from Pos leads to position BestNextPos.
minimax(Pos, BestNextPos, Val, Iteration) :-                     % Pos has successors
	%write(Pos),nl,
	Iteration2 is Iteration +1, 
    bagof(NextPos, move(Pos, NextPos, Iteration2), NextPosList),
	%write(Iteation2),nl,
    best(NextPosList, BestNextPos, Val, Iteration2), !.

minimax(Pos, _, Val, _) :-                     % Pos has no successors
    utility(Pos, Val).
	%write(Val),nl.


best([Pos], Pos, Val, Iteration) :-
    minimax(Pos, _, Val, Iteration).
	%write('test1'),nl, !.

best([Pos1 | PosList], BestPos, BestVal, Iteration) :-
    minimax(Pos1, _, Val1, Iteration),
	%write(Val1),nl,
    best(PosList, Pos2, Val2, Iteration),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).



betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    min_to_move(Pos0),                         % MIN to move in Pos0
    Val0 > Val1, !                             % MAX prefers the greater value
    ;
    max_to_move(Pos0),                         % MAX to move in Pos0
    Val0 < Val1, !.                            % MIN prefers the lesser value

betterOf(_, _, Pos1, Val1, Pos1, Val1).        % Otherwise Pos1 better than Pos0

% move(+Pos, -NextPos)
% True if there is a legal (according to rules) move from Pos to NextPos.
move([X1, T1, play, Board], [X2, T2, win, NextBoard], _) :-
    nextPlayer(X1, X2),
    move_aux(X1, T1, Board, T2, NextBoard),
    winPos(X1, T1, NextBoard), !.

move([X1, T1, play, Board], [X2, T2, cut, NextBoard], NT2) :-
	n_tour(NT),
	NT2 is NT + 3,
    nextPlayer(X1, X2),
    move_aux(X1, T1, Board, T2, NextBoard), !.

move([X1, T1, play, Board], [X2, T2, play, NextBoard], _) :-
    nextPlayer(X1, X2), 
    move_aux(X1, T1, Board, T2, NextBoard).
	%write(T2),nl.

% move_aux(+Player, +Board, -NextBoard)
% True if NextBoard is Board whith an empty case replaced by Player mark.
move_aux(X1, T1, Plateau, T2, Plateau):-
	member([_,_, T2, T1, X1], Plateau),
	accessible(X1, T1, Plateau, L_accessible),
	L_accessible = [].
	%write(T2),nl.


move_aux(X1, T1, Plateau, T2, NPlateau):-
	member([Ligne, Colonne, _C, T1, X1], Plateau),
	accessible(X1, T1, Plateau, L_accessible),
	member([NLigne,NColonne,T2],L_accessible),
	modifier_plateau(Plateau, Ligne, Colonne, NLigne, NColonne, T1, X1, NPlateau).


% min_to_move(+Pos)
% True if the next player to play is the MIN player.
min_to_move([a, _, _, _]).

% max_to_move(+Pos)
% True if the next player to play is the MAX player.
max_to_move([b, _, _, _]).

% utility(+Pos, -Val) :-
% True if Val the the result of the evaluation function at Pos.
% We will only evaluate for final position.
% So we will only have MAX win, MIN win or draw.
% We will use  1 when MAX win
%             -1 when MIN win
%              0 otherwise.
utility([b, _, win, _], -100).       % Previous player (MAX) has win.
utility([a, _, win, _], 100).      % Previous player (MIN) has win.
utility([a, _, cut, Plateau], Eval):-
	eval(a, [brown,green,red,yellow,pink,purple,blue,orange],Plateau,Eval).
	%write(Eval),nl.

utility([b, _, cut, Plateau], Eval):-
	eval(b, [brown,green,red,yellow,pink,purple,blue,orange],Plateau,Eval).
	%write(Eval),nl.


eval(_,[],_,0).	

eval(a,[H|T],Plateau,Eval_t1):-
	accessible(a, H, Plateau, L_accessible_a),
	findall(X ,member([1,X,_], L_accessible_a), L_eval_a),
	length(L_eval_a, Eval_t2_a)==0,
	accessible(b, H, Plateau, L_accessible_b),
	findall(Y ,member([8,Y,_], L_accessible_b), L_eval_b),
	length(L_eval_b, Eval_t2_b)==0,
	eval(a,T,Plateau,Eval_t1).

eval(a,[H|T],Plateau,Eval_t1):-
	accessible(a, H, Plateau, L_accessible_a),
	findall(X ,member([1,X,_], L_accessible_a), L_eval_a),
	length(L_eval_a, Eval_t2_a)\==0,
	accessible(b, H, Plateau, L_accessible_b),
	findall(Y ,member([8,Y,_], L_accessible_b), L_eval_b),
	length(L_eval_b, Eval_t2_b)\==0,
	eval(a,T,Plateau,Eval_t1).

eval(a,[H|T],Plateau,Eval):-
	accessible(a, H, Plateau, L_accessible_a),
	findall(X ,member([1,X,_], L_accessible_a), L_eval_a),
	length(L_eval_a, Eval_t2_a)\==0,
	accessible(b, H, Plateau, L_accessible_b),
	findall(Y ,member([8,Y,_], L_accessible_b), L_eval_b),
	length(L_eval_b, Eval_t2_b)==0,
	eval(a,T,Plateau,Eval_t1),
	Eval is Eval_t1 + 1.

eval(a,[H|T],Plateau,Eval):-
	accessible(a, H, Plateau, L_accessible_a),
	findall(X ,member([1,X,_], L_accessible_a), L_eval_a),
	length(L_eval_a, Eval_t2_a)==0,
	accessible(b, H, Plateau, L_accessible_b),
	findall(Y ,member([8,Y,_], L_accessible_b), L_eval_b),
	length(L_eval_b, Eval_t2_b)\==0,
	eval(a,T,Plateau,Eval_t1),
	Eval is Eval_t1 - 1.

eval(b,[H|T],Plateau,Eval_t1):-
	accessible(a, H, Plateau, L_accessible_a),
	findall(X ,member([1,X,_], L_accessible_a), L_eval_a),
	length(L_eval_a, Eval_t2_a)==0,
	accessible(b, H, Plateau, L_accessible_b),
	findall(Y ,member([8,Y,_], L_accessible_b), L_eval_b),
	length(L_eval_b, Eval_t2_b)==0,
	eval(b,T,Plateau,Eval_t1).

eval(b,[H|T],Plateau,Eval_t1):-
	accessible(a, H, Plateau, L_accessible_a),
	findall(X ,member([1,X,_], L_accessible_a), L_eval_a),
	length(L_eval_a, Eval_t2_a)\==0,
	accessible(b, H, Plateau, L_accessible_b),
	findall(Y ,member([8,Y,_], L_accessible_b), L_eval_b),
	length(L_eval_b, Eval_t2_b)\==0,
	eval(b,T,Plateau,Eval_t1).

eval(b,[H|T],Plateau,Eval):-
	accessible(a, H, Plateau, L_accessible_a),
	findall(X ,member([1,X,_], L_accessible_a), L_eval_a),
	length(L_eval_a, Eval_t2_a)\==0,
	accessible(b, H, Plateau, L_accessible_b),
	findall(Y ,member([8,Y,_], L_accessible_b), L_eval_b),
	length(L_eval_b, Eval_t2_b)==0,
	eval(b,T,Plateau,Eval_t1),
	Eval is Eval_t1 - 1.

eval(b,[H|T],Plateau,Eval):-
	accessible(a, H, Plateau, L_accessible_a),
	findall(X ,member([1,X,_], L_accessible_a), L_eval_a),
	length(L_eval_a, Eval_t2_a)==0,
	accessible(b, H, Plateau, L_accessible_b),
	findall(Y ,member([8,Y,_], L_accessible_b), L_eval_b),
	length(L_eval_b, Eval_t2_b)\==0,
	eval(b,T,Plateau,Eval_t1),
	Eval is Eval_t1 + 1.

