:-module(ia_mathieu4,[jouer_mat4/0]).

:- use_module(library(random)).

% if the computer begins the play
% we choose randomly the color of the first move
% Compute the best move for computer with minimax
jouer_mat4:-
    joueur(L_joueur),
    member(b,L_joueur),
    couleur([Color,Color2|Tail]),
    plateau(Board),
    random_member(X, [Color,Color2|Tail]),
    retract(n_round(_)),
    assert(n_round(1)),
    bestMove([b, X, play, Board], [_NextPlayer, NColor, State, NextBoard]),
    n_round(NR),
    NR2 is NR+1,
    retract(n_round(_)),
    assert(n_round(NR2)),
    retract(joueur(_)),
    assert(joueur([a])),
    retract(couleur([Color,Color2|Tail])),
    assert(couleur([NColor])),
    retract(plateau(Board)),
    assert(plateau(NextBoard)),
    redessiner,
    fin(State),!.

% Compute the best move for computer with minimax
jouer_mat4:-
    joueur(L_joueur),
    member(b,L_joueur),
    couleur([Color]),
    plateau(Board),
    bestMove([b, Color, play, Board], [_NextPlayer, NColor, State, NextBoard]),
    n_round(NR),
    NR2 is NR+1,
    retract(n_round(_)),
    assert(n_round(NR2)),
    retract(joueur(_)),
    assert(joueur([a])),
    retract(couleur([Color])),
    assert(couleur([NColor])),
    retract(plateau(Board)),
    assert(plateau(NextBoard)),
    redessiner,
    fin(State),!.


fin(State):-
    State \= win,!.

% If Computer win
fin(State):-
    State = win,
    retract(joueur(_)),
    retract(couleur(_)),
    send(@p, display,new(Text, text('YOU LOOSE')), point(660, 140)),
    send(Text, font, font(times, bold, 40)).

% bestMove(+Pos, -NextPos)
% Compute the best Next Position from Position Pos
% with minimax algorithm.
bestMove(Pos, NextPos) :-
    minimax(Pos, NextPos, _, 0).

% winPos(+Player, +Board)
% True if Player win in Board.
winPos(b, _C, Board):-
    member([8,_,_,_,b],Board).

winPos(a, _C, Board):-
    member([1,_,_,_,a],Board).

% nextPlayer(X1, X2)
% True if X2 is the next player to play after X1.
nextPlayer(a, b).
nextPlayer(b, a).

% minimax(Pos, BestNextPos, Val)
% Pos is a position, Val is its minimax value.
% Best move from Pos leads to position BestNextPos.
minimax(Pos, BestNextPos, Val, Iteration) :-                     % Pos has successors
    Iteration2 is Iteration +1, 
    bagof(NextPos, move(Pos, NextPos, Iteration2), NextPosList),
    best(NextPosList, BestNextPos, Val, Iteration2), !.

minimax(Pos, _, Val, _) :-                     % Pos has no successors
    utility(Pos, Val).

best([Pos], Pos, Val, Iteration) :-
    minimax(Pos, _, Val, Iteration).

best([Pos1 | PosList], BestPos, BestVal, Iteration) :-
    minimax(Pos1, _, Val1, Iteration),
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
% The NextPos with cut allows to reduce the computation time by limiting the recursion to number of round + 3
move([X1, C1, play, Board], [X2, C2, win, NextBoard], _) :-
    nextPlayer(X1, X2),
    move_aux(X1, C1, Board, C2, NextBoard),
    winPos(X1, C1, NextBoard), !.

move([X1, C1, play, Board], [X2, C2, cut, NextBoard], NR2) :-
    n_round(NR),
    NR2 is NR + 3,
    nextPlayer(X1, X2),
    move_aux(X1, C1, Board, C2, NextBoard), !.

move([X1, C1, play, Board], [X2, C2, play, NextBoard], _) :-
    nextPlayer(X1, X2), 
    move_aux(X1, C1, Board, C2, NextBoard).

% move_aux(+Player, +ColorPlayer, +Board, -ColorNextPlayer, -NextBoard)
% True if NextBoard is Board with an empty case in the available list replaced by Player mark.
move_aux(X1, C1, Board, C2, Board):-
    member([_,_, C2, C1, X1], Board),
    accessible(X1, C1, Board, L_available),
    L_available = [].

move_aux(X1, C1, Board, C2, NextBoard):-
    member([Ligne, Colonne, _C, C1, X1], Board),
    accessible(X1, C1, Board, L_available),
    member([NLigne,NColonne,C2],L_available),
    modifier_plateau(Board, Ligne, Colonne, NLigne, NColonne, C1, X1, NextBoard).


% min_to_move(+Pos)
% True if the next player to play is the MIN player.
min_to_move([a, _, _, _]).

% max_to_move(+Pos)
% True if the next player to play is the MAX player.
max_to_move([b, _, _, _]).

% utility(+Pos, -Val) :-
% True if Val the the result of the evaluation function at Pos.
% We will evaluate for final position and cut position.
% We will use  100 when MAX win
%             -100 when MIN win
% and the number of winning position in the available list for every tower of the previous player when MIN cut
% its negation when MAX cut
utility([b, _, win, _], -100).       % Previous player (MAX) has win.
utility([a, _, win, _], 100).      % Previous player (MIN) has win.

utility([a, _, cut, Board], Eval):-
    eval(a, [brown,green,red,yellow,pink,purple,blue,orange],Board,Eval).

utility([b, _, cut, Board], Eval):-
    eval(b, [brown,green,red,yellow,pink,purple,blue,orange],Board,Eval).


eval(_,[],_,0).

eval(a,[H|T],Plateau,Eval_t1):-
	accessible(a, H, Plateau, L_accessible_a),
	findall(X ,member([1,X,_], L_accessible_a), L_eval_a),
	length(L_eval_a,Eval_a),
	Eval_a==0,
	accessible(b, H, Plateau, L_accessible_b),
	findall(Y ,member([8,Y,_], L_accessible_b), L_eval_b),
	length(L_eval_b,Eval_b),
	Eval_b==0,
	eval(a,T,Plateau,Eval_t1).

eval(a,[H|T],Plateau,Eval):-
	accessible(a, H, Plateau, L_accessible_a),
	findall(X ,member([1,X,_], L_accessible_a), L_eval_a),
	length(L_eval_a,Eval_a),
	Eval_a\==0,
	accessible(b, H, Plateau, L_accessible_b),
	findall(Y ,member([8,Y,_], L_accessible_b), L_eval_b),
	length(L_eval_b,Eval_b),
	Eval_b\==0,
	eval(a,T,Plateau,Eval_t1),
	Eval_ is Eval_t1 + Eval_a,
	Eval is Eval_ - Eval_b.

eval(a,[H|T],Plateau,Eval):-
	accessible(a, H, Plateau, L_accessible_a),
	findall(X ,member([1,X,_], L_accessible_a), L_eval_a),
	length(L_eval_a,Eval_a),
	Eval_a\==0,
	accessible(b, H, Plateau, L_accessible_b),
	findall(Y ,member([8,Y,_], L_accessible_b), L_eval_b),
	length(L_eval_b,Eval_b),
	Eval_b==0,
	eval(a,T,Plateau,Eval_t1),
	Eval_ is Eval_t1 + 20,
	Eval is Eval_ + Eval_a.

eval(a,[H|T],Plateau,Eval):-
	accessible(a, H, Plateau, L_accessible_a),
	findall(X ,member([1,X,_], L_accessible_a), L_eval_a),
	length(L_eval_a,Eval_a),
	Eval_a==0,
	accessible(b, H, Plateau, L_accessible_b),
	findall(Y ,member([8,Y,_], L_accessible_b), L_eval_b),
	length(L_eval_b,Eval_b),
	Eval_b\==0,
	eval(a,T,Plateau,Eval_t1),
	Eval_ is Eval_t1 - 20,
	Eval is Eval_ - Eval_b.

eval(b,[H|T],Plateau,Eval_t1):-
	accessible(a, H, Plateau, L_accessible_a),
	findall(X ,member([1,X,_], L_accessible_a), L_eval_a),
	length(L_eval_a,Eval_a),
	Eval_a==0,
	accessible(b, H, Plateau, L_accessible_b),
	findall(Y ,member([8,Y,_], L_accessible_b), L_eval_b),
	length(L_eval_b,Eval_b),
	Eval_b==0,
	eval(b,T,Plateau,Eval_t1).

eval(b,[H|T],Plateau,Eval):-
	accessible(a, H, Plateau, L_accessible_a),
	findall(X ,member([1,X,_], L_accessible_a), L_eval_a),
	length(L_eval_a,Eval_a),
	Eval_a\==0,
	accessible(b, H, Plateau, L_accessible_b),
	findall(Y ,member([8,Y,_], L_accessible_b), L_eval_b),
	length(L_eval_b,Eval_b),
	Eval_b\==0,
	eval(b,T,Plateau,Eval_t1),
	Eval_ is Eval_t1 - Eval_a,
	Eval is Eval_ + Eval_b.

eval(b,[H|T],Plateau,Eval):-
	accessible(a, H, Plateau, L_accessible_a),
	findall(X ,member([1,X,_], L_accessible_a), L_eval_a),
	length(L_eval_a,Eval_a),
	Eval_a\==0,
	accessible(b, H, Plateau, L_accessible_b),
	findall(Y ,member([8,Y,_], L_accessible_b), L_eval_b),
	length(L_eval_b,Eval_b),
	Eval_b==0,
	eval(b,T,Plateau,Eval_t1),
	Eval_ is Eval_t1 - 20,
	Eval is Eval_ - Eval_a.

eval(b,[H|T],Plateau,Eval):-
	accessible(a, H, Plateau, L_accessible_a),
	findall(X ,member([1,X,_], L_accessible_a), L_eval_a),
	length(L_eval_a,Eval_a),
	Eval_a==0,
	accessible(b, H, Plateau, L_accessible_b),
	findall(Y ,member([8,Y,_], L_accessible_b), L_eval_b),
	length(L_eval_b,Eval_b),
	Eval_b\==0,
	eval(b,T,Plateau,Eval_t1),
	Eval_ is Eval_t1 + 20,
	Eval is Eval_ + Eval_b.
