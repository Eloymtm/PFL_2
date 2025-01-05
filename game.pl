:- use_module(library(lists)).
:- use_module(library(random)).

% Game configuration
board_size(9, 5).  % 9x5 board

% Game loop
play :-
    initial_board(Board),
    display_game(Board),
    play_game(Board, w).  % w moves first

% Initial board setup
initial_board([[b,w,b,w,b,w,b,w,b],[w,e,e,e,e,e,e,e,w],[e,e,e,e,e,e,e,e,e],[b,e,e,e,e,e,e,e,b],[w,b,w,b,w,b,w,b,w]]).
inversa([[w,b,w,b,w,b,w,b,w],[b,e,e,e,e,e,e,e,b],[e,e,e,e,e,e,e,e,e],[w,e,e,e,e,e,e,e,w],[b,w,b,w,b,w,b,w,b]]).
final_board([[w,e,e,e,e,e,e,e,e],[e,e,e,w,e,e,e,e,e],[e,e,e,e,e,e,e,e,e],[b,e,e,e,e,e,e,e,e],[e,e,e,e,e,e,e,e,e]]).

% Display the game board
display_game(Board) :-
    nl,
    write('   1   2   3   4   5   6   7   8   9'), nl,
    display_board(Board, 5),
    nl.


% Display board helper
display_board([], _).
display_board([Row|Rest], RowNum) :-
    format('~w ', [RowNum]),
    display_row(Row),  % Added closing border
    nl,
    write(' '), nl,
    NextRow is RowNum - 1,
    display_board(Rest, NextRow).


display_row([]).
display_row([Cell|Rest]) :-
    display_cell(Cell),
    write(' '),
    display_row(Rest).

display_cell(b) :- write('\e[48;5;0m B \e[0m').  % b piece with b background
display_cell(w) :- write('\e[48;5;255m W \e[0m').  % w piece with w background
display_cell(e) :- write('\e[48;5;244m   \e[0m').


translate(1, 5).
translate(2, 4).
translate(3, 3).
translate(4, 2).
translate(5, 1).


% Play game with difficulty
play_game(Board, Player) :-
    player_mode(pvc),
    difficulty(easy),
    Player = b,
    !,
    bot_move(Board,Player,easy, NewBoard),
    display_game(NewBoard),
    next_player(Player, NextPlayer),
    play_game(NewBoard, NextPlayer).

play_game(Board, Player) :-
    player_mode(pvc),
    difficulty(medium),
    Player = b,
    !,
    bot_move(Board,Player,medium, NewBoard),
    display_game(NewBoard),
    next_player(Player, NextPlayer),
    game_over(NewBoard,NextPlayer),
    play_game(NewBoard, NextPlayer).

play_game(Board, Player) :-
    player_mode(cvp),
    difficulty(easy),
    Player = w,
    !,
    bot_move(Board,Player,easy, NewBoard),
    display_game(NewBoard),
    next_player(Player, NextPlayer),
    game_over(NewBoard,NextPlayer),
    play_game(NewBoard, NextPlayer).

play_game(Board, Player) :-
    player_mode(cvp),
    difficulty(medium),
    Player = w,
    !,
    bot_move(Board,Player,medium, NewBoard),
    display_game(NewBoard),
    next_player(Player, NextPlayer),
    game_over(NewBoard,NextPlayer),
    play_game(NewBoard, NextPlayer).

play_game(Board, Player) :-
    player_mode(cvc),
    difficulty1(easy),
    difficulty2(easy),
    play_turn_cvc(Board, Player, easy, easy).

play_game(Board, Player) :-
    player_mode(cvc),
    difficulty1(easy),
    difficulty2(medium),
    play_turn_cvc(Board, Player, easy, medium).

play_game(Board, Player) :-
    player_mode(cvc),
    difficulty1(medium),
    difficulty2(easy),
    play_turn_cvc(Board, Player, medium, easy).

play_game(Board, Player) :-
    player_mode(cvc),
    difficulty1(medium),
    difficulty2(medium),
    play_turn_cvc(Board, Player, medium, medium).

play_turn_cvc(Board, Bot, Difficulty1, Difficulty2) :-
    bot_move(Board, Bot, Difficulty1, NewBoard),
    display_game(NewBoard),
    next_player(Bot, Bot2),
    game_over(NewBoard, Bot2),
    play_turn_cvc(NewBoard, Bot2, Difficulty2, Difficulty1).

% Bot move based on difficulty
bot_move(Board, Player, easy, NewBoard) :-
    bot_easy_move(Board,Player, NewBoard).

bot_move(Board, Player, medium, NewBoard) :-
    get_all_valid_moves(Board, Player, Moves),
    find_best_medium_move(Board, Player, Moves, BestMove),
    BestMove = (From, To, Final),
    make_move(Board, From, To, Final, NewBoard).

find_best_medium_move(Board, Player, Moves, BestMove) :-
    findall(
        (Score, Move),
        (member(Move, Moves),
         value(Board, Move, Score)),
        ScoredMoves
    ),
    sort(ScoredMoves, SortedMoves),
    reverse(SortedMoves, [(_, BestMove)|_]).

% Bot easy move
bot_easy_move(Board, Player,NewBoard) :-
    get_all_valid_moves(Board, Player, Moves),
    random_member((From, To, Final), Moves),
    make_move(Board, From, To, Final, NewBoard).

bot_medium_move(Board, NewBoard) :-
    get_all_valid_moves(Board, b, Moves),
    find_best_move(Board, Moves, (0, [], []), BestMove),
    make_move(Board, BestMove, NewBoard).

value(Board, (From, To, Final), Score) :-
    Final = (FinalRow, FinalCol),
    make_move(Board, From, To, (FinalRow, FinalCol), NewBoard),
    next_player(Player, NextPlayer),
    get_all_valid_moves(Board, Player, Moves),
    find_best_moves(Board, Player, Moves, BestMoves),
    best_move((From, To , Final), BestMoves, Score).


best_move((From, To, Final), BestMoves,2):-
    member((From, To, Final), BestMoves).
best_move((From, To, Final), BestMoves,1).

count(Moves, BestMoves):-
    length(Moves, LenMoves),
    length(BestMoves, LenBestMoves),
    write('Number of moves: '), write(LenMoves), nl,
    write('Number of best moves: '), write(LenBestMoves), nl.


find_best_moves(Board, Player, Moves, BestMoves) :-
        findall(
            Move,
            (member(Move, Moves),
            Move = (From, To, Final),
            make_move(Board, From, To, Final, NewBoard),
            next_player(Player, NextPlayer),
            get_all_valid_moves(NewBoard, NextPlayer, OpponentMoves),
            \+ can_be_captured(OpponentMoves, Final)),
            BestMoves).

can_be_captured([], _) :- false.
can_be_captured([(_, Target, _)|Rest], Position) :-
    can_be_captured_condition(Rest, Target, Position).
can_be_captured_condition(Rest,Position, Position).
can_be_captured_condition(Rest,Target, Position):-
    can_be_captured(Rest,Position).

random_member(X, List) :-
    length(List, Len),
    random(0, Len, Index),
    nth0(Index, List, X).

% Get valid move
get_valid_move(Board, Player, (FromRow, FromCol), (ToRow, ToCol), Final):-
    repeat,
    write('Enter source position (row col): '),
    read((AlmostRow, FromCol)),
    translate(AlmostRow, FromRow),
    check_source_position(Board, Player, FromRow, FromCol),
    !,
    write('Enter destination position (row col): '),
    read((Almost, ToCol)),
    translate(Almost, ToRow),
    check_destination_move(Board, Player, (FromRow, FromCol), (ToRow, ToCol), Final),
    !,
    other_player(Player, OtherPlayer).

check_source_position(_, _, Row, Col) :-
    valid_position(Row, Col),
    !.
check_source_position(_, _, _, _):-
    write('Invalid position, try again.'),
    nl,
    fail.

check_destination_move(Board, Player, From, To, Final):-
    valid_move(Board, Player, From, To, Final),
    !.
check_destination_move(_, _, _, _, _):-
    write('Invalid move, try again.'),
    nl,
    fail.

% Valid move check
valid_move(Board, Player, From, To ,Final) :-
        piece_at(Board, From, Player),
        other_player(Player, OtherPlayer),
        piece_at(Board,To,OtherPlayer),
        calculate_dir(From, To, Dir),
        direction_to_delta(Board, From, Dir, To, Final).
        

calculate_dir((FromRow, FromCol), (ToRow, ToCol), Dir) :-
        ToRow < FromRow, ToCol is FromCol, Dir = u.
calculate_dir((FromRow, FromCol), (ToRow, ToCol), Dir) :-
        ToRow > FromRow, ToCol is FromCol, Dir = d.
calculate_dir((FromRow, FromCol), (ToRow, ToCol), Dir) :-
        ToRow is FromRow, ToCol < FromCol, Dir = l.
calculate_dir((FromRow, FromCol), (ToRow, ToCol), Dir) :-
        ToRow is FromRow, ToCol > FromCol, Dir = r.
calculate_dir((FromRow, FromCol), (ToRow, ToCol), Dir) :-   
   NewRow is ToRow,
   NewCol is ToCol,
   check_odd(FromRow, FromCol,0),
   check_diagonal((NewRow, NewCol), (FromRow, FromCol), Dir).

check_diagonal((FromRow, FromCol), (FromRow, FromCol), Dir).
check_diagonal((NewRow, NewCol), (FromRow, FromCol), ul) :-
  NextRow is NewRow + 1,
  NextCol is NewCol + 1,
  valid_position(NextRow,NextCol),
  check_diagonal((NextRow, NextCol), (FromRow, FromCol), ul).
check_diagonal((NewRow, NewCol), (FromRow, FromCol), ur) :-
  NextRow is NewRow + 1,
  NextCol is NewCol - 1,   
  valid_position(NextRow,NextCol),
  check_diagonal((NextRow, NextCol), (FromRow, FromCol), ur).

check_diagonal((NewRow, NewCol), (FromRow, FromCol), dl) :-
  NextRow is NewRow - 1,
  NextCol is NewCol + 1,
  valid_position(NextRow,NextCol),
  check_diagonal((NextRow, NextCol), (FromRow, FromCol), dl).

check_diagonal((NewRow, NewCol), (FromRow, FromCol), dr) :-
  NextRow is NewRow - 1,
  NextCol is NewCol - 1,
  valid_position(NextRow,NextCol),
  check_diagonal((NextRow, NextCol), (FromRow, FromCol), dr).

check_odd(Row, Col, Odd):-
    N is Row + Col,
    Odd is N mod 2.

direction_to_delta(Board, (FromRow,FromCol), r,(ToRow, ToCol), Final) :-
    NewCol is ToCol - 1,
    valid_position(ToRow, NewCol),
    piece_at(Board, (ToRow, NewCol), OtherPiece),
    OtherPiece = e,
    Final = (ToRow, NewCol).
direction_to_delta(Board, (FromRow,FromCol), l,(ToRow, ToCol), Final) :-
    NewCol is ToCol + 1,
    valid_position(ToRow, NewCol),
    piece_at(Board, (ToRow, NewCol), OtherPiece),
    OtherPiece = e,
    Final = (ToRow, NewCol).
direction_to_delta(Board, (FromRow,FromCol), u,(ToRow, ToCol), Final) :-
    NewRow is ToRow + 1,
    valid_position(NewRow, ToCol),
    piece_at(Board, (NewRow, ToCol), OtherPiece),
    OtherPiece = e,
    Final = (NewRow, ToCol).
direction_to_delta(Board, (FromRow,FromCol), d,(ToRow, ToCol), Final) :-
    NewRow is ToRow - 1,
    valid_position(NewRow, ToCol),
    piece_at(Board, (NewRow, ToCol), OtherPiece),
    OtherPiece = e,
    Final = (NewRow, ToCol).
direction_to_delta(Board, (FromRow,FromCol), ul,(ToRow, ToCol), Final) :-
    NewRow is ToRow + 1,
    NewCol is ToCol + 1,
    valid_position(NewRow, NewCol),
    piece_at(Board, (NewRow, NewCol), OtherPiece),
    OtherPiece = e,
    Final = (NewRow, NewCol).

direction_to_delta(Board, (FromRow,FromCol), ur,(ToRow, ToCol), Final) :-
    NewRow is ToRow + 1,
    NewCol is ToCol - 1,
    valid_position(NewRow, NewCol),
    piece_at(Board, (NewRow, NewCol), OtherPiece),
    OtherPiece = e,
    Final = (NewRow, NewCol).

direction_to_delta(Board, (FromRow,FromCol), dl,(ToRow, ToCol), Final) :-
    NewRow is ToRow - 1,
    NewCol is ToCol + 1,
    valid_position(NewRow, NewCol),
    piece_at(Board, (NewRow, NewCol), OtherPiece),
    OtherPiece = e,
    Final = (NewRow, NewCol).

direction_to_delta(Board, (FromRow,FromCol), dr,(ToRow, ToCol), Final) :-
    NewRow is ToRow - 1,
    NewCol is ToCol - 1,
    valid_position(NewRow, NewCol),
    piece_at(Board, (NewRow, NewCol), OtherPiece),
    OtherPiece = e,
    Final = (NewRow, NewCol).



% Make a move
make_move(Board, (FromRow, FromCol),(ToRow, ToCol),(FinalRow,FinalCol), NewBoard) :-
    piece_at(Board, (FromRow, FromCol), Piece),
    set_cell(Board, (FromRow, FromCol), e, TempBoard),
    set_cell(TempBoard, (ToRow, ToCol), e, TempBoard_),
    set_cell(TempBoard_, (FinalRow, FinalCol), Piece, NewBoard).
    

% Helper predicates
piece_at(Board, (Row, Col), Piece) :-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Piece).

set_cell(Board, (Row, Col), Value, NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_nth(Col, OldRow, Value, NewRow),
    replace_nth(Row, Board, NewRow, NewBoard).

replace_nth(1, [_|T], X, [X|T]) :- !.
replace_nth(N, [H|T], X, [H|R]) :-
    N > 1,
    N1 is N - 1,
    replace_nth(N1, T, X, R).

valid_position(Row, Col) :-
    board_size(MaxCol, MaxRow),
    Row > 0, Row =< MaxRow,
    Col > 0, Col =< MaxCol.

next_player(w, b).
next_player(b, w).

other_player(w, b).
other_player(b, w).

get_all_valid_moves(Board, Player, Moves) :-
    findall(
        (From, To, Final),
        get_single_valid_move(Board, Player, From, To, Final),
        Moves
    ).

% Find a single valid move
get_single_valid_move(Board, Player, From, To, Final) :-
    get_player_position(From),
    piece_at(Board, From, Player),
    get_target_position(To),
    validate_move(Board, Player, From, To, Final).

% Generate all possible board positions
get_player_position((Row, Col)) :-
    member(Row, [1,2,3,4,5]),
    member(Col, [1,2,3,4,5,6,7,8,9]).

% Generate all possible target positions
get_target_position((Row, Col)) :-
    member(Row, [1,2,3,4,5]),
    member(Col, [1,2,3,4,5,6,7,8,9]).

% Validate a single move
validate_move(Board, Player, From, To, Final) :-
    other_player(Player, OtherPlayer),
    piece_at(Board, To, OtherPlayer),
    calculate_dir(From, To, Dir),
    direction_to_delta(Board, From, Dir, To, Final).

% Improved game over check using get_all_valid_moves
game_over(Board, Player) :-
    get_all_valid_moves(Board, Player, Moves),
    length(Moves, Number),
    condition(Player,Number).
    

condition(Player, 0):-
    announce_winner(Player),
    halt.
condition(Player,_).

% Helper predicate to display all valid moves
display_valid_moves(Board, Player) :-
    get_all_valid_moves(Board, Player, Moves),
    format('Valid moves for player ~w:~n', [Player]),
    check_and_display_moves(Moves).

% Check if moves exist and display them
check_and_display_moves([]) :-
    write('No valid moves available - Game Over!'),
    nl.
check_and_display_moves(Moves) :-
    display_moves_list(Moves).

% Display list of moves recursively
display_moves_list([]).
display_moves_list([(From, To, Final)|Rest]) :-
    format('From: ~w, Captures: ~w, Lands: ~w~n', [From, To, Final]),
    display_moves_list(Rest).

announce_winner(Player) :-
    other_player(Player, Winner),
    format('Game Over! ~w wins!~n', [Winner]).

play_game(Board, Player) :-
    format('~w\'s turn.~n', [Player]),
    get_valid_move(Board, Player, From, To, Final),
    make_move(Board, From, To, Final, NewBoard),
    display_game(NewBoard),
    next_player(Player, NextPlayer),
    game_over(NewBoard,NextPlayer),
    play_game(NewBoard, NextPlayer).