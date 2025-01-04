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
    bot_easy_move(Board, NewBoard),
    display_game(NewBoard),
    next_player(Player, NextPlayer),
    play_game(NewBoard, NextPlayer).

play_game(Board, Player) :-
    format('~w\'s turn.~n', [Player]),
    get_valid_move(Board, Player, From, To, Final),
    make_move(Board, From, To, Final, NewBoard),
    display_game(NewBoard),
    next_player(Player, NextPlayer),
    play_game(NewBoard, NextPlayer).

% Bot easy move
bot_easy_move(Board, NewBoard) :-
    get_all_valid_moves(Board, b, Moves),
    random_member((From, To, Final), Moves),
    make_move(Board, From, To, Final, NewBoard).

random_member(X, List) :-
    length(List, Len),
    random(0, Len, Index),
    nth0(Index, List, X).

% Get valid move
get_valid_move(Board, Player, (FromRow, FromCol), (ToRow, ToCol), Final) :-
    repeat,
    write('Enter source position (row col): '),
    read((AlmostRow, FromCol)),
    translate(AlmostRow, FromRow),
    (valid_position(FromRow, FromCol) -> 
        write('Enter destination position (row col): '),
        read((Almost, ToCol)),
        translate(Almost, ToRow),
        (valid_move(Board, Player, (FromRow, FromCol), (ToRow, ToCol), Final) -> 
            true
        ;   
            write('Invalid move, try again.'), nl,
            fail
        )
    ;   
        write('Invalid position, try again.'), nl,
        fail
    ),
    other_player(Player, OtherPlayer).

% Valid move check
valid_move(Board, Player, From, To ,Final) :-
        piece_at(Board, From, Player),
        other_player(Player, OtherPlayer),
        piece_at(Board,To,OtherPlayer),
        calculate_dir(From, To, Dir),
        direction_to_delta(Board, From, Dir, To, Final).
        

calculate_dir((FromRow, FromCol), (ToRow, ToCol), Dir) :-
        ToRow < FromRow, ToCol is FromCol -> Dir = u.
calculate_dir((FromRow, FromCol), (ToRow, ToCol), Dir) :-
        ToRow > FromRow, ToCol is FromCol -> Dir = d.
calculate_dir((FromRow, FromCol), (ToRow, ToCol), Dir) :-
        ToRow is FromRow, ToCol < FromCol -> Dir = l.
calculate_dir((FromRow, FromCol), (ToRow, ToCol), Dir) :-
        ToRow is FromRow, ToCol > FromCol -> Dir = r.
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

    
direction_to_target(Board, From, Dir, To) :-
    direction_to_delta(Dir, From, InitialTarget),
    other_player(Player, OpponentColor),
    find_collision(Board, From, InitialTarget, To, OpponentColor).
% Find collision point
find_collision(Board, (Row, Col), (DRow, DCol), (FinalRow, FinalCol), OpponentColor) :-
    NextRow is Row + DRow,
    NextCol is Col + DCol,
    valid_position(NextRow, NextCol),
    (piece_at(Board, (NextRow, NextCol), OpponentColor) ->
        FinalRow = NextRow,
        FinalCol = NextCol;   
        piece_at(Board, (NextRow, NextCol), e),
        find_collision(Board, (NextRow, NextCol), (DRow, DCol), (FinalRow, FinalCol), OpponentColor)
    ).

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
    length(Moves, 0).

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
    play_game(NewBoard, NextPlayer).