:- use_module(library(lists)).

% Game configuration
board_size(9, 5).  % 9x5 board

% Game loop
play :-
    initial_board(Board),
    display_game(Board),
    play_game(Board, w).  % w moves first

% Initial board setup
initial_board([[b,w,b,w,b,w,b,w,b],[w,e,e,e,e,e,e,e,w],[e,e,e,e,e,e,e,e,e],[b,e,e,e,e,e,e,e,b],[w,b,w,b,w,b,w,b,w]]).

% Display the game board
display_game(Board) :-
    nl,
    write('   1   2   3   4   5   6   7   8   9'), nl,
    display_board(Board, 1),
    nl.

% Display board helper
display_board([], _).
display_board([Row|Rest], RowNum) :-
    format('~w ', [RowNum]),
    display_row(Row),  % Added closing border
    nl,
    write(' '), nl,
    NextRow is RowNum + 1,
    display_board(Rest, NextRow).


display_row([]).
display_row([Cell|Rest]) :-
    display_cell(Cell),
    write(' '),
    display_row(Rest).

display_cell(b) :- write('\e[48;5;0m B \e[0m').  % b piece with b background
display_cell(w) :- write('\e[48;5;255m W \e[0m').  % w piece with w background
display_cell(e) :- write('\e[48;5;244m   \e[0m').

% Get valid move
get_valid_move(Board, Player, (FromRow, FromCol), (ToRow, ToCol), Final) :-
    repeat,
    write('Enter source position (row col): '),
    read((FromRow, FromCol)),
    (valid_position(FromRow, FromCol) -> 
        write('Enter destination position (row col): '),
        read((ToRow, ToCol)),
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
        
        calculate_dir(From, To, Dir),
        write('Direction calculated: '), write(Dir), nl,
        direction_to_delta(Board, From, Dir, To, Final),
        write('Destination position is valid.').
        
  
calculate_dir((FromRow, FromCol), (ToRow, ToCol), Dir) :-
        ToRow < FromRow, ToCol is FromCol -> Dir = u.
calculate_dir((FromRow, FromCol), (ToRow, ToCol), Dir) :-
        ToRow > FromRow, ToCol is FromCol -> Dir = d.
calculate_dir((FromRow, FromCol), (ToRow, ToCol), Dir) :-
        ToRow is FromRow, ToCol < FromCol -> Dir = l.
calculate_dir((FromRow, FromCol), (ToRow, ToCol), Dir) :-
        ToRow is FromRow, ToCol > FromCol -> Dir = r.
calculate_dir((FromRow, FromCol), (ToRow, ToCol), Dir) :-
        ToRow < FromRow, ToCol < FromCol -> Dir = ul.
calculate_dir((FromRow, FromCol), (ToRow, ToCol), Dir) :-
        ToRow < FromRow, ToCol > FromCol -> Dir = ur.
calculate_dir((FromRow, FromCol), (ToRow, ToCol), Dir) :-
        ToRow > FromRow, ToCol < FromCol -> Dir = dl.
calculate_dir((FromRow, FromCol), (ToRow, ToCol), Dir) :-
        ToRow > FromRow, ToCol > FromCol -> Dir = dr.
        

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
    nth1(Col, BoardRow, Piece),
    write('Piece at source position is correct.'), nl.

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

% Check if game is over
game_over(Board, Player) :-
    \+ can_capture(Board, Player).

% Check if player can capture
can_capture(Board, Player) :-
    piece_at(Board, (Row, Col), Player),
    member(Dir, [u,d,l,r,ul,ur,dl,dr]),
    valid_move(Board, Player, (Row, Col), Dir, _).

% Announce winner
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
