:- use_module(library(lists)).
:- use_module(library(random)).

% Game loop
play :-
    initial_board(Board),
    display_game(Board),
    play_game(Board).

% Main game loop
play_game(Board) :-
    game_over(Board),
    !,
    announce_winner(Board).
play_game(Board) :-
    get_move(Move),
    make_move(Board, Move, NewBoard),
    display_game(NewBoard),
    play_game(NewBoard).

% Initial board setup (example 6x6)
initial_board([
    [1,2,1,2,1,2],
    [2,1,2,1,2,1],
    [1,2,1,2,1,2],
    [2,1,2,1,2,1],
    [1,2,1,2,1,2],
    [2,1,2,1,2,1]
]).

% Display the game board
display_game(Board) :-
    nl,
    display_board(Board),
    nl.

% Display board helper
display_board([]).
display_board([Row|Rest]) :-
    write('|'),
    display_row(Row),
    nl,
    display_board(Rest).

display_row([]).
display_row([Cell|Rest]) :-
    write(' '),
    write(Cell),
    write(' |'),
    display_row(Rest).

% Get player move
get_move((Row, Col)) :-
    write('Enter row: '),
    read(Row),
    write('Enter column: '),
    read(Col).

% Game over check (placeholder)
game_over(Board) :-
    % Add game over conditions here
    false.

% Make move (placeholder)
make_move(Board, (Row, Col), NewBoard) :-
    % Add move logic here
    NewBoard = Board.

% Announce winner (placeholder)
announce_winner(Board) :-
    write('Game Over!'),
    nl.
    % Generate random board of given size
    generate_board(Size, Board) :-
        length(Board, Size),
        maplist(generate_row(Size), Board).

    generate_row(Size, Row) :-
        length(Row, Size),
        maplist(random_cell, Row).

    random_cell(Cell) :-
        random_between(1, 2, Cell).

    % Initialize board with given size
    initial_board(Board) :-
        board_size(Size),
        generate_board(Size, Board).