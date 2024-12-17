% Menu display and handling
:- dynamic player_mode/1.
:- dynamic board_size/1.

% Main menu
display_main_menu :-
    clear_console,
    write('=================================\n'),
    write('           COLLAPSE              \n'),
    write('=================================\n'),
    write('1. Player vs Player\n'),
    write('2. Player vs Computer\n'),
    write('3. Computer vs Computer\n'),
    write('4. How to Play\n'),
    write('5. Exit\n'),
    write('=================================\n'),
    write('Choose an option (1-5): ').

% Menu handler
menu :-
    display_main_menu,
    read(Option),
    handle_menu_option(Option).

% Menu options handling
handle_menu_option(1) :-
    retractall(player_mode(_)),
    asserta(player_mode(pvp)),
    select_board_size,
    initial_board(Board),
    display_game(Board),
    play_game(Board).
handle_menu_option(2) :-
    retractall(player_mode(_)),
    asserta(player_mode(pvc)),
    select_board_size,
    initial_board(Board),
    display_game(Board),
    play_game(Board).
handle_menu_option(3) :-
    retractall(player_mode(_)),
    asserta(player_mode(cvc)),
    select_board_size,
    initial_board(Board),
    display_game(Board),
    play_game(Board).
handle_menu_option(4) :-
    display_rules,
    menu.
handle_menu_option(5) :-
    write('Thank you for playing!\n'),
    halt.
handle_menu_option(_) :-
    write('Invalid option! Please try again.\n'),
    sleep(2),
    menu.

% Board size selection
select_board_size :-
    clear_console,
    write('Select board size:\n'),
    write('1. 6x6\n'),
    write('2. 8x8\n'),
    write('3. 10x10\n'),
    read(Size),
    handle_size_selection(Size).

handle_size_selection(1) :-
    asserta(board_size(6)),
    start_game.
handle_size_selection(2) :-
    asserta(board_size(8)),
    start_game.
handle_size_selection(3) :-
    asserta(board_size(10)),
    start_game.
handle_size_selection(_) :-
    write('Invalid size! Please try again.\n'),
    sleep(2),
    select_board_size.

% Clear console (works for both Unix and Windows)
clear_console :-
    write('\e[H\e[2J').

% Display game rules
display_rules :-
    clear_console,
    write('=================================\n'),
    write('           HOW TO PLAY           \n'),
    write('=================================\n'),
    write('1. Players take turns selecting cells\n'),
    write('2. Selected cells collapse and merge with adjacent same-colored cells\n'),
    write('3. Cells above fall down to fill empty spaces\n'),
    write('4. Game ends when no more moves are possible\n'),
    write('5. Player with the highest score wins!\n'),
    write('\nPress Enter to return to main menu\n'),
    read(_).

% Placeholder for game start
start_game :-
    % Add your game initialization here
    write('Starting game...\n').