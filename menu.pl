:- consult('game.pl').

% Menu display and handling
:- dynamic player_mode/1.

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
    initial_board(Board),
    display_game(Board),
    play_game(Board, w).
handle_menu_option(2) :-
    retractall(player_mode(_)),
    asserta(player_mode(pvc)),
    initial_board(Board),
    display_game(Board),
    play_game(Board, w).
handle_menu_option(3) :-
    retractall(player_mode(_)),
    asserta(player_mode(cvc)),
    initial_board(Board),
    display_game(Board),
    play_game(Board, w).
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

% Clear console
clear_console :-
    write('\e[H\e[2J').

% Display game rules
display_rules :-
    clear_console,
    write('=================================\n'),
    write('           HOW TO PLAY           \n'),
    write('=================================\n'),
    write('1. White moves first; turns alternate.\n'),
    write('2. Pieces can move in any direction (horizontal, vertical, diagonal)\n'),
    write('3. A move must result in capturing exactly one opponents piece.\n'),
    write('4. The piece cannot change direction or move where no opponents piece exists.\n'),
    wirte('5. A piece captures by colliding with an opponents piece.\n'),
    write('6. Game ends when a player cannot make a valid capture.\n'),
    write('7. A player loses if they have no valid moves or no pieces remaining.\n'),
    write('\nPress Enter to return to main menu\n'),
    read(_).
