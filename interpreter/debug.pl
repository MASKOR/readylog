/* ***************************************************************************
 *                                            ####   ####           .-""-.   
 *       # #                             #   #    # #    #         /[] _ _\  
 *       # #                                 #    # #             _|_o_LII|_ 
 * ,###, # #  ### ## ## ##   ###  ## ##  #   #    # #       ###  / | ==== | \
 * #   # # # #   # ## ## #  #   #  ## #  #   ###### #      #     |_| ==== |_|
 * #   # # # ####  #  #  #  #   #  #  #  #   #    # #      ####   ||" ||  || 
 * #   # # # #     #  #  #  #   #  #  #  #   #    # #    #    #   ||'----'|| 
 * '###'# # # #### #  #  ##  ### # #  ## ## #      # ####  ###   /__|    |__\
 * ***************************************************************************
 *
 *           $Id: config.pl,v 1.1 2006/04/13 15:50:19 stf Exp $
 *        author: KBSG
 *   description: Debugging mechanism (mostly specific to readylog)
 * last modified: $Date: 2006/04/13 15:50:19 $
 *            by: $Author: stf $
 *
 * ***************************************************************************
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; version 2 of the License
 *
 * **************************************************************************/

/* load auxiliary predicates for colored output */
:- ensure_loaded('../../utils/utils.pl').


/* BB debugging mimic */
bb_dbg(Level, String) :-
	bb_dbg(Level, String, []).

bb_dbg(Level, Color, String, Params) :-
	(
	  getenv("BB_DEBUG", L)
	;
	  L = "0"
	),
	term_string(LT, L),
	(
	  Level =< LT ->
	  join_string(["(", Level, ") ", String, "\n\n"], "", DBGString),
	  printColor(Color, DBGString, Params)
	;
	  true
	).

bb_dbg(Level, String, Params) :-
	bb_dbg(Level, normal, String, Params).


/** gologtrace toggles the trace functionality to calls of trans
 * during program execution.
 */
gologtrace :-
	getval(tracegolog, X),
	(X -> setval(tracegolog, false), printf("Golog tracing turned OFF\n", [])
	;
	  setval( tracegolog, true), printf("Golog tracing turned ON\n", [])).

/** doTrace takes a program and a history and displays the current trans trace of
 *  the program via the printProg predicate (defined below).
 */
doTrace(E,H) :-
	getval( tracegolog, Trace ),
	(
	  Trace ->
	  printf("\n++++++++++++++++++++ trans ++++++++++++++++++++\n+++ program: ",[]),
	  flatten(E, EFlat),
	  printProg(stdout, EFlat),
	  printf("\n+++ situation: \n [", []),
	  printSit(stdout, H),
	  printf("] \n", []),
	  printf("+++++++++++++++++++++++++++++++++++++++++++++++\n",[]),
	  flush(output),
	  get_char(_UserInput)
	;
	  true
	), !.



/* ************************************************************
 *  Pretty Printer for Readylog Programs
 * it is used in the gologtrace debug environment
 * ********************************************************** */

/* Print Program ********************** */

printProg(Stream, Program) :-
	printProg( Stream, Program, 1, []), !.

printProg( _Stream, [], _N, _L) :- !.
printProg( Stream, [X|R], N, L) :- !,
	printf(Stream, "[\n", []),
	printProgAux(Stream, [X|R], N, L), 
	printNTabs( Stream, N, L),
	printf(Stream, " ]", []).

printProg(Stream, cout(Str, Args), _N, _L) :- !,
 	clear_special_characters(Str, StrNew),
 	printf(Stream, "cout( %w, %w)", [StrNew, Args]).

printProg(Stream, while(Cond, E), N, L) :- !,
	printf(Stream, "while( %w, ", [Cond]),
	append( L, [N], NL),
	N1 is N+1,
	printProg(Stream, E, N1, NL),
	printf(Stream, " )", []).

printProg(Stream, pconc(A, B), N, L) :- !,
	printf(Stream, "pconc( ", []),
	append( L, [N], NL),
	N1 is N+1,
	printProg(Stream, A, N1, NL),
	printf(Stream, ", ", []),
	printProg(Stream, B, N1, NL),
	printf(Stream, " )", []).

printProg(Stream, interrupt(Cond, I, E), N, L) :- !,
	printf(Stream, "interrupt( %w, ", [Cond]),
	append( L, [N], NL),
	N1 is N+1,
	printProg(Stream, I, N1, NL),
	printf(Stream, "\n", []),
	printNTabs( Stream, N, L),
	printProg(Stream, E, N1, NL),
	printf(Stream, " )", []).

printProg(Stream, if( Cond, A, B), N, L) :-
	(
	  not (isNil(A), isNil(B)) -> 
	  M is N+1, 
	  printf(Stream, "if( %w, \n", [Cond]), 
	  printNTabs( Stream, M, L),
	  printf(Stream, "\342\224\234\342\224\200 ", []), 
	  append( L, [M], NL), 
	  printProg(Stream, A, M, NL), 
	  printf(Stream, "\n", []), printNTabs( Stream, M, L), 
	  printf(Stream, "\342\224\224\342\224\200 ", []),
	  printProg(Stream, B, M, L) 
	;
	  printf(Stream, "(..)", [] )
	).

printProg( Stream, X, _, _) :-
	printf(Stream, "%w", [X]), !.

printProgAux(_Stream, [], _N, _L).

printProgAux(Stream, [Arg], N, L) :-
	printNTabs( Stream, N, L),
	printProg(Stream, Arg, N, L),
	printf(Stream, "\n", []).
	
printProgAux(Stream, [Arg | RArg], N, L) :-
	RArg \= [],
	(
	  not isNil(Arg) ->
	  printNTabs( Stream, N, L),
	  printProg(Stream, Arg, N, L),
	  printf(Stream, ",\n", [])
	;
	  true
	),
	printProgAux(Stream, RArg, N, L).

clear_special_characters(X, XNew) :-
	split_string(X, "\t\n", "\n\t", Xtmp),
	join_string(Xtmp, "", XNew).


/* Print Situation ******************** */

printSit(_Stream, []).
printSit(Stream, [A]) :- !,
	printSitToken(Stream, A).
printSit(Stream, [A|SR]) :-
	printSitToken(Stream, A),
	printf(Stream, ", ", []),
	printSit(Stream, SR).

printSitToken(Stream, cout(Str, Args)) :-
	string(Str), !,
 	clear_special_characters(Str, StrNew),
 	printf(Stream, "cout(%w, %w)", [StrNew, Args]).

printSitToken(Stream, Term) :-
	printf(Stream, "%w", [Term]).


/* Print Policy *********************** */
	
printNTabs( _, 0, _).
printNTabs( Stream, N, []) :-
	printf(Stream, "\t", []),
	M is N-1,
	printNTabs( Stream, M, []).
printNTabs( Stream, N, [B|L]) :-
	printNTabsAux( Stream, N, 0, [B|L]).
printNTabsAux( _, 0, _, _).
printNTabsAux( Stream, N, C, [C|L]) :-
	printf(Stream, "\342\224\202\t", []), 
	M is N-1, CM is C+1,
	printNTabsAux( Stream, M, CM, L ).
printNTabsAux( Stream, N, C, L) :-
	printf(Stream, "\t", []), 
	M is N-1, CM is C+1,
	printNTabsAux( Stream, M, CM, L ).

isNil( [] ).


printPol(Stream, Policy) :-
	printPol( Stream, Policy, 0, []).

printPol( Stream, [ X | R], N, L) :-
	printf(Stream, "[ ", []),
	printPol(Stream, X, N, L), 
	printf(Stream, ", ", []),
	printPol(Stream, R, N, L),
	printf(Stream, " ]", []).

printPol(Stream, if( Cond, A, B), N, L) :-
	(
	  not (isNil(A), isNil(B)) -> 
	  M is N+1, 
	  printf(Stream, "if(%w) then \n", [Cond]), 
	  printNTabs( Stream, M, L),
	  printf(Stream, "\342\224\234\342\224\200 ", []), 
	  append( L, [M], NL), 
	  printPol(Stream, A, M, NL), 
	  printf(Stream, "\n", []), printNTabs( Stream, M, L), 
	  printf(Stream, "\342\224\224\342\224\200 ", []),
	  printPol(Stream, B, M, L) 
	;
	  printf(Stream, "(..)", [] )
	).

printPol(Stream, ifCond( Cond, A), N, L) :-
	printf(Stream, "ifCond( %w, ", [Cond]), 
	printPol(Stream, A, N, L), 
	printf(Stream, " )", []).

printPol(Stream, match(A), N, L) :-
	printf(Stream, "match(", []),
	printPol(Stream, A, N, L), 
	printf(Stream, " )", []).	

printPol( Stream, X, _, _) :- printf(Stream, "%w", [X]).
