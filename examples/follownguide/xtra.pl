/* ***************************************************************************
 *  ,=³ .%%% %%%%%%%. .%%%%%.  .%%%%%.   %%%%%.%%. %. %%%%%%%% %% Rheinisch-
 * [==].%%%   %%   %% %%   %%  %%   %%   %%  %%'%%.%%.%% %% %% %% Westfälische
 *  //l%%%    %%   %% %%%%. ' %%       @ %%%%%' '%%%%%%% %% %%%%% Technische
 * [==]%%     %%|=%%   %%=>%  %%         %%  %%  '%%'%%% %% %% %% Hochschule
 * [==]%%%    %%   %% . '%%%% %%  '%%%   %%   %%  '% '%% %% %% %% Aachen
 * [==]'%%%   %%   %% %%   %%  %%   %%   http://kbsg.rwth-aachen.de/
 * o^^o '%%% %%%%%%%' '%%%%%'O '%%%%%'   Knowledge Based Systems Group
 * ***************************************************************************
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; version 2 of the License.
 *
 * ***************************************************************************
 *
 *           $Id: xtra.pl 68 2007-12-06 18:30:15Z stf $
 *        author: Daniel Beck <beck@kbsg.rwth-aachen.de>
 *   description: eXecution TRAnsformation which maps primitive actions 
 *                to something meaningful in the real world ...
 *
 * ************************************************************************ */

:- write(" --> loading xtra.pl ... \n").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  settings                            %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

/* do we want to be asked for stochastic action outcomes? */
exec_ask4outcome :- true.


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  exogeneous fluents                  %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  this is where we hold real values
%%  of exogenous fluents hidden for the agent

/* real position of our agent */
:- setval( real_agent_pos, [1,1] ).

/* real position of the human */
:- setval( real_human_pos, [3,1] ).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%                                      %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

/** our main update method.
 */
update :- 
	%printf(" --- start UPDATE ...\n", []), 
	%refresh_window,
	(
	    getkey( Key ) ->
	    translateActionToKey( Key, Action ),
	    (
		exog_action( Action )->
		exoEnQueue( Action ) ;
		printf("\n%w is not a defined action\n", [Action]),
                flush( output )
             )
             ;
	     true
	),
        %vis_update,
	true.
	%printf(" --- UPDATE ... done.\n", []).

/*
vis_update :-
	redraw, 
	getval(real_start_pos, [StartX, StartY]),
	draw_start(StartX,StartY),
	getval(real_goal_pos, [GoalX, GoalY]),
	draw_goal(GoalX,GoalY),
	getval(real_agent_pos, [AgentX, AgentY]),
	draw_agent(AgentX,AgentY),
	getval(real_item_pos, [ItemX, ItemY]),
	( [ItemX, ItemY] = [-1,-1] ->
	    true
	;
	    draw_item(ItemX,ItemY)
	),
	true.
*/

/** exogf_Update.
 *  updates all the exogenous fluents (sensing/world_model/...)
 *  and takes care of (readylog) event dispatching
 */
xTra(exogf_Update, _H) :- !,
	getval( real_agent_pos, V_AGENT_POS ), 
	setval( wm_agent_pos, V_AGENT_POS ),
        getval( real_human_pos, V_HUMAN_POS ),
        manhattan_dist(V_HUMAN_POS, V_AGENT_POS, V),
        (
            V =< 5 ->
            setval( wm_human_pos, V_HUMAN_POS )
        ;
            setval( wm_human_pos, 0 )
        ),
	printf(" *** exogf_Update DONE. *** \n", []), flush(output).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  eXecution Transformation of         %%
%%  primitive actions in maze.          %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  include calls to drawing-routines

%% %%%%%%%%%%%%%%%%%%%%% %%
%% deterministic execution

xTra(goto_prim(Xrelto, Yrelto), H) :-
        printColor( pink, " xTra: EXEC 'goto_prim(%d, %d)'", [Xrelto, Yrelto]),
        getval( real_agent_pos, [X, Y] ),
        printf(" at %w,%w ", [X, Y ] ),
        has_val( epf_agent_pos, V, H ),
        printf(" epf at %w\n", [V]),
        execdelay,
        Xn is X + Xrelto,
        Yn is Y + Yrelto,
        setval( real_agent_pos, [Xn, Yn] ).

xTra(noop, H)  :- 
	printColor( pink, " xTra: EXEC 'noop'", []), 
	has_val( pos, V, H ), 
	V = [X,Y], 
	printf(" at %w \n", [V]), 
        execdelay,
	draw_action("0", X, Y).

%% %%%%%%%%%%%%%%%%%%%%% %%
%% stochastic execution

xTra(goto_stoch(Xrelto, Yrelto), _H) :- 
	printf(" xTra: EXEC 'goto_stoch(%w, %w)'", [Xrelto, Yrelto]).%, 
        /*
	has_val( pos, V, H ), 
	V = [X,Y], 
	printf(" at %w \n", [V]), 
	( exec_ask4outcome ->
	    printf("\n xTra: Choose outcome:\n  (1) right\n  (2) noop\n", []), flush(output),
	    getkey_blocking(Key), Outcome is Key - 48,
	    printf(" xTra: Chosen outcome: %w\n", [Outcome]), flush(output),!,
            (
              (
                Outcome = 1 -> X1 is X + 1, 
		setval(real_agent_pos, [X1,Y]),
		printf("Going right\n", []),
		draw_action("R", X, Y),
		realSleep(2)
              )
            ;
              (
                Outcome = 2 -> setval(real_agent_pos, [X,Y]),
                printf("Not going anywhere\n", [])
              )
	    )
	;
          execdelay,
	  Xn is X+1,
	  setval( real_agent_pos, [Xn,Y] ),
	  draw_action("R", X, Y)
      ).
*/

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  cout in various variations.         %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  transform couts to printf/printColor

xTra(cout(V), _H) :- !,
	printf("%w", [V]), flush(output).

xTra(cout(F, V), _H) :- !,
	printf(F, V), flush(output).

xTra(cout(Color,F,V),_H) :- !,
	printColor(Color,F,V).


%% ABSORBER ENTRY %%%%%%%%%%%%%%%%%%%%% %%
%% used to catch all the stuff
%% that doesn't have an own handle
xTra(Action, H) :-
	printColor( red, " xTra: DON'T KNOW HOW TO HANDLE '%w' IN SIT: '%w'\n", [Action,H]).


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  translate Keys to Actions           %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

% Show Fluents Key (s)
%
translateActionToKey( Key, Action ) :-
        Key = 115, !,
        printf( "Got Key 's'!\n", [] ), flush( output ),
        Action = show_fluents.


% down
translateActionToKey( Key, Action ) :-
        Key = 50, !,
        getval(real_human_pos, [X, Y]),
        YY is Y - 1,
        setval(real_human_pos, [X, YY]),
        Action = human_down.

% left
translateActionToKey( Key, Action ) :-
        Key = 52, !,
        getval(real_human_pos, [X, Y]),
        XX is X - 1,
        setval(real_human_pos, [XX, Y]),
        Action = human_left.

% right
translateActionToKey( Key, Action ) :-
        Key = 54, !,
        getval(real_human_pos, [X, Y]),
        XX is X + 1,
        setval(real_human_pos, [XX, Y]),
        Action = human_right.

% up
translateActionToKey( Key, Action ) :-
        Key = 56, !,
        getval(real_human_pos, [X, Y]),
        YY is Y + 1,
        setval(real_human_pos, [X, YY]),
        Action = human_up.
        
/*
% Teleport Button (t)
%
translateActionToKey( Key, Action ) :-
        Key = 116, !,
        printf( "Got Key 't'!\n", [] ), flush( output ),
        printf( "** please insert position to teleport to (NO ENTER REQUIRED!):\n", [] ), flush( output ),
        printf( "*** X:", [] ), flush( output ),
	getkey_blocking(Xkey), X is Xkey - 48,
        printf( "*** Y:", [] ), flush( output ),
	getkey_blocking(Ykey), Y is Ykey - 48,
        printf( "** you want to teleport to (%w,%w)\n", [X,Y] ), flush( output ),
        Action = teleport(X,Y).
*/
% Unknown Case
%
translateActionToKey( Key, Action ) :- !,
        printf( "Got Key '%w'!\n", [Key] ), flush( output ),
        Action = wait.


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  HELPER FUNCTIONS                                   %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

% Return the current Eclipse Time. Kind'o'a timestamp.
%   Parameter 1: Returns current time.
% 
currentTime( Time ) :-
        statistics( times, [_CPU1, _SYS1, Time] ), !.

% Real Sleep method. Uninterruptable sleep method.
%
realSleep( Time ) :-
        currentTime( Start ),
        repeat,
        sleep( 0.01 ),
        currentTime( Current ),
        Current - Start >= Time, !.

execdelay :- realSleep(2).

:- write(" <-- loading xtra.pl done.\n").
