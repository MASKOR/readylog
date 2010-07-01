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
 *        author: Stefan Schiffer <schiffer@cs.rwth-aachen.de>
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

/* real position of the item */
:- setval( real_item_pos, [4,4] ).

/* real position of the item */
:- setval( real_carry_item, false ).

/* starting position */
:- setval( real_start_pos, [1,1] ).

/* starting position */
:- setval( real_goal_pos, [5,5] ).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%                                      %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

/** our main update method.
 */
update :- 
	printf(" --- start UPDATE ...\n", []), 
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
	vis_update,
	true
	, printf(" --- UPDATE ... done.\n", [])
	.

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

:- setval( item_xtra_event_enabled,true).

process_xtra_events :-
        ( getval( item_xtra_event_enabled,true) -> xtra_loose_item_event; true ).

xtra_loose_item_prob(0.01).
xtra_loose_item_event :- !,
        xtra_loose_item_prob(Prob),
        frandom(Rand),
        ( getval(real_carry_item,true), Rand < Prob ->
	    ( 
		 setval(real_carry_item,false),
		printColor( red, " ********** LOST ITEM EvENT !!!! ********* \n%b", [])
	    )
	;
	  true
        ).



/** exogf_Update.
 *  updates all the exogenous fluents (sensing/world_model/...)
 *  and takes care of (readylog) event dispatching
 */
xTra(exogf_Update, _H) :- !, 
        %printf(" *** START exogf_Update *** \n", []), flush(output),
	%% event_dispatcher
	%process_xtra_events,
	%% worldmodel update
	getval( real_agent_pos, V_AGENT_POS ), 
	%printColor( yellow, " V_AGENT_POS = %w \n", [V_AGENT_POS]), 
	setval( wm_agent_pos, V_AGENT_POS ),
	getval( real_item_pos, V_ITEM_POS ), 
	%printColor( yellow, " V_ITEM_POS = %w \n", [V_ITEM_POS]), 
	setval( wm_item_pos, V_ITEM_POS ),
	getval( real_carry_item, V_CARRY_ITEM ), setval( wm_carry_item, V_CARRY_ITEM ),
	%%
	printf(" *** exogf_Update DONE. *** \n", []), flush(output).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  eXecution Transformation of         %%
%%  primitive actions in maze.          %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  include calls to drawing-routines

%% %%%%%%%%%%%%%%%%%%%%% %%
%% deterministic execution

%xTra(go_right,H) :- 
%	printColor( pink, " xTra: EXEC 'go_right'", []), 
%	getval( real_agent_pos, [X,Y] ),
%	printf(" at %w,%w ", [X,Y]), 
%	has_val( pos, V, H ), 
%	printf(" epf at %w \n", [V]), 
%        execdelay,
%	Xn is X+1,
%	setval( real_agent_pos, [Xn,Y] ),
%	process_xtra_events,
%	( getval(real_carry_item,true) -> setval( real_item_pos, [Xn,Y] ); true ),
%	draw_action("R", X, Y).

% nonpreemptive actions
%	
xTra( end( A ), _S ) :- prim_action( A ), npr( A ), !, 
	printColor( pink, " xTra: exog action '%w' finished", [ A ] ).
	



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
%xTra(A,S) :-
%	printColor( red, " xTra: DON'T KNOW HOW TO HANDLE '%w' IN SIT: '%w'\n", [A,S]).
xTra(A,_S) :-
	printColor( red, " xTra: DON'T KNOW HOW TO HANDLE '%w'\n", [A]).


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  translate Keys to Actions           %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

% Show Fluents Key (s)
%
translateActionToKey( Key, Action ) :-
        Key = 115, !,
        printf( "Got Key 's'!\n", [] ), flush( output ),
        Action = show_fluents.

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
