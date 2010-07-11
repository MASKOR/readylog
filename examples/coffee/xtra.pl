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
 *        author: Andreas Wortmann <andreas.wortmann@rwth-aachen.de>
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

% Any coffee?
:- setval( real_coffee_prepared, false ).

% Any requests?
:- setval( real_request, nil ).

% Where am I?
:- setval( real_pos, kitchen ).

% Performing action?
:- setval( real_active_action, nil ).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%% sleeping                             %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

sleep_action :- realSleep( 0.5 ).
sleep_wait   :- realSleep( 2 ).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%% update loop                          %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

/** our main update method.
 */
update :- 
%	printf(" --- start UPDATE ...\n", []), 
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
%	vis_update,
	true
%	, printf(" --- UPDATE ... done.\n", [])
	.

%vis_update :-
%	redraw, 
%	getval(real_start_pos, [StartX, StartY]),
%	draw_start(StartX,StartY),
%	getval(real_goal_pos, [GoalX, GoalY]),
%	draw_goal(GoalX,GoalY),
%	getval(real_agent_pos, [AgentX, AgentY]),
%	draw_agent(AgentX,AgentY),
%	getval(real_item_pos, [ItemX, ItemY]),
%	( [ItemX, ItemY] = [-1,-1] ->
%	    true
%	;
%	    draw_item(ItemX,ItemY)
%	),
%	true.

%:- setval( item_xtra_event_enabled,true).

/** exogf_Update.
 *  updates all the exogenous fluents (sensing/world_model/...)
 *  and takes care of (readylog) event dispatching
 */
xTra(exogf_Update, _H, _C ) :- !, 
    printf(" *** START exogf_Update *** \n", []), flush(output),
	%% event_dispatcher
	getval( real_coffee_prepared, V_COFFEE ), 
	printColor( red, " *** exogf_Update: wm_coffee_prepared = %w\n", [ V_COFFEE ] ),
	setval( wm_coffee_prepared, V_COFFEE ),
	getval( real_request, V_REQUEST ), 
	printColor( red, " *** exogf_Update: wm_request = %w\n", [ V_REQUEST ] ),
	setval( wm_request, V_REQUEST ),
	getval( real_pos, V_POS ), 
	printColor( red, " *** exogf_Update: wm_pos = %w\n", [ V_POS ] ),
	setval( wm_pos, V_POS ),
	update_components,
	update_nonpreemptive_actions
	, printf(" *** exogf_Update DONE. *** \n", []), flush(output)
	.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  eXecution Transformation of         %%
%%  primitive actions in maze.          %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  include calls to drawing-routines

%% %%%%%%%%%%%%%%%%%%%%% %%
%% deterministic execution

xTra( _Act, _H, _C ) :-
    has_val( pos, Pos, H ),
	printColor( pink, "  Pos = %w\n", [ Pos ] ),
	has_val( request, Req, H ),
	printColor( pink, "  Request = %w\n", [ Req ] ),
	has_val( holding, Hld, H ),
	printColor( pink, "  Holding = %w\n", [ Hld ] ),
	has_val( located, Loc, H ),
	printColor( pink, "  Located = %w\n", [ Loc ] ),
	has_val( calibrated, Cal, H ),
	printColor( pink, "  Calibrated = %w\n", [ Cal ] ),
	has_val( task, Task, H ),
	printColor( pink, "  Task = %w\n", [ Task ] ),
%	getval( coffee_prepared, CoffPrep ),
%	printColor( pink, "  wm_coffee_prepared = %w\n", [ CoffPrep ] ),
	flush(output),
	fail.

/**
 * Handling of nonpreemptive actions. 
 */	
xTra( Act, _H, _C ) :-
    starter( A, Act ), 
    nonpreemptive( A, AWM, AReal ),
    printColor( red, " *** xTra: enabling action %w\n", [ A ] ),
    setval( AWM, true ),
    setval( AReal, true ), 
    fail.
xTra( Act, _H, _C ) :-
    finisher( A, Act ), 
    nonpreemptive( A, AWM, AReal ),
    printColor( red, " *** xTra: disabling action %w\n", [ A ] ),
    setval( AWM, false ),
    setval( AReal, false ), fail.

xTra( wait, _H, _C ) :-
	printColor( black, " *** xTra waiting\n", [ ] ),
	sleep_wait,
	flush(output).
	
xTra( wait( A ), _H, _C ) :-
    printColor( red, " *** Waiting for end of action '%w'\n", [ A ] ),
%    setval( real_active_action, A ),
%    flush(output),
    sleep_wait.

xTra( prepare_coffee, _H, _C ) :-
%	printColor( black, " *** xTra: start_prepare_coffee\n", [ ] ),
	setval( real_coffee_prepared, true ),
	sleep_action.
	
xTra( start_goto( _R ), _H, _C ) :-
%	printColor( black, " *** xTra start_goto(w)\n", [ R ] ),
%	nonpreemptive( goto( _R ), _GWM, GReal ),
%	getval( GReal, V ),
%	printColor( red, " *** xTra: real_going_to = %w\n", [ V ] ),
	sleep_action.
	
xTra( stop_goto( R ), _H, _C ) :-
	printColor( red, " *** xTra: stop_goto(%w)\n", [ R ] ),
    setval( real_pos, R ),
    printColor( red, " *** xTra: real_pos = %w\n", [ R ] ),
	sleep_action.
	
% icp([exogf_Update,start_goto(1),stop_goto(1),wait])
	
xTra( start_take_order( X ), _H, _C ) :-
%	printColor( black, " *** xTra start_take_order(%w)\n", [ X ] ),
	printColor( red, "WD-42: Starting to take order from person #%w.\n", [ X ] ),
	sleep_action.
	
xTra( stop_take_order( X ), _H, _C ) :-
%	printColor( black, " *** xTra stop_take_order(%w)\n", [ X ] ),
	printColor( red, "WD-42: I have taken the order from person #%w.\n", [ X ] ),
	setval( real_request, nil ),
	sleep_action.
	
xTra( start_place_order( P ), _H, _C ) :-
%	printColor( black, " *** xTra start_place_order(%w)\n", [ P ] ),
	printColor( red, "WD-42: I have coffee order from room #%w.\n", [ P ] ),
	sleep_action.
	
xTra( start_pickup( _P ), _H, _C ) :-
%	printColor( black, " *** xTra start_pickup(%w)\n", [ P ] ),
	sleep_action.
	
xTra( start_locate( _P, _R ), _H, _C ) :-
%	printColor( black, " *** xTra start_locate(%w,%w)\n", [ P, R ] ),
	sleep_action.
	
xTra( start_drop( _X, _R ), _H, _C ) :-
%	printColor( black, " *** xTra start_drop(%w,%w)\n", [ X, R ] ),
	sleep_action.
	
xTra( start_move_arm, _H, _C ) :-
%	printColor( black, " *** xTra start_move_arm\n", [ ] ),
	sleep_action.
	
xTra( start_interact, _H, _C ) :-
%	printColor( black, " *** xTra start_interact\n", [ ] ),
	sleep_action.

% nonpreemptive actions
%	
xTra( end( A ), _H, _C ) :- prim_action( A ), npr( A ), !, 
    printColor( pink, " *** xTra end(%w) finished", [ A ] ),
    setval( wm_active_action, nil ).	



%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  cout in various variations.         %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  transform couts to printf/printColor

xTra( cout(V), _H, _C) :- !,
	printf("%w", [V]), flush(output).

xTra( cout(F, V), _H, _C) :- !,
	printf(F, V), flush(output).

xTra( cout(Color,F,V), _H, _C) :- !,
	printColor(Color,F,V).
	
	
%% COMPONENT ACTIONS %%%%%%%%%%%%%%%%%% %%

xTra( A, _H, _C ) :-
    component( C, _Cs, _CsWM, CsReal, _, _ ),
    getval( CsReal, V ),
    edge( C, V, A, G ),
    setval( CsReal, G ),
    printColor( pink, " *** xTra: Translating %w from %w with %w to %w\n", [ C, V, A, G ] ),
    sleep_action.

%% ABSORBER ENTRY %%%%%%%%%%%%%%%%%%%%% %%
%% used to catch all the stuff
%% that doesn't have an own handle
xTra(A, _H, _C) :-
	printColor( red, " *** xTra: DON'T KNOW HOW TO HANDLE '%w'\n", [A]).
	
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  translate Keys to Actions           %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

% Alter Component State Key (c)
% Allows to select a component and a state for this component.
%
translateActionToKey( Key, Action ) :-
        Key = 99, !,
        printf( "Got Key 'c'! -> alter_comp_state\n", [] ), flush( output ),
        % 1. Print a list of components and select which to modify
        get_comp_list( Components ),
        printNumberedList( Components, 0 ),
        printf( "*** Please insert component # to alter state (NO ENTER REQUIRED!):\n", [] ), flush( output ),
        printf( "*** #: ", [] ), flush( output ),
	    getkey_blocking( Ikey ), I is Ikey - 48,
	    get_comp( Components, I, Comp ),
	    % 2. Print the states of the selected component and select which to 
	    % assign
	    printf( "*** Component '%w' has the following states\n", [ Comp ] ), flush( output ),
	    get_comp_states( Comp, States ),
	    printNumberedList( States, 0 ),
	    printf( "*** Please insert component state # to assign (NO ENTER REQUIRED!):\n", [] ), flush( output ),
	    printf( "*** #: ", [] ), flush( output ),
	    getkey_blocking( Jkey ), J is Jkey - 48,
	    get_comp_state( States, J, State ),
	    printf( "*** Setting state of %w to %w.\n", [ Comp, State ] ), flush( output ),
	    component( Comp, _Cs, _CsWm, CsReal, _CsIv ),
	    setval( CsReal, State ),
	    Action = exog_noop.
	    
% End Action Key (e)
%
translateActionToKey( Key, Action ) :-
    Key = 101, !,
    printf( "Got Key 'e'! -> end_action\n", [] ), flush( output ),
    % 1. List active nonpreemptive actions
    get_active_npr_actions_list( Actions ),
    (
        Actions \= []
      ->
        (
            printf( "*** The following actions are active:\n", [] ), 
            flush( output ),
            printNumberedList( Actions, 0 ),
            printf( "*** Please insert action # to terminate:\n", [] ), 
            flush( output ),
            printf( "*** #: ", [] ), flush( output ),
            getkey_blocking( Ikey ), I is Ikey - 48,
            get_action( Actions, I, Action ),       
            printf( "*** Terminating action '%w'\n", [ Action ] ),
            nonpreemptive( Action, _AWM, AReal ),        
            setval( AReal, false )
%           , setval( AWM  , false )
        )
      ;
        (
            printf( "*** There are no active actions\n", [ ] ), 
            flush( output )
        )
    ),
    Action = exog_noop.
        
% End Action Key (n)
%
translateActionToKey( Key, Action ) :-
    Key = 110, !,
    printf( "Got Key 'n'! -> show_nonpreemptive_actions\n", [] ), flush( output ),
    % 1. List active nonpreemptive actions
    get_active_npr_actions_list( Actions ),
    printNumberedList( Actions, 0 ),
    Action = exog_noop.

% Prepare Coffee Key (p)
%
translateActionToKey( Key, Action ) :-
        Key = 112, !,
        printf( "Got Key 'p' -> prepare_coffee\n", [] ), flush( output ),
        Action = prepare_coffee,
        setval( real_coffee_prepared, true ).

% Issue Request Key (r)
%
translateActionToKey( Key, Action ) :-
        Key = 114, !,
        printf( "Got Key 'r'! -> set_request\n", [] ), flush( output ),
        printf( "*** Please insert room # to issue request at (NO ENTER REQUIRED!):\n", [] ), flush( output ),
        printf( "*** #: ", [] ), flush( output ),
	    getkey_blocking(Rkey), R is Rkey - 48,
        printf( "*** You issue a request at room #%w\n", [ R ] ), flush( output ),
        Action = set_request( R ),
        setval( real_request, R ).       
        
% Show System State Key (s)
%
translateActionToKey( Key, Action ) :-
        Key = 115, !,
        printf( "Got Key 's'! -> system_state\n", [] ), flush( output ),
        get_comp_list( Components ),
        print_components_states( Components ),
        flush( output ),
        Action = exog_noop.

% Teleport Button (t)
%
translateActionToKey( Key, Action ) :-
        Key = 116, !,
        printf( "Got Key 't'!\n", [] ), flush( output ),
        printf( "*** Please insert room to teleport to (NO ENTER REQUIRED!):\n", [] ), flush( output ),
        printf( "*** #: ", [] ), flush( output ),
	getkey_blocking(Rkey), R is Rkey - 48,
        printf( "*** You want to teleport to room #%w\n", [ R ] ), flush( output ),
        Action = teleport( R ),
        setval( real_pos, R ).

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
