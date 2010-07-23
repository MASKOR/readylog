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

% Step wise execution?
:- setval( exec_stepwise, true ).


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  exogeneous fluents                  %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  this is where we hold real values
%%  of exogenous fluents hidden for the agent

% Any coffee?
:- setval( real_coffee_prepared, false ).

% Any requests?
:- setval( real_requests, [] ).

% Where am I?
:- setval( real_pos, kitchen ).

% Performing action?
:- setval( real_active_action, nil ).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%% graphical representation             %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

/* starting position */
:- setval( real_start_pos, [0,0] ). 

/* starting position */
:- setval( real_goal_pos, [5,5] ).

/* real position of our agent */
:- setval( real_agent_pos, [1,1] ).

/* real position of the item */
:- setval( real_item_pos, [4,4] ).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%% sleeping                             %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

debug_sleep :- true.

sleep_action :- debug_sleep -> realSleep( 0.5 ) ; realSleep( 0.25 ).
sleep_wait   :- debug_sleep -> realSleep( 0.5 ) ; realSleep( 1 ).
sleep_npr    :- debug_sleep -> realSleep( 4 )   ; true .% else they're nonpreemptive

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%% update loop                          %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

/** our main update method.
 */
update :- 
%	printf(" --- start UPDATE ...\n", []), 
	%refresh_window,
	(
	    getkey( Key ) 
	  ->
	    translateActionToKey( Key, Action ),
	    (
		    exog_action( Action )
		  ->
    		exoEnQueue( Action ) 
          ;
		    printf("\n%w is not a defined action\n", [Action]),
            flush( output )
        )
      ;
        true
	),
	vis_update,
	true
%	, printf(" --- UPDATE ... done.\n", [])
	.

vis_update :-
%    printColor( pink, "*** VIS UPDATE ***\n", [] ),
	redraw, 
	% draw the offices 
	findall( office_loc( Office, Xo, Yo ), office_loc( Office, Xo, Yo ), L ),
%	printEq( 'Offices', L ),
	draw_offices( L ),
    % draw kitchen
    kitchen_loc( Xk, Yk ),
	draw_goal( Xk, Yk ),
	% draw agent
	getval( real_pos, R ),
	( R = kitchen -> kitchen_loc( Xa, Ya ) ; office_loc( R, Xa, Ya ) ),
	draw_agent( Xa, Ya ),
	% draw requests
	(
	    getval( real_requests, Requests ) 
	  ->
%	    printEq( 'Requests', Requests ),
	    draw_requests( Requests )
	  ;
	    true
	).
	
draw_offices( [] ).
draw_offices( [ H | T ] ) :-
    H = office_loc( _Office, X, Y ),

    draw_start( X, Y ),
	draw_offices( T ).
	
draw_requests( [] ).
draw_requests( [ R | T ] ) :-
    office_loc( R, X, Y ),
%    printColor( pink, " *** Drawing request %w at (%w,%w)\n", [ R, X, Y ] ),
    draw_item( X, Y ),
	draw_requests( T ).
	
printVal( Name, H ) :-
    has_val( Name, V, H ),
	printColor( black, "  %w = %w\n", [ Name, V ] ).
    

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
	getval( real_requests, V_REQUESTS ), 
	printColor( red, " *** exogf_Update: wm_requests = %w\n", [ V_REQUESTS ] ),
	setval( wm_requests, V_REQUESTS ),
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

/**
 * Print agent state to screen
 */
xTra( Act, H, _C ) :-
    ( Act \= wait, Act \= wait( _A ), Act \= noop, Act \= exog_noop ),
    getval( exec_stepwise, true ),
    printColor( black, "********** AGENT STATE **********\n", [] ),
    printVal( pos, H ),
	printVal( requests, H ),
	printVal( holding, H ),
	printVal( located, H ),
	printVal( task, H ), 
	printVal( colli_state, H ),
	printVal( arm_state, H ),
	printVal( motor_state, H ),
%	printVal( calibrated, H ),
	printVal( coffee_prepared, H ),
    get_active_npr_actions_list( Actions ),
    printNumberedList( Actions, 0 ),
	flush(output),	
	printColor( black, " Press 'x' to abort stepwise execution, any other key to continue: \n", [] ), 
	printColor( black, " You can return to stepwise execution any time by pressing 'x' again: \n", [] ), 
	flush( output ),
	getkey_blocking( Key ), 
	(
	    Key = 120
	  ->
	    setval( exec_stepwise, false )
	  ; 
	    true
	),	
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
	
xTra( start_goto( R ), _H, _C ) :-
%	printColor( black, " *** xTra start_goto(w)\n", [ R ] ),
%	nonpreemptive( goto( _R ), _GWM, GReal ),
%	getval( GReal, V ),
	printColor( red, " *** xTra: Started going to room #%w\n", [ R ] ),
	sleep_npr.
	
xTra( stop_goto( R ), _H, _C ) :-
	printColor( red, " *** xTra: stop_goto(%w)\n", [ R ] ),
    setval( real_pos, R ),
    printColor( red, " *** xTra: real_pos = %w\n", [ R ] ),
	sleep_npr.
	
xTra( start_take_order( X ), _H, _C ) :-
%	printColor( black, " *** xTra start_take_order(%w)\n", [ X ] ),
	printColor( red, "WD-42: Starting to take order from person #%w.\n", [ X ] ),
	sleep_action.
	
xTra( stop_take_order( X ), _H, _C ) :-
%	printColor( black, " *** xTra stop_take_order(%w)\n", [ X ] ),
	printColor( red, "WD-42: I have taken the order from person #%w.\n", [ X ] ),
	getval( real_requests, Val ),
	Val = [ _CurrentRequest | Remaining ],
	setval( real_requests, Remaining ),
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
%xTra( end( A ), _H, _C ) :- prim_action( A ), npr( A ), !, 
%    printColor( pink, " *** xTra end(%w) finished", [ A ] ),
%    setval( wm_active_action, nil ).	



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
    component( C, _Cs, _CsWM, _CsReal, _, _ ),
%    getval( CsReal, V ),
    edge( C, V, A, G ),
%    setval( CsReal, G ),
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
	    
	    component( Comp, Cs, _CsWm, _CsReal, _CsIv, _CsDv ),
	    Action = set_component( Cs, State ).
	    
	    /** EXOGENOUS FLUENT REPRESENTATION **
	    component( Comp, _Cs, _CsWm, CsReal, _CsIv, _CsDv ),
	    setval( CsReal, State ),
	    Action = exog_noop.
	    */
	    
	    
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
	    dbgXX,
        printf( "*** You issued a request at room #%w\n", [ R ] ), flush( output ),
        getval( real_requests, R0 ),
        (
            member( R, R0 )
          ->
            R1 = R0,
            printf( "*** There is already a request at room #%w\n", [ R ] ), 
            flush( output )
          ;
            printf( "*** Appending request #%w to '%w'\n", [ R, R0 ] ), 
            flush( output ),
            append( R0, [ R ], R1 )
        ),
%        Action = set_request( R ),
        Action = exog_noop,
        setval( real_requests, R1 ).       
       

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
        
% Return to stepwise mode (x)
%
translateActionToKey( Key, Action ) :-
        Key = 120, !,
        printf( "Got Key 'x'! -> returning to stepwise mode\n", [] ),
        setval( exec_stepwise, true ),
        Action = exog_noop.

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
