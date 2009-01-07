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
 *           $Id: xtra.pl 01 2008-08-14 18:34:15Z dp $
 *        author: Dennis Pannhausen <Dennis.Pannhausen@rwth-aachen.de>
 *   description: eXecution TRAnsformation which maps primitive actions 
 *                to something meaningful in the real world ...
 *
 * ************************************************************************ */

:- write(" --> loading utils.pl ... \n").

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

/* list of visited cells */
:- setval( real_cells_visited, [ [1,1] ] ).

/* list of unvisited cells */
:- setval( real_cells_shaded, [ [] ] ).

/* number of unexplored cells currently lying in front of the agent */
/* should be initialised dynamically! */
:- setval( real_num_facedShades, 3 ).

/* list of (sensed) breezy cells */
:- setval( real_cells_breezy, [ [] ] ).

/* list of (sensed) smelly cells */
:- setval( real_cells_smelly, [ [] ] ).

/* list of cells that are known to contain a pit */
%:- setval( real_cells_know_pit, [ ] ).

/* list of cells that are known to contain no pit */
:- setval( real_cells_know_no_pit, [[1,1]] ).

/* list of cells that are known to contain the wumpus */
%:- setval( real_cells_know_wumpus, [ ] ).

/* list of cells that are known to contain no wumpus */
:- setval( real_cells_know_no_wumpus, [[1,1]] ).

/* real position of our agent
 * default value
 * overwritten randomly via place_agent(Seed)*/
:- setval( real_agent_pos, [1,1] ).

/* real direction our agent is facing in */
:- setval( real_agent_direction, "east" ).

/* is the arrow still available to the agent */
:- setval( real_agent_arrow, "T" ).

/* real position of the wumpus
 * default value
 * overwritten randomly via place_wumpus(Seed)*/
:- setval( real_wumpus_pos, [1,3] ).

/* is wumpus alive */
:- setval( real_wumpus_alive, "T" ).

/* real position of the gold
 * default value
 * overwritten randomly via place_gold(Seed)*/
:- setval( real_gold_pos, [2,3] ).

/* is our agent carrying the gold */
:- setval( real_carry_gold, false ).

/* can the agent reach the gold safely */
:- setval( real_gold_reachable, "T" ).

/* starting position
 * default value
 * overwritten randomly via place_agent(Seed)*/
:- setval( real_start_pos, [1,1] ).

/* distance to closest cell that can be explored safely */
/* should be initialised dynamically! */
:- setval( real_distance_to_closest_safe_cell, 8 ).

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
	vis_update,
	true.
	%printf(" --- UPDATE ... done.\n", []).

vis_update :-
	%printf(" --- DOING A VIS_UPDATE ... \n", []),
	redraw,
	
        getval(real_start_pos, [StartX, StartY]),
	draw_start(StartX,StartY),
	
        getval(real_agent_pos, [AgentX, AgentY]),
        getval(real_agent_direction, AgentD),
        getval(real_agent_arrow, AgentA),
	draw_wumpus_hunter(AgentX,AgentY,AgentD,AgentA),
        
        getval(real_wumpus_pos, [WumpusX, WumpusY]),
        getval(real_wumpus_alive, WumpusA),
        draw_wumpus(WumpusX,WumpusY,WumpusA),
	
        getval(real_gold_pos, [GoldX, GoldY]),
	( [GoldX, GoldY] = [-1,-1] ->
	    true
	;
	    draw_gold(GoldX,GoldY)
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
	getval( real_cells_visited, V_CELLS_VIS ),
        setval( wm_cells_visited, V_CELLS_VIS ),

        getval( real_cells_shaded, V_CELLS_SHA ),
        setval( wm_cells_shaded, V_CELLS_SHA ),
        update_shades(V_CELLS_SHA),

        getval( real_agent_pos, V_AGENT_POS ), 
	setval( wm_agent_pos, V_AGENT_POS ),
        
        getval( real_agent_direction, V_AGENT_DIRECTION ),
	%printColor( yellow, " V_AGENT_DIRECTION = %w \n", [V_AGENT_DIRECTION]), 
        ( V_AGENT_DIRECTION = "east" -> setval( wm_agent_direction, east )
                                        ;
                                        true ),
        ( V_AGENT_DIRECTION = "south" -> setval( wm_agent_direction, south )
                                         ;
                                         true ),
        ( V_AGENT_DIRECTION = "west" -> setval( wm_agent_direction, west )
                                        ;
                                        true ),
        ( V_AGENT_DIRECTION = "north" -> setval( wm_agent_direction, north )
                                         ;
                                         true ),
        
        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
        %%  count the number of unexplored cells lying in front of the agent      %%
        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
        getval( real_num_facedShades, RNFS ),
        setval( wm_num_facedShades, RNFS ),

%        setval( shade_count_east, 0),
%        setval( shade_count_south, 0),
%        setval( shade_count_west, 0),
%        setval( shade_count_north, 0),
%
%        setval( wm_num_facedShades, 0 ), /** default value */
%        ( foreach( [ShadeX, ShadeY], V_CELLS_SHA ) do
%          ( ( getval( real_agent_direction, V_AGENT_DIR ),
%              V_AGENT_DIR = east,
%              getval( real_agent_pos, [CurrX, CurrY] ), 
%              CurrY = ShadeY, /** same row */
%              ShadeX > CurrX  /** shade in front */
%            ) -> incval(shade_count_east),
%                 getval(shade_count_east, SHADE_COUNT_EAST),
%%                 printColor( blue, " [%w,%w] is lying in front of agent \n", [ShadeX, ShadeY]),        
%%                 printColor( blue, " increased shade_count is: %w \n", [SHADE_COUNT_EAST]),        
%                 setval( wm_num_facedShades, SHADE_COUNT_EAST )
%                 ;
%                 true
%          ),
%          ( ( getval( real_agent_direction, V_AGENT_DIR ),
%              V_AGENT_DIR = south,
%              getval( real_agent_pos, [CurrX, CurrY] ), 
%              CurrX = ShadeX, /** same column */
%              ShadeY < CurrY  /** shade in front */
%            ) -> incval(shade_count_south),
%                 getval(shade_count_south, SHADE_COUNT_SOUTH),
%%                 printColor( blue, " [%w,%w] is lying in front of agent \n", [ShadeX, ShadeY]),        
%%                 printColor( blue, " increased shade_count is: %w \n", [SHADE_COUNT_SOUTH]),        
%                 setval( wm_num_facedShades, SHADE_COUNT_SOUTH )
%                 ;
%                 true
%          ),
%          ( ( getval( real_agent_direction, V_AGENT_DIR ),
%              V_AGENT_DIR = west,
%              getval( real_agent_pos, [CurrX, CurrY] ), 
%              CurrY = ShadeY, /** same row */
%              ShadeX < CurrX  /** shade in front */
%            ) -> incval(shade_count_west),
%                 getval(shade_count_west, SHADE_COUNT_WEST),
%%                 printColor( blue, " [%w,%w] is lying in front of agent \n", [ShadeX, ShadeY]),        
%%                 printColor( blue, " increased shade_count is: %w \n", [SHADE_COUNT_WEST]),        
%                 setval( wm_num_facedShades, SHADE_COUNT_WEST )
%                 ;
%                 true
%          ),
%          ( ( getval( real_agent_direction, V_AGENT_DIR ),
%              V_AGENT_DIR = north,
%              getval( real_agent_pos, [CurrX, CurrY] ), 
%              CurrX = ShadeX, /** same column */
%              ShadeY > CurrY  /** shade in front */
%            ) -> incval(shade_count_north),
%                 getval(shade_count_north, SHADE_COUNT_NORTH),
%%                 printColor( blue, " [%w,%w] is lying in front of agent \n", [ShadeX, ShadeY]),        
%%                 printColor( blue, " increased shade_count is: %w \n", [SHADE_COUNT_NORTH]),        
%                 setval( wm_num_facedShades, SHADE_COUNT_NORTH )
%                 ;
%                 true
%          )
%        ),
%%        getval( wm_num_facedShades, V_CELLS_FSHA ), 
%        printColor( red, " Currently I'm facing %w shaded cells\n", [V_CELLS_FSHA]),        

        V_AGENT_POS = [CurrX, CurrY],
        
        getval( real_cells_breezy, V_CELLS_BRE ),
        printColor( blue, " Memorised breeze at: %w \n", [V_CELLS_BRE]),        
        /** If there is a breeze at the current position, and it's not memorised yet, do so! */
        ( breeze(CurrX, CurrY) ->
%                              printColor( blue, " Sensing a breeze at %w \n", [V_AGENT_POS]),
                              ( not member([CurrX, CurrY], V_CELLS_BRE ) -> append( V_CELLS_BRE, [[CurrX, CurrY]], V_CELLS_BRE_NEW ),
                                                                            setval( real_cells_breezy, V_CELLS_BRE_NEW ),
                                                                            setval( wm_cells_breezy, V_CELLS_BRE_NEW )
                                                                            ;
                                                                            true
                              )
                              ;
                              true
        ),
        
        getval( real_cells_smelly, V_CELLS_SME ),
        printColor( green, " Memorised stench at: %w \n", [V_CELLS_SME]),        
        /** If there is a stench at the current position, and it's not memorised yet, do so! */
        ( stench(CurrX, CurrY) ->
%                              printColor( green, " Sensing a stench at %w \n", [V_AGENT_POS]),
                              ( not member([CurrX, CurrY], V_CELLS_SME ) -> append( V_CELLS_SME, [[CurrX, CurrY]], V_CELLS_SME_NEW ),
                                                                            setval( real_cells_smelly, V_CELLS_SME_NEW ),
                                                                            setval( wm_cells_smelly, V_CELLS_SME_NEW )
                                                                            ;
                                                                            true
                              )
                              ;
                              true
        ),

        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
        %%  Diagnostic rules to infer the position of pits(not yet)/no pits  %%
        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%        getval( real_cells_know_no_pit, V_CELLS_K_N_P ),
        % cells that have been visited and are free of breezes, have no pits as neighbours
        get_all_positions( V_CELLS_POS ),
        % printColor( red, " All positions: %w \n", [V_CELLS_POS]),
        %V_CELLS_K_N_P_NEW = V_CELLS_K_N_P,
        getval( real_cells_know_no_pit, V_CELLS_K_N_P_NEW ),
        setval( real_cells_know_no_pit, V_CELLS_K_N_P_NEW),
        setval( wm_cells_know_no_pit, V_CELLS_K_N_P_NEW ),

        ( foreach( [NotPitX, NotPitY], V_CELLS_POS ) do
          ( ( getval( real_cells_breezy, V_CELLS_BRE ),
%              printColor( red, " *****  V_CELLS_BRE: %w \n", [V_CELLS_BRE]),
              not member([NotPitX, NotPitY], V_CELLS_BRE),
              getval( real_cells_visited, V_CELLS_VIS ),
              member([NotPitX, NotPitY], V_CELLS_VIS),
%               printColor( red, " *****  not member([NotPitX, NotPitY], V_CELLS_BRE): %w \n", [not member([NotPitX, NotPitY], V_CELLS_BRE)]),
              East is NotPitX+1,
              South is NotPitY-1,
              West is NotPitX-1,
              North is NotPitY+1,
              NeighbourRight = [East, NotPitY],
              NeighbourLower = [NotPitX, South],
              NeighbourLeft = [West, NotPitY],
              NeighbourUpper = [NotPitX, North] )            
            ->
%        printColor( red, " **** found candidates \n", []),
%        getval( real_cells_know_no_pit, V_CELLS_K_N_P ),
        get_all_positions( V_CELLS_POS ),
            ( ( member(NeighbourRight, V_CELLS_POS),
%                printColor( red, " *****  NeighbourRight: %w \n", [NeighbourRight]),
%                printColor( red, " *****  V_CELLS_POS: %w \n", [V_CELLS_POS]),
                getval( real_cells_know_no_pit, V_CELLS_K_N_P_RIGHT ),
%                printColor( red, " *****  V_CELLS_K_N_P_RIGHT: %w \n", [V_CELLS_K_N_P_RIGHT]),
                not member(NeighbourRight, V_CELLS_K_N_P_RIGHT) )
              -> append( V_CELLS_K_N_P_RIGHT, [NeighbourRight], V_CELLS_K_N_P_NEW_RIGHT),%; true ),
%                 printColor(green, " ***** appending %w \n",[NeighbourRight]),
%                 printColor(green, " ***** V_CELLS_K_N_P_NEW_RIGHT: %w \n",[V_CELLS_K_N_P_NEW_RIGHT]),
                 setval( real_cells_know_no_pit, V_CELLS_K_N_P_NEW_RIGHT ),
                 setval( wm_cells_know_no_pit, V_CELLS_K_N_P_NEW_RIGHT )
%                     getval( real_cells_know_no_pit, V_CELLS_K_N_P_TEST ),
%                     printColor(blue, " ***** V_CELLS_K_N_P: %w \n",[V_CELLS_K_N_P_TEST])
                 ;
                 true ),
            ( ( member(NeighbourLower, V_CELLS_POS),
%                printColor( red, " *****  NeighbourLower: %w \n", [NeighbourLower]),
%                printColor( red, " *****  V_CELLS_POS: %w \n", [V_CELLS_POS]),
                getval( real_cells_know_no_pit, V_CELLS_K_N_P_LOWER ),
%                printColor( red, " *****  V_CELLS_K_N_P_LOWER: %w \n", [V_CELLS_K_N_P_LOWER]),
                not member(NeighbourLower, V_CELLS_K_N_P_LOWER) )
              -> append( V_CELLS_K_N_P_LOWER, [NeighbourLower], V_CELLS_K_N_P_NEW_LOWER),
%                 printColor(green, " ***** appending %w \n",[NeighbourLower]),
%                 printColor(green, " ***** V_CELLS_K_N_P_NEW_LOWER: %w \n",[V_CELLS_K_N_P_NEW_LOWER]),
                 setval( real_cells_know_no_pit, V_CELLS_K_N_P_NEW_LOWER ),
                 setval( wm_cells_know_no_pit, V_CELLS_K_N_P_NEW_LOWER )
                 ;
                 true ),
            ( ( member(NeighbourLeft, V_CELLS_POS), 
%                printColor( red, " *****  NeighbourLeft: %w \n", [NeighbourLeft]),
%                printColor( red, " *****  V_CELLS_POS: %w \n", [V_CELLS_POS]),
                getval( real_cells_know_no_pit, V_CELLS_K_N_P_LEFT ),
%                printColor( red, " *****  V_CELLS_K_N_P_LEFT: %w \n", [V_CELLS_K_N_P_LEFT]),
                member(NeighbourLeft, V_CELLS_POS),
                not member(NeighbourLeft, V_CELLS_K_N_P_LEFT) )
              ->  append( V_CELLS_K_N_P_LEFT, [NeighbourLeft], V_CELLS_K_N_P_NEW_LEFT),
%                 printColor(green, " ***** appending %w \n",[NeighbourLeft]),
%                 printColor(green, " ***** V_CELLS_K_N_P_NEW_LEFT: %w \n",[V_CELLS_K_N_P_NEW_LEFT]),
                 setval( real_cells_know_no_pit, V_CELLS_K_N_P_NEW_LEFT ),
                 setval( wm_cells_know_no_pit, V_CELLS_K_N_P_NEW_LEFT )
                ;
                true ),
            ( ( member(NeighbourUpper, V_CELLS_POS),
%                printColor( red, " *****  NeighbourUpper: %w \n", [NeighbourUpper]),
%                printColor( red, " *****  V_CELLS_POS: %w \n", [V_CELLS_POS]),
                getval( real_cells_know_no_pit, V_CELLS_K_N_P_UPPER ),
%                printColor( red, " *****  V_CELLS_K_N_P_UPPER: %w \n", [V_CELLS_K_N_P_UPPER]),
                 not member(NeighbourUpper, V_CELLS_K_N_P_UPPER) )
              -> append( V_CELLS_K_N_P_UPPER, [NeighbourUpper], V_CELLS_K_N_P_NEW_UPPER),
%                 printColor(green, " ***** appending %w \n",[NeighbourUpper]),
%                 printColor(green, " ***** V_CELLS_K_N_P_NEW_UPPER: %w \n",[V_CELLS_K_N_P_NEW_UPPER]),
                 setval( real_cells_know_no_pit, V_CELLS_K_N_P_NEW_UPPER ),
                 setval( wm_cells_know_no_pit, V_CELLS_K_N_P_NEW_UPPER )
                 ;
                 true )
            ;
            true )
         ), %% end foreach( [NotPitX, NotPitY], V_CELLS_POS )
        getval( real_cells_know_no_pit, V_CELLS_K_N_P_TEST),
        printColor( red, " I know that here are *no* pits: %w \n", [V_CELLS_K_N_P_TEST]),

        %getval( real_cells_know_pit, V_CELLS_K_P),
        %setval( wm_cells_know_pit, V_CELLS_K_P ),
        
        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
        %%  Diagnostic rules to infer the position of the wumpus(not yet)/no wumpus  %%
        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%        getval( real_cells_know_no_wumpus, V_CELLS_K_N_W ),
        % cells that have been visited and are free of stench, have no wumpus as neighbour
        get_all_positions( V_CELLS_POS ),
        % printColor( red, " All positions: %w \n", [V_CELLS_POS]),
        getval( real_cells_know_no_wumpus, V_CELLS_K_N_W_NEW ),
        setval( real_cells_know_no_wumpus, V_CELLS_K_N_W_NEW),
        setval( wm_cells_know_no_wumpus, V_CELLS_K_N_W_NEW ),

        getval( real_wumpus_alive, V_WUMPUS_ALIVE ),
        ( V_WUMPUS_ALIVE = "T" -> setval( wm_wumpus_alive, true )
                                  ;
                                  setval( wm_wumpus_alive, false ) ),

        ( ( V_WUMPUS_ALIVE = "T" ) ->

        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%  If the wumpus is still alive do the following:
        ( foreach( [NotWumpusX, NotWumpusY], V_CELLS_POS ) do
          ( ( getval( real_cells_smelly, V_CELLS_SME ),
              not member([NotWumpusX, NotWumpusY], V_CELLS_SME),
              getval( real_cells_visited, V_CELLS_VIS ),
              member([NotWumpusX, NotWumpusY], V_CELLS_VIS),
              East is NotWumpusX+1,
              South is NotWumpusY-1,
              West is NotWumpusX-1,
              North is NotWumpusY+1,
              NeighbourRight = [East, NotWumpusY],
              NeighbourLower = [NotWumpusX, South],
              NeighbourLeft = [West, NotWumpusY],
              NeighbourUpper = [NotWumpusX, North] )            
            ->
        get_all_positions( V_CELLS_POS ),
            ( ( member(NeighbourRight, V_CELLS_POS),
                getval( real_cells_know_no_wumpus, V_CELLS_K_N_W_RIGHT ),
                not member(NeighbourRight, V_CELLS_K_N_W_RIGHT) )
              -> append( V_CELLS_K_N_W_RIGHT, [NeighbourRight], V_CELLS_K_N_W_NEW_RIGHT),
                 setval( real_cells_know_no_wumpus, V_CELLS_K_N_W_NEW_RIGHT ),
                 setval( wm_cells_know_no_wumpus, V_CELLS_K_N_W_NEW_RIGHT )
                 ;
                 true ),
            ( ( member(NeighbourLower, V_CELLS_POS),
                getval( real_cells_know_no_wumpus, V_CELLS_K_N_W_LOWER ),
                not member(NeighbourLower, V_CELLS_K_N_W_LOWER) )
              -> append( V_CELLS_K_N_W_LOWER, [NeighbourLower], V_CELLS_K_N_W_NEW_LOWER),
                 setval( real_cells_know_no_wumpus, V_CELLS_K_N_W_NEW_LOWER ),
                 setval( wm_cells_know_no_wumpus, V_CELLS_K_N_W_NEW_LOWER )
                 ;
                 true ),
            ( ( member(NeighbourLeft, V_CELLS_POS), 
                getval( real_cells_know_no_wumpus, V_CELLS_K_N_W_LEFT ),
                member(NeighbourLeft, V_CELLS_POS),
                not member(NeighbourLeft, V_CELLS_K_N_W_LEFT) )
              ->  append( V_CELLS_K_N_W_LEFT, [NeighbourLeft], V_CELLS_K_N_W_NEW_LEFT),
                 setval( real_cells_know_no_wumpus, V_CELLS_K_N_W_NEW_LEFT ),
                 setval( wm_cells_know_no_wumpus, V_CELLS_K_N_W_NEW_LEFT )
                ;
                true ),
            ( ( member(NeighbourUpper, V_CELLS_POS),
                getval( real_cells_know_no_wumpus, V_CELLS_K_N_W_UPPER ),
                 not member(NeighbourUpper, V_CELLS_K_N_W_UPPER) )
              -> append( V_CELLS_K_N_W_UPPER, [NeighbourUpper], V_CELLS_K_N_W_NEW_UPPER),
                 setval( real_cells_know_no_wumpus, V_CELLS_K_N_W_NEW_UPPER ),
                 setval( wm_cells_know_no_wumpus, V_CELLS_K_N_W_NEW_UPPER )
                 ;
                 true )
            ;
            true )
        ) %% end foreach ( [NotWumpusX, NotWumpusY], V_CELLS_POS )

        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%  Else if the wumpus is dead already do this instead:
        ;
        printColor( red, " I think the Wumpus is dead \n", []),
        setval( real_cells_know_no_wumpus, V_CELLS_POS ), %% V_CELLS_POS contains all valid positions
        setval( wm_cells_know_no_wumpus, V_CELLS_POS )
        
        ), %% end (V_WUMPUS_ALIVE = "T") -> do; else

        getval( real_cells_know_no_wumpus, V_CELLS_K_N_W_TEST),
        printColor( red, " I know that here is *no* wumpus: %w \n", [V_CELLS_K_N_W_TEST]),

        %getval( real_cells_know_wumpus, V_CELLS_K_W ),
        %setval( wm_cells_know_wumpus, V_CELLS_K_W ),

        getval( real_agent_arrow, V_AGENT_ARROW ),
        ( V_AGENT_ARROW = "T" ->  setval( wm_agent_arrow, true)
                                  ;
                                  setval( wm_agent_arrow, false)  ),

        getval( real_wumpus_pos, V_WUMPUS_POS ),
        setval( wm_wumpus_pos, V_WUMPUS_POS ),

        %getval( real_wumpus_alive, V_WUMPUS_ALIVE ), %% done above
        %setval( wm_wumpus_alive, V_WUMPUS_ALIVE ),   %% done above

        getval( real_gold_pos, V_GOLD_POS ),
        setval( wm_gold_pos, V_GOLD_POS ),

        getval( real_carry_gold, V_CARRY_GOLD ),
        setval( wm_carry_gold, V_CARRY_GOLD ),

        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
        %%  Diagnostic rules to deduce whether the agent can reach the gold safely  %%
        %%  according what he has learned about the environment                     %%
        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

        /** The "margin" are those cells that are shaded (unknown) and at the same time 
         *  are neighbours of already explored cells
         */
	getval( real_cells_visited, M_CELLS_VIS ),
        getval( real_cells_shaded, M_CELLS_SHA ),
        findall([MarginX, MarginY],
                ( member([ExploredX, ExploredY], M_CELLS_VIS),
%                  [ExploredX, ExploredY] = [MarginX, MarginY]
                  ExploredXEast is ExploredX + 1,
                  ExploredYSouth is ExploredY - 1,
                  ExploredXWest is ExploredX - 1,
                  ExploredYNorth is ExploredY + 1,
                  ( (is_pos(ExploredXEast, ExploredY),
                     member([ExploredXEast, ExploredY], M_CELLS_SHA),
                     [ExploredXEast, ExploredY] = [MarginX, MarginY] );
                    (is_pos(ExploredX, ExploredYSouth),
                     member([ExploredX, ExploredYSouth], M_CELLS_SHA),
                     [ExploredX, ExploredYSouth] = [MarginX, MarginY] );
                    (is_pos(ExploredXWest, ExploredY),
                     member([ExploredXWest, ExploredY], M_CELLS_SHA),
                     [ExploredXWest, ExploredY] = [MarginX, MarginY] );
                    (is_pos(ExploredX, ExploredYNorth),
                     member([ExploredX, ExploredYNorth], M_CELLS_SHA),
                     [ExploredX, ExploredYNorth] = [MarginX, MarginY] )
                   )
                ),                
                MARGIN),
%         printColor(blue, " ***** MARGIN:: %w \n",[MARGIN]),

        /** As the agent has an aversion against risk he should give up searching for the gold <=>
         *  he doesn't know any safe cell to explore in the margin <=>
         *  there is no cell that's in the lists real_cells_know_no_wumpus && real_cells_know_no_pit.
         *  ( not really sure about the "<=>"... what if there's information of more than one neighbour
         *  of the margin cell? ).
         *  If the wumpus dies, the list real_cells_know_no_wumpus is maximized (see above), which possibly will
         *  lead to an increase of safe cells.
         */
%        getval( real_gold_reachable, V_GOLD_REACHABLE ),
        findall([SafeX, SafeY],
                ( member([SafeX, SafeY], MARGIN),
                  member([SafeX, SafeY], V_CELLS_K_N_P_TEST),
                  member([SafeX, SafeY], V_CELLS_K_N_W_TEST)
                ),
                SAFE),
 
        printColor( red, " I know that these cells are safe to be explored: %w \n", [SAFE]),
        ( ( length(SAFE,0) ) -> setval( wm_gold_reachable, "F" ),
                                printColor(red, " ***** GOLD IS NOT REACHABLE SAFELY ANY MORE! \n", [])
                                ;
                                setval( wm_gold_reachable, "T" )
        ),

        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
        %%  compute the Manhattan distance from the agent to the closest  %%
        %%  cell that is safe to be explored                              %%
        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

        /** value should be set dynamically depending on the size of the grid world */
        setval( real_distance_to_closest_safe_cell, 8 ),
        setval( wm_distance_to_closest_safe_cell, 8 ),
%        Candidate is V_MANHATTAN,
%        printColor( green, " Candidate before: %w \n", [Candidate]),
        ( foreach([X,Y], SAFE) do
          /** compute Manhattan distance to agent */
%           printColor( green, " trying: %w \n", [[X,Y]]),
           getval( real_distance_to_closest_safe_cell, V_MANHATTAN ),
           getval( real_agent_pos, [AgentX, AgentY] ),
           DistX is (X - AgentX),
           DistY is (Y - AgentY),
           abs(DistX, AbsDistX),
           abs(DistY, AbsDistY),
           Manhattan is (AbsDistX + AbsDistY),
%           printColor( green, " Manhattan is: %w \n", [Manhattan]),
           ( (Manhattan < V_MANHATTAN) -> setval( real_distance_to_closest_safe_cell, Manhattan ),
                                          setval( wm_distance_to_closest_safe_cell, Manhattan )
                                          ;
                                          setval( real_distance_to_closest_safe_cell, V_MANHATTAN ),
                                          setval( wm_distance_to_closest_safe_cell, V_MANHATTAN ) )
        ),
%        printColor( green, " Candidate afterwards: %w \n", [Candidate]),

	
        printf(" *** exogf_Update DONE. *** \n", []), flush(output).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  eXecution Transformation of         %%
%%  primitive actions in wumpus.        %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  include calls to drawing-routines

%% %%%%%%%%%%%%%%%%%%%%% %%
%% deterministic execution

xTra(go_forward,_H) :-
        printColor( pink, " xTra: EXEC 'go_forward'", []),
        getval( real_agent_pos, [X,Y] ),
        printf(" at %w,%w ", [X,Y]),
        getval( real_agent_direction, D ),
        printf(" facing %w ", [D]),
        getval( real_agent_arrow, A),
        printf(" has arrow: %w \n", [A]),
        execdelay,
        ( D="east" -> Xn is X+1,
                      Yn is Y,
                      setval( real_agent_pos, [Xn,Yn] ),
                      ( getval(real_carry_gold,true) -> setval( real_gold_pos, [Xn,Yn] ); true ),
                      printf(" draw_wumpus_hunter(%w,%w,%w,%w) \n", [Xn, Yn, "east", A]),
                      draw_wumpus_hunter(Xn, Yn, "east", A),
                      draw_action("R", X, Y),
                      getval(real_cells_visited, RCV_east),
                      ( not member([Xn,Yn], RCV_east) -> append(RCV_east, [[Xn,Yn]], RCVN_east),
                                                         setval(real_cells_visited, RCVN_east),
                                                         setval(wm_cells_visited, RCVN_east)
                                                         ;
                                                         true ),
%                      getval(real_cells_visited, RCVC_east),
%                      printf(" the following cells have been visited: %w \n", [RCVC_east]),
                      getval(real_cells_shaded, RCS_east),
                      ( member([Xn,Yn], RCS_east) -> delete([Xn,Yn], RCS_east, RCSN_east),
                                                     setval(real_cells_shaded, RCSN_east),
                                                     setval(wm_cells_shaded, RCSN_east)
                                                     ;
                                                     true ),
                      getval(real_cells_shaded, RCSC_east),
%                      printf(" the following cells are shaded: %w \n", [RCSC_east]),                    
                      update_shades(RCSC_east),
%%                       /** count faced shades */
%                       printColor(blue, " counting faced shades... \n",[]),
%%                       getval( real_cells_shaded, CS_EAST ),
%                       printColor(blue, " real_cells_shaded: %w \n",[CS_EAST]),
                       world( XMin, YMin, XMax, YMax ),
                              findall([Xs, Y],
                                      ( XEast is X+1,
                                        between(XEast, XMax, 1, Xs),
                                        is_pos(Xs,Y),
                                        getval(real_cells_visited, RCV_facing_east),
                                        not member([Xs, Y], RCV_facing_east)
                                      ),
                                      SHADES_EAST),
%                       printColor(blue, " SHADES_EAST: %w \n",[SHADES_EAST]),
                       length(SHADES_EAST, V_EAST),
                       setval( real_num_facedShades, V_EAST), 
                       setval( wm_num_facedShades, V_EAST)                    
                       ;
                       true ),
        ( D="south" -> Xn is X,
                       Yn is Y-1,
                       setval( real_agent_pos, [Xn,Yn] ),
                       ( getval(real_carry_gold,true) -> setval( real_gold_pos, [Xn,Yn] ); true ),
                       printf(" draw_wumpus_hunter(%w,%w,%w,%w) \n", [Xn, Yn, "south", A]),
                       draw_wumpus_hunter(Xn, Yn, "south", A),
                       draw_action("D", X, Y),
                       getval(real_cells_visited, RCV_south),
                       ( not member([Xn,Yn], RCV_south) -> append(RCV_south, [[Xn,Yn]], RCVN_south),
                                                           setval(real_cells_visited, RCVN_south),
                                                           setval(wm_cells_visited, RCVN_south)
                                                           ;
                                                           true ),
%                       getval(real_cells_visited, RCVC_south),
%                       printf(" the following cells have been visited: %w \n", [RCVC_south]),
                       getval(real_cells_shaded, RCS_south),
                       ( member([Xn,Yn], RCS_south) -> delete([Xn,Yn], RCS_south,  RCSN_south),
                                                       setval(real_cells_shaded, RCSN_south),
                                                       setval(wm_cells_shaded, RCSN_south)
                                                       ;
                                                       true ),
                       getval(real_cells_shaded, RCSC_south),
%                       printf(" the following cells are shaded: %w \n", [RCSC_south]),                    
                       update_shades(RCSC_south),
%%                       /** count faced shades */
%%                       getval( real_cells_shaded, CS_SOUTH ),
                       world( XMin, YMin, XMax, YMax ),
                              findall([X, Ys],
                                      ( YSouth is Y-1,
                                        between(YMin, YSouth, 1, Ys),
                                        is_pos(X,Ys),
                                        getval(real_cells_visited, RCV_facing_south),
                                        not member([X, Ys], RCV_facing_south)
                                      ),
                                      SHADES_SOUTH),
                       length(SHADES_SOUTH, V_SOUTH),
                       setval( real_num_facedShades, V_SOUTH), 
                       setval( wm_num_facedShades, V_SOUTH)                    
                       ;
                       true ),
        ( D="west" -> Xn is X-1,
                      Yn is Y,
                      setval( real_agent_pos, [Xn,Yn] ),
                      ( getval(real_carry_gold,true) -> setval( real_gold_pos, [Xn,Yn] ); true ),
                       printf(" draw_wumpus_hunter(%w,%w,%w,%w) \n", [Xn, Yn, "west", A]),
                      draw_wumpus_hunter(Xn, Yn, "west", A),
                      draw_action("L", X, Y),
                      getval(real_cells_visited, RCV_west),
                      ( not member([Xn,Yn], RCV_west) -> append(RCV_west, [[Xn,Yn]], RCVN_west),
                                                         setval(real_cells_visited, RCVN_west),
                                                         setval(wm_cells_visited, RCVN_west)
                                                         ;
                                                         true ),
%                      getval(real_cells_visited, RCVC_west),
%                      printf(" the following cells have been visited: %w \n", [RCVC_west]),
                      getval(real_cells_shaded, RCS_west),
                      ( member([Xn,Yn], RCS_west) -> delete([Xn,Yn], RCS_west, RCSN_west),
                                                     setval(real_cells_shaded, RCSN_west),
                                                     setval(wm_cells_shaded, RCSN_west)
                                                     ;
                                                     true ),
                      getval(real_cells_shaded, RCSC_west),
%                      printf(" the following cells are shaded: %w \n", [RCSC_west]),                    
                      update_shades(RCSC_west),
%%                      /** count faced shades */
%%                      getval( real_cells_shaded, CS_WEST ),
                      world( XMin, YMin, XMax, YMax ),
                             findall([Xs, Y],
                                     ( XWest is X-1,
                                       between(XMin, XWest, 1, Xs),
                                       is_pos(Xs,Y),
                                       getval(real_cells_visited, RCV_facing_west),
                                       not member([Xs, Y], RCV_facing_west)
                                     ),
                                     SHADES_WEST),
                      length(SHADES_WEST, V_WEST),
                      setval( real_num_facedShades, V_WEST), 
                      setval( wm_num_facedShades, V_WEST)                    
                      ;
                      true ),
        ( D="north" -> Xn is X,
                       Yn is Y+1,
                       setval( real_agent_pos, [Xn,Yn] ),
                       ( getval(real_carry_gold,true) -> setval( real_gold_pos, [Xn,Yn] ); true ),
                       printf(" draw_wumpus_hunter(%w,%w,%w,%w) \n", [Xn, Yn, "north", A]),
                       draw_wumpus_hunter(Xn, Yn, "north", A),
                       draw_action("U", X, Y),
                       getval(real_cells_visited, RCV_north),
                       ( not member([Xn,Yn], RCV_north) -> append(RCV_north, [[Xn,Yn]], RCVN_north),
                                                           setval(real_cells_visited, RCVN_north),
                                                           setval(wm_cells_visited, RCVN_north)
                                                           ;
                                                           true ),
%                       getval(real_cells_visited, RCVC_north),
%                       printf(" the following cells have been visited: %w \n", [RCVC_north]),
                       getval(real_cells_shaded, RCS_north),
                       ( member([Xn,Yn], RCS_north) ->  delete([Xn,Yn], RCS_north, RCSN_north),
                                                        setval(real_cells_shaded, RCSN_north),
                                                        setval(wm_cells_shaded, RCSN_north)
                                                        ;
                                                        true ),
                       getval(real_cells_shaded, RCSC_north),
%                       printf(" the following cells are shaded: %w \n", [RCSC_north]),                    
                       update_shades(RCSC_north),
%%                       /** count faced shades */
%%                       getval( real_cells_shaded, CS_WEST ),
                       world( XMin, YMin, XMax, YMax ),
                              findall([X, Ys],
                                      ( YNorth is Y+1,
                                        between(YNorth, YMax, 1, Ys),
                                        is_pos(X,Ys),
                                        getval(real_cells_visited, RCV_facing_north),
                                        not member([X, Ys], RCV_facing_north)
                                      ),
                                      SHADES_NORTH),
                       length(SHADES_NORTH, V_NORTH),
                       setval( real_num_facedShades, V_NORTH), 
                       setval( wm_num_facedShades, V_NORTH)                    
                       ;
                       true ).
        
xTra(turn_left,_H) :-
        printColor( pink, " xTra: EXEC 'turn_left'", []),
        getval( real_agent_pos, [X,Y] ),
        printf(" at %w,%w ", [X,Y]),
        getval( real_agent_direction, D ),
        printf(" facing %w ", [D]),
        getval( real_agent_arrow, A),
        printf(" has arrow: %w \n", [A]),
        execdelay,
%        process_xtra_events,
        ( D="east" -> setval(real_agent_direction, "north"),
                      setval(wm_agent_direction, north),
                      printf(" draw_wumpus_hunter(%w,%w,%w,%w) \n", [X, Y, "north", A]),
                      draw_wumpus_hunter(X, Y, "north", A),
                      draw_action("1", X, Y),
%%                      /** count faced shades */
%%                      getval( real_cells_shaded, CS_NORTH ),
                      world( XMin, YMin, XMax, YMax ),
                             findall([X, Ys],
                                     ( YNorth is Y+1,
                                       between(YNorth, YMax, 1, Ys),
                                       is_pos(X,Ys),
                                       getval(real_cells_visited, RCV_north),
                                       not member([X, Ys], RCV_north)
                                     ),
                                     SHADES_NORTH),
                      length(SHADES_NORTH, V_NORTH),
                      setval( real_num_facedShades, V_NORTH), 
                      setval( wm_num_facedShades, V_NORTH)                    
                      ;
                      true ),
        ( D="south" -> setval(real_agent_direction, "east"),
                       setval(wm_agent_direction, east),
                       printf(" draw_wumpus_hunter(%w,%w,%w,%w) \n", [X, Y, "east", A]),
                       draw_wumpus_hunter(X, Y, "east", A),
                       draw_action("1", X, Y),
%%                       /** count faced shades */
%%                       getval( real_cells_shaded, CS_EAST ),
                       world( XMin, YMin, XMax, YMax ),
                              findall([Xs, Y],
                                      ( XEast is X+1,
                                        between(XEast, XMax, 1, Xs),
                                        is_pos(Xs,Y),
                                        getval(real_cells_visited, RCV_east),
                                        not member([Xs, Y], RCV_east)
                                      ),
                                      SHADES_EAST),
                       length(SHADES_EAST, V_EAST),
                       setval( real_num_facedShades, V_EAST), 
                       setval( wm_num_facedShades, V_EAST)                    
                       ;
                       true ),
        ( D="west" -> setval(real_agent_direction, "south"),
                      setval(wm_agent_direction, south),
                      printf(" draw_wumpus_hunter(%w,%w,%w,%w) \n", [X, Y, "south", A]),
                      draw_wumpus_hunter(X, Y, "south", A),
                      draw_action("1", X, Y),
%%                      /** count faced shades */
%%                      getval( real_cells_shaded, CS_SOUTH ),
                      world( XMin, YMin, XMax, YMax ),
                             findall([X, Ys],
                                     ( YSouth is Y-1,
                                       between(YMin, YSouth, 1, Ys),
                                       is_pos(X,Ys),
                                       getval(real_cells_visited, RCV_south),
                                       not member([X, Ys], RCV_south)
                                     ),
                                     SHADES_SOUTH),
                      length(SHADES_SOUTH, V_SOUTH),
                      setval( real_num_facedShades, V_SOUTH), 
                      setval( wm_num_facedShades, V_SOUTH)                    
                      ;
                      true ),
        ( D="north" -> setval(real_agent_direction, "west"),
                       setval(wm_agent_direction, west),
                       printf(" draw_wumpus_hunter(%w,%w,%w,%w) \n", [X, Y, "west", A]),
                       draw_wumpus_hunter(X, Y, "west", A),
                       draw_action("1", X, Y),
%%                       /** count faced shades */
%%                       getval( real_cells_shaded, CS_WEST ),
                       world( XMin, YMin, XMax, YMax ),
                              findall([Xs, Y],
                                      ( XWest is X-1,
                                        between(XMin, XWest, 1, Xs),
                                        is_pos(Xs,Y),
                                        getval(real_cells_visited, RCV_west),
                                        not member([Xs, Y], RCV_west)
                                      ),
                                      SHADES_WEST),
                       length(SHADES_WEST, V_WEST),
                       setval( real_num_facedShades, V_WEST), 
                       setval( wm_num_facedShades, V_WEST)                    
                       ;
                       true ).
        
xTra(turn_right,_H) :-
        printColor( pink, " xTra: EXEC 'turn_left'", []),
        getval( real_agent_pos, [X,Y] ),
        printf(" at %w,%w ", [X,Y]),
        getval( real_agent_direction, D ),
        printf(" facing %w ", [D]),
        getval( real_agent_arrow, A),
        printf(" has arrow: %w \n", [A]),
        execdelay,
        ( D="east" -> setval(real_agent_direction, "south"),
                      setval(wm_agent_direction, south),
                      printf(" draw_wumpus_hunter(%w,%w,%w,%w) \n", [X, Y, "south",A]),
                      draw_wumpus_hunter(X, Y, "south",A),
                      draw_action("2", X, Y),
%%                      /** count faced shades */
%%                      getval( real_cells_shaded, CS_SOUTH ),
                      world( XMin, YMin, XMax, YMax ),
                             findall([X, Ys],
                                     ( YSouth is Y-1,
                                       between(YMin, YSouth, 1, Ys),
                                       is_pos(X,Ys),
                                       getval(real_cells_visited, RCV_south),
                                       not member([X, Ys], RCV_south)
                                     ),
                                     SHADES_SOUTH),
                      length(SHADES_SOUTH, V_SOUTH),
                      setval( real_num_facedShades, V_SOUTH), 
                      setval( wm_num_facedShades, V_SOUTH)                    
                      ;
                      true ),
        ( D="south" -> setval(real_agent_direction, "west"),
                       setval(wm_agent_direction, west),
                       printf(" draw_wumpus_hunter(%w,%w,%w,%w) \n", [X, Y, "west", A]),
                       draw_wumpus_hunter(X, Y, "west", A),
                       draw_action("2", X, Y),
%%                       /** count faced shades */
%%                       getval( real_cells_shaded, CS_WEST ),
                       world( XMin, YMin, XMax, YMax ),
                              findall([Xs, Y],
                                      ( XWest is X-1,
                                        between(XMin, XWest, 1, Xs),
                                        is_pos(Xs,Y),
                                        getval(real_cells_visited, RCV_west),
                                        not member([Xs, Y], RCV_west)
                                      ),
                                      SHADES_WEST),
                       length(SHADES_WEST, V_WEST),
                       setval( real_num_facedShades, V_WEST), 
                       setval( wm_num_facedShades, V_WEST)                    
                       ;
                       true ),
        ( D="west" -> setval(real_agent_direction, "north"),
                      setval(wm_agent_direction, north),
                      printf(" draw_wumpus_hunter(%w,%w,%w,%w) \n", [X, Y, "north", A]),
                      draw_wumpus_hunter(X, Y, "north", A),
                      draw_action("2", X, Y),
%%                      /** count faced shades */
%%                      getval( real_cells_shaded, CS_NORTH ),
                      world( XMin, YMin, XMax, YMax ),
                             findall([X, Ys],
                                     ( YNorth is Y+1,
                                       between(YNorth, YMax, 1, Ys),
                                       is_pos(X,Ys),
                                       getval(real_cells_visited, RCV_north),
                                       not member([X, Ys], RCV_north)
                                     ),
                                     SHADES_NORTH),
                      length(SHADES_NORTH, V_NORTH),
                      setval( real_num_facedShades, V_NORTH), 
                      setval( wm_num_facedShades, V_NORTH)                    
                      ;
                      true ),
        ( D="north" -> setval(real_agent_direction, "east"),
                       setval(wm_agent_direction, east),
                       printf(" draw_wumpus_hunter(%w,%w,%w,%w) \n", [X, Y, "east", A]),
                       draw_wumpus_hunter(X, Y, "east", A),
                       draw_action("2", X, Y),
%%                       /** count faced shades */
%%                       getval( real_cells_shaded, CS_EAST ),
                       world( XMin, YMin, XMax, YMax ),
                              findall([Xs, Y],
                                      ( XEast is X+1,
                                        between(XEast, XMax, 1, Xs),
                                        is_pos(Xs,Y),
                                        getval(real_cells_visited, RCV_east),
                                        not member([Xs, Y], RCV_east)
                                      ),
                                      SHADES_EAST),
                       length(SHADES_EAST, V_EAST),
                       setval( real_num_facedShades, V_EAST), 
                       setval( wm_num_facedShades, V_EAST)                    
                       ;
                       true ).

xTra(shoot,_H) :-
        printColor( pink, " xTra: EXEC 'shoot'", []),
        setval(real_agent_arrow, "F"),
        setval(wm_agent_arrow, false),
        world( XMin, YMin, XMax, YMax ),
        getval( real_agent_pos, [X,Y]),
        getval( real_agent_direction, D),
%%        getval( real_wumpus_pos, [WX, WY]),
        /** animation of flying arrow: */
        execdelay,
        ( D="east" ->  findall(XA_east, between(X, XMax, 1, XA_east), XLIST_east),
                       ( foreach( XA_east, XLIST_east ) do
                         getval( real_agent_pos, [_X_east,Y_east]),
                         getval( real_wumpus_pos, [WX_east, WY_east]),
                         is_pos(XA_east,Y_east),
                         draw_arrow(XA_east, Y_east, "east"),
                         execdelay, /** window refresh hides arrow :( */
                         ( [WX_east, WY_east] = [XA_east,Y_east] -> setval(real_wumpus_alive, "F"),
                                                                    setval(wm_wumpus_alive, false),
                                                                    printf(" Wumpus was killed \n",[]),
                                                                    getval(real_wumpus_alive, A_east),
                                                                    draw_wumpus(XA_east,Y_east,A_east)
                                                                    ;
                                                                    true 
                         )
                       )
                       ;
                       true ),
        ( D="south" -> findall(YA_south, between(YMin, Y, 1, YA_south), YLIST_south),
                       ( foreach( YA_south, YLIST_south ) do
                         getval( real_agent_pos, [X_south, _Y_south]),
                         getval( real_wumpus_pos, [WX_south, WY_south]),
                         is_pos(X_south, YA_south),
                         draw_arrow(X_south, YA_south, "south"),
                         execdelay,
                         ( [WX_south, WY_south] = [X_south, YA_south] -> setval(real_wumpus_alive, "F"),
                                                                         setval(wm_wumpus_alive, false),
                                                                         printf(" Wumpus was killed \n",[]),
                                                                         getval(real_wumpus_alive, A_south),
                                                                         draw_wumpus(X_south, YA_south, A_south)
                                                                         ;
                                                                         true
                         )
                       )
                       ;
                       true ),
        ( D="west" ->  findall(XA_west, between(XMin, X, 1, XA_west), XLIST_west),
                       ( foreach( XA_west, XLIST_west ) do
                         getval( real_agent_pos, [_X_west,Y_west]),
                         getval( real_wumpus_pos, [WX_west, WY_west]),
                         is_pos(XA_west,Y_west),
                         draw_arrow(XA_west, Y_west, "west"),
                         execdelay, % window refresh hides arrow :(
                         ( [WX_west, WY_west] = [XA_west,Y_west] -> setval(real_wumpus_alive, "F"),
                                                                    setval(wm_wumpus_alive, false),
                                                                    printf(" Wumpus was killed \n",[]),
                                                                    getval(real_wumpus_alive, A_west),
                                                                    draw_wumpus(XA_west,Y_west,A_west)
                                                                    ;
                                                                    true 
                         )
                       )
                       ;
                       true ),
        ( D="north" -> findall(YA_north, between(Y, YMax, 1, YA_north), YLIST_north),
                       ( foreach( YA_north, YLIST_north) do
                         getval( real_agent_pos, [X_north, _Y_north]),
                         getval( real_wumpus_pos, [WX_north, WY_north]),
                         is_pos(X_north, YA_north),
                         draw_arrow(X_north, YA_north, "north"),
                         execdelay,
                         ( [WX_north, WY_north] = [X_north, YA_north] -> setval(real_wumpus_alive, "F"),
                                                                         setval(wm_wumpus_alive, false),
                                                                         printf(" Wumpus was killed \n",[]),
                                                                         getval(real_wumpus_alive, A_north),
                                                                         draw_wumpus(X_north, YA_north, A_north)
                                                                         ;
                                                                         true
                         )
                       )
                       ;
                       true ).
          

xTra(rest,_H)  :- 
	printColor( pink, " xTra: EXEC 'rest'", []), 
        getval( real_agent_pos, [X,Y] ),
        printf(" at %w,%w ", [X,Y]),
        getval( real_agent_direction, D ),
        printf(" facing %w ", [D]),
        getval( real_agent_arrow, A),
        printf(" has arrow: %w \n", [A]),
        execdelay,
	draw_action("0", X, Y).

xTra(noop,_H)  :- 
	printColor( pink, " xTra: EXEC 'noop'", []), 
        getval( real_agent_pos, [X,Y] ),
        printf(" at %w,%w ", [X,Y]),
        getval( real_agent_direction, D ),
        printf(" facing %w ", [D]),
        getval( real_agent_arrow, A),
        printf(" has arrow: %w \n", [A]),
        execdelay,
	draw_action("0", X, Y).

xTra(pickup_gold,_H) :- 
	printColor( pink, " xTra: EXEC 'pickup_gold'", []), 
        getval( real_agent_pos, [X,Y] ),
        printf(" at %w,%w ", [X,Y]),
        getval( real_agent_direction, D ),
        printf(" facing %w ", [D]),
        getval( real_agent_arrow, A),
        printf(" has arrow: %w \n", [A]),
        execdelay,
	setval(real_carry_gold, true),
        setval(wm_carry_gold, true).

/** Randomly distribute 1 to 3 pits in the world.
 *  The Seed allows for reproducibility of configurations. */
xTra(distribute_pits(Seed), _H) :- !,
        random_number(Seed, 1, 3, NumberOfPits),
        printf("Number of pits is: %w\n", [NumberOfPits]),
        /** Distribute recursively till NumberOfPits is 0.
         *  Implemented in wumpus.readylog. */
        distribute_pits_aux(NumberOfPits, Seed),
        findall( [X,Y],
                 pit(X,Y),
                 PitList ),
        print_list(PitList),
        printf("update_pits...", []),
        update_pits(PitList),
        printf(" successful.\n", []),
        redraw.

/** Re-compute the position of breezes. */
xTra(distribute_breezes, _H) :- !,
%        /** update_breezes_aux implemented
%         *  in wumpus.readylog */
%        update_breezes_aux,
        printf("Retracting all breezes.\n", []),
        retract_all(breeze(_,_)),
        findall( [X,Y],
                 ( world( XMin, YMin, XMax, YMax ),
                   between(XMin, XMax, 1, X),
                   between(YMin, YMax, 1, Y),
                   not is_pit(X,Y),
                   /** horizontal and vertical neighbour cells of
                    *  pits are breezy */
                   XW is X-1, XE is X+1, YN is Y+1, YS is Y-1,
                   ( is_pit(XW, Y);
                     is_pit(X, YS);
                     is_pit(XE, Y);
                     is_pit(X, YN)
                   )
                 ),
                 BreezeList
               ),
%        print_list(BreezeList),
        setval( real_cells_breezy, BreezeList ),
        ( foreach([X,Y], BreezeList) do
              printf("Asserting breeze(%w, %w).\n", [X,Y]),
              assert(breeze(X,Y))
        ),
        printf("update_breezes...", []),
        update_breezes(BreezeList),
        printf(" successful.\n", []),
        redraw.

/** Re-compute the position of stenches. */
xTra(distribute_stenches, _H) :- !,
        printf("Retracting all stenches.\n", []),
        retract_all(stench(_,_)),
        findall( [X,Y],
                 ( world( XMin, YMin, XMax, YMax ),
                   between(XMin, XMax, 1, X),
                   between(YMin, YMax, 1, Y),
                   not is_pit(X,Y),
                   /** horizontal and vertical neighbour cells of
                    *  pits are breezy */
                   XW is X-1, XE is X+1, YN is Y+1, YS is Y-1,
                   ( is_wumpus(XW, Y);
                     is_wumpus(X, YS);
                     is_wumpus(XE, Y);
                     is_wumpus(X, YN)
                   )
                 ),
                 StenchList
               ),
%        print_list(StenchList),
        setval( real_cells_smelly, StenchList ),
        ( foreach([X,Y], StenchList) do
              printf("Asserting stench(%w, %w).\n", [X,Y]),
              assert(stench(X,Y))
        ),
        printf("update_stenches...", []),
        update_stenches(StenchList),
        printf(" successful.\n", []),
        redraw.

/** Randomly place the wumpus.
 *  The Seed allows for reproducibility of placement. */
xTra(place_wumpus(Seed), _H) :- !,
        world( XMin, YMin, XMax, YMax),
        SeedTmp1 is (Seed * 23), 
        mod( SeedTmp1, 2147483647, NewSeed1),
        random_number(NewSeed1, XMin, XMax, PosX),
        SeedTmp2 is (NewSeed1 * 23), 
        mod( SeedTmp2, 2147483647, NewSeed2),
        random_number(NewSeed2, YMin, YMax, PosY),
        printf("Trying to put wumpus to [%w, %w]\n", [PosX, PosY]),
        ( pit(PosX, PosY) ->
              /** There's a pit already, try again */
              place_wumpus(NewSeed2)
        ;
              printf("Placing wumpus at [%w,%w].\n", [PosX,PosY]),
              setval( real_wumpus_pos, [PosX, PosY] )
        ),
        redraw.

/** Randomly place the agent (and the start/exit cell).
 *  The Seed allows for reproducibility of placement. */
xTra(place_agent(Seed), _H) :- !,
        world( XMin, YMin, XMax, YMax),
        SeedTmp1 is (Seed * 23), 
        mod( SeedTmp1, 2147483647, NewSeed1),
        random_number(NewSeed1, XMin, XMax, PosX),
        SeedTmp2 is (NewSeed1 * 23), 
        mod( SeedTmp2, 2147483647, NewSeed2),
        random_number(NewSeed2, YMin, YMax, PosY),
        printf("Trying to put agent to [%w, %w]\n", [PosX, PosY]),
        getval( real_wumpus_pos, [WumpusX, WumpusY] ),
        ( ( pit(PosX, PosY); [WumpusX, WumpusY] = [PosX, PosY] ) ->
              /** There's a pit or the wumpus already, try again */
              place_agent(NewSeed2)
        ;
              printf("Placing agent at [%w,%w].\n", [PosX,PosY]),
              setval( real_agent_pos, [PosX, PosY] ),
              setval( real_start_pos, [PosX, PosY] ),
              /** agent has seen the starting cell */
              findall( [X,Y],
                       ( world( XMin, YMin, XMax, YMax ),
                         between(XMin, XMax, 1, X),
                         between(YMin, YMax, 1, Y),
                         is_pos(X,Y),
                         [X,Y] \== [PosX, PosY]
                       ),
                       ShadeList ),
              print_list(ShadeList),
              setval(real_cells_shaded, ShadeList),
              update_shades(ShadeList)
        ),
        redraw.

/** Randomly place the gold.
 *  The Seed allows for reproducibility of placement. */
xTra(place_gold(Seed), _H) :- !,
        world( XMin, YMin, XMax, YMax),
        SeedTmp1 is (Seed * 23), 
        mod( SeedTmp1, 2147483647, NewSeed1),
        random_number(NewSeed1, XMin, XMax, PosX),
        SeedTmp2 is (NewSeed1 * 23), 
        mod( SeedTmp2, 2147483647, NewSeed2),
        random_number(NewSeed2, YMin, YMax, PosY),
        printf("Trying to put gold to [%w, %w]\n", [PosX, PosY]),
        getval( real_agent_pos, [AgentX, AgentY] ),
        ( ( pit(PosX, PosY); [AgentX, AgentY] = [PosX, PosY] ) ->
              /** There's a pit or the agent already, try again */
              place_gold(NewSeed2)
        ;
              printf("Placing gold at [%w,%w].\n", [PosX,PosY]),
              setval( real_gold_pos, [PosX, PosY] )
        ),
        redraw.

xTra(reset_visited,_H) :- 
	printColor( pink, " xTra: EXEC 'reset_visited'", []), 
        execdelay,
	clear_history.


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
xTra(Action,Situation) :-
	printColor( red, " xTra: DON'T KNOW HOW TO HANDLE '%w' IN SIT: '%w'\n", [Action,Situation]).


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
