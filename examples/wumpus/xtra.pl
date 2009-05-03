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

/** for minlist */
:- lib(fd_global).

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

reset_values :-

/* list of visited cells
 * default value
 * overwritten randomly via place_agent(Seed)*/
   setval( real_cells_visited, [[1,1]] ),

/* number of visited cells */
   setval( real_num_visited, 1 ),

/* list of unvisited cells */
   setval( real_cells_shaded, [ ] ),

/* number of unexplored cells currently lying in front of the agent
 * default value
 * overwritten randomly via place_agent(Seed)*/
   setval( real_num_facedShades, 3 ),

/* list of (sensed) breezy cells */
   setval( real_cells_breezy, [ ] ),

/* list of (sensed) smelly cells */
   setval( real_cells_smelly, [ ] ),
 
/* list of cells that are known to contain a pit */
%:- setval( real_cells_know_pit, [ ] ).

/* list of cells that are known to contain no pit
 * default value
 * overwritten randomly via place_agent(Seed)*/
   setval( real_cells_know_no_pit, [[1,1]] ),

/* degenerated list of the cell that is known to contain the wumpus */
   setval( real_known_wumpus_pos_X, "N/A" ),
   setval( real_known_wumpus_pos_Y, "N/A" ),

/* list of cells that are known to contain no wumpus
 * default value
 * overwritten randomly via place_agent(Seed)*/
   setval( real_cells_know_no_wumpus, [[1,1]] ),

/* list of cells that are safe to be explored */
   setval( real_cells_safe, [ ] ),

/* real position of our agent
 * default value
 * overwritten randomly via place_agent(Seed)*/
   setval( real_agent_pos_X, 1 ),
   setval( real_agent_pos_Y, 1 ),

/* real direction our agent is facing in */
   setval( real_agent_direction, "east" ),

/* direction that the agent had been facing in
 * just before his most recent move */
   setval( real_previous_direction, "N/A" ),

/* tells whether the agent has turned to the
 * same direction he was facing just before.
 * indicates direct turning in circles */
   setval( real_turning_back_again, false ),

/* is the arrow still available to the agent */
   setval( real_agent_arrow, true ),

/* real position of the wumpus
 * default value
 * overwritten randomly via place_wumpus(Seed)*/
   setval( real_wumpus_pos_X, 1 ),
   setval( real_wumpus_pos_Y, 3 ),

/* is wumpus alive */
   setval( real_wumpus_alive, true ),

/* real position of the gold
 * default value
 * overwritten randomly via place_gold(Seed)*/
   setval( real_gold_pos_X, 2 ),
   setval( real_gold_pos_Y, 2 ),

/* is our agent carrying the gold */
   setval( real_carry_gold, false ),

/* can the agent reach the gold safely */
   setval( real_gold_reachable, true ),

/* if the agent shoots now, does it know it will
 * hit the wumpus */
   setval( real_sure_to_hit, true ),

/* starting position
 * default value
 * overwritten randomly via place_agent(Seed)*/
   setval( real_start_pos_X, 1 ),
   setval( real_start_pos_Y, 1 ),

/* distance to closest cell that can be explored safely
 * default value
 * initialised to maximum Manhattan distance in exogf_Update */
   setval( real_distance_to_closest_safe_cell, 8 ),

/* estimation of the agent if there is still time to explore,
 * or if there is not, and agent should start going home */
   setval( real_still_time, true ),

/* indicates, if the agent has returned home after its
 * exploration of the dungeon */
   setval( real_returned_safely, false ),

/* start time of the level */
   cputime(StartTime),
   setval( real_start_time, StartTime ),

/* remaining time left for exploration of the dungeon */
   setval( real_remaining_time, 240.0 ).


:- reset_values.

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
	
        getval( real_start_pos_X, StartX ),
        getval( real_start_pos_Y, StartY ),
	draw_start( StartX, StartY ),
	
        getval( real_agent_pos_X, AgentX ),
        getval( real_agent_pos_Y, AgentY ),
        getval( real_agent_direction, AgentD ),
        getval( real_agent_arrow, AgentABool ),
        ( ( AgentABool ) ->
            AgentA = 1
        ;
            AgentA = 0
        ),
	draw_wumpus_hunter( AgentX, AgentY, AgentD, AgentA),
        
        getval( real_wumpus_pos_X, WumpusX ),
        getval( real_wumpus_pos_Y, WumpusY ),
        getval( real_wumpus_alive, WumpusABool ),
        ( ( WumpusABool ) ->
            WumpusA = 1
        ;
            WumpusA = 0
        ),
        draw_wumpus( WumpusX, WumpusY, WumpusA),
	
        getval( real_gold_pos_X, GoldX),
        getval( real_gold_pos_Y, GoldY),
	( [GoldX, GoldY] = [-1,-1] ->
	    true
	;
	    draw_gold( GoldX,GoldY )
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
	getval( real_cells_visited, V_CELLS_VIS_UNSORTED ),
        sort_list_of_2D_vectors( V_CELLS_VIS_UNSORTED, V_CELLS_VIS),
        setval( real_cells_visited, V_CELLS_VIS ),
        setval( wm_cells_visited, V_CELLS_VIS ),

	getval( real_num_visited, V_NUM_VIS ),
        setval( wm_num_visited, V_NUM_VIS ),

        getval( real_cells_shaded, V_CELLS_SHA ),
        setval( wm_cells_shaded, V_CELLS_SHA ),
        update_shades(V_CELLS_SHA),

        getval( real_agent_pos_X, V_AGENT_POS_X ), 
	setval( wm_agent_pos_X, V_AGENT_POS_X ),
        getval( real_agent_pos_Y, V_AGENT_POS_Y ), 
	setval( wm_agent_pos_Y, V_AGENT_POS_Y ),
%	printColor( yellow, " V_AGENT_POS_X = %w \n", [V_AGENT_POS_X]), 
%	printColor( yellow, " V_AGENT_POS_Y = %w \n", [V_AGENT_POS_Y]), 
        
        getval( real_agent_direction, V_AGENT_DIRECTION ),
%	printColor( yellow, " V_AGENT_DIRECTION = %w \n", [V_AGENT_DIRECTION]), 
        ( V_AGENT_DIRECTION = "east" ->
                setval( wm_agent_direction, east )
        ;
                true
        ),
        ( V_AGENT_DIRECTION = "south" ->
                setval( wm_agent_direction, south )
        ;
                true
        ),
        ( V_AGENT_DIRECTION = "west" ->
                setval( wm_agent_direction, west )
        ;
                true
        ),
        ( V_AGENT_DIRECTION = "north" ->
                setval( wm_agent_direction, north )
        ;
                true
        ),
        
        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
        %%  count the number of unexplored cells lying in front of the agent      %%
        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

        count_faced_shades([V_AGENT_POS_X, V_AGENT_POS_Y], V_AGENT_DIRECTION, FACED_SHADES),
        setval( real_num_facedShades, FACED_SHADES), 
        setval( wm_num_facedShades, FACED_SHADES),                    

        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
        %%  logging breezes and stenches  %%
        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

        getval( real_cells_breezy, V_CELLS_BRE ),
        printColor( blue, " Memorised breeze at: %w \n", [V_CELLS_BRE]),        
        /** If there is a breeze at the current position,
         *  and it's not memorised yet, memorise it! */
        ( breeze(V_AGENT_POS_X, V_AGENT_POS_Y) ->
%                printColor( blue, " Sensing a breeze at %w \n", [V_AGENT_POS]),
                ( nonmember([V_AGENT_POS_X, V_AGENT_POS_Y], V_CELLS_BRE ) ->
                        V_CELLS_BRE_NEW_UNSORTED = [[V_AGENT_POS_X, V_AGENT_POS_Y] | V_CELLS_BRE],
                        sort_list_of_2D_vectors( V_CELLS_BRE_NEW_UNSORTED, V_CELLS_BRE_NEW),
                        setval( real_cells_breezy, V_CELLS_BRE_NEW ),
                        setval( wm_cells_breezy, V_CELLS_BRE_NEW )
                ;
                        setval( wm_cells_breezy, V_CELLS_BRE )
                )
        ;
                setval( wm_cells_breezy, V_CELLS_BRE )
        ),
        
        getval( real_cells_smelly, V_CELLS_SME ),
        printColor( green, " Memorised stench at: %w \n", [V_CELLS_SME]),        
        /** If there is a stench at the current position,
         *  and it's not memorised yet, memorise it! */
        ( stench(V_AGENT_POS_X, V_AGENT_POS_Y) ->
%                printColor( green, " Sensing a stench at %w \n", [V_AGENT_POS]),
                ( nonmember([V_AGENT_POS_X, V_AGENT_POS_Y], V_CELLS_SME ) ->
                        V_CELLS_SME_NEW_UNSORTED = [[V_AGENT_POS_X, V_AGENT_POS_Y] | V_CELLS_SME],
                        sort_list_of_2D_vectors(V_CELLS_SME_NEW_UNSORTED, V_CELLS_SME_NEW),
                        setval( real_cells_smelly, V_CELLS_SME_NEW ),
                        setval( wm_cells_smelly, V_CELLS_SME_NEW )
                ;
                        setval( wm_cells_smelly, V_CELLS_SME )
                )
        ;
                setval( wm_cells_smelly, V_CELLS_SME )
        ),

        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
        %%  Diagnostic rules to infer the position of pits(not implemented yet)/no pits  %%
        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

        % cells that have been visited and are free of breezes, have no pits as neighbours
        get_all_positions( V_CELLS_POS ),
        % printColor( red, " All positions: %w \n", [V_CELLS_POS]),

        getval( real_cells_know_no_pit, V_CELLS_K_N_P1_UNSORTED ),
        sort_list_of_2D_vectors( V_CELLS_K_N_P1_UNSORTED, V_CELLS_K_N_P1 ),
        setval( real_cells_know_no_pit, V_CELLS_K_N_P1 ),
        setval( wm_cells_know_no_pit, V_CELLS_K_N_P1 ),

        getval( real_cells_breezy, V_CELLS_BRE1_UNSORTED ),
        sort_list_of_2D_vectors( V_CELLS_BRE1_UNSORTED, V_CELLS_BRE1 ),
%        printColor( red, " *****  V_CELLS_BRE1: %w \n", [V_CELLS_BRE1]),
        getval( real_cells_visited, V_CELLS_VIS1_UNSORTED ),
        sort_list_of_2D_vectors( V_CELLS_VIS1_UNSORTED, V_CELLS_VIS1 ),
%        printColor( red, " *****  V_CELLS_VIS1: %w \n", [V_CELLS_VIS1]),

        ( foreach( PossPitPos, V_CELLS_POS ),
        param(V_CELLS_BRE1, V_CELLS_VIS1, V_CELLS_K_N_P1)
        do
              ( ( nonmember( PossPitPos, V_CELLS_BRE1 ),
                     member( PossPitPos, V_CELLS_VIS1 ) ) ->
                        get_cross_cells( PossPitPos, CrossListP ),
                        ( foreach( PitPos, CrossListP ),
                          param(V_CELLS_K_N_P1)
                          do
                          ( nonmember( PitPos, V_CELLS_K_N_P1 ) ->
                                  V_CELLS_K_N_P1_NEW_UNSORTED = [PitPos | V_CELLS_K_N_P1],
                                  sort_list_of_2D_vectors(V_CELLS_K_N_P1_NEW_UNSORTED, V_CELLS_K_N_P1_NEW),
                                  setval( real_cells_know_no_pit, V_CELLS_K_N_P1_NEW ),
                                  setval( wm_cells_know_no_pit, V_CELLS_K_N_P1_NEW )
                          ;
                                  true
                          )
                        )
              ;
                        true
              )
        ), %% end foreach
        /** check if there is a breeze at the current cell */
        ( member( V_AGENT_POS, V_CELLS_BRE1 ) ->
                getval( real_cells_know_no_pit, V_CELLS_K_N_P2 ),
                ( nonmember( V_AGENT_POS, V_CELLS_K_N_P2 ) ->
                        V_CELLS_K_N_P2_NEW_UNSORTED = [V_AGENT_POS | V_CELLS_K_N_P2],
                        sort_list_of_2D_vectors(V_CELLS_K_N_P2_NEW_UNSORTED, V_CELLS_K_N_P2_NEW),
                        setval( real_cells_know_no_pit, V_CELLS_K_N_P2_NEW ),
                        setval( wm_cells_know_no_pit, V_CELLS_K_N_P2_NEW )
                ;
                        true
                )
        ;
                true
        ),

        getval( real_cells_know_no_pit, V_CELLS_K_N_P_TEST),
        printColor( red, " I know that here are *no* pits: %w \n", [V_CELLS_K_N_P_TEST]),

        %getval( real_cells_know_pit, V_CELLS_K_P),
        %setval( wm_cells_know_pit, V_CELLS_K_P ),
        
        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
        %%  Diagnostic rules to infer the position of the wumpus/no wumpus           %%
        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%  First find out, where there is no wumpus:
        % Cells that have been visited and are free of stench, have no wumpus as neighbour.
        % Note that real_cells_smelly only contains visited cells. 
        getval( real_cells_know_no_wumpus, V_CELLS_K_N_W1_UNSORTED ),
        sort_list_of_2D_vectors( V_CELLS_K_N_W1_UNSORTED, V_CELLS_K_N_W1 ),
        setval( real_cells_know_no_wumpus, V_CELLS_K_N_W1 ),
        setval( wm_cells_know_no_wumpus, V_CELLS_K_N_W1 ),
        getval( real_cells_smelly, V_CELLS_SME1_UNSORTED ),
        sort_list_of_2D_vectors( V_CELLS_SME1_UNSORTED, V_CELLS_SME1 ),

        getval( real_wumpus_alive, V_WUMPUS_ALIVE ),
        setval( wm_wumpus_alive, V_WUMPUS_ALIVE ),
        ( ( V_WUMPUS_ALIVE = true ) ->

        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%  If the wumpus is still alive do the following:
        ( foreach( PossWumpusPos, V_CELLS_POS ),
        param(V_CELLS_SME1, V_CELLS_VIS1, V_CELLS_K_N_W1)
        do
                ( ( nonmember( PossWumpusPos, V_CELLS_SME1 ),
                       member( PossWumpusPos, V_CELLS_VIS1 ) ) ->
                          get_cross_cells( PossWumpusPos, CrossListW ),
                          ( foreach( WumpusPos, CrossListW ),
                            param(V_CELLS_K_N_W1)
                            do
                            ( nonmember( WumpusPos, V_CELLS_K_N_W1 ) ->
                                    V_CELLS_K_N_W1_NEW_UNSORTED = [WumpusPos | V_CELLS_K_N_W1],
                                    sort_list_of_2D_vectors(V_CELLS_K_N_W1_NEW_UNSORTED, V_CELLS_K_N_W1_NEW),
                                    setval( real_cells_know_no_wumpus, V_CELLS_K_N_W1_NEW ),
                                    setval( wm_cells_know_no_wumpus, V_CELLS_K_N_W1_NEW )
                            ;
                                    true
                            )
                          )
                ;
                          true
                )
        ), %% end foreach
        /** check if there is a stench at the current cell */
        ( member( V_AGENT_POS, V_CELLS_SME1 ) ->
                getval( real_cells_know_no_wumpus, V_CELLS_K_N_W2 ),
                ( nonmember( V_AGENT_POS, V_CELLS_K_N_W2 ) ->
                        V_CELLS_K_N_W2_NEW_UNSORTED = [V_AGENT_POS | V_CELLS_K_N_W2],
                        sort_list_of_2D_vectors(V_CELLS_K_N_W2_NEW_UNSORTED, V_CELLS_K_N_W2_NEW),
                        setval( real_cells_know_no_wumpus, V_CELLS_K_N_W2_NEW ),
                        setval( wm_cells_know_no_wumpus, V_CELLS_K_N_W2_NEW )
                ;
                        true
                )
        ;
                true
        )

        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%  Else, if the wumpus is dead already, do this instead:
        ;
        printColor( red, " I think the Wumpus is dead \n", []),
        setval( real_cells_know_no_wumpus, V_CELLS_POS ), %% V_CELLS_POS contains all valid positions
        setval( wm_cells_know_no_wumpus, V_CELLS_POS )
        
        ), %% end (V_WUMPUS_ALIVE = "T") -> do; else

        getval( real_cells_know_no_wumpus, V_CELLS_K_N_W_TEST),
        printColor( red, " I know that here is *no* wumpus: %w \n", [V_CELLS_K_N_W_TEST]),

        getval( real_agent_arrow, V_AGENT_ARROW ),
        setval( wm_agent_arrow, V_AGENT_ARROW ),

        getval( real_wumpus_pos_X, V_WUMPUS_POS_X ),
        setval( wm_wumpus_pos_X, V_WUMPUS_POS_X ),
        getval( real_wumpus_pos_Y, V_WUMPUS_POS_Y ),
        setval( wm_wumpus_pos_Y, V_WUMPUS_POS_Y ),

        getval( real_gold_pos_X, V_GOLD_POS_X ),
        setval( wm_gold_pos_X, V_GOLD_POS_X ),
        getval( real_gold_pos_Y, V_GOLD_POS_Y ),
        setval( wm_gold_pos_Y, V_GOLD_POS_Y ),

        getval( real_carry_gold, V_CARRY_GOLD ),
        setval( wm_carry_gold, V_CARRY_GOLD ),

        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
        %%  Diagnostic rules to deduce whether the agent can reach the gold safely  %%
        %%  according what it has learned about the environment                     %%
        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

        /** The "margin" are those cells that are shaded (unknown) and at the same time 
         *  are neighbours of already explored cells
         */
	getval( real_cells_visited, M_CELLS_VIS_UNSORTED ),
        sort_list_of_2D_vectors(M_CELLS_VIS_UNSORTED, M_CELLS_VIS),
        getval( real_cells_shaded, M_CELLS_SHA_UNSORTED ),
        sort_list_of_2D_vectors(M_CELLS_SHA_UNSORTED, M_CELLS_SHA),
        getval( real_cells_shaded, M_CELLS_SHA ),
        findall( MarginCell,
                 ( member( MarginCell, M_CELLS_SHA ),
                   get_cross_cells( MarginCell, MCrossList ),
                   delete( MarginCell, MCrossList, MNeighbours ),
                   intersection( MNeighbours, M_CELLS_VIS, MExploredNeighbours ),
                   length( MExploredNeighbours, MExploredNeighboursLength ),
                   MExploredNeighboursLength > 0
                 ),
                 MARGIN ),
         
%         printColor(blue, " ***** real_cells_visited:: %w \n", [M_CELLS_VIS]),
%         printColor(blue, " ***** real_cells_shaded:: %w \n", [M_CELLS_SHA]),
%         printColor(blue, " ***** MARGIN:: %w \n", [MARGIN]),

        /** As the agent has an aversion against risk it should give up searching for the gold <=>
         *  it doesn't know any safe cell to explore in the margin and all possibilities for enlarging
         *  the margin by killing the wumpus have been exhausted
         *  <=>
         *  there is no cell that's in the lists real_cells_know_no_wumpus && real_cells_know_no_pit.
         *  ( not really sure about the "<=>"... what if there's information of more than one neighbour
         *  of the margin cell? )
         *  If the wumpus dies, the list real_cells_know_no_wumpus is maximized (see above), which possibly will
         *  lead to an increase of safe cells. This is also considered in the causes_val(shoot).
         */
%        getval( real_gold_reachable, V_GOLD_REACHABLE ),
        findall([SafeX, SafeY],
                ( member([SafeX, SafeY], MARGIN),
                  member([SafeX, SafeY], V_CELLS_K_N_P_TEST),
                  member([SafeX, SafeY], V_CELLS_K_N_W_TEST)
                ),
                SAFE_UNSORTED),

        sort_list_of_2D_vectors(SAFE_UNSORTED, SAFE),
        setval( real_cells_safe, SAFE ),
        setval( wm_cells_safe, SAFE ),
 
        printColor( red, " I know that these cells are safe to be explored: %w \n", [SAFE]),
        ( ( length(SAFE, 0) ) ->
                  setval( real_gold_reachable, false ),
                  setval( wm_gold_reachable, false ),
                  printColor(red, " ***** GOLD IS NOT REACHABLE SAFELY ANY MORE! \n", [])
        ;
                  setval( real_gold_reachable, true ),
                  setval( wm_gold_reachable, true )
        ),
        
        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
        %%  Diagnostic rules to deduce if the agent will hit the wumpus if shooting now  %%
        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

        /** There are two main cases where the agent knows, where the wumpus is located:
         *
         *  1) It knows there is a stench in both horizontal or vertical neighbour cells:
         *      _____    _____
         *     |_|_|_|  |_|S|_|
         *     |S|W|S|  |_|W|_|
         *     |_|_|_|  |_|S|_|
         *
         *  2) It knows that there is a stench in two "diagonal" neighbour cells, but also
         *     that there is no wumpus (N) at one common neighbour cell:
         *      _____    _____    _____    _____
         *     |_|_|_|  |N|S|_|  |_|S|N|  |_|_|_|
         *     |S|W|_|  |S|W|_|  |_|W|S|  |_|W|S|
         *     |N|S|_|  |_|_|_|  |_|_|_|  |_|S|N|
         *
         *  Additionally, the agent could infer the location of the wumpus once enough
         *  cells of the maze have been explored, by recognising the cross pattern of
         *  the stenches and the wumpus. As this case is rather messy and error-prone,
         *  and as in this case the stench cells are safe to be explored (and thus the
         *  first two cases reachable), we do not treat it here.
         */

         setval( wm_sure_to_hit, false ),
         getval( real_known_wumpus_pos_X, KNOWN_WUMPUS_POS_X ),
         getval( real_known_wumpus_pos_Y, KNOWN_WUMPUS_POS_Y ),
         determine_sure_to_hit( V_AGENT_POS_X, V_AGENT_POS_Y, V_AGENT_DIRECTION, V_CELLS_K_N_W_TEST, V_CELLS_SME,
                                KNOWN_WUMPUS_POS_X, KNOWN_WUMPUS_POS_Y, SureToHit,
                                WumpusPosX, WumpusPosY ),
         setval( real_sure_to_hit, SureToHit ),
         setval( wm_sure_to_hit, SureToHit ),
         setval( real_known_wumpus_pos_X, WumpusPosX ),
         setval( wm_known_wumpus_pos_X, WumpusPosX ),
         setval( real_known_wumpus_pos_Y, WumpusPosY ),
         setval( wm_known_wumpus_pos_Y, WumpusPosY ),

         getval( wm_known_wumpus_pos_X, KNOWN_W_POS_TEST_X ),
         getval( wm_known_wumpus_pos_Y, KNOWN_W_POS_TEST_Y ),
         ( ( KNOWN_W_POS_TEST_X \= "N/A",
             KNOWN_W_POS_TEST_Y \= "N/A" ) -> 
                printColor( red, " I deduced that the wumpus must be at: [%w, %w] \n",
                            [KNOWN_W_POS_TEST_X, KNOWN_W_POS_TEST_Y])
         ;
                printColor( red, " I don't know yet where the wumpus is.\n", [])
         ),


        getval( real_turning_back_again, V_T_B_A ), 
        setval( wm_turning_back_again, V_T_B_A ),
        getval( real_previous_direction, V_P_D ),
        setval( wm_previous_direction, V_P_D ),

        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
        %%  compute the Manhattan distance from the agent to the closest  %%
        %%  cell that is safe to be explored                              %%
        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

        world(_XMin, _YMin, XMax, YMax), 
        LargestDist is (XMax * YMax),
        setval( real_distance_to_closest_safe_cell, LargestDist ),
        setval( wm_distance_to_closest_safe_cell, LargestDist ),

        compute_distance_to_closest_safe_cell( V_AGENT_POS_X, V_AGENT_POS_Y, SAFE, ResultDist ),
%        printColor( blue, "Computing distance to closest safe cell: %w\n", [ResultDist] ),
        setval( real_distance_to_closest_safe_cell, ResultDist ),
        setval( wm_distance_to_closest_safe_cell, ResultDist ),

        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
        %%  compute the time spent in the dungeon (doesn't pause for thinking)  %%
        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

        getval( real_start_time, RealStartTime ),
        setval( wm_start_time, RealStartTime ),
        cputime( TimeNow ),
        TimeDiff is (TimeNow - RealStartTime),
        getval( real_remaining_time, RemainingTimeOld ),
        RemainingTimeNew is (RemainingTimeOld - TimeDiff),
        setval( real_remaining_time, RemainingTimeNew ),
        setval( wm_remaining_time, RemainingTimeNew ),
        printColor( green, "Time left: %w\n", [RemainingTimeNew] ),

        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
        %%  estimate whether there is still time for exploration, or if the     %%
        %%  agent should better be going home                                   %%
        %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

        ( (RemainingTimeNew > 60) ->
            setval( real_still_time, true ),
            setval( wm_still_time, true )
        ;
            setval( real_still_time, false ),
            setval( wm_still_time, false )
        ),

        getval( real_returned_safely, ReturnedSafely ),
        setval( wm_returned_safely, ReturnedSafely ),

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
        getval( real_agent_pos_X, X ),
        getval( real_agent_pos_Y, Y ),
        printf(" at %w,%w ", [X,Y]),
        getval( real_agent_direction, D ),
        printf(" facing %w ", [D]),
        getval( real_agent_arrow, A),
        printf(" has arrow: %w \n", [A]),
        execdelay,
        go_forward_aux([X,Y], D, A).
        
xTra(turn_left,_H) :-
        printColor( pink, " xTra: EXEC 'turn_left'", []),
        getval( real_agent_pos_X, X ),
        getval( real_agent_pos_Y, Y ),
        printf(" at %w,%w ", [X,Y]),
        getval( real_agent_direction, D ),
        printf(" facing %w ", [D]),
        getval( real_agent_arrow, A),
        printf(" has arrow: %w \n", [A]),
        execdelay,
        turn_aux(left, [X,Y], D, A).
        
xTra(turn_right,_H) :-
        printColor( pink, " xTra: EXEC 'turn_right'", []),
        getval( real_agent_pos_X, X ),
        getval( real_agent_pos_Y, Y ),
        printf(" at %w,%w ", [X,Y]),
        getval( real_agent_direction, D ),
        printf(" facing %w ", [D]),
        getval( real_agent_arrow, A),
        printf(" has arrow: %w \n", [A]),
        execdelay,
        turn_aux(right, [X,Y], D, A).

xTra(shoot,_H) :-
        printColor( pink, " xTra: EXEC 'shoot'", []),
        setval(real_agent_arrow, false),
        setval(wm_agent_arrow, false),
        getval( real_agent_pos_X, X ),
        getval( real_agent_pos_Y, Y ),
        getval( real_agent_direction, D),
        /** animation of flying arrow: */
        execdelay,
        shoot_aux([X,Y], D).

xTra(rest,_H)  :- 
	printColor( pink, " xTra: EXEC 'rest'", []), 
        getval( real_agent_pos_X, X ),
        getval( real_agent_pos_Y, Y ),
        printf(" at %w,%w ", [X,Y]),
        getval( real_agent_direction, D ),
        printf(" facing %w ", [D]),
        getval( real_agent_arrow, A),
        printf(" has arrow: %w \n", [A]),
        execdelay,
	draw_action("0", X, Y).

xTra(noop,_H)  :- 
	printColor( pink, " xTra: EXEC 'noop'", []), 
        getval( real_agent_pos_X, X ),
        getval( real_agent_pos_Y, Y ),
        printf(" at %w,%w ", [X,Y]),
        getval( real_agent_direction, D ),
        printf(" facing %w ", [D]),
        getval( real_agent_arrow, A),
        printf(" has arrow: %w \n", [A]),
        execdelay,
	draw_action("0", X, Y).

xTra(make_yourself_at_home,_H) :- 
	printColor( pink, " xTra: EXEC 'make_yourself_at_home'", []), 
        execdelay,
	setval(real_returned_safely, true),
        setval(wm_returned_safely, true).

xTra(pickup_gold,_H) :- 
	printColor( pink, " xTra: EXEC 'pickup_gold'", []), 
        getval( real_agent_pos_X, X ),
        getval( real_agent_pos_Y, Y ),
        printf(" at %w,%w ", [X,Y]),
        getval( real_agent_direction, D ),
        printf(" facing %w ", [D]),
        getval( real_agent_arrow, A),
        printf(" has arrow: %w \n", [A]),
        execdelay,
	setval(real_carry_gold, true),
        setval(wm_carry_gold, true).

xTra(initialise_dungeon(Seed), _H) :- 
	printColor( pink, " xTra: EXEC 'initialise_dungeon(%w)'\n", [Seed]),
        distribute_pits(Seed),
        SeedTmp1 is (Seed * 23), 
        mod( SeedTmp1, 2147483647, NewSeed1),
        place_wumpus(NewSeed1),
        SeedTmp2 is (NewSeed1 * 23), 
        mod( SeedTmp2, 2147483647, NewSeed2),
        place_agent(NewSeed2),
        SeedTmp3 is (NewSeed2 * 23), 
        mod( SeedTmp3, 2147483647, NewSeed3),
        place_gold(NewSeed3),
        execdelay,
	clear_history.

xTra(reset_visited,_H) :- 
	printColor( pink, " xTra: EXEC 'reset_visited'\n", []), 
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

:- mode go_forward_aux(++, ++, ++).
go_forward_aux([X,Y], D, A) :- !,        
        ( D="east" -> Action = "R",
                      Xn is X+1,
                      Yn is Y
        ;
          true
        ),
        ( D="south" -> Action = "D",
                      Xn is X,
                      Yn is Y-1
        ;
          true
        ),
        ( D="west" -> Action = "L",
                      Xn is X-1,
                      Yn is Y
        ;
          true
        ),
        ( D="north" -> Action = "U",
                      Xn is X,
                      Yn is Y+1
        ;
          true
        ),
        /** reset turning information */
        setval( real_turning_back_again, false ), 
        setval( wm_turning_back_again, false ),
        setval( real_previous_direction, "N/A" ),
        setval( wm_previous_direction, "N/A" ),

        setval( real_agent_pos_X, Xn ),
        setval( real_agent_pos_Y, Yn ),
        ( getval(real_carry_gold,true) ->
                setval( real_gold_pos_X, Xn ),
                setval( real_gold_pos_Y, Yn )
        ;
          true
        ),
%        printf(" draw_wumpus_hunter(%w,%w,%w,%w) \n", [Xn, Yn, D, A]), flush(stdout),
        /** bool to int conversion */
        ( ( A ) ->
            AInt = 1
        ;
            AInt = 0
        ),
        draw_wumpus_hunter(Xn, Yn, D, AInt),
        draw_action(Action, X, Y),
        getval(real_cells_visited, RCV),
        ( nonmember([Xn,Yn], RCV) -> RCVN = [[Xn, Yn] | RCV],
                                     setval(real_cells_visited, RCVN),
                                     setval(wm_cells_visited, RCVN),
                                     getval(real_num_visited, RNumV),
                                     RNumVN is (RNumV + 1),
                                     setval(real_num_visited, RNumVN),
                                     setval(wm_num_visited, RNumVN)
        ;
                                     true
        ),
%        getval(real_cells_visited, RCVC_east),
%        printf(" the following cells have been visited: %w \n", [RCVC_east]),
        getval(real_cells_shaded, RCS),
        ( member([Xn,Yn], RCS) -> delete([Xn,Yn], RCS, RCSN),
                                  setval(real_cells_shaded, RCSN),
                                  setval(wm_cells_shaded, RCSN)
        ;
                                  true
        ),
        getval(real_cells_shaded, RCSC),
%        printf(" the following cells are shaded: %w \n", [RCSC]),                    
        update_shades(RCSC),
        count_faced_shades([X,Y], D, NumShaded),
        setval( real_num_facedShades, NumShaded), 
        setval( wm_num_facedShades, NumShaded),
        getval( real_cells_know_no_wumpus, CellsKnowNoWumpus ),
        ( stench(Xn, Yn) ->
           true
        ;
           get_cross_cells([Xn, Yn], CrossCellsWumpus),
           merge(1, =<, CrossCellsWumpus, CellsKnowNoWumpus, CellsKnowNoWumpusNew),
           setval( real_cells_know_no_wumpus, CellsKnowNoWumpusNew ),
           setval( wm_cells_know_no_wumpus, CellsKnowNoWumpusNew )
        ),
        getval( real_cells_know_no_pit, CellsKnowNoPit ),
        ( breeze(Xn, Yn) ->
           true
        ;
           get_cross_cells([Xn, Yn], CrossCellsPit),
           merge(1, =<, CrossCellsPit, CellsKnowNoPit, CellsKnowNoPitNew),
           setval( real_cells_know_no_pit, CellsKnowNoPitNew ),
           setval( wm_cells_know_no_pit, CellsKnowNoPitNew )
        ).
           

:- mode turn_aux(++, ++, ++, ++).        
turn_aux( Side, [X,Y], D, A ) :- !,
        ( D="east" ->
                ( Side = left ->
                        NewD = north,
                        Action = "1"
                ;
                        NewD = south,
                        Action = "2"
                )
        ;
                true
        ),
        ( D="south" ->
                ( Side = left ->
                        NewD = east,
                        Action = "1"
                ;
                        NewD = west,
                        Action = "2"
                )
        ;
                true
        ),
        ( D="west" ->
                ( Side = left ->
                        NewD = south,
                        Action = "1"
                ;
                        NewD = north,
                        Action = "2"
                )
        ;
                true
        ),
        ( D="north" ->
                ( Side = left ->
                        NewD = west,
                        Action = "1"
                ;
                        NewD = east,
                        Action = "2"
                )
        ;
                true
        ),
        term_string(NewD, NewDS),
        /** Check if agent turns directly in circles,
         *  by testing for immediate oscillation. */
        getval( real_previous_direction, PreviousD ),
        printColor(green, "Turning...\n", []),
        printColor(green, "Previous direction: %w, Current direction: %w, New direction: %w\n", [PreviousD, D, NewDS]),
        ( ( NewDS = PreviousD ) ->
                printColor(green, "Oh my! I'm so stupid, I'm turning in circles!!!!\n", []),
                setval( real_turning_back_again, true ), 
                setval( wm_turning_back_again, true )
        ;
                printColor(green, "I'm turning in a new direction!\n", []),
                /** New direction (or PreviousD = "N/A") */
                setval( real_turning_back_again, false ), 
                setval( wm_turning_back_again, false )
        ),
        setval(real_previous_direction, D),
        setval(wm_previous_direction, D),
        setval(real_agent_direction, NewDS),
        setval(wm_agent_direction, NewD),
%        printf(" draw_wumpus_hunter(%w,%w,%w,%w) \n", [X, Y, NewDS, A]),
        /** bool to int conversion */
        ( ( A ) ->
            AInt = 1
        ;
            AInt = 0
        ),
        draw_wumpus_hunter(X, Y, NewDS, AInt),
        draw_action(Action, X, Y),
        count_faced_shades([X,Y], NewDS, NumShaded),
        setval( real_num_facedShades, NumShaded), 
        setval( wm_num_facedShades, NumShaded). 

:- mode shoot_aux(++, ++).        
shoot_aux( [X,Y], D ) :-
        world( XMin, YMin, XMax, YMax ),
        ( D="east" ->
                findall([XEast, Y], between(X, XMax, 1, XEast), Cells)
        ;
                true
        ),
        ( D="south" ->
                findall([X, YSouth], between(YMin, Y, 1, YSouth), CellsTmp),
                reverse(CellsTmp, Cells)
        ;
                true
        ),
        ( D="west" ->
                findall([XWest, Y], between(XMin, X, 1, XWest), CellsTmp),
                reverse(CellsTmp, Cells)
        ;
                true
        ),
        ( D="north" ->
                findall([X, YNorth], between(Y, YMax, 1, YNorth), Cells)
        ;
                true
        ),
        getval( real_wumpus_pos_X, WumpusX ),
        getval( real_wumpus_pos_Y, WumpusY ),
        ( foreach( [CellX, CellY], Cells ),
          param(D, WumpusX, WumpusY)
          do
                  is_pos(CellX, CellY),
                  draw_arrow(CellX, CellY, D),
                  execdelay, /** window refresh hides arrow :( */
                  ( [WumpusX, WumpusY] = [CellX, CellY] ->
                          setval(real_wumpus_alive, false),
                          setval(wm_wumpus_alive, false),
                          printf(" Wumpus was killed \n",[]),
                          draw_wumpus(WumpusX, WumpusY, 0)
                  ;
                          true 
                  )
        ).

/** Randomly distribute 1 to 3 pits in the world.
 *  The Seed allows for reproducibility of configurations. */
:- mode distribute_pits(++).
distribute_pits(Seed) :-
        printf("Retracting all pits.\n", []),
        retract_all(pit(_,_)),
        random_number(Seed, 1, 3, NumberOfPits),
        printf("Number of pits is: %w\n", [NumberOfPits]),
        /** Distribute recursively till NumberOfPits is 0.
         *  Implemented in wumpus.readylog. */
        distribute_pits_aux(NumberOfPits, Seed),
        findall( [X,Y],
                 pit(X,Y),
                 PitList ),
%        print_list(PitList),
        printf("update_pits...", []),
        update_pits(PitList),
        printf(" successful.\n", []),
        distribute_breezes,
        redraw.

/** Distribute pits randomly, until NumberOfPits is 0. */
:- mode distribute_pits_aux(++, ++).
distribute_pits_aux(0, _Seed) :- !.

distribute_pits_aux(NumberOfPits, Seed) :-
        world( XMin, YMin, XMax, YMax ),
        SeedTmp1 is (Seed * 23), 
        mod( SeedTmp1, 2147483647, NewSeed1),
        random_number(NewSeed1, XMin, XMax, XRand),
        SeedTmp2 is (NewSeed1 * 23), 
        mod( SeedTmp2, 2147483647, NewSeed2),
        random_number(NewSeed2, YMin, YMax, YRand),
        ( pit(XRand, YRand) ->
              /** Pit already exists. Try again. */
              distribute_pits_aux(NumberOfPits, NewSeed2)
        ;
              printf("assert(pit(%w, %w))\n", [XRand, YRand]),
              assert(pit(XRand, YRand)),
              ( pit(XRand, YRand) ->
                 printf("Pit has successfully been asserted.\n", [])
              ;
                 true
              ),
              NewNumberOfPits is (NumberOfPits - 1),
              distribute_pits_aux(NewNumberOfPits, NewSeed2)
        ).

/** Randomly place the wumpus.
 *  The Seed allows for reproducibility of placement. */
:- mode place_wumpus(++).
place_wumpus(Seed) :-
        place_wumpus_aux(Seed),
        redraw.

:- mode place_wumpus_aux(++).
place_wumpus_aux( Seed ) :- !,        
        setval(wm_wumpus_alive, true),
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
              place_wumpus_aux(NewSeed2)
        ;
              printf("Placing wumpus at [%w, %w].\n", [PosX,PosY]),
              setval( real_wumpus_pos_X, PosX ),
              setval( real_wumpus_pos_Y, PosY )%,
%              setval( wm_wumpus_pos_X, PosX ),
%              setval( wm_wumpus_pos_Y, PosY )
        ),
        distribute_stenches.

/** Randomly place the agent (and the start/exit cell).
 *  The Seed allows for reproducibility of placement. */
:- mode place_agent(++).
place_agent(Seed) :-
        place_agent_aux(Seed),
        redraw.

:- mode place_agent_aux(++).
place_agent_aux( Seed ) :- !,
        world( XMin, YMin, XMax, YMax),
        SeedTmp1 is (Seed * 23), 
        mod( SeedTmp1, 2147483647, NewSeed1),
        random_number(NewSeed1, XMin, XMax, PosX),
        SeedTmp2 is (NewSeed1 * 23), 
        mod( SeedTmp2, 2147483647, NewSeed2),
        random_number(NewSeed2, YMin, YMax, PosY),
        printf("Trying to put agent to [%w, %w]\n", [PosX, PosY]),
        /** Spin agent around randomly */
        SeedTmp3 is (NewSeed2 * 23), 
        mod( SeedTmp3, 2147483647, NewSeed3),
        random_number(NewSeed3, 1, 4, DirTmp),
        ( DirTmp = 1 ->
           Dir = "east"
        ;
           true
        ),
        ( DirTmp = 2 ->
           Dir = "south"
        ;
           true
        ),
        ( DirTmp = 3 ->
           Dir = "west"
        ;
           true
        ),
        ( DirTmp = 4 ->
           Dir = "north"
        ;
           true
        ),
        setval( real_agent_dir, Dir ),
        setval( wm_agent_dir, Dir ),
        getval( real_wumpus_pos_X, WumpusX ),
        getval( real_wumpus_pos_Y, WumpusY ),
        ( ( pit(PosX, PosY); [WumpusX, WumpusY] = [PosX, PosY] ) ->
              /** There's a pit or the wumpus already, try again */
              place_agent_aux(NewSeed2)
        ;
              printf("Placing agent at [%w, %w].\n", [PosX,PosY]),
              setval( real_agent_pos_X, PosX ),
              setval( real_agent_pos_Y, PosY ),
              setval( wm_agent_pos_X, PosX ),
              setval( wm_agent_pos_Y, PosX ),
              setval( real_start_pos_X, PosX ),
              setval( real_start_pos_Y, PosY ),
              /** Start position is constant for the level, so
               *  the world model value is set here and not in
               *  exogf update */
              setval( wm_start_pos_X, PosX ),
              setval( wm_start_pos_Y, PosY ),
              setval( real_cells_know_no_pit, [[PosX, PosY]] ),
              setval( wm_cells_know_no_pit, [[PosX, PosY]] ),
              setval( real_cells_know_no_wumpus, [[PosX, PosY]] ),
              setval( wm_cells_know_no_wumpus, [[PosX, PosY]] ),
              /** agent has seen the starting cell */
              setval( real_cells_visited, [[PosX, PosY]] ),
              setval( wm_cells_visited, [[PosX, PosY]] ),
              world( XMin, YMin, XMax, YMax ),
              findall( [X,Y],
                       ( between(XMin, XMax, 1, X),
                         between(YMin, YMax, 1, Y),
                         is_pos(X,Y),
                         [X,Y] \== [PosX, PosY]
                       ),
                       ShadeList ),
%              print_list(ShadeList),
              setval(real_cells_shaded, ShadeList),
              setval(wm_cells_shaded, ShadeList),
              update_shades(ShadeList),
              /** compute how many shades cells are initially lying
               *  in front of the agent */
              count_faced_shades( [PosX, PosY], Dir, NumShaded ),
              setval( real_num_facedShades, NumShaded ),
              setval( real_carry_gold, false ),
              setval( wm_known_wumpus_pos_X, "N/A" ),
              setval( wm_known_wumpus_pos_Y, "N/A" )
        ).

/** Randomly place the gold.
 *  The Seed allows for reproducibility of placement. */
:- mode place_gold(++).
place_gold(Seed) :-
        place_gold_aux(Seed),
        redraw.

place_gold_aux(Seed) :- !,
        world( XMin, YMin, XMax, YMax),
        SeedTmp1 is (Seed * 23), 
        mod( SeedTmp1, 2147483647, NewSeed1),
        random_number(NewSeed1, XMin, XMax, PosX),
        SeedTmp2 is (NewSeed1 * 23), 
        mod( SeedTmp2, 2147483647, NewSeed2),
        random_number(NewSeed2, YMin, YMax, PosY),
        printf("Trying to put gold to [%w, %w]\n", [PosX, PosY]),
        getval( real_agent_pos_X, AgentX ),
        getval( real_agent_pos_Y, AgentY ),
        ( ( pit(PosX, PosY); [AgentX, AgentY] = [PosX, PosY] ) ->
              /** There's a pit or the agent already, try again */
              place_gold_aux(NewSeed2)
        ;
              printf("Placing gold at [%w,%w].\n", [PosX,PosY]),
              setval( real_gold_pos_X, PosX ),
              setval( real_gold_pos_Y, PosY ),
              setval( wm_gold_pos_X, PosX ),
              setval( wm_gold_pos_Y, PosY )
        ).

/** Re-compute the position of breezes. */
distribute_breezes :-
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
%        /* real_cells_breezy is only the list of *memorised* breezes */
%        setval( real_cells_breezy, BreezeList ),
%        setval( wm_cells_breezy, BreezeList ),
        ( foreach([X,Y], BreezeList) do
              printf("Asserting breeze(%w, %w).\n", [X,Y]),
              assert(breeze(X,Y))
        ),
        printf("update_breezes...", []),
        update_breezes(BreezeList),
        printf(" successful.\n", []).

/** Re-compute the position of stenches. */
distribute_stenches :-
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
%        /* real_cells_smelly is only the list of *memorised* stenches */
%        setval( real_cells_smelly, StenchList ),
%        setval( wm_cells_smelly, StenchList ),
        ( foreach([X,Y], StenchList) do
              printf("Asserting stench(%w, %w).\n", [X,Y]),
              assert(stench(X,Y))
        ),
        printf("update_stenches...", []),
        update_stenches(StenchList),
        printf(" successful.\n", []).

:- mode get_cross_cells(++, -).
get_cross_cells( Pos, CrossList ) :-
              Pos = [X, Y],              
              XWest is (X - 1),
              XEast is (X + 1),
              findall( [CrossX, Y],
                       ( between( XWest, XEast, 1, CrossX ),
                         is_pos(CrossX, Y)
                       ),
                       CrossListV ),
%              printf("CrossListV %w.\n", [CrossListV]),
              YSouth is (Y - 1),
              YNorth is (Y + 1),
              findall( [X, CrossY],
                       ( between( YSouth, YNorth, 1, CrossY ),
                         is_pos(X, CrossY)
                       ),
                       CrossListH ),
%              printf("CrossListH %w.\n", [CrossListH]),
              /** remove double entry of middle cell */
              delete(Pos, CrossListH, CrossListHTmp),
              append(CrossListV, CrossListHTmp, CrossList).

% Compute the number of faced shaded cells from position Pos into
% direction D
:- mode count_faced_shades(++, ++, -).
count_faced_shades( Pos, D, Number ) :-
        /** count faced shades */
        Pos = [X, Y],
%        printColor(blue, " counting faced shades... \n",[]),
%        getval( real_cells_shaded, CS ),
%        printColor(blue, " real_cells_shaded: %w \n",[CS]),
        world( XMin, YMin, XMax, YMax ),
        getval(real_cells_visited, RCV),
        ( ( D = "east"; D = east ) ->
                 findall([Xs, Y],
                         ( XEast is X+1,
                           between(XEast, XMax, 1, Xs),
                           is_pos(Xs,Y),
                           nonmember([Xs, Y], RCV)
                         ),
                         Shades)
        ;
                 true
        ),
        ( ( D = "south"; D = south ) ->
                 findall([X, Ys],
                         ( YSouth is Y-1,
                           between(YMin, YSouth, 1, Ys),
                           is_pos(X,Ys),
                           nonmember([X, Ys], RCV)
                         ),
                         Shades)
        ;
                 true
        ),
        ( ( D = "west"; D = west )  ->
                 findall([Xs, Y],
                         ( XWest is X-1,
                           between(XMin, XWest, 1, Xs),
                           is_pos(Xs,Y),
                           nonmember([Xs, Y], RCV)
                         ),
                         Shades)
        ;
                 true
        ),
        ( ( D = "north"; D = north ) ->
                 findall([X, Ys],
                         ( YNorth is Y+1,
                           between(YNorth, YMax, 1, Ys),
                           is_pos(X,Ys),
                           nonmember([X, Ys], RCV)
                         ),
                         Shades)
        ;
                 true
        ),
        ( ( D \= "east", D \= "south", D \= "west", D \= "north",
            D \= east, D \= south, D \= west, D \= north ) ->
                 printColor(blue, " ERROR in count_faced_shades: Argument",[]),
                 printColor(blue, " D = %w is illegal!\n",[D]),
                 Shades = []
        ;
          true
        ),
%        printColor(blue, " Shades: %w \n",[Shades]),
        length(Shades, Number).%,
%        printColor(blue, " Facing %w shades. \n",[Number])        

% Find out if agent can be sure that wumpus is located in
% one of the cells of the list
:- mode deduce_wumpus(++, ++, ++, -, -).
deduce_wumpus( [], _KnowNoWumpus, _KnowSmell, Result, ResultPos ) :- !,
        Result = false,
        ResultPos = ["N/A", "N/A"].

deduce_wumpus( Cells, KnowNoWumpus, KnowSmell, Result, ResultPos ) :-
        ( foreach( Cell, Cells ),
          param(KnowNoWumpus, KnowSmell, Result, ResultPos)
          do
             Cell = [X, Y],
%             printf( "checking cell [%w, %w]...\n", [X, Y] ),
             XEast is (X+1),
             XWest is (X-1),
             YNorth is (Y+1),
             YSouth is (Y-1),
             CellEast = [XEast, Y],
             CellSouth = [X, YSouth],
             CellWest = [XWest, Y],
             CellNorth = [X, YNorth],
             /** Case 1) */
             /** TODO: What if neighbours lie outside of maze? */
             ( ( ( member(CellWest, KnowSmell),
                   member(CellEast, KnowSmell) );
                 ( member(CellNorth, KnowSmell),
                   member(CellSouth, KnowSmell) ) ) ->
                         printColor(red, "CASE 1) wumpus found.\n", [] ),
                         ResultPos = Cell,
                         printColor(red, "CASE 1) wumpus found at: %w!\n", [ResultPos] ),
                         Result = true
             ;
                /** Case 2) */
                CellSouthEast = [XEast, YSouth],
                CellSouthWest = [XWest, YSouth],
                CellNorthWest = [XWest, YNorth],
                CellNorthEast = [XEast, YNorth],
                ( ( ( member(CellSouth, KnowSmell),
                      member(CellWest, KnowSmell),
                      member(CellSouthWest, KnowNoWumpus) );
                    ( member(CellWest, KnowSmell),
                      member(CellNorth, KnowSmell),
                      member(CellNorthWest, KnowNoWumpus) );
                    ( member(CellNorth, KnowSmell),
                      member(CellEast, KnowSmell),
                      member(CellNorthEast, KnowNoWumpus) );
                    ( member(CellEast, KnowSmell),
                      member(CellSouth, KnowSmell),
                      member(CellSouthEast, KnowNoWumpus) ) ) ->
                            printColor(red, "CASE 2) wumpus found.\n", [] ),
                            ResultPos = Cell,
                            printColor(red, "CASE 2) wumpus found at: %w!\n", [ResultPos] ),
                            Result = true
                ;
%                  printf( "WUMPUS NOT FOUND!\n", [] ),
                  true
%                  Result = false,
%                  ResultPos = ["N/A", "N/A"]
                )
             )
        ),
        ( ( ground(Result), ground(ResultPos) ) ->
                true
        ;
                Result = false,
                ResultPos = ["N/A", "N/A"]
        ).

%  Succeeds if the agent is immediately turning back again.
:- mode determine_turning_back_again(++, ++, ++).
determine_turning_back_again(right, east, south).
determine_turning_back_again(right, south, west).
determine_turning_back_again(right, west, north).
determine_turning_back_again(right, north, east).
determine_turning_back_again(left, east, north).
determine_turning_back_again(left, south, east).
determine_turning_back_again(left, west, south).
determine_turning_back_again(left, north, west).

:- mode merge_lists(++, ++, -).
merge_lists([], Result, Result).

merge_lists([Head|Tail], List, Result) :-
        ( nonmember(Head, List) ->
           Result = [Head | List]
        ;
           Result = merge_lists(Tail, List, Result)
        ).

/** Shortcut... inefficient, if you already know the variable values! */
%determine_sure_to_hit( SureToHit ) :-
%        getval( real_agent_pos_X, CurrX ),
%        getval( real_agent_pos_Y, CurrY ),
%        getval( real_agent_direction, AgentDir ),
%        getval( real_cells_know_no_wumpus, CellsKnowNoWumpus),
%        getval( real_cells_smelly, CellsSmelly ),
%        getval( real_known_wumpus_pos_X, KnownWumpusPosX ),
%        getval( real_known_wumpus_pos_Y, KnownWumpusPosY ),
%        determine_sure_to_hit( CurrX, CurrY, AgentDir, CellsKnowNoWumpus,
%                               CellsSmelly, KnownWumpusPosX, KnownWumpusPosY, SureToHit, _WumpusPosX, _WumpusPosY ).

:- mode determine_sure_to_hit(++, ++, ++, ++, ++, ++, -, -, -).
determine_sure_to_hit( CurrX, CurrY, AgentDir, CellsKnowNoWumpus, CellsSmelly,
                       KnownWumpusPosX, KnownWumpusPosY, SureToHit, WumpusPosX, WumpusPosY ) :-
%printColor(pink, "determine_sure_to_hit\n", []),
         world( XMin, YMin, XMax, YMax ),
         ( AgentDir="east" ->
                 findall([XEast, CurrY], between(CurrX, XMax, 1, XEast), ArrowCells)
         ;
                 true
         ),
         ( AgentDir="south" ->
                 findall([CurrX, YSouth], between(YMin, CurrY, 1, YSouth), ArrowCells)
         ;
                 true
         ),
         ( AgentDir="west" ->
                 findall([XWest, CurrY], between(XMin, CurrX, 1, XWest), ArrowCells)
         ;
                 true
         ),
         ( AgentDir="north" ->
                 findall([CurrX, YNorth], between(CurrY, YMax, 1, YNorth), ArrowCells)
         ;
                 true
         ),
         ( ( KnownWumpusPosX \= "N/A",
             KnownWumpusPosY \= "N/A" ) ->
                 /** Position of Wumpus already known */
%printColor(pink, "Position of Wumpus already known\n", []),
                 ( ( cell_in_list( [KnownWumpusPosX, KnownWumpusPosY], ArrowCells,
                                   CellInList ),
                     CellInList = true ) ->
                         SureToHit = true,
%                         getval(real_known_wumpus_pos_X, WumpusPosX),
%                         getval(real_known_wumpus_pos_Y, WumpusPosY)
                         WumpusPosX = KnownWumpusPosX,
                         WumpusPosY = KnownWumpusPosY
                 ;
                         SureToHit = false,
%                         getval(real_known_wumpus_pos_X, WumpusPosX),
%                         getval(real_known_wumpus_pos_Y, WumpusPosY)
                         WumpusPosX = KnownWumpusPosX,
                         WumpusPosY = KnownWumpusPosY
                 )
         ;
                 /** Position of Wumpus not known yet */
%printColor(pink, "Position of Wumpus not known yet\n", []),
%printColor(pink, "deduce_wumpus( %w, %w, %w, %w, %w )\n", [ArrowCells, CellsKnowNoWumpus, CellsSmelly,
%                                WumpusFound, [DeducedPosX, DeducedPosY]]),
                 deduce_wumpus( ArrowCells, CellsKnowNoWumpus, CellsSmelly,
                                WumpusFound, [DeducedPosX, DeducedPosY] ),
%printColor(pink, "deduce_wumpus( %w, %w, %w, %w, %w )\n", [ArrowCells, CellsKnowNoWumpus, CellsSmelly,
%                                WumpusFound, [DeducedPosX, DeducedPosY]]),
                 ( WumpusFound ->
                         ( ( cell_in_list( [KnownWumpusPosX, KnownWumpusPosY], ArrowCells,
                                           CellInList ),
                           CellInList = true ) ->
                                  SureToHit = true,
                                  WumpusPosX = DeducedPosX,
                                  WumpusPosY = DeducedPosY
                         ;
                                  SureToHit = false,
                                  WumpusPosX = DeducedPosX,
                                  WumpusPosY = DeducedPosY
                         )
                 ;
                         SureToHit = false,
                         WumpusPosX = "N/A",
                         WumpusPosY = "N/A"
                 )
         ).

:- mode cell_in_list(++, ++, -).
cell_in_list( _Cell, [], Result ) :- !,
        Result = false.

cell_in_list( Cell, [Cell | _Rest], Result ) :- !,
        Result = true.

cell_in_list( Cell, [_Head | Rest], Result ) :-
        /** Because of cut in second clause, Head is different from Cell */
        cell_in_list(Cell, Rest, ResultTmp),
        Result = ResultTmp.

:- mode compute_distance_to_closest_safe_cell(++, ++, ++, -).
compute_distance_to_closest_safe_cell( AgentX, AgentY, SafeCells, Result ) :-
        ( length(SafeCells, 0) ->
                 world( _XMin, _YMin, XMax, YMax ),
                 Result is (XMax * YMax)
        ;
%                 printColor( green, "Computing distance to closest safe cell from [%w, %w]... \n", [AgentX, AgentY]),
                 compute_distance_list( AgentX, AgentY, SafeCells, DistanceList ),
%                 print_list( DistanceList ),
                 minlist( DistanceList, MinDist ),
%                 printColor( green, "Manhattan is: %w \n", [MinDist]),
                 Result = MinDist
        ).

:- mode compute_distance_list(++, ++, ++, -).
compute_distance_list( _PosX, _PosY, [], OutputList ) :- !, 
        OutputList = [].

compute_distance_list( PosX, PosY,
                       [[CellX, CellY] | Rest], OutputList ) :-
        manhattan_dist([PosX, PosY], [CellX, CellY], Distance),
        compute_distance_list( PosX, PosY, Rest, OutputListTmp ),
        OutputList = [Distance | OutputListTmp].

:- mode manhattan_dist(++, ++, -).
manhattan_dist( [X1, Y1], [X2, Y2], Result ) :-
        DistX is (X1 - X2),
        DistY is (Y1 - Y2),
        abs(DistX, AbsDistX),
        abs(DistY, AbsDistY),
        Result is (AbsDistX + AbsDistY).        
              
%  Takes a list of 2-element lists, and sorts them increasingly
%  first by the first dimension, and then by the second dimension.
:- mode sort_list_of_2D_vectors(++, -).
sort_list_of_2D_vectors( List, Result ) :-
%        printColor( blue, "List before sorting: %w\n", [List]),
        %  We first sort by the second component and keep results
        %  with the same y-component.
        sort(0, <, List, Result).
%        sort(1, =<, List, ResultTmp1),
%%        sort(2, =<, List, ResultTmp),
        %  As sorting is stable, we won't destroy the presorting
        %  by the second component now that we sort by the first one.
%        sort(1, =<, ResultTmp1, ResultTmp2).
        %  Remove duplicates.
%        sort([1,2], =, ResultTmp1, ResultTmp2).

%        %  Replace all elements that are lists of two elements [X,Y] by a
%        %  functor vec(X,Y). This is necessary as the functor of a list is
%        %  ".", its first argument is "X", but its second argument is "[Y]".
%        findall( VecX,
%                 ( member(X, List),
%                   arg(1, X, Arg1),
%                   arg(2, X, Arg2List),
%                   Arg2List = [Arg2],
%                   VecX = vec(Arg1, Arg2)
%                 ),
%                 Vecs ),
%        sort(2, =<, Vecs, VecsSorted1),
%        sort(1, =<, VecsSorted1, VecsSorted),
%        printColor( blue, "VecsSorted: %w\n", [VecsSorted]),
%        %  Convert all vecs back to the list format. Note, that the 
%        %  ordering will be retained due to matching order of member.
%        findall( ListX,
%                 ( member(X, VecsSorted),
%                   arg(1, X, Arg1),
%                   arg(2, X, Arg2),
%                   ListX = [Arg1, Arg2]
%                 ),
%                 Result ),

%        printColor( blue, "List after sorting: %w\n", [Result]).

 

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

execdelay.
%execdelay :- realSleep(2).

:- write(" <-- loading xtra.pl done.\n").
