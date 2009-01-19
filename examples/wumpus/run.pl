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
 *           $Id: run.pl 01 2008-08-14 18:33:15Z stf $
 *        author: Dennis Pannhausen <Dennis.Pannhausen@rwth-aachen.de>
 *   description: run-time file.
 *
 * ************************************************************************ */

:- write(" --- loading run.pl ...\n").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  load everything we need to get going               %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

%% connection to execution system
:- ensure_loaded("xtra.pl").
%% the agent program
:- ensure_loaded("agent.readylog").
%% the pre-processed agent (SSAs and stuff)
:- ensure_loaded("processed_agent.pl").
%% the ipl-pre-processed agent (transformed procs with
%% solve contexts in standard form)
:- ensure_loaded("ipl_processed_agent.pl").

%:- initial_val( pos, V ), setval(real_pos, V).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% wanna autorun?
%:- go.


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  TESTING                                            %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

%:- spy transPr.
%:- trace.
%:- printf("Here!\n", []), gos.

%t:- icp([if(f_is_pos(0,0), cout("1"), cout("2"))]).

%t0 :- icp([exogf_Update, g, cout(pos)]).

%t1 :- icp([?(pos=[X, Y]), cout(X), cout(Y)]).

%t2 :- icp([if(and([ pos = [X,Y],
%                  f_is_pos(X,Y),
%                  Y2 is Y + 1,
%                  f_is_pos(X,Y2),
%                  f_is_wall(X,Y,X,Y2)
%                     ]), cout("1"), cout("2"))]).

%t3 :- icp([ if(and([ pos = [X,Y],
%                     is_pos(X,Y),
%                     Y2 is Y + 1,
%	             is_pos(X,Y2),
%	             not is_wall(X,Y,X,Y2)
%                   ]), 
%               cout("1 (if)"), 
%               cout("2 (else)"))
%    ]).


/* dennis's test stuff */
%dp :- toggle_iplearn, vis_wumpus, initialise_wumpus_lists, icp( find_gold ).
dp :- toggle_iplearn,
      vis_wumpus,
      initialise_shades,
      icp( find_gold(6) ),
      icp( find_gold(3) ).
      
dpdebug :- toggle_dtdebug, vis_wumpus, initialise_wumpus_lists, icp( find_gold ).
dp_novis :- initialise_wumpus_lists, icp( find_gold ).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  SHORTUTS                                           %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  Shortcuts (for convenience)

initialise_wumpus_lists :- initialise_shades, initialise_breezy, initialise_smelly.

/** initialise unvisited cells */
initialise_shades :- get_shades_wumpus(SH),
                     setval( real_cells_shaded, SH ).

% Done by the agent
%/** initialise breezy cells */
%initialise_breezy :- get_breezy_wumpus(B),
%                     setval( real_cells_breezy, B ).

% Done by the agent
%/** initialise smelly cells */
%initialise_smelly :- get_smelly_wumpus(S),
%                     setval( real_cells_smelly, S ).

/** startup visualization 
 *  and draw initial stuff
 */
vis_wumpus :- display_wumpus,
%	getval(real_start_pos, [StartX, StartY]),
%	draw_start(StartX,StartY),
	getval(real_gold_pos, [GoldX, GoldY]),
	draw_gold(GoldX,GoldY).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- write(" --> loading run.pl done.\n").
