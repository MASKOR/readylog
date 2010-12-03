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
 *           $Id$
 *        author: Stefan Schiffer <schiffer@cs.rwth-aachen.de>
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

t0 :- icp([exogf_Update, g, cout(pos)]).

t1 :- icp([?(pos=[X, Y]), cout(X), cout(Y)]).

t2 :- icp([if(and([ pos = [X,Y],
                  f_is_pos(X,Y),
                  Y2 is Y + 1,
                  f_is_pos(X,Y2),
                  f_is_wall(X,Y,X,Y2)
                     ]), cout("1"), cout("2"))]).

t3 :- icp([ if(and([ pos = [X,Y],
                     is_pos(X,Y),
                     Y2 is Y + 1,
	             is_pos(X,Y2),
	             not is_wall(X,Y,X,Y2)
                   ]), 
               cout("1 (if)"), 
               cout("2 (else)"))
    ]).


/* stefan's test stuff */
%testf :- icp( and([ epf_sense_item = V ]) ).
testf :- icp( testf ).
stf :- vis, icp( findus ).
%stf :- vis, icp( search_item(X,Y) ).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  SHORTUTS                                           %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  Shortcuts (for convenience)

/** startup visualization 
 *  and draw initial stuff
 */
vis :-  display,
	getval(real_start_pos, [StartX, StartY]),
	draw_start(StartX,StartY),
	getval(real_goal_pos, [GoalX, GoalY]),
	draw_goal(GoalX,GoalY),
	getval(real_item_pos, [ItemX, ItemY]),
	draw_item(ItemX,ItemY).

/* dt-planning on primitive actions 
 * with simple reward function */
go_prim  :- icp( solve( goto(5,5), 10, reward ) ).

/* dt-planning on deterministic actions
 * with reward function that knows about goal */
go_det   :- icp( solve( goto_det(5,5), 10, reward(5,5) ) ).

/* dt-planning with stochastic actions
 * with reward function that knows about goal */
go_stoch :- icp( solve( goto_stoch(5,5), 10, reward(5,5) ) ).

/* dt-planning with stochastic actions
 * with a reward function that knows about goal
 * and uses a heuristic (manhatdist) */
go_stoheu :- icp( solve( goto_stoch(5,5), 9, reward_heuristic(5,5) ) ).

go_memo  :- icp( solve( goto_memo(3,3), 7, reward(3,3) ) ).
go_vis   :- icp( solve( goto_vis(3,3), 7, reward(3,3) ) ).
go_visit :- icp( solve( goto_vis(3,3), 7, reward_heuristic(3,3) ) ).

go_st :- icp( solve( goto_stoch(2,2), 4, reward_heuristic(2,2) ) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- write(" --> loading run.pl done.\n").
