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
 *           $Id:$
 *        author: Stefan Schiffer <schiffer@cs.rwth-aachen.de>
 *   description: run-time file.
 *
 * ************************************************************************ */

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% load everything we need:
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% connection to execution system
:- ensure_loaded("xtra.pl").
%% the agent
:- ensure_loaded("agent.readylog").
%% the pre-processed agent
:- ensure_loaded("processed_agent.pl").


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% wanna autorun?
%:- go.


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  TEST shortuts

%:- spy transPr.
%:- trace.
%:- printf("Here!\n", []), gos.

t:- icp([if(f_is_pos(0,0), cout("1"), cout("2"))]).

t0 :- icp([exogf_Update, g, cout(pos)]).

t1 :- icp([?(pos=[X, Y]), cout(X), cout(Y)]).

t2 :- icp([if(and([ pos = [X,Y],
                  f_is_pos(X,Y),
                  Y2 is Y + 1,
                  f_is_pos(X,Y2),
                  f_is_wall(X,Y,X,Y2)
                     ]), cout("1"), cout("2"))]).

t3 :- 
	icp([ if(and([ pos = [X,Y],
	               is_pos(X,Y),
	               Y2 is Y + 1,
	               is_pos(X,Y2),
	               not is_wall(X,Y,X,Y2)
		   ]), 
		   cout("1"), cout("2"))
	       ]).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Shortcuts (for convenience)

vis :-
	display,
	draw_start(1,1),
	draw_goal(5,5).

go_prim  :- icp( solve( goto(5,5), 10, reward ) ).
go_det   :- icp( solve( goto_det(5,5), 10, reward(5,5) ) ).
go_stoch :- icp( solve( goto_stoch(5,5), 10, reward(5,5) ) ).
go_stocheu :- icp( solve( goto_stoch(5,5), 9, reward_heuristic(5,5) ) ).
go_memo  :- icp( solve( goto_memo(3,3), 7, reward(3,3) ) ).
go_vis   :- icp( solve( goto_vis(3,3), 7, reward(3,3) ) ).
go_visit :- icp( solve( goto_vis(3,3), 7, reward_heuristic(3,3) ) ).

