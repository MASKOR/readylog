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
 *           $Id: run.pl 68 2007-12-06 18:30:15Z stf $
 *        author: Daniel Beck <beck@kbsg.rwth-aachen.de>
 *   description: run-time file.
 *
 * ************************************************************************ */

:- write(" --- loading run.pl ...\n").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  load everything we need to get going               %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

%% some utilities
:- ensure_loaded("utils.pl").
%% connection to execution system
:- ensure_loaded("xtra.pl").
%% the agent program
:- ensure_loaded("agent.readylog").
%% the pre-processed agent (SSAs and stuff)
:- ensure_loaded("processed_agent.pl").

%:- initial_val( pos, V ), setval(real_pos, V).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  TESTING                                            %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

t1 :- icp( [exogf_Update, ?(V = epf_agent_pos), cout(V)] ).
t2 :- icp( [ exogf_Update,
             ?(V1 = epf_human_pos) ,cout(V1),
             step_right, ?(V = epf_agent_pos), cout(V),
             ?(V2 = epf_human_pos) ,cout(V2)] ).


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  SHORTUTS                                           %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  Shortcuts (for convenience)

/** startup visualization 
 *  and draw initial stuff
 */
/*
vis :-  display,
	getval(real_start_pos, [StartX, StartY]),
	draw_start(StartX,StartY),
	getval(real_goal_pos, [GoalX, GoalY]),
	draw_goal(GoalX,GoalY),
	getval(real_item_pos, [ItemX, ItemY]),
	draw_item(ItemX,ItemY).
*/



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- write(" --> loading run.pl done.\n").
