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


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  SHORTUTS                                           %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%  Shortcuts (for convenience)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- write(" --> loading run.pl done.\n").
