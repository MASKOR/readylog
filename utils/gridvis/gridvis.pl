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
 *           $Id$
 *        author: Alexander Ferrein <ferrein@cs.rwth-aachen.de>
 *        mod by: Stefan Schiffer <schiffer@cs.rwth-aachen.de>
 *        mod by: Dennis Pannhausen  <Dennis.Pannhausen@rwth-aachen.de>
 *   description: external interface to draw gridworlds (maze etc)
 *
 * ************************************************************************ */


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  definition of external methods
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- external(start_display/3, "p_StartDisplay").

:- external(start_display_ng/4, "p_StartDisplayNG").

:- external(start_display_wumpus/6, "p_StartDisplayWumpus").

:- external(draw_action/3, "p_DrawAction").

:- external(update_pits/1, "p_UpdatePits").

:- external(update_breezes/1, "p_UpdateBreezes").

:- external(update_stenches/1, "p_UpdateStenches").

:- external(update_shades/1, "p_UpdateShades").

:- external(draw_goto/5, "p_DrawGoto").

:- external(draw_policy/3, "p_DrawPolicy").

:- external(draw_agent/2, "p_DrawAgent").

:- external(draw_arrow/3, "p_DrawArrow").

:- external(draw_wumpus_hunter/4, "p_DrawWumpusHunter").

:- external(draw_wumpus/3, "p_DrawWumpus").

:- external(draw_human/2, "p_DrawHuman").

:- external(draw_goal/2, "p_DrawGoal").

:- external(draw_gold/2, "p_DrawGold").

:- external(draw_item/2, "p_DrawItem").

:- external(draw_start/2, "p_DrawStart").

:- external(draw_values/1, "p_DrawValues").

:- external(refresh_window/0, "p_Refresh").

:- external(redraw/0, "p_Redraw").

:- external(clear_history/0, "p_ClearHistory").

:- external(clear_window/0, "p_Clear").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  shortcuts for specific calls
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sd1 :- start_display(10,10, []).

sd2 :- start_display(5,5, []).

sd3 :- start_display_ng(20,20, [], [[1,2],[2,3],[3,4],[4,5]]).

display :- get_domain(X, Y, W), start_display(X, Y, W).

display_ng :- get_domain_ng(X, Y, W, O), start_display_ng(X, Y, W, O).

display_wumpus :- get_domain_wumpus(X, Y, W, P, WI, S), start_display_wumpus(X, Y, W, P, WI, S).

