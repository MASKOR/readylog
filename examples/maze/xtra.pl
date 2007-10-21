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
 *        author: Stefan Schiffer <schiffer@cs.rwth-aachen.de>
 *   description: eXecution TRAnsformation which maps primitive actions 
 *                to something meaningful in the real world ...
 *
 * ************************************************************************ */

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/** exogf_Update.
 */
xTra(exogf_Update, _H) :- !, 
	printf(" *** exogf_Update *** \n", []), flush(output).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  cout in various variations.

xTra(cout(V), _H) :- !,
	printf("%w", [V]), flush(output).

xTra(cout(F, V), _H) :- !,
	printf(F, V), flush(output).

xTra(cout(Color,F,V),_H) :- !,
	printColor(Color,F,V).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  primitive actions in maze.
%%  includes calls to draw-routines

xTra(go_right,H) :- 
	printf(" EXEC: go_right", []), 
	has_val( pos, V, H ), 
	V = [X,Y], 
	printf(" at %w \n", [V]), 
	draw_action("R", X, Y).
xTra(go_left,H)  :- 
	printf(" EXEC: go_left", []), 
	has_val( pos, V, H ), 
	V = [X,Y], 
	printf(" at %w \n", [V]), 
	draw_action("L", X, Y).
xTra(go_up,H)    :- 
	printf(" EXEC: go_up", []), 
	has_val( pos, V, H ), 
	V = [X,Y], 
	printf(" at %w \n", [V]), 
	draw_action("U", X, Y).
xTra(go_down,H)  :- 
	printf(" EXEC: go_down", []), 
	has_val( pos, V, H ), 
	V = [X,Y], 
	printf(" at %w \n", [V]), 
	draw_action("D", X, Y).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update :- 
	printf(" --- UPDATE ...\n", []), 
	%refresh_window,
	redraw,
	printf(" --- UPDATE ... done.\n", []).



%% ABSORBER ENTRY %%%%%%%%%%%%%%%%%%%%%%%%
%% used to catch all the stuff
%% that doesn't have an own handle
xTra(Action,Situation) :-
	printColor( red, " xTra: DON'T KNOW HOW TO HANDLE '%w' IN SIT: '%w'\n", [Action,Situation]).
