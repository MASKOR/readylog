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
 *           $Id: maze.readylog 68 2007-12-06 18:30:15Z stf $
 *        author: Daniel Beck <beck@kbsg.rwth-aachen.de>
 *   description: Definition of the Follow & Guide domain in ReadyLog
 *
 * ************************************************************************ */

manhattan_dist([Xfrom, Yfrom], [Xto, Yto], V) :-
        manhattan_dist(Xfrom, Yfrom, Xto, Yto, V).

manhattan_dist(Xfrom, Yfrom, Xto, Yto, V) :-
        Xtmp is Xfrom - Xto,
        Ytmp is Yfrom - Yto,
        abs(Xtmp, Xdiff),
        abs(Ytmp, Ydiff),
        V is Xdiff + Ydiff.

