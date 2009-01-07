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
 *           $Id: wumpus_utils.pl 01 2008-08-14 11:11:15Z dp $
 *        author: Dennis Pannhausen <Dennis.Pannhausen@rwth-aachen.de>
 *   description: Utilities for the Wumpus domain in ReadyLog
 *
 * ************************************************************************ */

:- write(" --> loading wumpus_utils.pl ... \n").

/** Return a random number between A and B (inclusive)*/
random_number(Seed, A, B, Number) :-
        seed(Seed),
        random(N),
        Tmp1 is (B - A + 1),
        mod(N, Tmp1, Tmp2),
        Number is (A + Tmp2).

%/** Distribute pits randomly until NumberOfPits is 0. */
%distribute_pits_aux(0, _Seed) :- !.
%
%distribute_pits_aux(NumberOfPits, Seed) :-
%        world( XMin, YMin, XMax, YMax ),
%        SeedTmp1 is (Seed * 23),
%        mod( SeedTmp1, 2147483647, NewSeed1),
%        random_number(NewSeed1, XMin, XMax, XRand),
%        SeedTmp2 is (NewSeed1 * 23),
%        mod( SeedTmp2, 2147483647, NewSeed2),
%        random_number(NewSeed2, YMin, YMax, YRand),
%        ( is_pit(XRand, YRand) ->
%              /** Pit already exists. Try again. */
%              distribute_pits_aux(NumberOfPits, NewSeed2)
%        ;
%              assert(pit(XRand, YRand)),
%              NewNumberOfPits is (NumberOfPits - 1),
%              distribute_pits_aux(NewNumberOfPits, NewSeed2)
%        ).

:- write(" <-- loading wumpus_utils.pl done.\n").
