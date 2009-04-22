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
dp_profile :- toggle_iplearn,
              vis_wumpus,
              initialise_shades,
              TotalRuns = 2,
              printf("######## Run 1 of %w... ########\n", [TotalRuns]),
              icp( find_gold(6, _) ),
              reset_values,
              printf("######## Run 2 of %w... ########\n", [TotalRuns]),
              icp( find_gold(3, _) ),
              reset_values.

dp :- toggle_iplearn,
      initialise_iplearner,
      ( exists('wumpus.log') ->
         delete('wumpus.log')
      ;
         true
      ),
      open('wumpus.log', write, StreamLog),
      vis_wumpus,
      initialise_shades,
      TotalRuns = 1500,
      printf(StreamLog, "# TotalRuns = %w\n", [TotalRuns]),
      setval(total_gold_found, 0),
      setval(avg_user_time_per_level, 0.0),
      setval(avg_system_time_per_level, 0.0),
      setval(avg_real_time_per_level, 0.0),
      statistics(times, [GlobalStartTimeUser, 
                         GlobalStartTimeSystem,
                         GlobalStartTimeReal]),
      date(StartDate),
      printf(StreamLog, "# StartTime = %w\n\n", [StartDate]),
      printf(StreamLog, "-----------------------------------------------------------------------\n", []),
      printf(StreamLog, "Level \t Time for Level                \t Found Gold \t Goldbars Found\n", []),
      printf(StreamLog, "      \t [UserCPU, SystemCPU, Session] \t            \t (in percent) \n", []),
      printf(StreamLog, "      \t (in seconds)\n", []),
      printf(StreamLog, "-----------------------------------------------------------------------\n\n", []),
      ( count(I,1314,TotalRuns),
        param(TotalRuns, StreamLog, GlobalStartTimeUser, GlobalStartTimeSystem, GlobalStartTimeReal)
        do
          statistics(times, [LevelStartTimeUser,
                             LevelStartTimeSystem,
                             LevelStartTimeReal]),
          printf("######## Run %w of %w... ########\n", [I, TotalRuns]),
          printf(StreamLog, "%w \t ", [I]),
          icp( find_gold(I, FoundGold) ),
          reset_values,
          statistics(times, [LevelEndTimeUser,
                             LevelEndTimeSystem,
                             LevelEndTimeReal]),
          LevelTimeDiffUserTmp is (LevelEndTimeUser - LevelStartTimeUser),
          round(LevelTimeDiffUserTmp, LevelTimeDiffUser),
          LevelTimeDiffSystemTmp is (LevelEndTimeSystem - LevelStartTimeSystem),
          round(LevelTimeDiffSystemTmp, LevelTimeDiffSystem),
          LevelTimeDiffRealTmp is (LevelEndTimeReal - LevelStartTimeReal),
          round(LevelTimeDiffRealTmp, LevelTimeDiffReal),
          printf("######## Finished level %w in %w seconds. ########\n",
                 [I, LevelTimeDiffReal]), flush(stdout),
          printf(StreamLog, "[%w, %w, %w] \t \t ", [LevelTimeDiffUser,
                                                    LevelTimeDiffSystem,
                                                    LevelTimeDiffReal]),

          getval(avg_user_time_per_level, AvgUserTimePerLevel),
          AvgUserTimePerLevelSum is (AvgUserTimePerLevel + LevelTimeDiffUser),
          AvgUserTimePerLevelDiv is (AvgUserTimePerLevelSum / 2.0),
          round(AvgUserTimePerLevelDiv, AvgUserTimePerLevelNew),
          setval(avg_user_time_per_level, AvgUserTimePerLevelNew),

          getval(avg_system_time_per_level, AvgSystemTimePerLevel),
          AvgSystemTimePerLevelSum is (AvgSystemTimePerLevel + LevelTimeDiffSystem),
          AvgSystemTimePerLevelDiv is (AvgSystemTimePerLevelSum / 2.0),
          round(AvgSystemTimePerLevelDiv, AvgSystemTimePerLevelNew),
          setval(avg_system_time_per_level, AvgSystemTimePerLevelNew),

          getval(avg_real_time_per_level, AvgRealTimePerLevel),
          AvgRealTimePerLevelSum is (AvgRealTimePerLevel + LevelTimeDiffReal),
          AvgRealTimePerLevelDiv is (AvgRealTimePerLevelSum / 2.0),
          round(AvgRealTimePerLevelDiv, AvgRealTimePerLevelNew),
          setval(avg_real_time_per_level, AvgRealTimePerLevelNew),

          ( FoundGold ->
             getval(total_gold_found, TotalGoldFound),
             TotalGoldFoundNew is (TotalGoldFound + 1),
             setval(total_gold_found, TotalGoldFoundNew),
             printf("######## Found the gold in level %w. ########\n",
                    [I]), flush(stdout),
             printf(StreamLog, "true \t \t ", [])
          ;
             printf("######## Didn't find the gold in level %w. ########\n",
                    [I]), flush(stdout),
             printf(StreamLog, "false \t \t ", [])
          ),
          getval(total_gold_found, TGF),
          float(TGF, TGFf),
          float(I, If),
          TGFPercentTmp1 is (TGFf / If),
          TGFPercentTmp2 is (TGFPercentTmp1 * 100.0),
          round(TGFPercentTmp2, TGFPercent),
          printf("######## Found %w of %w possible goldbars (%w percent). ########\n\n",
                 [TGF, I, TGFPercent]), flush(stdout),
          printf(StreamLog, "%w \n", [TGFPercent]),

          LevelsLeft is (TotalRuns - I),
          RealTimeRemainingTmp is (LevelsLeft * AvgRealTimePerLevelNew),
          round(RealTimeRemainingTmp, TimeRemaining),
          seconds_to_hms(TimeRemaining, ETAHours, ETAMinutes, ETASeconds),
          printf("######## Estimated time left for solving all levels: %w:%w:%w (H:M:S). ########\n",
                 [ETAHours, ETAMinutes, ETASeconds]), flush(stdout),
          ( mod(I, 10, 0) ->
             %  Log some statistics after every 10th level.
             print_statistics(I,
                              GlobalStartTimeUser,
                              GlobalStartTimeSystem,
                              GlobalStartTimeReal,
                              StreamLog)
          ;
             true
          )
      ),
      print_statistics(TotalRuns,
                       GlobalStartTimeUser,
                       GlobalStartTimeSystem,
                       GlobalStartTimeReal,
                       stdout),
      print_statistics(TotalRuns,
                       GlobalStartTimeUser,
                       GlobalStartTimeSystem,
                       GlobalStartTimeReal,
                       StreamLog),
      close(StreamLog).

      
dpdebug :- toggle_dtdebug, vis_wumpus, initialise_wumpus_lists, icp( find_gold ).
dp_novis :- initialise_wumpus_lists, icp( find_gold ).

%  Converts a unix Time in seconds into three values
%  for Hours, Minutes, and Seconds.
:- mode seconds_to_hms(++, -, -, -).
seconds_to_hms(Time, Hours, Minutes, Seconds) :-
        HoursTmp1 is (Time / 3600),
        floor(HoursTmp1, HoursTmp2),
        integer(HoursTmp2, Hours),

        MinutesTmp1 is (Time / 60),
        floor(MinutesTmp1, MinutesTmp2),
        integer(MinutesTmp2, MinutesTmp3),
        Minutes is (MinutesTmp3 mod 60), 

        round(Time, SecondsTmp1),
        integer(SecondsTmp1, SecondsTmp2),
        Seconds is (SecondsTmp2 mod 60).

%  Prints some time statistics to Stream.
:- mode print_statistics(++, ++, ++, ++, ++).
print_statistics( CurrentRun,
                  GlobalStartTimeUser, GlobalStartTimeSystem, GlobalStartTimeReal,
                  Stream ) :-
        statistics(times, [GlobalEndTimeUser, 
                           GlobalEndTimeSystem,
                           GlobalEndTimeReal]),
        GlobalTimeDiffUser is (GlobalEndTimeUser - GlobalStartTimeUser),
        GlobalTimeDiffSystem is (GlobalEndTimeSystem - GlobalStartTimeSystem),
        GlobalTimeDiffReal is (GlobalEndTimeReal - GlobalStartTimeReal),
        seconds_to_hms(GlobalTimeDiffUser, GTDHoursUser, GTDMinutesUser, GTDSecondsUser),
        seconds_to_hms(GlobalTimeDiffSystem, GTDHoursSystem, GTDMinutesSystem, GTDSecondsSystem),
        seconds_to_hms(GlobalTimeDiffReal, GTDHoursReal, GTDMinutesReal, GTDSecondsReal),
        printf(Stream, "\n# Finished %w runs in %w hours, %w minutes, %w seconds of User CPU Time.\n",
               [CurrentRun, GTDHoursUser, GTDMinutesUser, GTDSecondsUser]),
        getval(avg_user_time_per_level, ATPLUser),
        printf(Stream, "# Average user cpu time per level: %w seconds.\n",
               [ATPLUser]), flush(stdout),
        printf(Stream, "# Finished %w runs in %w hours, %w minutes, %w seconds of System CPU Time.\n",
               [CurrentRun, GTDHoursSystem, GTDMinutesSystem, GTDSecondsSystem]), flush(stdout),
        getval(avg_system_time_per_level, ATPLSystem),
        printf(Stream, "# Average system cpu time per level: %w seconds.\n",
               [ATPLSystem]), flush(stdout),
        printf(Stream, "# Finished %w runs in %w hours, %w minutes, %w seconds of Real Session Time.\n",
               [CurrentRun, GTDHoursReal, GTDMinutesReal, GTDSecondsReal]), flush(stdout),
        getval(avg_real_time_per_level, ATPLReal),
        printf(Stream, "# Average real session time per level: %w seconds.\n\n",
               [ATPLReal]), flush(stdout).


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
	getval(real_gold_pos_X, GoldX),
	getval(real_gold_pos_Y, GoldY),
	draw_gold(GoldX,GoldY).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- write(" --> loading run.pl done.\n").
