/*
 ********************************************************************************
 *
 *     robocup knowledge base implementation file by Alexander Ferrein
 *     Copyright (C) 2002  KBSG, Aachen University <ferrein@cs.rwth-aachen.de>
 *
 *     This program is free software; you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation; either version 2 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 ********************************************************************************

                                                                             
                                             ####   ####           .-""-.    
        # #                             #   #    # #    #         /[] _ _\   
        # #                                 #    # #             _|_o_LII|_  
  ,###, # #  ### ## ## ##   ###  ## ##  #   #    # #       ###  / | ==== | \ 
  #   # # # #   # ## ## #  #   #  ## #  #   ###### #      #     |_| ==== |_| 
  #   # # # ####  #  #  #  #   #  #  #  #   #    # #      ####   ||" ||  ||  
  #   # # # #     #  #  #  #   #  #  #  #   #    # #    #    #   ||LI  o ||  
  '###'# # # #### #  #  ##  ### # #  ## ## #      # ####  ###    ||'----'||  
                                                                /__|    |__\ 
                                                                             

 ********************************************************************************
 *
 *  $Id: param.pl,v 1.1 2006/04/13 15:50:34 stf Exp $
 *
 *  Description: this loads all needed files for running the golog agent for MSL
 *
 *  Author:   Alexander Ferrein
 *  Contact:  <ferrein@cs.rwth-aachen.de>
 *
 *  last modified: $Date: 2006/04/13 15:50:34 $
 *             by: $Author: stf $
 *
 ********************************************************************************
*/


/* ----------------------------------------------------------
   shortcuts for convenience
   (1.3.04)
---------------------------------------------------------- */

config_highkick(Value) :-
    get_flag(hostname, Host),
    split_string(Host, ".", "", L1), 
    L1=[Name|Rest], 
    join_string(Rest, ".", Domain),
    split_string(Name, "_", "", L2), 
    L2=[N1 |_],
    L3=[N1, "_rc."], 
    append(L3, Domain, L4),
    flatten(L4, L5), 
    join_string(L5, "", Hostname), 
    Kicker="is_high_kick",
    config(rcsoftconfigfile(lowlevel(kicker(kickhardware(hostname(Hostname), kickconstant(name(Kicker), datatype("bool"), Value)))))). 


config_fielddata(S) :-
	config(rcsoftconfigfile(general(fielddata(S)))).

config_skillmodule(S) :-
	config(rcsoftconfigfile(midlevel(skillmodule(S)))).

config_currentlocation(Loc) :-
	config_fielddata(currentlocation(Loc)).

config_fieldlength(Length) :-
	config_currentlocation(Loc),
	config_fielddata(location(name(Loc),
				  length(datatype(_), Length))).

config_HighLevelParams(S) :-
	config(rcsoftconfigfile(highlevel(parameters(S)))).

/* ****************************************************** */


legal_BB_Number(X) :- (X=0;X=1;X=2;X=3;X=4).


rolename(X, Y) :-
	config(rcsoftconfigfile(general(rolenumber(name(YStr),
						   datatype(_),
						   X)))),
	term_string(Y, YStr).


/* start position by role name */
start_pos_by_rolename(X, Pos) :-
	term_string(X, XStr),
	config(rcsoftconfigfile(highlevel(positions(startpos(name(XStr),
							     datatype(_),
							     Pos))))).

/* kickoff position by role name */
kickoff_pos_by_rolename(X, Pos) :-
	term_string(X, XStr),
	config(rcsoftconfigfile(highlevel(positions(ourkickoffpos(name(XStr),
								  datatype(_),
								  Pos))))).

/* opponent kickoff position by role name */
opp_kickoff_pos_by_rolename(X, Pos) :-
	term_string(X, XStr),
	config(rcsoftconfigfile(highlevel(positions(ourkickoffpos(name(XStr),
								  datatype(_),
								  Pos))))).

goal_size(S) :-
	config_currentlocation(Loc),
	config_fielddata(location(name(Loc),
				  goalsize(datatype(_),
					   S))).


opponent_goal([X, 0.0]) :- config_fieldlength(Length), X is Length/2.0.
opponent_goalpost_left([X,Y]) :-
	opponent_goal([X, _Y]),
	config_currentlocation(Loc),
	config_fielddata(location(name(Loc),
				  goalsize(datatype(_),
					   GoalSize))),
	Y is -(GoalSize/2.0).
opponent_goalpost_right([X,Y]) :-
	opponent_goal([X, _Y]),
	config_currentlocation(Loc),
	config_fielddata(location(name(Loc),
				  goalsize(datatype(_),
					   GoalSize))),
	Y is (GoalSize/2.0).

/* ----------------------------------------------------------
   Thresholds
---------------------------------------------------------- */

threshold_dribblable(X, Y) :-
	config_skillmodule(motorabilities(dribbleballconstants(name("skill_dribble_ball_xdiff"),
							       datatype("float"),
							       X))),
	config_skillmodule(motorabilities(dribbleballconstants(name("skill_dribble_ball_ydiff"),
							       datatype("float"),
							       Y))).

threshold_kickable(X, Y) :-
	config_skillmodule(mergedabilities(movekickconstants(name("skill_move_and_kick_dokick_xdiff"),
							     datatype("float"),
							     X))),
	config_skillmodule(mergedabilities(movekickconstants(name("skill_move_and_kick_dokick_ydiff"),
							     datatype("float"),
							     Y))).

threshold_move_kick(X, Y) :-
	config_skillmodule(mergedabilities(movekickconstants(name("skill_move_and_kick_xdiff"),
							     datatype("float"),
							     X))),
	config_skillmodule(mergedabilities(movekickconstants(name("skill_move_and_kick_ydiff"),
							     datatype("float"),
							     Y))).

/* when to ignore local and take global ball pos instead */
%threshold_local_ballconfidence( 0.6 ).
threshold_local_ballconfidence( Conf ) :-
	config_skillmodule(baseabilities(baseskillconstants(name("ball_valid_confidence"),
							    datatype("float"),
							    Conf))).



/** angle which the robot should be able to
    dribble (drive) without loosing the ball
*/
threshold_dribble_angle(X) :-
	config_HighLevelParams(threshold(name("dribble_angle"),
					 datatype(_),
					 X)).
/** 1: minimal distance an opponent has to have from the dribble trajectory,
    2: min my distance (alternatively)
*/
threshold_dribble_min_opponent_distance(He, Me) :-
	config_HighLevelParams(threshold(name("dribble_min_distances"),
					 datatype(_),
					 [He, Me])).

threshold_move_kick_angle(X) :-
	config_HighLevelParams(threshold(name("movekick_angle"),
					 datatype(_),
					 X)).
threshold_move_kick_min_opponent_distance(He, Me) :-
	config_HighLevelParams(threshold(name("movekick_min_distances"),
					 datatype(_),
					 [He, Me])).

/* ****************************************************** */





/* ----------------------------------------------------------
   Specifications
---------------------------------------------------------- */



field_size(X, Y) :-
	config_currentlocation(Loc),
	config_fielddata(location(name(Loc),
				  length(datatype(_),
					 X))),
	config_fielddata(location(name(Loc),
				  width(datatype(_),
					Y))).



speed_drive( 0, S) :-
	config_HighLevelParams(specification(name("speed_drive"),
					     arguments("slow"),
					     datatype(_),
					     S)).
speed_drive( 1, S) :-
	config_HighLevelParams(specification(name("speed_drive"),
					     arguments("moderate"),
					     datatype(_),
					     S)).
speed_drive( 2, S) :-
	config_HighLevelParams(specification(name("speed_drive"),
					     arguments("fast"),
					     datatype(_),
					     S)).

speed_drive( X, S) :- X > 2,
	config_HighLevelParams(specification(name("speed_drive"),
					     arguments("fast"),
					     datatype(_),
					     S)).

speed_turn( S ) :- 
	config_HighLevelParams(specification(name("speed_turn"),
					     datatype(_),
					     S)).



/* distance we can shoot until the ball stops moving */
kick_distance(S) :-
	config_HighLevelParams(specification(name("kick_distance"),
					     datatype(_),
					     S)).

/* speed of ball after kick in m/s */
kick_speed(S) :-
	config_HighLevelParams(specification(name("kick_speed"),
					     datatype(_),
					     S)).

opponent_reaction_time(S) :-
	config_HighLevelParams(specification(name("opponent_reaction_time"), datatype("float"), S)).

opponent_max_speed(S) :-
	config_HighLevelParams(specification(name("opponent_max_speed"), datatype("float"), S)).




/* ****************************************************** */



/* ----------------------------------------------------------
   Parameters
---------------------------------------------------------- */

param_cycletime(S) :-
	config_HighLevelParams(parameter(name("cycle_time"),
					 datatype("float"),
					 S)).

/* for setting positions of ball and robot in planning */
param_intercept_distance(S) :-
	config_skillmodule(mergedabilities(interceptballconstants(name("skill_intercept_pre_ball_distance"),
								  datatype(_),
								  S))).

param_inFrontOfRobot_distance(S) :-
	param_intercept_distance(S).

/** the minimal X position a robot must have
to be considered for a pass */
param_pass_receiver_minimal_X(S) :-
	config_HighLevelParams(parameter(name("pass_receiver_minimal_x"),
					 datatype(_),
					 S)).

param_pass_targets_for_defender(S) :-
	config_HighLevelParams(parameter(name("pass_targets_for_defender"),
					 datatype("list"),
					 S)).

param_max_ballx_for_defender_bestInterceptor(S) :-
	config_HighLevelParams(parameter(name("max_ballx_for_defender_bestinterceptor"),
					 datatype(_),
					 S)).

param_min_ballx_for_attacker_bestInterceptor(S) :-
	config_HighLevelParams(parameter(name("min_ballx_for_attacker_bestinterceptor"),
					 datatype(_),
					 S)).


/** minimal X coord for position where to support at ball */
param_minX_for_support_at_ball(S) :-
	config_HighLevelParams(parameter(name("minx_for_support_at_ball"),
					 datatype(_),
					 S)).

/** latency for getting bestInterceptor (respectively stop being) */
param_bestInterceptor_Latency(S) :-
	config_HighLevelParams(parameter(name("bestinterceptor_latency"),
					 datatype("int"),
					 S)).

/* ****************************************************** */
