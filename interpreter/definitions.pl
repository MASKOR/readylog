/* ***************************************************************************
 *                                            ####   ####           .-""-.   
 *       # #                             #   #    # #    #         /[] _ _\  
 *       # #                                 #    # #             _|_o_LII|_ 
 * ,###, # #  ### ## ## ##   ###  ## ##  #   #    # #       ###  / | ==== | \
 * #   # # # #   # ## ## #  #   #  ## #  #   ###### #      #     |_| ==== |_|
 * #   # # # ####  #  #  #  #   #  #  #  #   #    # #      ####   ||" ||  || 
 * #   # # # #     #  #  #  #   #  #  #  #   #    # #    #    #   ||'----'|| 
 * '###'# # # #### #  #  ##  ### # #  ## ## #      # ####  ###   /__|    |__\
 * ***************************************************************************
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; only version 2 of the License.
 * ***************************************************************************
 *
 *            $Id$
 *         author: alexander ferrein
 *    description: readylog definitions
 *  last modified: $Date$
 *             by: $Author$
 *
 * **************************************************************************/

:- lib(clpr).
%:- use_module( library(clpr) ).
:- lib(fd).
:- lib(lists).
%:- lib(scattered).       /* to avoid error: non consecutive */
%% the above is deprecated (since v5.6) in favor of discontinguous/1
%% but we should be using dynamic anyway!
%:- lib(ordset).
% <DP was here>
/** support for hashing (needed for IPLearning) */
:- lib(hash). 
% </DP was here>

:- dynamic
	progression_enabled/0, pe/0, simple_progrssion/0, progression_debug/0,
	use_state_abstraction/0, use_caching/0,
	delta/1,
	senses/2, execute/3, 
	exog_action/1, exog_occurs/2,
	initial_val/2, initial_val/3, initial_Sit/1,
	initial_P/2, initial_LL/1, initial_LL/2, 
	current_val/3, temp_val/3, temp_val/4, causes_val/4, ssa/3, 
	proc/2, proc_model/2, proc_poss/2, proc_costs/3,
% <DP was here>
        /* --  IPL transformed procs             */
        ipl_proc/2,
% </DP was here>        
	/* --  stoch_procs                       */
	stoch_proc/1,
	stoch_proc_poss/2,
	stoch_proc_costs/3, 
	stoch_proc_outcomes/4,
	/* ---------------------------------   */
	prim_action/1, poss/2, prolog_poss/1,
	prim_fluent/1, cont_fluent/1,
	exog_prim_fluent/1, exog_cont_fluent/1,
	special_fluent/1,special_fluent/2,special_fluent/4,
	exog_fluent_getValue/3,
	user_init/0,
	exogf_update_action/1,
	const/1,
	function/3, prolog_function/1, prolog_function/3,
	exog_function/1, exog_function_getValue/2,
	t_function/1, t_function/3, register/1, cache/2,
	/* --  options: user provided            */
	option/3,          /* option-skeleton */
	option_init/3,     /* initiation condition */
	options_epsilon/1, /* epsilon for option convergence */
	option_mapping/4,  /* state, situation, conditions mapping */
	option_variables/2,/* alternatively: list state variables */
	option_sense/2,    /* sensing program for full-observability*/
	/* --  options: created by preprocessor  */
	opt_prob/4, opt_prob_max/3,
	opt_exit_prob/4, opt_exit_prob_max/3,
	opt_exp_reward/4, opt_exp_reward_max/3,
	opt_pol/4, opt_pol_max/3,
	opt_state/3, opt_costs/3,
	opt_probability_list/3,
	/* ---------------------------------     */
        param_cycletime/1,
	/* --  events                            */
	events_list/1, 
	event_model/2, 
	event_aux/1, 
	event_poss/2, 
	event_costs/3,
	/* --  events: created by preprocessor   */
	prolog_event_poss/2, 
	prolog_event_costs/3,
	/* --  prolog_poss: created by preprocessor   */
	prolog_poss/2,
	/* --  decisionTheoretic predicates */
        bestDoM/9,
        bestDoMOpt/10,
	/* --  other */
        causes_val/3,
        sets_val/4.



fluent(F) :- (prim_fluent(F);cont_fluent(F);exog_fluent(F)),!.
exog_fluent(F) :- (exog_prim_fluent(F) ; exog_cont_fluent(F)), !.

is_prim_fluent(F) :- (prim_fluent(F) ; exog_prim_fluent(F)), !.
is_cont_fluent(F) :- (cont_fluent(F) ; exog_cont_fluent(F)), !.


/* T2 is T1 with X1 substituted by X2 */
subv(_X1,_X2,T1,T2) :- (var(T1);number(T1)), !, T2 = T1. 
subv(X1,X2,T1,T2) :- T1 = X1, !, T2 = X2.    
subv(X1,X2,T1,T2) :- T1 =..[F|L1], subvl(X1,X2,L1,L2), T2 =..[F|L2]. 

subvl(_X1,_X2,[],[]). 
subvl(X1,X2,[T1|L1],[T2|L2]) :-
 	subv(X1,X2,T1,T2),
	subvl(X1,X2,L1,L2). 


