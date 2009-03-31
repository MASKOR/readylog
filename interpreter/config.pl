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
 *  the Free Software Foundation; version 2 of the License
 * ***************************************************************************
 *
 *           $Id$
 *        author: alexander ferrein
 *   description: Global config values and definitions
 * last modified: $Date$
 *            by: $Author$
 *
 * **************************************************************************/

/** debug flag.
 *  trace calls to Trans.
 *  used in the predicates gologtrace and doTrace (defined below).
 */
:- setval( tracegolog, false ).

/** progession of database.
 *  this flag enables/disables progression of the database
 *  it means that after each action the database will be progressed
 */
progression_enabled :- false. %true

/** progression of the database.
 *  the database is progressed after n actions and regressed before
 *  aftter prgrat action in the action history progression is applied
 *  it turnes out that for the soccer specification a number of 10 is
 *  a good threshold 
 */
%pe :- false.
pe :- true.
:- setval(prgrat, 10).
%  <DP was here>
/** saving the epf-values to be able to regress prim_fluents that
 *  depend on epfs. (Turn off, if you use parameterised epfs!)
 */
:- setval(save_epf_values, true).
save_epf_values :- getval(save_epf_values, X), X=true.
toggle_save_epf_values :- getval(save_epf_values, X),
       (
          X ->
          setval(save_epf_values, false),
          printf("save_epf_values turned OFF\n", [])
       ;
          setval(save_epf_values, true),
          printf("save_epf_values turned ON\n", [])
       ).

/* einfache Projektion oder Progression waehrend der Projektion? */
simple_projection :- not progression_enabled, !.
simple_projection :- true.

/* Testausgabe bei der Projektion ? */
projection_debug :- fail.

/** Delta ist die kleinste Einheit, in der ein kontinuierlicher
 *  Fluent gemessen wird.
 */
delta(0.000001).


/** use qualitative predicates for planning.
 *  if set all fluent and functions values are abstracted by the
 *  qualitative world model. 
 */
:- setval(use_qq, false).


/** use state abtraction based on qualitative world model.
 */
use_state_abstraction :- true.

/** use caching of previously computed plans.
 */
use_caching :- true.


/** print exogenous action queue.
 */
:- setval(debug_exoq, true).

% <DP was here>
/** IPLearn: Inductive policy learning for DT-planning.
 * turn on for activating the IPLearning component
 */
:- setval(iplearn, false).
iplearn :- getval(iplearn, X), X=true.
toggle_iplearn :- getval(iplearn, X),
       (
          X ->
          setval(iplearn, false),
          printf("IPLearn turned OFF\n", [])
       ;
          setval(iplearn, true),
          printf("IPLearn turned ON\n", [])
       ).

/** Is IPL still in pre-training phase (collecting calls for
 *  parameterised exogeneous fluents) */
:- setval(ipl_pre_training_phase, true).
ipl_pre_training_phase :- getval(ipl_pre_training_phase, X), X=true.

/** Is IPL still in training phase or already in consultation phase */
%:- setval(ipl_training_phase, true).
%ipl_training_phase :- getval(ipl_training_phase, X), X=true.

/** Constant giving a maximum threshold for the hypothesis error [0,1].
 *  If the error of the decision tree of a solve context it below that
 *  error, we switch to the consultation phase for that solve. */
:- setval(max_hypothesis_error, 0.2).

/** Create a hash table to store the filenames (keys) for
 *  the different solve contexts (values). */
%:- local reference(solve_hash_zable).
:- hash_create(SolveHashTable), setval(solve_hash_table, SolveHashTable).

/** Constant defining the maximum domain size for a pickBest. */
pick_best_domain_size_max(10).

/** Constant telling if there are parameterised exogeneous
 *  primitive fluents (compounds containing vars) in the world model. */
:- setval( param_exog_prim_fluents, false ).
param_exog_prim_fluents :- getval(param_exog_prim_fluents, X), X=true.

/** Global list that stores has_val calls to parameterised exogeneous
 *  primitive fluents (compounds containing vars), whenever IPLearning
 *  is active. */
:- setval( param_exog_prim_fluent_calls, [] ).

/** System time of the last change
 *  to the list param_exog_prim_fluent_calls */
:- setval( last_change_to_fluent_calls, _Uninstantiated ).

/** Set the (heuristic) time difference, that we use to decide
 *  when to start with the IPL training phase.
 *  If the time of the last change to the list
 *  param_exog_prim_fluent_calls has been over for
 *  param_exog_prim_fluent_delta, then determine_ipl_phase/1
 *  triggers the training phase. */
:- setval( param_exog_prim_fluent_delta, 0.2 ).

 
% </DP was here>

