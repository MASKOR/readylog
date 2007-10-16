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
pe :- true.
:- setval(prgrat, 10).

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



