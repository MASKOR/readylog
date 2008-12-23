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
 *  the Free Software Foundation; only version 2 of the License!
 * ***************************************************************************
 *
 *           $Id: iplearner.pl 183 2008-12-22 10:05:00Z dp $
 *         @date: 22.12.08
 *       @author: Dennis Pannhausen <Dennis.Pannhausen@rwth-aachen.de>
 *   description: (i)nductive (p)olicy (l)earning
 *
 * **************************************************************************/

/* ================================================================== */
/*  INDUCTIVE POLICY LEARNER                                          */
/* ================================================================== */

/** If the corresponding flag is true, the IPLearner is invoked
 *  whenever a solve statement is interpreted by Readylog.
 *  The task of inductive policy learning is to learn a decision
 *  tree from seeing different situations (described by all fluent
 *  values) and corresponding decisions (here: decision-theoretic
 *  policies).
 *  When the the decision tree for a certain solve statement is
 *  sufficiently good, the IPLearner replaces the solve statement
 *  by the tree in the Readylog code. Readylog then supplies the
 *  tree with the current fluent values and the tree returns a
 *  policy (approximating a decision theoretically planned one).
 */

:- write("** loading iplearner.pl\n"). 

/* --------------------------------------------------------- */
/*  Header + Flags                                           */
/* --------------------------------------------------------- */
% {{{ header + flags

%:- ensure_loaded('iplpreprocessor.pl').
%:- ensure_loaded('readylog.pl').
%:- ensure_loaded("../../utils/utils.pl").

/** for get_all_fluent_names */
:- lib(ordset).

% }}}

/* ----------------------------------------------------------
   utilities                           
---------------------------------------------------------- */

/** Compute an ordered set of all fluent names. */
get_all_fluent_names(Result) :-
        /** get all fluents */
        findall(PrimF, prim_fluent(PrimF), PrimFList),
        findall(ContF, cont_fluent(ContF), ContFList),
        findall(ExogPrimF, exog_prim_fluent(ExogPrimF), ExogPrimFList),
        findall(ExogCrimF, exog_cont_fluent(ExogCrimF), ExogContFList),
        /** get all registers (which are fluents, too) */
        findall(Reg, register(Reg), RegL),
        /** define all built-in fluents */
        BuiltInTemp=[online, start, pll(_, _, _, _), pproj(_, _), 
                     lookahead(_, _, _, _), bel(_), ltp(_)],
%        ignore_fluents(IFL), 
%        append(BuiltInTemp, IFL, BuiltIn),
        /** the list of all defined fluents */
        FluentLTmp=[PrimFList, ContFList, ExogPrimFList, ExogContFList],
        flatten(FluentLTmp, FluentL),
%        BuiltInTmp = [BuiltIn, RegL], flatten(BuiltInTmp, BuiltInL),
        BuiltInTmp = [BuiltInTemp, RegL],
        flatten(BuiltInTmp, BuiltInL),
        subtract(FluentL, BuiltInL, ResultList),
        list_to_ord_set(ResultList, Result).

/** Compute an ordered set of all fluent values. */
get_all_fluent_values(H, Result) :-
        get_all_fluent_names(Fluents),
        subfl(Fluents, Result, H).
