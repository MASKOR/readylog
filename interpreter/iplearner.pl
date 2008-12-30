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

:- ensure_loaded('iplutils.pl').
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
get_all_fluent_values(S, Result) :-
        get_all_fluent_names(Fluents),
        findall( ValX,
                 ( member(X, Fluents),
                   /** check if fluent is instantiated */
                   ( ground(X) ->
                          subf(X, ValX, S)
                   ;
                          printf(stdout, "*** Warning: *** ", []),
                          printf(stdout, "Fluent %w is not instantiated. ", [X]),
                          printf(stdout, "Fluent is ignored.\n", [])
                   )
                 ),
                 Result ).
%        Fluents = [First | Rest],
%        subf(First, ValFirst, H),
%        Result = [ValFirst].
%        subfl(Fluents, Result, H).

/* --------------------------------------------------------- */
/*  Learning Instances                                       */
/* --------------------------------------------------------- */
% {{{ Learning Instances

/** The C4.5 .names file defines the values for the decision
 *  outcomes, and the domains of the attribute values. The domain
 *  can be "continuous", "discrete", or a comma-separated list of
 *  discrete values (which is preferable to "discrete").
 *  As we do not require the Readylog programmer to declare the
 *  domain of the fluents, we try to optimise the automatic
 *  declaration at least. We do so by updating the .names
 *  file adaptively, whenever a new instance is added. */

write_learning_instance( solve(Prog, Horizon, RewardFunction), Policy, S ) :-
        /** Create a hash key for the solve context. */
        getval(solveHashTable, SolveHashTable),
        term_hash(solve(Prog, Horizon, RewardFunction), -1, 1000, HashKey),
        printf("solve has hash key %w.\n", [HashKey]),
        ( hash_contains(SolveHashTable, HashKey) ->
                /** solve context has been encountered before. */
                printf("This solve context has been encountered before.\n", []),
                term_string(HashKey, HashKeyString),
                /** Continue with .names file. */
                concat_strings("solve_context_", HashKeyString, TmpString1),
                concat_strings(TmpString1, ".name", FileName),
                /** Check, if the decision (policy) has been already declared. */
                open(FileName, read, NameStreamRead),
                read_string(NameStreamRead, end_of_file, _Length, NameStreamString),
                close(NameStreamRead),
                term_string(Policy, PolicyString),
                printf("PolicyString: %w.\n", [PolicyString]),
                ( substring(NameStreamString, PolicyString, _Pos) ->
                        printf("Policy already declared.\n", []),
                                true
                ;
                                printf("Declaring policy.\n", []),
                                open(FileName, append, NameStream),
                                printf(NameStream, ", %w", [Policy]),
                                close(NameStream)
                ),
                /** Continue with .data file. */
                concat_strings("solve_context_", HashKeyString, TmpString2),
                concat_strings(TmpString2, ".data", FileData),
                open(FileData, append, DataStream),
                get_all_fluent_values(S, FluentValues),
                /** remove outer brackets [] */
                list_to_string(FluentValues, FluentValuesString),
                printf(DataStream, "%w, %w\n", [FluentValuesString, Policy]),
                close(DataStream)
        ;
                /** solve context encountered for the first time. */
                printf("First encounter of this solve context.\n", []),
                hash_set(SolveHashTable, HashKey,
                         solve(Prog, Horizon, RewardFunction)),
                         /** update "global" variable */
                         setval(solveHashTable, SolveHashTable),
                         /** Construct a C4.5 .name file for this solve context. */
                         term_string(HashKey, HashKeyString),
                         concat_strings("solve_context_", HashKeyString, TmpString1),
                         concat_strings(TmpString1, ".name", FileName),
                         open(FileName, write, NameStream),
                         printf(NameStream, "| ********************************\n", []),
                         printf(NameStream, "| * file generated from ReadyLog *\n", []),
                         printf(NameStream, "| ********************************\n", []),
                         printf(NameStream, "|\n", []),
                         printf(NameStream, "| This is the C4.5 declaration file for the solve context:\n", []),
                         term_string(solve(Prog, Horizon, RewardFunction), ContextString),
                         printf(NameStream, "| --------------------------------------------------------\n", []),
                         printf(NameStream, "| %w\n", [ContextString]),
                         printf(NameStream, "| --------------------------------------------------------\n", []),
                         printf(NameStream, "|\n", []),
                         printf(NameStream, "| First we list the possible decisions (policies),\n", []),
                         printf(NameStream, "| separated by commas:\n", []),
                         printf(NameStream, "\n", []),
                         printf(NameStream, "%w", [Policy]),
                         close(NameStream),
                         /** Construct a C4.5 .data file for this solve context. */
                         concat_strings("solve_context_", HashKeyString, TmpString2),
                         concat_strings(TmpString2, ".data", FileData),
                         open(FileData, write, DataStream),
                         printf(DataStream, "| ********************************\n", []),
                         printf(DataStream, "| * file generated from ReadyLog *\n", []),
                         printf(DataStream, "| ********************************\n", []),
                         printf(DataStream, "|\n", []),
                         printf(DataStream, "| This is the C4.5 instance data file for the solve context:\n", []),
                         printf(DataStream, "| --------------------------------------------------------\n", []),
                         printf(DataStream, "| %w\n", [ContextString]),
                         printf(DataStream, "| --------------------------------------------------------\n", []),
                         printf(DataStream, "|\n", []),
                         printf(DataStream, "| Each example consists of one line.\n", []),
                         printf(DataStream, "| First in this line comes a comma-separated list of the\n", []),
                         printf(DataStream, "| fluent values, and then the decision (policy).\n", []),
                         printf(DataStream, "\n", []),
                         
                         get_all_fluent_names(FluentNames),
                         /** remove outer brackets [] */
                         printf(DataStream, "%w\n", [FluentNames]),

                         get_all_fluent_values(S, FluentValues),
                         /** remove outer brackets [] */
                         list_to_string(FluentValues, FluentValuesString),
                         printf(DataStream, "%w, %w\n", [FluentValuesString, Policy]),
                         close(DataStream)
        ).

% }}}
