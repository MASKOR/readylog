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
        findall( ValXNoComma,
                 ( member(X, Fluents),
                   /** check if fluent is instantiated */
%%                   ( ground(X) ->
                   ( (ground(X), has_val(X, ValTmpX, S)) ->
%                          subf(X, ValX, S)
                          has_val(X, ValX, S),
                          /** replace commas, as C4.5 forbids them in
                           *  attribute values */
                          term_string(ValX, ValXString),
                          replace_string(ValXString, ",", "COMMA",
                                         ValXStringNoComma, stdout),
                          string_to_list(ValXStringNoComma, ValXNoComma)
                   ;
                          printf(stdout, "*** Warning: *** ", []),
                          printf(stdout, "Fluent %w is not instantiated. ", [X]),
                          printf(stdout, "Fluent is ignored.\n", [])
                   )%,
                   /** ignore uninstantiated fluents */
%                   ground(X)
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
 *  domain of the fluents, we process the code to find out
 *  which value each discrete fluent can take. */

write_learning_instance( solve(Prog, Horizon, RewardFunction), Policy, S ) :-
        /** Create a hash key for the solve context. */
        getval(solveHashTable, SolveHashTable),
        term_hash(solve(Prog, Horizon, RewardFunction), -1, 1000, HashKey),
        printf(stdout, "solve has hash key %w.\n", [HashKey]),
        ( hash_contains(SolveHashTable, HashKey) ->
                /** solve context has been encountered before. */
                printf(stdout, "This solve context has been encountered before.\n", []),
                term_string(HashKey, HashKeyString),
                /** Continue with .names file. */
                concat_string(["solve_context_", HashKeyString, ".names"], FileName),
                /** Check, if the decision (policy) has been already declared. */
                open(FileName, read, NameStreamRead),
                read_string(NameStreamRead, end_of_file, _Length, NameStreamString),
                close(NameStreamRead),
                term_string(Policy, PolicyString),
                printf(stdout, "PolicyString: %w.\n", [PolicyString]),
                /** replace commas in policy, as C4.5 forbids them in class names */
                replace_string(PolicyString, ",", "COMMA",
                               PolicyStringNoComma, stdout),
                ( substring(NameStreamString, PolicyString, _Pos) ->
                                printf(stdout, "Policy already declared.\n", []),
                                true
                ;
                                printf(stdout, "Declaring policy.\n", []),
%                                open(FileName, append, NameStream),
                                open(FileName, update, NameStream),
                                string_length(NameStreamString, NameStreamStringLength),
                                substring(NameStreamString, ".|append policies here|",
                                          PolicyAppendingPos),
                                PolicyAppendingPosLeft is (PolicyAppendingPos - 1),
                                substring(NameStreamString, 1, PolicyAppendingPosLeft,
                                          NameStreamStringLeft),
%                                string_length(NameStreamStringLeft, LeftLength),
                                RestLength is (NameStreamStringLength - PolicyAppendingPosLeft),
                                substring(NameStreamString, PolicyAppendingPos,
                                          RestLength, NameStreamStringRight),
                                concat_string([NameStreamStringLeft, ", ", PolicyStringNoComma,
                                                NameStreamStringRight], NameStreamStringNew),
%                                printf(NameStream, ", %w", [Policy]),
                                printf(NameStream, ", %w", [NameStreamStringNew]),
                                close(NameStream)
                ),
                /** Continue with .data file. */
                concat_string(["solve_context_", HashKeyString, ".data"], FileData),
                open(FileData, append, DataStream),
                get_all_fluent_values(S, FluentValues),
                /** remove outer brackets [] */
                list_to_string(FluentValues, FluentValuesString),
                printf(DataStream, "%w, %w\n", [FluentValuesString, PolicyStringNoComma]),
                close(DataStream)
        ;
                /** solve context encountered for the first time. */
                printf(stdout, "First encounter of this solve context.\n", []),
                hash_set(SolveHashTable, HashKey,
                         solve(Prog, Horizon, RewardFunction)),
                         /** update "global" variable */
                         setval(solveHashTable, SolveHashTable),
                         /** Construct a C4.5 .names file for this solve context. */
                         term_string(HashKey, HashKeyString),
                         concat_string(["solve_context_", HashKeyString, ".names"],
                                       FileName),
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
                         printf(NameStream, "| separated by commas and terminated by a fullstop:\n", []),
                         printf(NameStream, "\n", []),
                         /** replace commas, as C4.5 forbids them in class names */
                         term_string(Policy, PolicyString),
                         replace_string(PolicyString, ",", "COMMA", PolicyStringNoComma, stdout),
                         printf(NameStream, "%w", [PolicyStringNoComma]),
                         printf(NameStream, ".|append policies here|", []),
                         printf(NameStream, "\n", []),
                         printf(NameStream, "\n", []),
                         printf(NameStream, "| Then we list the attributes (fluents) and their domains.\n", []),
                         printf(NameStream, "| The domains can be continuous, discrete, or a set of discrete values\n", []),
                         printf(NameStream, "| (notated as a comma-separated list).\n", []),
                         printf(NameStream, "\n", []),
                         get_all_fluent_names(FluentNames),
                         /** As we are not given the domain for discrete fluents
                          *  by the Readylog programmer, we must find out a priori
                          *  which values each discrete fluent can take. */
                         ( foreach(Fluent, FluentNames),
                           param(NameStream)
                           do
                                 ( is_cont_fluent(Fluent) ->
                                         printf(NameStream, "%w: continuous.\n", [Fluent])
                                 ;
                                         ( is_prim_fluent(Fluent) ->
%                                                 get_domain(Fluent, Domain), 
%                                                 printf(NameStream, "%w: %w.\n", [Fluent, Domain])
                                                 printf(NameStream, "%w: discrete 100.\n", [Fluent])
                                         ;
                                                 printf(NameStream, "%w: discrete 100.\n", [Fluent]),
                                                 printf(NameStream, "| WARNING: is neither cont nor prim!\n,", [Fluent]),
                                                 printf(stdout, "*** WARNING ***: %w is neither cont nor prim!", [Fluent])
                                         )
                                 )
                         ),
                         close(NameStream),
                         /** Construct a C4.5 .data file for this solve context. */
                         concat_string(["solve_context_", HashKeyString, ".data"], FileData),
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
                         
%                         printf(DataStream, "%w\n", [FluentNames]),

                         get_all_fluent_values(S, FluentValues),
                         /** remove outer brackets [] */
                         list_to_string(FluentValues, FluentValuesString),
                         length(FluentNames, FluentNo),
                         length(FluentValues, ValueNo),
                         printf(stdout, "%w Fluents, %w Fluent Values\n", [FluentNo, ValueNo]),
                         ( (FluentNo \= ValueNo) ->
                                 printf(stdout, "*** Warning: Numbers of fluents and values are not the same!\n", [])
                         ;
                                 true
                         ),
%                         print_list(FluentNames),
%                         print_list(FluentValues),
                         printf(DataStream, "%w, %w\n", [FluentValuesString, PolicyStringNoComma]),
                         close(DataStream)
        ).

/** Find out which values the (discrete) Fluent can take. */
get_domain(Fluent, Domain) :-
    %    compile("xtra.pl"),
    % /** Doesn't help, even if commentaries would be filtered out.
    %  *  The second parameter of setval may contain variables. */
%        system('grep -o "setval(.*,.*)" xtra.pl | awk '{ print $2 }''). /** Doesn't help, even if commentaries would be filtered out
                 findall( FluentVal,
                 setval(Fluent, FluentVal),
                 Domain ).

% }}}
