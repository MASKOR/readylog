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

% ================================================================== %
%  INDUCTIVE POLICY LEARNER                                          %
% ================================================================== %

%  If the corresponding flag is true, the IPLearner is invoked
%  whenever a solve statement is interpreted by Readylog.
%  The task of inductive policy learning is to learn a decision
%  tree from seeing different situations (described by all fluent
%  values) and corresponding decisions (here: decision-theoretic
%  policies).
%  When the the decision tree for a certain solve statement is
%  sufficiently good, the IPLearner replaces the solve statement
%  by the tree in the Readylog code. Readylog then supplies the
%  tree with the current fluent values and the tree returns a
%  policy (approximating a decision theoretically planned one).


:- write("** loading iplearner.pl\n"). 

% --------------------------------------------------------- %
%  Header + Flags                                           %
% --------------------------------------------------------- %
% {{{ header + flags

:- ensure_loaded('iplutils.pl').

%  needed by "get_all_fluent_names"
:- lib(ordset).

% }}}

% ---------------------------------------------------------- %
%  Utilities                                                 %
% ---------------------------------------------------------- %

%  Returns an ordered set Result of all fluent names.
:- mode get_all_fluent_names(-).
get_all_fluent_names(Result) :-
        %  Get all fluents.
        findall(PrimF, prim_fluent(PrimF), PrimFList),
        findall(ContF, cont_fluent(ContF), ContFList),
        findall(ExogPrimF, exog_prim_fluent(ExogPrimF), ExogPrimFList),
        findall(ExogCrimF, exog_cont_fluent(ExogCrimF), ExogContFList),
        %  Get all registers (which are fluents, too).
        findall(Reg, register(Reg), RegL),
        %  Define all built-in fluents.
%        BuiltInTemp=[online, start, pll(_, _, _, _), pproj(_, _), 
%                     lookahead(_, _, _, _), bel(_), ltp(_)],
        %  TODO: Find out why useAbstraction messes up subf/hasval.
        BuiltInTemp=[useAbstraction, online, start, pll(_, _, _, _),
                    pproj(_, _), lookahead(_, _, _, _), bel(_), ltp(_)],
%        ignore_fluents(IFL), 
%        append(BuiltInTemp, IFL, BuiltIn),
        %  The list of all defined fluents.
        FluentLTmp=[PrimFList, ContFList, ExogPrimFList, ExogContFList],
        flatten(FluentLTmp, FluentL),
%        BuiltInTmp = [BuiltIn, RegL], flatten(BuiltInTmp, BuiltInL),
        BuiltInTmp = [BuiltInTemp, RegL],
        flatten(BuiltInTmp, BuiltInL),
        subtract(FluentL, BuiltInL, ResultList),
        list_to_ord_set(ResultList, Result).


%  Returns an ordered set Result of all fluent names of parameterless
%  fluents and of *fully instantiated* parameterised exog_prim_fluents.
:- mode ipl_get_all_fluent_names(-).
ipl_get_all_fluent_names(Result) :-
        not(param_exog_prim_fluents),
        !,
        %  Get all fluents.
        get_all_fluent_names( Result ).

ipl_get_all_fluent_names(Result) :-
        %  Get all fluents.
        get_all_fluent_names( AllFluents ),
        %  Remove all exog_prim_fluents with parameters that are
        %  not instantiated.
        findall( NonGroundParamF,
                 ( memberchk(NonGroundParamF, AllFluents),
                   is_param_exog_prim_fluent(NonGroundParamF),
                   not(ground(NonGroundParamF))
                 ),
                 NonGroundParamFluents ),
        subtract( AllFluents, NonGroundParamFluents, GroundFluents ),
        %  Add all parameterised from the list
        %  param_exog_prim_fluent_calls. Those are all ground.
        getval( param_exog_prim_fluent_calls, CalledFluents ),
        ResultTmp = [GroundFluents, CalledFluents],
        flatten( ResultTmp, ResultFlat ),
        list_to_ord_set( ResultFlat, Result ).


%  Returns an ordered set Result of all fluent values.
%  Gets the current situation S as input.
:- mode get_all_fluent_values(++, -).
get_all_fluent_values(S, Result) :-
        printf(stdout, "Querying all fluent values... ", []),
        cputime(TQueryBegin),
        get_all_fluent_names(Fluents),
        findall( ValFStringNoComma,
                 ( member(F, Fluents),
                   %  Check if fluent is instantiated.
                   ( ( ( nonvar(F)
                         , F \= useAbstraction ) -> % TODO: Find out why
                                                    % useAbstraction messes
                                                    % up subf/hasval.
                         ( exog_fluent(F) ->
%                             printf(stdout,
%                             "Fluent %w is an exogenous fluent...\n", [F]),
                             exog_fluent_getValue(F, ValF, S)%,
%                             printf(stdout, "and has value %w.\n", [ValF])
                         ;
%                             printf(stdout,
%                             "Fluent %w is *NOT* an exogenous fluent...\n",
%                             [F]),
                             subf(F, ValF, S)%,
%                             printf(stdout, "and has value %w.\n", [ValF])
                         ),
                         %  Replace commas, as C4.5 forbids them in
                         %  attribute values.
                         %  Note, that, in general, ValF is a list!
                         term_string(ValF, ValFString),
                         replace_string(ValFString, ",", "COMMA",
                                        ValFStringTmp),
                         %  Replace ", as they are part of the fluent value
                         %  and otherwise would be interpreted as string
                         %  identifier by Prolog during later conversion.
                         replace_string(ValFStringTmp, "\"", "QUOTATION",
                                        ValFStringNoComma)
                     ;
                         printf(stdout, "*** Warning: *** ", []),
                         printf(stdout, "Fluent %w is not instantiated. ", [F]),
                         printf(stdout, "Fluent is ignored.\n", []),
                         false
                     )
                   )
                 ),
                 Result ),
%        print_list(Result),
        cputime(TQueryEnd),
        TQueryDiff is TQueryEnd - TQueryBegin,
        printf(stdout, "with success in %w sec.\n", [TQueryDiff]).


%  Decide, whether we are in the pre-training phase, where we still collect
%  param_exog_prim_fluent calls, or we are in the training phase, where we
%  still collect training data and train the decision tree for the given
%  solve-context, or we are in the consultation phase for this solve-context.
:- mode determine_ipl_phase(++, -).
determine_ipl_phase( _Solve, Phase ) :-
        param_exog_prim_fluents,
        ipl_pre_training_phase,
        getval( last_change_to_fluent_calls, TLastChange ),
        var( TLastChange ),
        !,
        Phase = "pre_train".

determine_ipl_phase( _Solve, Phase ) :-
        param_exog_prim_fluents,
        ipl_pre_training_phase,
        getval( last_change_to_fluent_calls, TLastChange ),
        %  Since above clause failed, we know: nonvar( TLastChange )
        cputime(TNow),
        TDiff is (TNow - TLastChange),
        getval( param_exog_prim_fluent_delta, CollectionDelta ),
        (TDiff < CollectionDelta),
        !,
        printf(stdout, "Still collecting calls for parameterised ", []),
        printf(stdout, "primitive exogeneous fluents. Solve is handled ", []),
        printf(stdout, "via DT-planning.\n", []),
        printf(stdout, "TLastChange: %w, TNow: %w, TDiff: %w\n", [TLastChange, TNow, TDiff]), flush(stdout),
%        sleep(10),
        Phase = "pre_train".

determine_ipl_phase( _Solve, Phase ) :-
        param_exog_prim_fluents,
        ipl_pre_training_phase,
        %  Since above clause failed, we know: (TDiff >= CollectionDelta)
        !,
        getval( param_exog_prim_fluent_calls, FluentCalls ),
        length( FluentCalls, Calls ),
        printf(stdout, "Triggering IPL Training Phase!\n", []),
        printf(stdout, "We have collected %w calls for ", [Calls]),
        printf(stdout, "parameterised exogeneous fluents.\n", []),
        flush(stdout),
%        sleep(10),
        setval( ipl_pre_training_phase, false ),
        Phase = "train".

determine_ipl_phase( HashKey, Phase ) :-
        getval(solve_hash_table, SolveHashTable),
        not(hash_contains(SolveHashTable, HashKey)),
        !,
        %  solve context encountered for the first time.
        Phase = "train".

determine_ipl_phase( HashKey, Phase ) :-
        %  solve context has been encountered before.
%%        hypothesis_error(HashKey, Error),
%%        getval( max_hypothesis_error, MaxError ),
%%        ( Error > MaxError ),
        !,
        Phase = "train".

determine_ipl_phase( _HashKey, Phase ) :-
        %  Since above clause failed, we know: ( Error =< MaxError )
        Phase = "consult".


%  Creates a hash key for the solve context and its filenames.
:- mode create_hash_key(++, -).
create_hash_key( solve(Prog, Horizon, RewardFunction), HashKey ) :-
        term_hash(solve(Prog, Horizon, RewardFunction), -1, 1000, HashKey),
        printf(stdout, "solve has hash key %w.\n", [HashKey]).


%  The predicate is true iff the given Stream gets ready for I/O in time 100.
:- mode stream_ready(++).
stream_ready( Stream ) :-
%        not at_eof( Stream ), % does not work as intended with the pipe stream
        select([Stream], 100, ReadyStream),
        ReadyStream \= [].

%  Skips through the (output)-stream Stream until Pattern is found or
%  Stream is "quiet for a while".
%  Prints skipped lines to stdout.
%  Returns String of the line where pattern is found.
%  Helper predicate for chatting with C4.5.
:- mode print_skip(++, ++, -).
print_skip( Stream, _Pattern, String ) :-
        not stream_ready( out ), !,
        printf("[print_skip] ***Error*** Got empty Stream %w ", [Stream]),
        printf("while trying to skip!\n", []),
        flush(stdout),
        String = "".

print_skip( Stream, Pattern, String ) :-
        stream_ready( out ),
        read_string(Stream, end_of_line, _, String),
        ( substring(String, Pattern, _) ->
           true
        ;
           print_skip( Stream, Pattern, String )
        ).

%  Skips through the (output)-stream Stream until Pattern is found or
%  Stream is "quiet for a while".
%  Does not print skipped lines to stdout.
%  Returns String of the line where pattern is found.
%  Helper predicate for chatting with C4.5.
:- mode quiet_skip(++, ++, -).
quiet_skip( Stream, _Pattern, String ) :-
        not stream_ready( out ), !,
        printf("[print_skip] ***Error*** Got empty Stream %w ", [Stream]),
        printf("while trying to skip!\n", []),
        flush(stdout),
        String = "".

quiet_skip( Stream, Pattern, String ) :-
        stream_ready( out ),
        read_string(Stream, end_of_line, _, String),
        ( substring(String, Pattern, _) ->
           true
        ;
           quiet_skip( Stream, Pattern, String )
        ).

% --------------------------------------------------------- %
%  Learning Instances                                       %
% --------------------------------------------------------- %
% {{{ Learning Instances

%  The C4.5 .names file defines the values for the decision
%  outcomes, and the domains of the attribute values. The domain
%  can be "continuous", "discrete", or a comma-separated list of
%  discrete values (which is preferable to "discrete").
%  As we do not require the Readylog programmer to declare the
%  domain of the fluents, we simply define the domain of
%  discrete fluents as a discrete domain with 1000 entries maximum.

%  Writes the learning example to the C4.5 .names and .data files.
%  Gets the solve context (Prog, Horizon, RewardFunction),
%  the Policy that was computed by decision-theoretic planning,
%  the Value and TermProb of this DT policy,
%  the PolicyTree of this DT policy (for DT-debugging),
%  and the current situation S.
:- mode write_learning_instance(++, ++, ++, ++, ++, ++).
write_learning_instance( solve(Prog, Horizon, RewardFunction), Policy,
                         Value, TermProb, PolicyTree, S ) :-
        %  Create a hash key for the solve context and its filenames.
        getval(solve_hash_table, SolveHashTable),
        term_hash(solve(Prog, Horizon, RewardFunction), -1, 1000, HashKey),
        printf(stdout, "solve has hash key %w.\n", [HashKey]),
        term_string(HashKey, HashKeyString),
        ( not(hash_contains(SolveHashTable, HashKey)) ->
                %  solve context encountered for the first time.
                printf(stdout, "First encounter of this solve context.\n", []),
                hash_set(SolveHashTable, HashKey,
                        solve(Prog, Horizon, RewardFunction)),
                %  update "global" variable
                setval(solve_hash_table, SolveHashTable),

                % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %
                %  Construct a                       %
                %  ####### C4.5 .names file #######  %
                %  for this solve context.           %
                %  Instantiates ContextString,       %
                %  FluentNames, and DecisionString   %
                % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %
                construct_names_file( solve(Prog, Horizon,
                                      RewardFunction),
                                      Policy, Value, TermProb,
                                      PolicyTree, HashKeyString,
                                      ContextString, FluentNames,
                                      DecisionString ),

                % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %
                %  Construct a                       %
                %  ##### C4.5 .data file #######     %
                %  for this solve context.           %
                % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %
                construct_data_file( S, HashKeyString, ContextString,
                                     FluentNames, DecisionString )
        ;
                /** solve context has been encountered before. */
                printf(stdout, "This solve context has been encountered ", []),
                printf(stdout, "before.\n", []),

                % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %
                %  Continue with                     %
                %  ##### C4.5 .names file #######    %
                %  Instantiates DecisionString.      %
                % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %
                continue_names_file( Policy, Value, TermProb, PolicyTree,
                                     HashKeyString, DecisionString ),

                % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %
                %  Continue with                     %
                %  ##### C4.5 .data file #######     %
                % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %
                continue_data_file( S, HashKeyString, DecisionString )
        ).

%  Creates the C4.5 .names file for the solve context with key HashKeyString.
%  Returns the ContextString, FluentNames, and DecisionString which are
%  needed by construct_data_file/5.
%  Helper predicate for write_learning_instance/6.
:- mode construct_names_file(++, ++, ++, ++, ++, ++, -, -, -).
construct_names_file( solve(Prog, Horizon, RewardFunction), Policy, Value,
                      TermProb, PolicyTree, HashKeyString,
                      ContextString, FluentNames, DecisionString ) :-
        concat_string(["solve_context_", HashKeyString, ".names"], FileName),
        open(FileName, write, NameStream),
        printf(NameStream, "| ********************************\n", []),
        printf(NameStream, "| * file generated from ReadyLog *\n", []),
        printf(NameStream, "| ********************************\n", []),
        printf(NameStream, "|\n", []),
        printf(NameStream, "| This is the C4.5 declaration file for ", []),
        printf(NameStream, "the solve context:\n", []),
        term_string(solve(Prog, Horizon, RewardFunction), ContextString),
        printf(NameStream, "| --------------------------------------", []),
        printf(NameStream, "------------------\n", []),
        printf(NameStream, "| %w\n", [ContextString]),
        printf(NameStream, "| --------------------------------------", []),
        printf(NameStream, "------------------\n", []),
        printf(NameStream, "|\n", []),
        printf(NameStream, "| First we list the possible decisions\n", []),
        printf(NameStream, "| (policy, value, prob, tree),\n", []),
        printf(NameStream, "| separated by commas and terminated by ", []),
        printf(NameStream, "a fullstop.\n", []),
        printf(NameStream, "\n", []),
        %  Replace commas, as C4.5 forbids them in class names.
        term_string(Policy, PolicyString),
        replace_string(PolicyString, ",", "\\,", PolicyStringNoComma),
%        printf(NameStream, "%w", [PolicyStringNoComma]),
        term_string(Value, ValueString),
        term_string(TermProb, TermProbString),
        %  TODO: PolicyTreeString can get too big for
        %        reasonable processing.
        %        We replace it by the placeholder "Tree", as
        %        it only is used for DT-debugging anyway.
%%        term_string(PolicyTree, PolicyTreeString),
%%        replace_string(PolicyTreeString, ",", "\\,", PolicyTreeStringNoComma),
        PolicyTreeStringNoComma = "Tree",
        concat_string(["(", PolicyStringNoComma, " <Value_", ValueString, ">",
                       " <TermProb_", TermProbString, ">", 
                       " <PolicyTree_", PolicyTreeStringNoComma, ">)"],
                       DecisionString),
        printf(NameStream, "%w", [DecisionString]),
        printf(NameStream, ".|append policies here|", []),
        printf(NameStream, "\n", []),
        printf(NameStream, "\n", []),
        printf(NameStream, "| Then we list the attributes (fluents) ", []),
        printf(NameStream, "and their domains.\n", []),
        printf(NameStream, "| The domains can be continuous, discrete, ", []),
        printf(NameStream, "or a set of discrete values\n", []),
        printf(NameStream, "| (notated as a comma-separated list).\n", []),
        printf(NameStream, "\n", []),
        ipl_get_all_fluent_names(FluentNames),
        %  As we are not given the domain for discrete fluents
        %  by the Readylog programmer, we simply declare the
        %  domain for discrete fluents as discrete with 1000
        %  entries maximum
        ( foreach(Fluent, FluentNames),
          param(NameStream)
          do
             %  TODO: How do we handle parameterised fluents?
             nonvar(Fluent),
             ( is_cont_fluent(Fluent) ->
                printf(NameStream, "%w: continuous.\n", [Fluent])
             ;
                ( is_prim_fluent(Fluent) ->
                   printf(NameStream, "%w: discrete 1000.\n", [Fluent])
                ;
                   printf(NameStream, "%w: discrete 1000.\n", [Fluent]),
                   printf(NameStream, "| WARNING: %w is neither ", [Fluent]),
                   printf(NameStream, "cont nor prim!\n,", []),
                   printf(stdout, "*** WARNING ***: %w is neither ", [Fluent]),
                   printf(stdout, "cont nor prim!", [])
                )
             )
        ),
        close(NameStream).

%  Creates the C4.5 .data file for the solve context with key HashKeyString.
%  Needs the ContextString, FluentNames, and DecisionString from
%  construct_names_file/9 as input.
%  Helper predicate for write_learning_instance/6.
:- mode construct_data_file(++, ++, ++, ++, ++).
construct_data_file( S, HashKeyString, ContextString, FluentNames,
                     DecisionString ) :-
        concat_string(["solve_context_", HashKeyString, ".data"], FileData),
        open(FileData, write, DataStream),
        printf(DataStream, "| ********************************\n", []),
        printf(DataStream, "| * file generated from ReadyLog *\n", []),
        printf(DataStream, "| ********************************\n", []),
        printf(DataStream, "|\n", []),
        printf(DataStream, "| This is the C4.5 instance data file for ", []),
        printf(DataStream, "the solve context:\n", []),
        printf(DataStream, "| ----------------------------------------", []),
        printf(DataStream, "----------------\n", []),
        printf(DataStream, "| %w\n", [ContextString]),
        printf(DataStream, "| ----------------------------------------", []),
        printf(DataStream, "----------------\n", []),
        printf(DataStream, "|\n", []),
        printf(DataStream, "| Each example consists of one line.\n", []),
        printf(DataStream, "| First in this line comes a ", []),
        printf(DataStream, "comma-separated list of the\n", []),
        printf(DataStream, "| fluent values, and then the ", []),
        printf(DataStream, "decision (policy).\n", []),
        printf(DataStream, "\n", []),
                         
        get_all_fluent_values(S, FluentValues),
        %  remove outer brackets []
        fluent_values_to_string(FluentValues, FluentValuesString),

        length(FluentNames, FluentNo),
        length(FluentValues, ValueNo),
        printf(stdout, "%w Fluents, %w Fluent Values\n", [FluentNo, ValueNo]),
        ( ( FluentNo \= ValueNo ) ->
           printf(stdout, "*** Warning: Numbers of fluents and values ", []),
           printf(stdout, "are not the same!\n", [])
        ;
           true
        ),
%        print_list(FluentNames),
%        print_list(FluentValues),
        printf(DataStream, "%w, %w\n", [FluentValuesString, DecisionString]),
        close(DataStream).


%  Continues the C4.5 .names file for the solve context with key HashKeyString.
%  Returns the DecisionString which is needed by continue_data_file/3
%  Helper predicate for write_learning_instance/6.
:- mode continue_names_file(++, ++, ++, ++, ++, ++).
continue_names_file( Policy, Value, TermProb, PolicyTree, HashKeyString,
                     DecisionString ) :-
        concat_string(["solve_context_", HashKeyString, ".names"], FileName),
        %  Check, if the decision (policy) has been already declared.
        open(FileName, read, NameStreamRead),
        read_string(NameStreamRead, end_of_file, _Length, NameStreamString),
        close(NameStreamRead),
        term_string(Policy, PolicyString),
%        printf(stdout, "PolicyString: %w.\n", [PolicyString]),
        %  replace commas in policy, as C4.5 forbids them in class names
        replace_string(PolicyString, ",", "\\,",
                       PolicyStringNoComma),
        term_string(Value, ValueString),
        term_string(TermProb, TermProbString),
        %  TODO: PolicyTreeString can get too big for
        %        reasonable processing.
        %        We replace it by the placeholder "Tree", as
        %        it only is used for DT-debugging anyway.
%%        term_string(PolicyTree, PolicyTreeString),
%%        replace_string(PolicyTreeString, ",", "\\,", PolicyTreeStringNoComma),
        PolicyTreeStringNoComma = "Tree",
        concat_string(["(", PolicyStringNoComma, " <Value_", ValueString, ">",
                       " <TermProb_", TermProbString, ">", 
                       " <PolicyTree_", PolicyTreeStringNoComma, ">)"],
                       DecisionString),
        ( substring(NameStreamString, DecisionString, _Pos) ->
           printf(stdout, "Policy already declared.\n", [])
        ;
           printf(stdout, "Declaring policy.\n", []),
           open(FileName, update, NameStream),
           string_length(NameStreamString, NameStreamStringLength),
           substring(NameStreamString, ".|append policies here|",
                     PolicyAppendingPos),
           PolicyAppendingPosLeft is (PolicyAppendingPos - 1),
           substring(NameStreamString, 1, PolicyAppendingPosLeft,
                     NameStreamStringLeft),
           RestLength is (NameStreamStringLength - PolicyAppendingPosLeft),
           substring(NameStreamString, PolicyAppendingPos,
                     RestLength, NameStreamStringRight),
           concat_string([NameStreamStringLeft, ", ", DecisionString,
                          NameStreamStringRight],
                         NameStreamStringNew),
           printf(NameStream, "%w", [NameStreamStringNew]),
           close(NameStream)
        ).

%  Continues the C4.5 .data file for the solve context with key HashKeyString.
%  Needs the DecisionString from continue_names_file/6 as input.
%  Helper predicate for write_learning_instance/6.
:- mode continue_data_file(++, ++, ++).
continue_data_file( S, HashKeyString, DecisionString ) :-
        concat_string(["solve_context_", HashKeyString, ".data"], FileData),
        open(FileData, append, DataStream),
        get_all_fluent_values(S, FluentValues),
        %  remove outer brackets []
        fluent_values_to_string(FluentValues, FluentValuesString),
        printf(DataStream, "%w, %w\n", [FluentValuesString, DecisionString]),
        close(DataStream).

% }}}

% --------------------------------------------------------- %
%  Consultation of Learned Decision Trees                   %
% --------------------------------------------------------- %
% {{{ Consultation of dtrees


%  Consult the decision tree that has been generated by C4.5 for the given
%  solve context.
%  This predicate supplies only those attribute values that C4.5 needs
%  to come to a classification.
%  Returns a Policy together with its Value, TermProb, Tree (for debug output),
%  and a truth value Success to indicate whether the consultation was
%  successful, or it was unsuccessful (and we have to replan).
:- mode consult_dtree(++, ++, ++, ++, -, -, -, -, -). 
consult_dtree( Prog, Horizon, RewardFunction, S,
               Policy, Value, TermProb, Tree, Success) :-
        term_hash(solve(Prog, Horizon, RewardFunction), -1, 1000, HashKey),
        term_string(HashKey, HashKeyString),
        concat_string(["solve_context_", HashKeyString], FileStem),
        consult_dtree_aux( Prog, Horizon, RewardFunction, S, FileStem,
                           Policy, Value, TermProb, Tree, Success).

%  Helper predicate for the decision tree consultation.
%  Takes care of missing file exceptions.
:- mode consult_dtree_aux(++, ++, ++, ++, ++, -, -, -, -, -).
consult_dtree_aux( Prog, Horizon, RewardFunction, S, FileStem,
                   Policy, Value, TermProb, Tree, _Success) :-
        concat_strings(FileStem, ".tree", FileTree),
        not(existing_file(FileStem, [".tree"], [readable], FileTree)),
        !,
        printf(stdout, "[Consultation Phase]: (*** Error ***) ", []),
        printf(stdout, "Decision tree not found!\n", []),
        printf(stdout, "[Consultation Phase]: Consulting ", []),
        printf(stdout, "DT planner instead.\n", []),
        bestDoM(Prog, S, Horizon, Policy,
                Value, TermProb, checkEvents, Tree, RewardFunction).

consult_dtree_aux( Prog, Horizon, RewardFunction, S, FileStem,
                   Policy, Value, TermProb, Tree, _Success) :-
        concat_strings(FileStem, ".names", FileNames),
        not(existing_file(FileStem, [".names"], [readable], FileNames)),
        !,
        printf(stdout, "[Consultation Phase]: (*** Error ***) ", []),
        printf(stdout, "file %w not found!\n", [FileNames]),
        printf(stdout, "[Consultation Phase]: Consulting ", []),
        printf(stdout, "DT planner instead.\n", []),
        bestDoM(Prog, S, Horizon, Policy,
                Value, TermProb, checkEvents, Tree, RewardFunction).

consult_dtree_aux( _Prog, _Horizon, _RewardFunction, S, FileStem,
                   Policy, Value, TermProb, Tree, Success) :-
        canonical_path_name(FileStem, FullPath),
        printf(stdout, "Consulting %w...\n", [FullPath]),
        flush(stdout),
        %  Run the C4.5/Prolog interface as another process.
        exec(["../../libraries/c45_lib/consultobj/ConsultObjectTest2", "-f",
              FullPath],
             [in, out, err], Pid),
        %  Do the Loop
        %  "C4.5 asks for attribute value ->
        %   Readylog provides Fluent Value ->
        %   C4.5 asks..."
        %  until either C4.5 gives a decision, or the consultation
        %  fails.
        repeat,
           ask_c45_for_decision( in, out, err, S,
                                 DecisionString, Success ),
%        at_eof(out), %  Somehow doesn't work as intended
%                     %  with the pipe stream
%        ( ground(DecisionString) ; ground(Success) ),
        ( nonvar(DecisionString) ; nonvar(Success) ),
        !,
        close(in),
        close(out),
        wait(Pid, _Stat),

        ( ( Success == false ) ->
           true
        ;
           extract_consultation_results( DecisionString,
                                         Policy, Value, TermProb, Tree ),
           Success = true
        ),

        ( Success ->
            printf(stdout, "[Consultation Phase]: ", []),
            printf(stdout, "Consultation successful!\n", [])
        ;
            printf(stdout, "[Consultation Phase]: ", []),
            printf(stdout, "Consultation failed!\n", [])
        ).


%  Ask C4.5 for a decision. If C4.5 needs more information still, it asks
%  for the value of one attribute with this predicate.
%  The IndicatorStrings with the markers "####" are produced by
%  the (C++) C4.5/Prolog interface library "c45_lib".
:- mode ask_c45_for_decision(++, ++, ++, ++, ?, ?).        
ask_c45_for_decision( in, out, err, S, DecisionString, Success ) :-
%         print_skip( out, "####", IndicatorString ),
        quiet_skip( out, "####", IndicatorString ),
        ask_c45_for_decision_aux( in, out, err, S, IndicatorString,
                                  DecisionString, Success ).

%  Helper predicate for the interprocess communication with C4.5.
%  Is used to differ between different C4.5 outputs.
%  The IndicatorStrings with the markers "####" are produced by
%  the (C++) C4.5/Prolog interface library "c45_lib".
:- mode ask_c45_for_decision_aux(++, ++, ++, ++, ++, -, -).        
ask_c45_for_decision_aux( in, out, err, S, IndicatorString,
                          _DecisionString, _Success ) :-
        substring(IndicatorString,
                  "#### Here comes the attribute name ####", _),
        !,
        read_string(out, end_of_line, _, AttributeNameS),
        term_string(AttributeName, AttributeNameS),
        printf(stdout, "--------\n", []),
        printf(stdout, "[PROLOG] C4.5 asked for the value of attribute: ", []),
        printf(stdout, "%w\n", [AttributeNameS]),
        flush(stdout),
        exog_fluent_getValue(AttributeName, ValTmp, S),
        %  Replace commas, as C4.5 forbids them in
        %  attribute values.
        term_string(ValTmp, ValTmpString),
        replace_string(ValTmpString, ",", "\\,", AttributeValue),
        flush(stdout),
        select([in], 100, _ReadyStream),
%        printf(stdout, "Stream %w ready for I/O.\n", [ReadyStream]),
        flush(stdout),
        printf(in, "%w\n", [AttributeValue]),
        flush(in),
        read_string(out, end_of_line, _, PrologAnswer),
        printf(stdout, "%w\n", [PrologAnswer]),
        flush(stdout).

ask_c45_for_decision_aux( in, out, err, _S, IndicatorString,
                          DecisionString, _Success ) :-
        substring(IndicatorString, "#### Here comes the decision ####", _),
        !,
        printf(stdout, "--------\n", []),
        printf(stdout, "[PROLOG] C4.5 is giving a decision.\n", []),
        flush(stdout),
        read_string(out, end_of_line, _, Decision),
        string_length(Decision, DecisionLength),
        RawDecisionLength is (DecisionLength - 2),
        substring(Decision, 2, RawDecisionLength, DecisionString),
        printf(stdout, "[C4.5] Decision: %w\n", [DecisionString]),
        printf(stdout, "--------\n", []),
        flush(stdout),
        ( stream_ready(out) ->
           read_string(out, end_of_file, _, _Rest)
        ;
           true
        ).

ask_c45_for_decision_aux( in, out, err, _S, IndicatorString,
                          _DecisionString, Success ) :-
        %  Happens when the attribute value has not been seen during
        %  training.
        substring(IndicatorString, "#### Error ####", _), !,
        printf(stdout, "--------\n", []),
        printf(stdout, "[C4.5] This attribute value has not appeared ", []),
        printf(stdout, "during training.\n", []),
        printf(stdout, "       Will use planning instead.\n", []),
        printf(stdout, "--------\n", []),
        Success = false,
        flush(stdout),
        ( stream_ready(out) ->
           read_string(out, end_of_file, _, _Rest)
        ;
           true
        ).

ask_c45_for_decision_aux( in, out, err, _S, _IndicatorString,
                          _DecisionString, Success ) :-
        printf(stdout, "--------\n", []),
        printf(stdout, "[C4.5] Error: This should not happen. ", []),
        printf(stdout, "Error in parsing?\n", []),
        printf(stdout, "--------\n", []),
        Success = false,
        flush(stdout),
        ( stream_ready(out) ->
           read_string(out, end_of_file, _, _Rest)
        ;
           true
        ).
        
%  From the classification DecisionString, extract the policy, 
%  its Value, TermProb, and Tree.
%  They are encoded in the string by markers
%  <Value_x> <TermProb_y> <PolicyTree_z>, where x, y, z stand for the
%  stored values that we are interested in.
%  The actual policy does not have a marker; it just is located
%  in front of the value-marker.
:- mode extract_consultation_results(++, -, -, -, -).
extract_consultation_results( DecisionString,
                              Policy, Value, TermProb, Tree ) :-
         %%%  Cut out Policy.  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         substring(DecisionString, ValuePos, 7, "<Value_"),
         ValuePosLeft is (ValuePos - 2),
         substring(DecisionString, 1, ValuePosLeft, PolicyString),
         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

         %%%  Cut out Value.  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         string_length(PolicyString, PolicyLength),
         string_length(DecisionString, DecisionStringLength),
         BeyondPolicyLength is ( DecisionStringLength
                                - PolicyLength - 1),
         PolicyEndRight is (PolicyLength + 2),
         substring(DecisionString, PolicyEndRight, BeyondPolicyLength,
                   BeyondPolicy),
         substring(BeyondPolicy, ValueEndPos, 12, "> <TermProb_"),
%         printf("BeyondPolicy: %w\n", [BeyondPolicy]),
         ValueLength is (ValueEndPos - 8),
         substring(BeyondPolicy, 8, ValueLength, ValueString),
         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

         %%%  Cut out TermProb.  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         ValueEndPosRight is (ValueEndPos + 2),
         BeyondValueLength is ( BeyondPolicyLength
                                - ValueLength - 9 ),
         substring(BeyondPolicy, ValueEndPosRight, BeyondValueLength,
                   BeyondValue),
%         printf("BeyondValue: %w\n", [BeyondValue]),
         substring(BeyondValue, TermProbEndPos, 14, "> <PolicyTree_"),
         TermProbLength is (TermProbEndPos - 11),
         substring(BeyondValue, 11, TermProbLength, TermProbString),
         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

         %%%  Cut out Tree.  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         TermProbEndPosRight is (TermProbEndPos + 2),
         BeyondTermProbLength is ( BeyondValueLength
                                   - TermProbLength - 12 ),
         substring(BeyondValue, TermProbEndPosRight, BeyondTermProbLength,
                   BeyondTermProb),
%         printf("BeyondTermProb: %w\n", [BeyondTermProb]),
         substring(BeyondTermProb, TreeEndPos, 1, ">"),
         TreeLength is (TreeEndPos - 13),
         substring(BeyondTermProb, 13, TreeLength, TreeString),
         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%         printf("Policy: %w\n", [Policy]),
%         printf("Value: %w\n", [Value]),
%         printf("TermProb: %w\n", [TermProb]),
%         printf("Tree: %w\n", [Tree]),
%         flush(stdout),
         term_string(Policy, PolicyString),
         term_string(Value, ValueString),
         term_string(TermProb, TermProbString),
         term_string(Tree, TreeString).

% }}}
