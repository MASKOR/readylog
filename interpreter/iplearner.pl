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
%        BuiltInTemp=[online, start, pll(_, _, _, _), pproj(_, _), 
%                     lookahead(_, _, _, _), bel(_), ltp(_)],
        /** TODO: Find out why useAbstraction messes up subf/hasval */
        BuiltInTemp=[useAbstraction, online, start, pll(_, _, _, _), pproj(_, _), 
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
        printf(stdout, "Querying all fluent values... ", []),
        cputime(TQueryBegin),
        get_all_fluent_names(Fluents),
        findall( ValFStringNoComma,
                 ( member(F, Fluents),
                   /** check if fluent is instantiated */
                   /** TODO: should this be nonvar(F) instead? */
                   ( ( ( ground(F) /** TODO: Find out why useAbstraction messes up subf/hasval */
                       , F \= useAbstraction )  ->
                         ( exog_fluent(F) ->
%                             printf(stdout, "Fluent %w is an exogenous fluent...\n", [F]),
                             exog_fluent_getValue(F, ValF, S)%,
%                             printf(stdout, "and has value %w.\n", [ValF])
                         ;
%                             printf(stdout, "Fluent %w is *NOT* an exogenous fluent...\n", [F]),
                             subf(F, ValF, S)%,
%                             printf(stdout, "and has value %w.\n", [ValF])
                         ),
                         /** replace commas, as C4.5 forbids them in
                          *  attribute values.
                          *  Note, that, in general, ValF is a list! */
                         term_string(ValF, ValFString),
                         replace_string(ValFString, ",", "COMMA",
                                        ValFStringTmp, stdout),
                         /** replace ", as they are part of the fluent value
                          *  and otherwise would be interpreted as string identifier
                          *  by Prolog during later conversion. */
                         replace_string(ValFStringTmp, "\"", "QUOTATION",
                                        ValFStringNoComma, stdout)
                     ;
                         printf(stdout, "*** Warning: *** ", []),
                         printf(stdout, "Fluent %w is not instantiated. ", [F]),
                         printf(stdout, "Fluent is ignored.\n", []),
                         false
                     )
                   )
                 ),
                 Result ),%.
%        print_list(Result),
        cputime(TQueryEnd),
        TQueryDiff is TQueryEnd - TQueryBegin,
        printf(stdout, "with success in %w sec.\n", [TQueryDiff]).
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

write_learning_instance( solve(Prog, Horizon, RewardFunction), Policy, Value, TermProb, PolicyTree, S ) :-
        /** Create a hash key for the solve context. */
        getval(solveHashTable, SolveHashTable),
        term_hash(solve(Prog, Horizon, RewardFunction), -1, 1000, HashKey),
        printf(stdout, "solve has hash key %w.\n", [HashKey]),
        ( not(hash_contains(SolveHashTable, HashKey)) ->
                /** solve context encountered for the first time. */
                printf(stdout, "First encounter of this solve context.\n", []),
                hash_set(SolveHashTable, HashKey,
                         solve(Prog, Horizon, RewardFunction)),
                         /** update "global" variable */
                         setval(solveHashTable, SolveHashTable),

                         /** Construct a
                          *  ####### C4.5 .names file #######
                          *  for this solve context. */
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
                         printf(NameStream, "| First we list the possible decisions\n", []),
                         printf(NameStream, "| (policy, value, prob, tree),\n", []),
                         printf(NameStream, "| separated by commas and terminated by a fullstop.\n", []),
                         printf(NameStream, "\n", []),
                         /** replace commas, as C4.5 forbids them in class names */
                         term_string(Policy, PolicyString),
                         replace_string(PolicyString, ",", "\\,", PolicyStringNoComma, stdout),
%                         printf(NameStream, "%w", [PolicyStringNoComma]),
                         term_string(Value, ValueString),
                         term_string(TermProb, TermProbString),
%                         term_string(PolicyTree, PolicyTreeString),
                                PolicyTreeString = "Tree",
                         replace_string(PolicyTreeString, ",", "\\,", PolicyTreeStringNoComma, stdout),
                         concat_string(["(", PolicyStringNoComma, " <Value_", ValueString, ">",
                                        " <TermProb_", TermProbString, ">", 
                                        " <PolicyTree_", PolicyTreeStringNoComma, ">)"],
                                        DecisionString),
                         printf(NameStream, "%w", [DecisionString]),
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
                                 ground(Fluent),
                                 ( is_cont_fluent(Fluent) ->
                                         printf(NameStream, "%w: continuous.\n", [Fluent])
                                 ;
                                         ( is_prim_fluent(Fluent) ->
%                                                 get_domain(Fluent, Domain), 
%                                                 printf(NameStream, "%w: %w.\n", [Fluent, Domain])
                                                 printf(NameStream, "%w: discrete 1000.\n", [Fluent])
                                         ;
                                                 printf(NameStream, "%w: discrete 1000.\n", [Fluent]),
                                                 printf(NameStream, "| WARNING: is neither cont nor prim!\n,", [Fluent]),
                                                 printf(stdout, "*** WARNING ***: %w is neither cont nor prim!", [Fluent])
                                         )
                                 )
                         ),
                         close(NameStream),

                         /** Construct a
                          *  ##### C4.5 .data file #######
                          *  for this solve context. */
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
                         fluent_values_to_string(FluentValues, FluentValuesString),

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
                         printf(DataStream, "%w, %w\n", [FluentValuesString, DecisionString]),
                         close(DataStream)
        ;
                /** solve context has been encountered before. */
                printf(stdout, "This solve context has been encountered before.\n", []),
                term_string(HashKey, HashKeyString),

                /** Continue with
                 *  ####### C4.5 .names file ####### */
                concat_string(["solve_context_", HashKeyString, ".names"], FileName),
                /** Check, if the decision (policy) has been already declared. */
                open(FileName, read, NameStreamRead),
                read_string(NameStreamRead, end_of_file, _Length, NameStreamString),
                close(NameStreamRead),
                term_string(Policy, PolicyString),
                printf(stdout, "PolicyString: %w.\n", [PolicyString]),
                /** replace commas in policy, as C4.5 forbids them in class names */
                replace_string(PolicyString, ",", "\\,",
                               PolicyStringNoComma, stdout),
                term_string(Value, ValueString),
                term_string(TermProb, TermProbString),
%                term_string(PolicyTree, PolicyTreeString),
                PolicyTreeString = "Tree",
                replace_string(PolicyTreeString, ",", "\\,", PolicyTreeStringNoComma, stdout),
                concat_string(["(", PolicyStringNoComma, " <Value_", ValueString, ">",
                               " <TermProb_", TermProbString, ">", 
                               " <PolicyTree_", PolicyTreeStringNoComma, ">)"],
                               DecisionString),
                ( substring(NameStreamString, DecisionString, _Pos) ->
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
                                concat_string([NameStreamStringLeft, ", ", DecisionString, NameStreamStringRight],
                                               NameStreamStringNew),
                                printf(NameStream, "%w", [NameStreamStringNew]),
                                close(NameStream)
                ),

                /** Continue with
                 *  ####### C4.5 .data file ####### */
                concat_string(["solve_context_", HashKeyString, ".data"], FileData),
                open(FileData, append, DataStream),
                get_all_fluent_values(S, FluentValues),
                /** remove outer brackets [] */
                fluent_values_to_string(FluentValues, FluentValuesString),
                printf(DataStream, "%w, %w\n", [FluentValuesString, DecisionString]),
                close(DataStream)
        ).



%/** Find out which values the (discrete) Fluent can take. */
%get_domain(Fluent, Domain) :-
%    %    compile("xtra.pl"),
%    % /** Doesn't help, even if commentaries would be filtered out.
%    %  *  The second parameter of setval may contain variables. */
%%        system('grep -o "setval(.*,.*)" xtra.pl | awk '{ print $2 }''). /** Doesn't help, even if commentaries would be filtered out
%                 findall( FluentVal,
%                 setval(Fluent, FluentVal),
%                 Domain ).

% }}}

/* --------------------------------------------------------- */
/*  Consultation of learned decision trees                   */
/* --------------------------------------------------------- */
% {{{ Consultation of dtrees

test(Answer) :-
        Answer = "Goal test(Answer) succeeded.".

ask_prolog_for_value( AttributeName, Value, S ) :-
        printf(stdout, "inside ask_prolog_for_value\n", []),
        printf(stdout, "AttributeName is: %w\n", [AttributeName]),
        printf(stdout, "Situation is: %w\n", [S]),
        ( exog_fluent(AttributeName) ->
            printf(stdout, "is exog_fluent\n", []),
            exog_fluent_getValue(AttributeName, ValueTmp, S)
        ;
            printf(stdout, "is NOT exog_fluent\n", []),
            subf(AttributeName, ValueTmp, S)
        ),
        term_string(ValTmp, Value),
        printf(stdout, "Value is: %w\n", [Value]).
        
%put_string(Stream, "") :- !.

%put_string(Stream, nl) :- !.

%put_string(Stream, [First|Rest]) :-
%        put_char(Stream, First),
%        put_string(Stream, Rest).

stream_ready( Stream ) :-
%        not at_eof( Stream ), % does not work as intended with the pipe stream
        select([Stream], 100, ReadyStream),
        ReadyStream \= [].

print_skip( Stream, Pattern, String ) :-
        not stream_ready( out ), !,
        printf("***Error*** Got empty Stream %w while trying to skip!\n", [Stream]),
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

quiet_skip( Stream, Pattern, String ) :-
        not stream_ready( out ), !,
        printf("***Error*** Got empty Stream %w while trying to skip!\n", [Stream]),
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

consult_dtree( Prog, [clipOnline|S], Horizon, Policy, Value, TermProb, Tree, RewardFunction, Success) :-
        term_hash(solve(Prog, Horizon, RewardFunction), -1, 1000, HashKey),
        term_string(HashKey, HashKeyString),
        concat_string(["solve_context_", HashKeyString], FileStem),
        concat_strings(FileStem, ".tree", FileTree),
        ( not(existing_file(FileStem, [".tree"], [readable], FileTree)) ->
                /** solve context encountered for the first time. */
                printf(stdout, "[Consultation Phase]: (*** Error ***) Decision tree not found!\n", []),
                printf(stdout, "[Consultation Phase]: Consulting DT planner instead.\n", []),
                bestDoM(Prog, [clipOnline|S], Horizon, Policy,
                        Value, TermProb, checkEvents, Tree, RewardFunction)
        ;
%                exec([ls,"-C"], [null, out], Pid),
                ( not(existing_file(FileStem, [".names"], [readable], FileNames)) ->
                         printf(stdout, "[Consultation Phase]: (*** Error ***) %w file not found!\n", [FileNames])
                ;
                         true
                ),
                concat_string(["-f ", FileStem], ConsultParams),
                canonical_path_name(FileStem, FullPath),
%                os_file_name(FullPath, FullPathOS),
%                concat_string(["-f ", "/", FullPathOS], ConsultParams),
                printf(stdout, "consulting %w\n", [FullPath]),
                flush(stdout),
%                open(string("consultStreamOut"), read, sigio(ConsultOut)),
%%                exec([consult, "-f", "test"], [null, ConsultOut], Pid),
%%%%%%%%%%%%%%%%%%%%%%
                term_string([clipOnline|S], SituationString),
%                exec(["/home/drcid/kbsgolog/libraries/c45_lib/consultobj/ConsultObjectTest2", "-f",
%                      FileStem],
%                      [in, out, err], Pid),
                exec(["../../libraries/c45_lib/consultobj/ConsultObjectTest2", "-f",
                      FullPath],
                      [in, out, err], Pid),
%                repeat,
%                   read_string(out, end_of_line, _, Skip),
%                   printf(stdout, "%w\n", [Skip]),
%                   flush(stdout),
%                substring(Skip, "####", _),
%%                substring(Skip, "#### Here comes the attribute name ####", _),
%                !,
                repeat,
%                   print_skip( out, "####", IndicatorString ),
                   quiet_skip( out, "####", IndicatorString ),
                   ( substring(IndicatorString, "#### Here comes the attribute name ####", _) ->
                         read_string(out, end_of_line, _, AttributeNameS),
                         term_string(AttributeName, AttributeNameS),
                         printf(stdout, "--------\n", []),
                         printf(stdout, "[PROLOG] C4.5 asked for the value of attribute: %w\n", [AttributeNameS]),
                         flush(stdout),
                         exog_fluent_getValue(AttributeName, ValTmp, [clipOnline|S]),
%                ask_prolog_for_value(AttributeName, AttributeValue, [clipOnline|S]),
                         /** replace commas, as C4.5 forbids them in
                          *  attribute values. */
                         term_string(ValTmp, ValTmpString),
                         replace_string(ValTmpString, ",", "\\,",
                                        AttributeValue, stdout),
%                         /** replace ", as they are part of the fluent value
%                          *  and otherwise would be interpreted as string identifier
%                          *  by Prolog during later conversion. */
%                         replace_string(ValTmpStringNoComma, "\"", "QUOTATION",
%                                        AttributeValue, stdout),
                         flush(stdout),
                         select([in], 100, _ReadyStream),
%                          printf(stdout, "Stream %w ready for I/O.\n", [ReadyStream]),
                         flush(stdout),
                         printf(in, "%w\n", [AttributeValue]),
                         flush(in),
%                         select([out], 100, ReadyStream2),
%                         printf(stdout, "Stream %w ready for I/O.\n", [ReadyStream2]),
%                         flush(stdout),
                         read_string(out, end_of_line, _, PrologAnswer),
                         printf(stdout, "%w\n", [PrologAnswer]),
                         flush(stdout)

                   ;
                         ( substring(IndicatorString, "#### Here comes the decision ####", _) ->
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
                               )
                         ;
                               % Happens when the attribute value has not been seen during
                               % training
                               ( substring(IndicatorString, "#### Error ####", _) ->
                                     printf(stdout, "--------\n", []),
                                     printf(stdout, "[C4.5] This attribute value has not appeared during training.\n", []),
                                     printf(stdout, "       Will use planning instead.\n", []),
                                     printf(stdout, "--------\n", []),
                                     Success = false,
                                     flush(stdout),
                                     ( stream_ready(out) ->
                                          read_string(out, end_of_file, _, _Rest)
                                     ;
                                          true
                                     )
                               ;
                                     printf(stdout, "--------\n", []),
                                     printf(stdout, "[C4.5] Error: This should not happen. Error in parsing?\n", []),
                                     printf(stdout, "--------\n", []),
                                     Success = false,
                                     flush(stdout),
                                     ( stream_ready(out) ->
                                          read_string(out, end_of_file, _, _Rest)
                                     ;
                                          true
                                     )
                               )
                         )
                   ),
%                at_eof(out), % Somehow doesn't work as intended with the pipe stream
%                ( not stream_ready(out) ),
                ( ground(DecisionString) ; ground(Success) ),
                !,
                close(in),
                close(out),
                wait(Pid, Stat),

                ( ( Success == false ) ->
                   true
                ;
                   /** Cut out the policy, value, termprob, and tree */
                   /** Cut out policy */
                   substring(DecisionString, ValuePos, 7, "<Value_"),
                   ValuePosLeft is (ValuePos - 2),
                   substring(DecisionString, 1, ValuePosLeft, PolicyString),
                   /** Cut out value */
                   string_length(PolicyString, PolicyLength),
                   string_length(DecisionString, DecisionStringLength),
                   BeyondPolicyLength is ( DecisionStringLength
                                           - PolicyLength - 1),
                   PolicyEndRight is (PolicyLength + 2),
                   substring(DecisionString, PolicyEndRight, BeyondPolicyLength,
                             BeyondPolicy),
                   substring(BeyondPolicy, ValueEndPos, 12, "> <TermProb_"),
%                   printf("BeyondPolicy: %w\n", [BeyondPolicy]),
                   ValueLength is (ValueEndPos - 8),
                   substring(BeyondPolicy, 8, ValueLength, ValueString),
                   /** Cut out TermProb */
                   ValueEndPosRight is (ValueEndPos + 2),
                   BeyondValueLength is ( BeyondPolicyLength
                                          - ValueLength - 9 ),
                   substring(BeyondPolicy, ValueEndPosRight, BeyondValueLength,
                             BeyondValue),
%                   printf("BeyondValue: %w\n", [BeyondValue]),
                   substring(BeyondValue, TermProbEndPos, 14, "> <PolicyTree_"),
                   TermProbLength is (TermProbEndPos - 11),
                   substring(BeyondValue, 11, TermProbLength, TermProbString),
                   /** Cut out Tree */
                   TermProbEndPosRight is (TermProbEndPos + 2),
                   BeyondTermProbLength is ( BeyondValueLength
                                             - TermProbLength - 12 ),
                   substring(BeyondValue, TermProbEndPosRight, BeyondTermProbLength,
                             BeyondTermProb),
%                   printf("BeyondTermProb: %w\n", [BeyondTermProb]),
                   substring(BeyondTermProb, TreeEndPos, 1, ">"),
                   TreeLength is (TreeEndPos - 13),
                   substring(BeyondTermProb, 13, TreeLength, TreeString),
%                   printf("Policy: %w\n", [Policy]),
%                   printf("Value: %w\n", [Value]),
%                   printf("TermProb: %w\n", [TermProb]),
%                   printf("Tree: %w\n", [Tree]),
%                   flush(stdout),
                   term_string(Policy, PolicyString),
                   term_string(Value, ValueString),
                   term_string(TermProb, TermProbString),
                   term_string(Tree, TreeString),

                   Success = true
                ),
 
                ( Success ->
                         printf(stdout, "[Consultation Phase]: Consultation successful!\n", [])
                ;
                         printf(stdout, "[Consultation Phase]: Consultation failed!\n", [])
                )
        ).
        

% }}}
