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
                                                 printf(NameStream, "%w: discrete 100.\n", [Fluent])
                                         ;
                                                 printf(NameStream, "%w: discrete 100.\n", [Fluent]),
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

print_skip( Stream, Pattern, String ) :-
        read_string(out, end_of_line, _, String),
        printf(stdout, "%w\n", [String]),
        flush(stdout),
        ( substring(String, Pattern, _) ->
           true
        ;
           print_skip( Stream, Pattern, String )
        ).

quiet_skip( Stream, Pattern, String ) :-
        read_string(out, end_of_line, _, String),
        ( substring(String, Pattern, _) ->
           true
        ;
           quiet_skip( Stream, Pattern, String )
        ).

consult_dtree( Prog, [clipOnline|S], Horizon, Policy, Value, TermProb, Tree, RewardFunction) :-
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
%                canonical_path_name(FileStem, FullPath),
%                os_file_name(FullPath, FullPathOS),
%                concat_string(["-f ", "/", FullPathOS], ConsultParams),
                printf(stdout, "consult %w\n", [ConsultParams]),
                flush(stdout),
%                open(string("consultStreamOut"), read, sigio(ConsultOut)),
%%                exec([consult, "-f", "test"], [null, ConsultOut], Pid),
%%%%%%%%%%%%%%%%%%%%%%
                term_string([clipOnline|S], SituationString),
                exec(["/home/drcid/kbsgolog/libraries/c45_lib/consultobj/ConsultObjectTest2", "-f",
                      "/home/drcid/kbsgolog/programs/wumpus.readylog/solve_context_254", "-s", SituationString],
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
                         printf(stdout, "--------\n", []),
                         printf(stdout, "[PROLOG] C4.5 asked for the value of attribute: %w\n", [AttributeNameS]),
                         flush(stdout),
                         read_string(out, end_of_line, _, AttributeNameS),
                         term_string(AttributeName, AttributeNameS),
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
                          select([out], 100, _ReadyStream2),
%                          printf(stdout, "Stream %w ready for I/O.\n", [ReadyStream2]),
                          flush(stdout),
                          read_string(out, end_of_line, _, ConsultString5),
                          printf(stdout, "%w\n", [ConsultString5]),
                          flush(stdout)
                   ;
                         ( substring(IndicatorString, "#### Here comes the decision ####", _) ->
                               printf(stdout, "--------\n", []),
                               printf(stdout, "[PROLOG] C4.5 is giving a decision.\n", []),
                               flush(stdout),
                               read_string(out, end_of_line, _, Decision),
                               string_length(Decision, DecisionLength),
                               RawDecisionLength is (DecisionLength - 10),
                               substring(Decision, 10, RawDecisionLength, DecisionString),
                               printf(stdout, "[C4.5] Decision: %w\n", [DecisionString]),
                               printf(stdout, "--------\n", []),
                               flush(stdout),
                               read_string(out, end_of_file, _, _Rest)
                         ;
                               % shouldn't happen
                               true
                         )
                   ),
                at_eof(out),
                !,

                





%%%%%%%%%%%%%%%%%%%%%%
%                out -> readable (not directly printable, or flushable)
%                in -> writeable, flushable
%                exec([consult, "-f", "test"], [in, out, err], Pid),

%                exec([ls, "-la"], [in, out], Pid),
%                printf(in, '%w\n', [sunny]),
%                printf(in, '%w\n', [42]),
%                select([in, out, err], 100, ReadyStream),
%                printf(stdout, "Streams %w ready for I/O.\n", [ReadyStream]),
%                flush(stdout),
%                printf(in, '%w\n', [sunny]),
%                flush(in),
%                select([in, out], 100, ReadyStream),
%                printf(stdout, "Streams %w ready for I/O.\n", [ReadyStream]),
%                read_string(ReadyStream, end_of_file, _, ConsultString),
%                printf(stdout, "%w", [ConsultString]),
%                read_string(out, end_of_file, _, ConsultString),
%                printf(stdout, "%w", [ConsultString]),
%                flush(stdout),
%                select([in], 100, _),
%                write(in, '%w\n', [sunny]),
%                flush(in),
%                read_string(out, end_of_file, _, ConsultString2),
%                printf(stdout, "%w", [ConsultString2]),
%                flush(stdout),
%                at_eof(out),
%                printf(stdout, "at_eof out!\n", []),
%                flush(stdout),
                close(in),
                close(out),
                wait(Pid, Stat),
                sleep(1000),
%                concat_string(["consult", " -f ", FileStem], ConsultCall),

%                system(ConsultCall),

                % start the subprocess
%	 	exec(bc, [In,Out], PID),
	
	        % send one query and read the answer
%                writeln(In, "21376123*23186238"), flush(In),
%	        read_string(Out, end_of_line, _, Answer1), writeln(Answer1),
	
               	% send another query and read the answer
%       	        writeln(In, "123233/3312"), flush(In),
%                read_string(Out, end_of_line, _, Answer2), writeln(Answer2),

                % close the other process' stdin:
	 	% this will cause it to terminate
%	 	close(In),
                % make sure it has terminated properly
%       	        wait(PID, Status),
%
%                concat_string(["consult -f ", FileStem], ConsultCall),
%                printf(stdout, "system('%w')\n", [ConsultCall]),
%                ( system(ConsultCall) ->
                ( true ->
/*                         printf(stdout, "true\r\n", []),
                         printf(stdout, "east\r\n", []),
                         printf(stdout, "[1\\, 3]\r\n", []),
                         printf(stdout, "false\r\n", []),
                         printf(stdout, "[]\r\n", []),
                         printf(stdout, "[[2\\, 3]\\, [1\\, 2]\\, [1\\, 4]\\, [1\\, 3]]\r\n", []),
                         printf(stdout, "[[2\\, 3]\\, [1\\, 2]\\, [1\\, 4]\\, [1\\, 3]]\r\n", []),
                         printf(stdout, "[[1\\, 2]\\, [1\\, 4]\\, [2\\, 3]]\r\n", []),
                         printf(stdout, "[[1\\, 1]\\, [1\\, 2]\\, [1\\, 4]\\, [2\\, 1]\\, [2\\, 2]\\, [2\\, 3]\\, [2\\, 4]\\, [3\\, 1]\\, [3\\, 2]\\, [3\\, 3]\\, [3\\, 4]\\, [4\\, 1]\\, [4\\, 2]\\, [4\\, 3]\\, [4\\, 4]]\r\n", []),
                         printf(stdout, "[]\r\n", []),
                         printf(stdout, "[[1\\, 3]]\r\n", []),
                         printf(stdout, "1\r\n", []),
                         printf(stdout, "[3\\, 4]\r\n", []),
                         printf(stdout, "true\r\n", []),
                         printf(stdout, "[]\r\n", []),
                         printf(stdout, "3\r\n", []),
                         printf(stdout, "1\r\n", []),
                         printf(stdout, "N/A\r\n", []),
                         printf(stdout, "false\r\n", []),
                         printf(stdout, "false\r\n", []),
                         printf(stdout, "false\r\n", []),
                         printf(stdout, "false\r\n", []),
                         printf(stdout, "false\r\n", []),
                         printf(stdout, "true\r\n", []),
                         printf(stdout, "[3\\, 4]\r\n", []),
                         printf(stdout, "true\r\n", []),
                         printf(stdout, "true\r\n", []),*/

                         printf(stdout, "[Consultation Phase]: Consultation call successful!\n", [])
                ;
                         printf(stdout, "[Consultation Phase]: (*** Error ***) Consultation call failed!\n", [])
                )
        ).
        

% }}}
