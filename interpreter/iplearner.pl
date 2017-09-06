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
%  Initialisation                                            %
% ---------------------------------------------------------- %

initialise_iplearner :-
%        get_flag(cwd, CWD),
%        printf( stdout, "******* CURRENT WORKING DIR: %w\n", [CWD]),
%        flush(stdout),
        %  Constant telling if there are parameterised exogenous
        %  primitive fluents (compounds containing vars) in the world model.
        get_all_fluent_names(FluentNames),
        contains_param_exog_prim_fluent(FluentNames, Result),
        setval(param_exog_prim_fluents, Result),
        printf(stdout, "setval(param_exog_prim_fluents, %w)\n", [Result]),
        %  Constant giving a maximum threshold for the hypothesis error [0,1].
        %  If adaptive IPL is enabled and the error of the decision tree of a
        %  solve context it below that error, we switch to the consultation
        %  phase for that solve.
        setval(max_hypothesis_error, 0.1),
        %  Constant defining the minimum of required training instances for
        %  adaptive IPL to switch to the consultation phase.
        setval( minimum_examples, 20 ),
%        %  Create a hash table to store the filenames (keys) for
%        %  the different solve contexts (values).
%        hash_create(SolveHashTable), setval(solve_hash_table, SolveHashTable),
        %  If not already stored on disk, create a fresh hash table to store the keys
        %  for the different policies (values).
        ( not(exists('solves.hash')) ->
           printf("solves.hash doesn't exists -> create fresh hash tables\n", []),
           flush(stdout),
           %  Create a hash table to store the filenames (keys) for
           %  the different solve contexts (values).
           hash_create(SolveHashTable), setval(solve_hash_table, SolveHashTable),
           hash_create(PolicyHashTable), setval(policy_hash_table, PolicyHashTable),
           %  Create hash tables to store the (average) Value, (average) TermProb,
           %  and (debugging) Tree for a policy with the corresponding hash key.
           hash_create(PolicyValueHashTable),
           hash_create(PolicyTermprobHashTable),
           hash_create(PolicyTreeHashTable)
        ;
           printf("solves.hash exists -> create hash tables from file\n", []),
           flush(stdout),
           %  Otherwise construct hash lists from the file.
           create_hash_lists_from_files( SolveHashTable,
                                         PolicyHashTable,
                                         PolicyValueHashTable,
                                         PolicyTermprobHashTable,
                                         PolicyTreeHashTable )
        ),
        setval(solve_hash_table, SolveHashTable),
        setval(policy_hash_table, PolicyHashTable),
        setval(policy_value_hash_table, PolicyValueHashTable),
        setval(policy_termprob_hash_table, PolicyTermprobHashTable),
        setval(policy_tree_hash_table, PolicyTreeHashTable),
        %  Global list that stores the fluent list that we use for training
        %  the C4.5 decision tree.
        ( not(exists('ipl.fluents')) ->
           printf("ipl.fluents doesn't exists -> setting ipl_fluents = []\n",
                  []),
           flush(stdout),
           setval( ipl_fluents, [] ),
           %  Is IPL still in pre-training phase (collecting calls for
           %  parameterised exogenous fluents).
           setval(ipl_pre_training_phase, true)
        ;
           printf("ipl.fluents exists -> initialising ipl_fluents from file\n",
                   []),
           printf("setting ipl_pre_training_phase = false\n", []),
           flush(stdout),
           initialise_ipl_fluents,
           %  Is IPL still in pre-training phase (collecting calls for
           %  parameterised exogenous fluents).
           setval(ipl_pre_training_phase, false)
        ),
        %  Global list that stores has_val calls to parameterised exogenous
        %  primitive fluents (compounds containing vars), whenever IPLearning
        %  is active.
        setval( param_exog_prim_fluent_calls, [] ),
        %  System time of the last change
        %  to the list param_exog_prim_fluent_calls. 
        setval( last_change_to_fluent_calls, _Uninstantiated ),
        %  Set the (heuristic) time difference, that we use to decide
        %  when to start with the IPL training phase.
        %  If the time of the last change to the list
        %  param_exog_prim_fluent_calls has been over for
        %  param_exog_prim_fluent_delta, then determine_ipl_phase/1
        %  triggers the training phase.
        setval( param_exog_prim_fluent_delta, 0.5 ).
        
%  Shortcuts.
param_exog_prim_fluents :- getval(param_exog_prim_fluents, X), X=true.

ipl_pre_training_phase :- getval(ipl_pre_training_phase, X), X=true.



% ---------------------------------------------------------- %
%  Utilities                                                 %
% ---------------------------------------------------------- %

%  Reads in the list of fluents that are relevant for IPL
%  from the file "ipl.fluents" and initialises the
%  global list ipl_fluents.
initialise_ipl_fluents :-
        printf(stdout, "Reading in ipl.fluents ... ", []),
        flush(stdout),
        open('ipl.fluents', read, Stream1),
        read_string(Stream1, end_of_line, _, _),
        read_string(Stream1, end_of_file, _, ByteList),
        printf(stdout, "successfully.\n", []),
        close(Stream1),
        bytes_to_term(ByteList, Fluents),
%        printf(stdout, "ipl.Fluents: %w\n", [Fluents]),
        setval(ipl_fluents, Fluents).

%  Returns an ordered set Result of all fluent names as Prolog terms.
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
        BuiltInTemp=[online, start, pll(_, _, _, _), pproj(_, _), 
                     lookahead(_, _, _, _), bel(_), ltp(_)],
%        ignore_fluents(IFL), 
%        append(BuiltInTemp, IFL, BuiltIn),

        %  In the following we give three hard-coded variants of IPL
        %  learning.
        %  The first one (default) is a standard agent for any
        %  domain with only univariate attributes (only fluents).
        %  The second one is used for a Wumpus World agent
        %  with multivariate attributes.
        %  The third one is used for a ReadyBot IPL agent with
        %  multivariate attributes.
        %  Please comment/uncomment the next lines accordingly.

        %%  Univariate Agent  %%
        FluentLTmp=[PrimFList, ContFList, ExogPrimFList, ExogContFList],

        %%  Multivariate Wumpus World Agent  %%
        %  For a test of multi-variate attributes, we build some fluents
        %  for the wumpus world by hand. We try to cover the correlations
        %  from the reward function in the evaluation of those fluents.
        %  The list of all defined fluents.
%        FluentLTmp=[PrimFList, ContFList, ExogPrimFList, ExogContFList,
%                    mvar_rew_exploration, mvar_rew_sum, mvar_rew_home],

        %%  Multivariate ReadyBot IPL Agent  %%
%        FluentLTmp=[PrimFList, ContFList, ExogPrimFList, ExogContFList,
%                    mvar_rew_dm, mvar_rew_bot_has_good_weapon, mvar_rew_get_my_score, mvar_rew_get_player_max_score,
%                    mvar_rew_has_double_damage, mvar_rew_weapon1, mvar_rew_health1, mvar_rew_health2, mvar_rew_health3,
%                    mvar_rew_armor1, mvar_rew_armor2, mvar_rew_armor3, mvar_rew_armor4, mvar_rew_double,
%                    mvar_rew_score1, mvar_rew_score2,
%                    mvar_prog_saw_opponent, mvar_prog_item_type_available_supershield,
%                    mvar_prog_item_type_available_shield, mvar_prog_item_type_available_health, mvar_prog_item_type_available_weapon],
        %  For using the double_damage item
        %  (not suggested for the Level 'Albatross')
        %  please use the following multivariate attribute list.
%        FluentLTmp=[PrimFList, ContFList, ExogPrimFList, ExogContFList,
%                    mvar_rew_dm, mvar_rew_bot_has_good_weapon, mvar_rew_get_my_score, mvar_rew_get_player_max_score,
%                    mvar_rew_has_double_damage, mvar_rew_weapon1, mvar_rew_health1, mvar_rew_health2, mvar_rew_health3,
%                    mvar_rew_armor1, mvar_rew_armor2, mvar_rew_armor3, mvar_rew_armor4, mvar_rew_double,
%                    mvar_rew_score1, mvar_rew_score2,
%                    mvar_prog_saw_opponent, mvar_prog_item_type_available_doubledamage, mvar_prog_item_type_available_supershield,
%                    mvar_prog_item_type_available_shield, mvar_prog_item_type_available_health, mvar_prog_item_type_available_weapon],                    

        flatten(FluentLTmp, FluentL),
        BuiltInTmp = [BuiltInTemp, RegL],
        flatten(BuiltInTmp, BuiltInL),
        subtract(FluentL, BuiltInL, ResultList),
        list_to_ord_set(ResultList, Result).


%  Returns an ordered set Result of all fluents that are
%  evaluable and have been projected to one dimension.
%  The list contains the fluent names in byte form!
:- mode ipl_get_all_fluent_names(-).
ipl_get_all_fluent_names(S, Result) :-
        param_exog_prim_fluents,
        ipl_pre_training_phase,
        !,
        %  ipl_fluents still is the empty list.
        %  Get all fluents.
        get_all_fluent_names( AllFluents ),
        %  Remove all exog_prim_fluents with parameters that are
        %  not instantiated or that are not evaluable (not valid)
        findall( NonGroundParamF,
                 ( member(NonGroundParamF, AllFluents),
                   is_param_exog_prim_fluent(NonGroundParamF),
                   not(ground(NonGroundParamF))
                 ),
                 NonGroundParamFluents ),
        subtract( AllFluents, NonGroundParamFluents, GroundFluents ),
        %  Add all parameterised from the list
        %  param_exog_prim_fluent_calls. Those are all ground.
        getval( param_exog_prim_fluent_calls, CalledFluents ),
        ResultTmp1 = [GroundFluents, CalledFluents],
        flatten( ResultTmp1, ResultTmp1Flat ),
        findall( Fluents1D,
                 ( member(EvaluableFT, ResultTmp1Flat),
                   %  Transform fluent names to byte form.
                   term_to_bytes(EvaluableFT, EvaluableFB),
                   %  Only pick fluents that can be evaluated in
                   %  situation S.
                   is_valid_fluent(EvaluableFB, S),
                   %  Substitute n scalar fluents for
                   %  n-dimensional vectors.
                   project_from_n_to_1(EvaluableFB, S, Fluents1D)
                 ),
                 ResultTmp2 ),
        flatten( ResultTmp2, ResultFlat ),
        list_to_ord_set( ResultFlat, Result ).

ipl_get_all_fluent_names(_S, Result) :-
        param_exog_prim_fluents,
        !,
        %  ipl_fluents is set already.
        getval( ipl_fluents, Result ).

ipl_get_all_fluent_names(S, Result) :-
        %  We know: not(param_exog_prim_fluents)
        %  Get all fluents.
        get_all_fluent_names( ResultTmp1Flat ),
        findall( Fluents1D,
                 ( member(EvaluableFT, ResultTmp1Flat),
                   %  Transform fluent names to byte form.
                   term_to_bytes(EvaluableFT, EvaluableFB),
                   %  Only pick fluents that can be evaluated in
                   %  situation S.
                   is_valid_fluent(EvaluableFB, S),
                   %  Substitute n scalar fluents for
                   %  n-dimensional vectors.
                   project_from_n_to_1(EvaluableFB, S, Fluents1D)
                 ),
                 ResultTmp2 ),
        flatten( ResultTmp2, ResultFlat ),
        list_to_ord_set( ResultFlat, Result ).


%  Returns an ordered set Result of all fluent values.
%  Gets the current situation S as input.
:- mode get_all_fluent_values(++, -).
get_all_fluent_values(S, Result) :-
        printf(stdout, "Querying all fluent values...\n", []),
        cputime(TQueryBegin),
        ipl_get_all_fluent_names(S, Fluents),
        findall( ValFStringNoComma,
                 ( member(F, Fluents),
                   %  Check if fluent is a custom multivariate fluent.
                   bytes_to_term(F, FT),
                   ( get_mvar_value(FT, S, ValF) ->
                        true
                     ;
                   %  Check if fluent is instantiated and we can
                   %  evaluate it,
                   %  or if it is a projection of an n-dimensional
                   %  fluent; then we can evaluate it with
                   %  get_value_from_n_dim_fluent/4.
                   ( is_valid_fluent(F, S) ->
                      %  Check if the fluent is a projection of an
                      %  n-dimensional fluent
                      ( is_projected_fluent(F) ->
                          get_value_from_n_dim_fluent(F, S, ValF)
                      ;
                          bytes_to_term(F, FT),
                          %  Fluent is instantiated and 1-dimensional.
                          ( exog_fluent(FT) ->
%                             printf(stdout, "Fluent %w is an exogenous fluent...\n", [FT]),
                             exog_fluent_getValue(FT, ValF, S)%,
%                             printf(stdout, "and has value %w.\n", [ValF])
                          ;
%                             printf(stdout, "Fluent %w is *NOT* an exogenous fluent...\n", [FT]),
                             subf(FT, ValF, S)%,
%                             printf(stdout, "and has value %w.\n", [ValF])
                          )
                      )
                   ;
                      printf(stdout, "*** Warning: *** ", []),
                      printf(stdout, "Fluent %w is not valid. ", [F]),
                      printf(stdout, "Fluent is ignored.\n", []),
                      false
                   )   %%,
                   ),  %% REMOVE ME, ONCE MULTIVAR TESTING IS GONE!
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
                 ),
                 Result ),
%        print_list(Result),
        cputime(TQueryEnd),
        TQueryDiff is TQueryEnd - TQueryBegin,
        printf(stdout, "with success in %w sec.\n", [TQueryDiff]).
                   

%  Tests if fluent F (in byte form!) is instantiated and we can evaluate
%  it in situation S.
:- mode is_valid_fluent(++, ++).
is_valid_fluent(F, S) :-
        %  Fluents that have been projected to one dimension
        %  are valid.
        is_projected_fluent(F),
        !,
        get_value_from_n_dim_fluent(F, S, _ValF).

is_valid_fluent(F, S) :-
        nonvar(F),
        bytes_to_term(F, FT),
        exog_fluent(FT),
        !,
        exog_fluent_getValue(FT, _ValF, S).

is_valid_fluent(F, S) :-
        nonvar(F),
        bytes_to_term(F, FT),
        exog_fluent(FT),
        subf(FT, _ValF, S).
        

%  Checks if a fluent (in byte form!) has been projected to a scalar from
%  an n-dimensional fluent.
:- mode is_projected_fluent(++).
is_projected_fluent(F) :-
         bytes_to_term(F, ProjectedFluent),
         ProjectedFluent = projected_fluent(_I, _N, _FluentND).


%  Decides, if fluent F (in byte form!) is continuous,
%  based on its value in situation S.
:- mode is_continuous(++, ++).
is_continuous( F, S ) :-
        is_projected_fluent(F),
        !,
        %  Fluent has been projected to one dimension.
        get_value_from_n_dim_fluent(F, S, ValF),
        float(ValF).

is_continuous( F, S ) :-
        nonvar(F),
        bytes_to_term(F, FT),
        exog_fluent(FT), !,
        exog_fluent_getValue(FT, ValF, S),
        float(ValF).

is_continuous( F, S ) :-
        nonvar(F),
        bytes_to_term(F, FT),
        subf(FT, ValF, S),
        float(ValF).
        

%  Sets the global list of fluent names for IPLearning in situation S.
:- mode set_ipl_fluent_names(++).
set_ipl_fluent_names(S) :-
        ipl_get_all_fluent_names( S, List ),
        setval( ipl_fluents, List ),
        %  Write ipl_fluents to file.
        %  First convert the list to byte form. This is not human-readable,
        %  but allows us to get valid terms from the string, even if
        %  the string contains quotation marks or is a callable like
        %  epf_fluent("Param1", param2).
        term_to_bytes(List, ByteList),
        open("ipl.fluents", write, Stream),
        printf(Stream, "### IPL fluents: ###\n", []),
        printf(Stream, "%w\n", [ByteList]),
        close(Stream).
                  

%  Returns the dimension N of a fluent F (in byte form) in situation S.
:- mode fluent_dimension(++, ++, -).
fluent_dimension(F, S, N) :-
        ( is_valid_fluent(F, S) ->
           bytes_to_term(F, FT),
           ( exog_fluent(FT) ->
              exog_fluent_getValue(FT, ValF, S)
           ;
              subf(FT, ValF, S)
           ),
           fluent_dimension_aux(ValF, N)%,
%           printf(stdout, "Dimension of Fluent %w is %w, its Val is: %w.\n",
%                 [FT, N, ValF]), flush(stdout)
        ;
           printf(stdout, "Fluent: %w is not a valid fluent.\n", [F]),
           flush(stdout),
           N = 1
        ).

%  Helper predicate for fluent_dimension/3.
%  Returns the dimension N of a fluent value ValF in situation S.
:- mode fluent_dimension_aux(++, -).
fluent_dimension_aux(ValF, N) :-
        not(is_list(ValF)),
        !,
        N = 1.

fluent_dimension_aux(ValF, N) :-
        length(ValF, N).
                      

%  Returns a list of n scalar fluents Fluents1D (in byte form!),
%  given a n-dimensional fluent FluentND (in byte form!) and a situation S.
:- mode project_from_n_to_1(++, ++, -).
project_from_n_to_1(FluentND, _S, Fluents1D) :-
        %  Hack for Wumpus World. It contains fluents that are dynamic lists
        %  of lists. Their dimension changes during program execution.
        %  So we use identity for projection.
        bytes_to_term(FluentND, FluentNDT),
        term_string(FluentNDT, FluentNDS),
        substring(FluentNDS, "epf_cells", 1),
        !,
        project_from_n_to_1_aux(FluentND, 1, Fluents1D).

project_from_n_to_1(FluentND, S, Fluents1D) :-
        fluent_dimension(FluentND, S, N),
        project_from_n_to_1_aux(FluentND, N, Fluents1D).

%  Helper predicate for project_from_n_to_1/3
%  Returns a list of n scalar fluent names Fluents1D,
%  given a N-dimensional fluent FluentND.
:- mode project_from_n_to_1_aux(++, ++, -).
project_from_n_to_1_aux(FluentND, 1, Fluents1D) :- !,
        %  Also convert 1D fluents to byte form.
        Fluents1D = FluentND.

project_from_n_to_1_aux(FluentND, N, Fluents1D) :-
        ( count(I, 1, N),
          foreach( Fluent1D, Fluents1D ),
          param(FluentND, N)
          do
%            term_string(I, IS),
%            term_string(N, NS),
            %  Convert the fluent name of the n-dimensional fluent
            %  to bytes, in order to be able to recover it later.
%            term_string(FluentND, FluentNDS),
%%            term_to_bytes(FluentND, FluentNDS),
%            replace_character(FluentNDSRaw, "\"", "\\\"", FluentNDS),
%            replace_character(FluentNDSRaw, "\"", "qUOTE", FluentNDS),
%%            concat_string(["dim_", IS, "_of_", NS, "_", FluentNDS],
%%                           Fluent1D)
            Fluent1DTmp = projected_fluent(I, N, FluentND),
            term_to_bytes(Fluent1DTmp, Fluent1D)
        ).


%  Evaluates a 1D fluent F (in byte form!) that it the result of the
%  projection from a n-dimensional fluent.
:- mode get_value_from_n_dim_fluent(++, ++, -).
get_value_from_n_dim_fluent(F, S, ValF) :-
        bytes_to_term(F, FT),
        FT = projected_fluent(I, _N, FluentND),
        %  Decipher name stem of n-dimensional fluent.
% printf(stdout, "FluentND: %w\n", [FluentND]), flush(stdout),
        bytes_to_term(FluentND, FluentNDT),
% printf(stdout, "get_value_from_n_dim_fluent(%w, %w, S, Valf)\n", [FluentNDT, I]), flush(stdout),
        get_value_from_n_dim_fluent(FluentNDT, I, S, ValF).


%        term_string(F, FString),
%        printf(stdout, "FString: %w.\n", [FString]),
%        string_length(FString, FStringLength),
%        substring(FString, OfPos, 4, "_of_"),
%        OfPosRight is (OfPos + 4),
%        CurrentDimLength is (OfPos - 6),
%        substring(FString, 6, CurrentDimLength, CurrentDimS),
%        printf(stdout, "CurrentDimS: %w.\n", [CurrentDimS]),
%
%        RestLength is (FStringLength - OfPosRight + 1),
%        substring(FString, OfPosRight, RestLength, RestString),
%        substring(RestString, BeforeStemPos, 1, "_"), !,  % Only match the
%                                                          % finding of the
%                                                          % pattern.
%        StemBegin is (BeforeStemPos + 1),
%        StemLength is (RestLength - StemBegin),
%        substring(RestString, StemBegin, StemLength, FluentStemString),
%        printf(stdout, "FluentStemString: %w.\n", [FluentStemString]),
%        term_string(FluentStemStringString, FluentStemString),
%        printf(stdout, "FluentStemStringString: %w.\n", [FluentStemStringString]),
%        bytes_to_term(FluentStemStringString, FluentStem),
%        printf(stdout, "FluentStem: %w.\n", [FluentStem]),
%        term_string(CurrentDim, CurrentDimS),
%        get_value_from_n_dim_fluent(FluentStem, CurrentDim, S, ValF).


%  Queries the CurrentDim's dimension of the TotalDim-dimensional
%  fluent FluentStem and returns the value of this entry.
:- mode get_value_from_n_dim_fluent(++, ++, ++, -).
get_value_from_n_dim_fluent(FluentStem, CurrentDim, S, ValF) :-
        ( exog_fluent(FluentStem) ->
           exog_fluent_getValue(FluentStem, ValFND, S)
        ;
           subf(FluentStem, ValFND, S)
        ),
% printf(stdout, "get_element(%w, %w, ValF)\n", [CurrentDim, ValFND]), flush(stdout),
        get_element(CurrentDim, ValFND, ValF).
% printf(stdout, "get_element(%w, %w, %w)\n", [CurrentDim, ValFND, ValF]), flush(stdout).
           

%  Returns the I'th element from a List.        
:- mode get_element(++, ++, -).
get_element(1, List, Element) :- !,
        List = [Element | _Tail].

get_element(I, List, Element) :-
        List = [_Head | Tail],
        J is I - 1,
        get_element(J, Tail, Element).


%  Decide, whether we are in the pre-training phase, where we still collect
%  param_exog_prim_fluent calls, or we are in the training phase, where we
%  still collect training data and train the decision tree for the given
%  solve-context, or we are in the consultation phase for this solve-context.
:- mode determine_ipl_phase(++, ++, -).
%  Uncomment next three lines to manually toggle the consultation phase
%  for all solve contexts. You must manually run C4.5 on each solve
%  context to generate the decision trees first!
%determine_ipl_phase( _Solve, _S, Phase ) :-
%        !,
%        Phase = "consult".

determine_ipl_phase( _Solve, _S, Phase ) :-
        param_exog_prim_fluents,
        ipl_pre_training_phase,
        getval( last_change_to_fluent_calls, TLastChange ),
        var( TLastChange ),
        !,
        Phase = "pre_train".

determine_ipl_phase( _Solve, _S, Phase ) :-
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
        printf(stdout, "primitive exogenous fluents. Solve is handled ", []),
        printf(stdout, "via DT-planning.\n", []),
        printf(stdout, "TLastChange: %w, TNow: %w, TDiff: %w\n", [TLastChange, TNow, TDiff]), flush(stdout),
        Phase = "pre_train".

determine_ipl_phase( _Solve, S, Phase ) :-
        param_exog_prim_fluents,
        ipl_pre_training_phase,
        %  Since above clause failed, we know: (TDiff >= CollectionDelta)
        !,
        getval( param_exog_prim_fluent_calls, FluentCalls ),
        length( FluentCalls, Calls ),
        printf(stdout, "Triggering IPL Training Phase!\n", []),
        printf(stdout, "We have collected %w calls for ", [Calls]),
        printf(stdout, "parameterised exogenous fluents.\n", []),
        printf(stdout, "Creating the list of fluents for IPLearning... ", []),
        set_ipl_fluent_names(S),
        printf(stdout, "done.\n", []),
        flush(stdout),
        setval( ipl_pre_training_phase, false ),
        Phase = "train".

determine_ipl_phase( HashKey, _S, Phase ) :-
        getval(solve_hash_table, SolveHashTable),
        not(hash_contains(SolveHashTable, HashKey)),
        !,
        %  solve context encountered for the first time.
        printf(stdout, "This solve was encountered for the first time.\n", []),
        Phase = "train".

determine_ipl_phase( HashKey, _S, Phase ) :-
        %  solve context has been encountered before.
        adaptive_ipl,
        printf(stdout, "Computing hypothesis error...\n", []),
        hypothesis_error(HashKey, Error),
        !,
        getval( max_hypothesis_error, MaxError ),
        ( Error < MaxError ->
           printf(stdout, "Triggering IPL Consultation Phase,\n", []),
           printf(stdout, "since Hypothesis Error with %w is below %w\n",
                 [Error, MaxError]),
           %  TODO: The current implementation of adaptive IPL does
           %  replace the solve context by a decision tree, once the
           %  error becomes small enough, but it does *not* continue
           %  training the tree and test if the error still decreases!
           Phase = "consult"
        ;
           printf(stdout, "Continuing Training,\n", []),
           printf(stdout, "since Hypothesis Error with %w is above %w\n",
                 [Error, MaxError]),
           Phase = "train"
        ).

determine_ipl_phase( _HashKey, _S, Phase ) :-
        %  This is the case when adaptive IPL is disabled,
        %  we want to train till we decide otherwise and
        %  manually trigger the consultation phase (by uncommenting
        %  the first clause).
        Phase = "train".

%  Creates a decision tree for the solve context with hash key HashKey,
%  and parses the estimated error of the *pruned* tree on *seen* data.
:- mode hypothesis_error(++, -).
hypothesis_error(HashKey, Error) :-
        term_string(HashKey, HashKeyString),
        concat_string(["solve_context_", HashKeyString], FileStem),
%        printf(stdout, "Executing C4.5...\n", []),
        exec(["c4.5", "-f",
              FileStem],
             [in, out, err], Pid),
%        printf(stdout, "successfully.\n", []),
        ( read_string(out, end_of_file, _, Output) ->
%            printf(stdout, "Writing output to file...\n", []),
            open('output.tmp', write, OutputFile),
            printf(OutputFile, "%w", [Output]),
            close(OutputFile),
            close(in),
            close(out),
            close(err),
            wait(Pid, _Stat),
%            printf(stdout, "successfully.\n", []),
            system("grep 'cases' output.tmp | sed -e 's/Read //g' | sed -e 's/ cases.*//g' > output.cases.tmp"),
            open('output.cases.tmp', read, CasesFile),
            read_string(CasesFile, end_of_line, _, CasesString) ->
            printf(stdout, "C4.5 says it found %w cases.\n", [CasesString]),
            flush(stdout),
            close(CasesFile),
            delete('output.cases.tmp'),
            term_string(Cases, CasesString),
            getval(minimum_examples, MinimumExamples),
            ( Cases < MinimumExamples ->
               printf(stdout, "Found less than %w examples -> continue training\n", [MinimumExamples]),
               delete('output.tmp'),
               false
            ;
               system("grep '<<' output.tmp | sed -e 's/(/ /g' | sed -e 's/%)//g' | awk '{print $6}' > output.error.tmp"),
               open('output.error.tmp', read, ErrorFile),
               read_string(ErrorFile, end_of_line, _, ErrorString) ->
               printf(stdout, "C4.5 says it found an error of %w percent.\n", [ErrorString]),
               flush(stdout),
               close(ErrorFile),
               delete('output.tmp'),
               term_string(ErrorPercent, ErrorString),
               Error is (ErrorPercent / 100.0)
            )
        ;
            close(in),
            close(out),
            close(err),
            wait(Pid, _Stat),
            false
        ).
        


%  Creates a hash key for the solve context/the policies and their filenames.
:- mode create_hash_key(++, -).
create_hash_key( Term, HashKey ) :-
        Term =.. [solve | _Args],
        !,
        term_hash( Term, -1, 1000, HashKey),
        printf(stdout, "solve has hash key %w.\n", [HashKey]).

create_hash_key( Term, HashKey ) :-
        term_hash( Term, -1, 1000, HashKey),
        printf(stdout, "Policy has hash key %w.\n", [HashKey]).


%  The predicate is true iff the given Stream gets ready for I/O in time 100.
:- mode stream_ready(++).
stream_ready( Stream ) :-
%        not at_eof( Stream ), % does not work as intended with the pipe stream
        select([Stream], 0, ReadyStream),
        ReadyStream = [Stream].

%  Skips through the (output)-stream Stream until Pattern is found or
%  Stream is "quiet for a while".
%  Prints skipped lines to stdout.
%  Returns String of the line where pattern is found.
%  Helper predicate for chatting with C4.5.
:- mode print_skip(++, ++, -).
print_skip( Stream, Pattern, String ) :-
%        ( stream_ready( Stream ) ->
        repeat,
           select([Stream], 0, ReadyStream),
        ( ReadyStream = [Stream] ),
        !,
        ( read_string(Stream, end_of_line, _, StringLine) ->
          ( substring(StringLine, Pattern, _) ->
             printf(stdout, "[C4.5] Skipped Line: %w\n", [StringLine]),
             String = StringLine
          ;
             printf(stdout, "[C4.5] Found Line: %w\n", [StringLine]),
             print_skip( Stream, Pattern, String )
          )
        ;
          printf(stdout, "[C4.5] Stream is at_eof.\n", []),
          String = "#### Error. Stream is at_eof. ####"
        ).

%  Skips through the (output)-stream Stream until Pattern is found or
%  Stream is "quiet for a while".
%  Does not print skipped lines to stdout.
%  Returns String of the line where pattern is found.
%  Helper predicate for chatting with C4.5.
:- mode quiet_skip(++, ++, -).
quiet_skip( Stream, Pattern, String ) :-
%        ( stream_ready( Stream ) ->
        repeat,
           select([Stream], 0, ReadyStream),
        ( ReadyStream = [Stream] ),
        !,
        ( read_string(Stream, end_of_line, _, StringLine) ->
          ( substring(StringLine, Pattern, _) ->
             String = StringLine
          ;
             quiet_skip( Stream, Pattern, String )
          )
        ;
          String = "#### Error. Stream is at_eof. ####"
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
        %  Remove all markers except for marker(true, false) and
        %  marker(false, true). Those are probably intended
        %  by the modeller to break a policy. But if, on the other hand,
        %  we deal with continuous fluent values
        %  (e.g., 3D positional vectors) in the marker we would get all
        %  different policies.
        printf(stdout, "Policy before cleaning markers: %w\n", [Policy]),
        flush(stdout),
        clean_up_markers(Policy, PolicyValidMarkers),
        printf(stdout, "Policy with clean markers: %w\n", [PolicyValidMarkers]),
        flush(stdout),
        term_to_bytes(PolicyValidMarkers, PolicyValidMarkersB),
        %  Create a hash key for the policy.
        getval(policy_hash_table, PolicyHashTable),
        %  Construct a Hash Key for the context "[Solve, PolicyValidMarkers]".
        %  The same Policy might appear in different solve contexts,
        %  and might have very different values in different solves.
        %  So we will keep apart policies from different solve contexts.
        term_hash([solve(Prog, Horizon, RewardFunction), PolicyValidMarkersB],
                  -1, 1000, PolicyHashKey),
        printf(stdout, "Policy has hash key %w.\n", [PolicyHashKey]),
        term_string(PolicyHashKey, PolicyHashKeyString),
        ( not(hash_contains(PolicyHashTable, PolicyHashKey)) ->
           %  Policy encountered for the first time.
           hash_set(PolicyHashTable, PolicyHashKey, PolicyValidMarkersB),
           setval(policy_hash_table, PolicyHashTable),
           %  Store the hash table on hard disk for later retrieval.
%           printf(stdout, "store_hash_list().\n", []),
           store_hash_list_binary(PolicyHashTable, 'policies.hash')%,
%           printf(stdout, "succeeded in store_hash_list().\n", [])
        ;
           true
        ),
        %  Add the (average) Value, (average) TermProb, and Tree for this
        %  policy to the corresponding hash tables.
        getval(policy_value_hash_table, PolicyValueHashTable),
        ( hash_contains(PolicyValueHashTable, PolicyHashKey) ->
           %  There is already an average value for this policy (for this
           %  solve context).
           hash_get(PolicyValueHashTable, PolicyHashKey, OldAvgValue),
           SumValue is (OldAvgValue + Value),
           NewAvgValue is (SumValue / 2.0),
           hash_set(PolicyValueHashTable, PolicyHashKey, NewAvgValue),
           store_hash_list(PolicyValueHashTable, 'values.hash')
        ;
           %  This is the first time that this policy is seen.
           hash_set(PolicyValueHashTable, PolicyHashKey, Value),
           setval(policy_value_hash_table, PolicyValueHashTable),
           store_hash_list(PolicyValueHashTable, 'values.hash')
        ),
        getval(policy_termprob_hash_table, PolicyTermprobHashTable),
        ( hash_contains(PolicyTermprobHashTable, PolicyHashKey) ->
           %  There is already an average termprob for this policy (for this
           %  solve context).
           hash_get(PolicyTermprobHashTable, PolicyHashKey, OldAvgTermprob),
           SumTermprob is (OldAvgTermprob + TermProb),
           NewAvgTermprob is (SumTermprob / 2.0),
           hash_set(PolicyTermprobHashTable, PolicyHashKey, NewAvgTermprob),
           setval(policy_termprob_hash_table, PolicyTermprobHashTable),
           store_hash_list(PolicyTermprobHashTable, 'termprobs.hash')
        ;
           %  This is the first time that this policy is seen.
           hash_set(PolicyTermprobHashTable, PolicyHashKey, TermProb),
           setval(policy_termprob_hash_table, PolicyTermprobHashTable),
           store_hash_list(PolicyTermprobHashTable, 'termprobs.hash')
        ),
        getval(policy_tree_hash_table, PolicyTreeHashTable),
        ( hash_contains(PolicyTreeHashTable, PolicyHashKey) ->
           %  There is already a tree for this policy.
           true
        ;
           %  This is the first time that this policy is seen.
           %  Store the trees in byte form, since otherwise we
           %  might face syntax errors during parsing.
           term_to_bytes(PolicyTree, PolicyTreeBytes),
           hash_set(PolicyTreeHashTable, PolicyHashKey, PolicyTreeBytes),
           setval(policy_tree_hash_table, PolicyTreeHashTable),
           store_hash_list_binary(PolicyTreeHashTable, 'trees.hash')
        ),
        ( not(hash_contains(SolveHashTable, HashKey)) ->
                %  solve context encountered for the first time.
                printf(stdout, "First encounter of this solve context.\n", []),
                hash_set(SolveHashTable, HashKey,
                        solve(Prog, Horizon, RewardFunction)),
                %  update "global" variable
                setval(solve_hash_table, SolveHashTable),
                %  write hash table to file
                store_hash_list(SolveHashTable, 'solves.hash'),

                % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %
                %  Construct a                       %
                %  ####### C4.5 .names file #######  %
                %  for this solve context.           %
                %  Instantiates ContextString,       %
                %  FluentNames, and DecisionString   %
                % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %
                construct_names_file( solve(Prog, Horizon,
                                      RewardFunction),
                                      PolicyHashKeyString, HashKeyString, S,
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
                /* solve context has been encountered before. */
                printf(stdout, "This solve context has been encountered ", []),
                printf(stdout, "before.\n", []),

                % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %
                %  Continue with                     %
                %  ##### C4.5 .names file #######    %
                %  Instantiates DecisionString.      %
                % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %
                continue_names_file( PolicyHashKeyString, HashKeyString,
                                     DecisionString ),

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
:- mode construct_names_file(++, ++, ++, ++, -, -, -).
construct_names_file( solve(Prog, Horizon, RewardFunction), PolicyHashKeyString,
                      HashKeyString, S,
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
        concat_string(["Policy_", PolicyHashKeyString],
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
        ipl_get_all_fluent_names(S, FluentNames),
 
        %  As we are not given the domain for discrete fluents
        %  by the Readylog programmer, we simply declare the
        %  domain for discrete fluents as discrete with 1000
        %  entries maximum
        ( foreach(Fluent, FluentNames),
          param(NameStream, S)
          do
             nonvar(Fluent),
             bytes_to_term(Fluent, FluentTerm),
             % ( is_cont_fluent(Fluent) -> %  Not really what we are looking
             %                                for. We will use our own test.
             %                                (The test implies that we can
             %                                evaluate the fluent.)
             ( get_mvar_value(FluentTerm, S, _ValF) ->
               printf(NameStream, "%w: discrete 10000.\n", [FluentTerm])
             ;  

             ( ( is_continuous(Fluent, S) ) ->
                   ( is_projected_fluent(Fluent) ->
                         %  Decipher name stem of n-dimensional fluent.
                         FluentTerm = projected_fluent(I, N, FluentND),
                         bytes_to_term(FluentND, FluentNDT),
                         term_string(FluentNDT, FluentNDTS),
                         term_string(I, IS),
                         term_string(N, NS),
                         concat_string(["dim_", IS, "_of_", NS, "_",
                                       FluentNDTS],
                                       FluentNameForHumans),
                         printf(NameStream, "%w: continuous.\n",
                               [FluentNameForHumans])
                   ;
                      printf(NameStream, "%w: continuous.\n", [FluentTerm])
                   )
             ;
                   ( is_projected_fluent(Fluent) ->
                         %  Decipher name stem of n-dimensional fluent.
                         FluentTerm = projected_fluent(I, N, FluentND),
                         bytes_to_term(FluentND, FluentNDT),
                         term_string(FluentNDT, FluentNDTS),
                         term_string(I, IS),
                         term_string(N, NS),
                         concat_string(["dim_", IS, "_of_", NS, "_",
                                       FluentNDTS],
                                       FluentNameForHumans),
                         printf(NameStream, "%w: discrete 10000.\n",
                               [FluentNameForHumans])
                   ;   
                      %  The fluent is in byte form... we make it human-
                      %  readable now.
                      ( is_prim_fluent(FluentTerm) ->
                         %  Make sure that we can evaluate the fluent.
                         ( exog_fluent(FluentTerm) ->
                            exog_fluent_getValue(FluentTerm, _ValF, S)
                         ;
                            subf(FluentTerm, _ValF, S)
                         ),
                         printf(NameStream, "%w: discrete 10000.\n", [FluentTerm])
                      ;
                         printf(NameStream, "%w: discrete 10000.\n", [FluentTerm]),
                         printf(NameStream, "| WARNING: %w is neither ", [FluentTerm]),
                         printf(NameStream, "cont nor prim!\n,", []),
                         printf(stdout, "*** WARNING ***: %w is neither ", [FluentTerm]),
                         printf(stdout, "cont nor prim!", [])
                      )
                   )
             )
             ) %% REMOVE ME ONCE MULTIVAR TESTING IS DONE!
        ),
        close(NameStream).

%  Creates the C4.5 .data file for the solve context with key HashKeyString.
%  Needs the ContextString, FluentNames, and DecisionString from
%  construct_names_file/7 as input.
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
:- mode continue_names_file(++, ++, ++).
continue_names_file( PolicyHashKeyString, HashKeyString, DecisionString ) :-
        concat_string(["solve_context_", HashKeyString, ".names"], FileName),
        %  Check, if the decision (policy) has been already declared.
        open(FileName, read, NameStreamRead),
        read_string(NameStreamRead, end_of_file, _Length, NameStreamString),
        close(NameStreamRead),
        concat_string(["Policy_", PolicyHashKeyString],
                       DecisionString),
        concat_string([DecisionString, ","],
                       DecisionStringAndComma),
        concat_string([DecisionString, "."],
                       DecisionStringAndFullstop),
        ( ( substring(NameStreamString, DecisionStringAndComma, _Pos)
            ;
            substring(NameStreamString, DecisionStringAndFullstop, _Pos) ) ->
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
%  Needs the DecisionString from continue_names_file/3 as input.
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


%  Stores a HashList to the hard disk in a File.
:- mode store_hash_list(++, ++).
store_hash_list(HashList, Filename) :-
        not(exists(Filename)),
        !,
        %  Create a new file.
        hash_list(HashList, HashKeys, HashValues),
        open(Filename, write, Stream),
        printf(Stream, "### HashKeys: ###\n", []),
        printf(Stream, "%w\n\n", [HashKeys]),
        printf(Stream, "### HashValues: ###\n", []),
        printf(Stream, "%w\n", [HashValues]),
        close(Stream),
        ( Filename = 'policies.hash' ->
          ( foreach(X, HashValues)
            do
              printf(stdout, "\n%w\n", [X]),
              flush(stdout)
          )
        ;
          true
        ).

store_hash_list(HashList, Filename) :-
        % File already exists. Delete it and try again.
        delete(Filename),
        store_hash_list(HashList, Filename).


%  Stores a HashList to the hard disk in a File with the values in
%  byte format.
:- mode store_hash_list_binary(++, ++).
store_hash_list_binary(HashList, Filename) :-
        not(exists(Filename)),
        !,
        %  Create a new file.
        hash_list(HashList, HashKeys, HashValues),
        open(Filename, write, Stream),
        printf(Stream, "### HashKeys: ###\n", []),
        printf(Stream, "%w\n\n", [HashKeys]),
        printf(Stream, "### HashValues: ###\n", []),
        term_to_bytes(HashValues, HashValuesBytes),
        printf(Stream, "%w\n", [HashValuesBytes]),
        close(Stream).
%        ( foreach(X, HashValues)
%          do
%            bytes_to_term(X, XT),
%            printf(stdout, "%w\n", [XT]),
%            flush(stdout)
%        ).

store_hash_list_binary(HashList, Filename) :-
        % File already exists. Delete it and try again.
        delete(Filename),
        store_hash_list_binary(HashList, Filename).


%  Reads the data from the files, puts them
%  into hash lists, and returns those lists.
:- mode create_hash_lists_from_files(-, -, -, -, -).
create_hash_lists_from_files( SolveHashTable,
                              PolicyHashTable,
                              PolicyValueHashTable,
                              PolicyTermprobHashTable,
                              PolicyTreeHashTable ) :-
        hash_create( SolveHashTable ),
        ( exists('solves.hash') ->
           printf(stdout, "Reading in solves.hash ... ", []),
           flush(stdout),
           open('solves.hash', read, Stream1),
           read_string(Stream1, end_of_line, _, _),
           read_string(Stream1, end_of_line, _, SolveHashKeysS),
           read_string(Stream1, end_of_line, _, _),
           read_string(Stream1, end_of_line, _, _),
           read_string(Stream1, end_of_line, _, SolveHashValuesS),
           close(Stream1),
           printf(stdout, "successfully.\n", []),
           term_string(SolveHashKeys, SolveHashKeysS),
           term_string(SolveHashValues, SolveHashValuesS),
           hash_set_recursively( SolveHashKeys, SolveHashValues,
                                 SolveHashTable )
        ;
           true
        ),
        printf(stdout, "Reading in policies.hash ... ", []),
        flush(stdout),
        open('policies.hash', read, Stream2),
        read_string(Stream2, end_of_line, _, _),
        read_string(Stream2, end_of_line, _, PolicyHashKeysS),
        read_string(Stream2, end_of_line, _, _),
        read_string(Stream2, end_of_line, _, _),
        %  Read in all lines that are left, as we stored the list of
        %  policies as byte value, which stretches over several lines.
        read_string(Stream2, end_of_file, _, PolicyHashValuesS),
        printf(stdout, "successfully.\n", []),
        close(Stream2),
        printf(stdout, "Creating PolicyHashTable ... ", []),
        hash_create( PolicyHashTable ),
        printf(stdout, "successfully.\n", []), flush(stdout),
        printf(stdout, "Instantiating PolicyHashTable ... ", []),
        term_string(PolicyHashKeys, PolicyHashKeysS),
        %  Leave the policies in byte form.
        bytes_to_term(PolicyHashValuesS, PolicyHashValues),
        hash_set_recursively( PolicyHashKeys, PolicyHashValues,
                              PolicyHashTable ),
        printf(stdout, "successfully.\n", []),
        hash_create( PolicyValueHashTable ),
        ( exists('values.hash') ->
           printf(stdout, "Reading in values.hash ... ", []),
           flush(stdout),
           open('values.hash', read, Stream3),
           read_string(Stream3, end_of_line, _, _),
           read_string(Stream3, end_of_line, _, PolicyValueHashKeysS),
           read_string(Stream3, end_of_line, _, _),
           read_string(Stream3, end_of_line, _, _),
           read_string(Stream3, end_of_line, _, PolicyValueHashValuesS),
           close(Stream3),
           printf(stdout, "successfully.\n", []),
           term_string(PolicyValueHashKeys, PolicyValueHashKeysS),
           term_string(PolicyValueHashValues, PolicyValueHashValuesS),
           hash_set_recursively( PolicyValueHashKeys, PolicyValueHashValues,
                                 PolicyValueHashTable )
        ;
           true
        ),
        hash_create( PolicyTermprobHashTable ),
        ( exists('termprobs.hash') ->
           printf(stdout, "Reading in termprobs.hash ... ", []),
           flush(stdout),
           open('termprobs.hash', read, Stream4),
           read_string(Stream4, end_of_line, _, _),
           read_string(Stream4, end_of_line, _, PolicyTermprobHashKeysS),
           read_string(Stream4, end_of_line, _, _),
           read_string(Stream4, end_of_line, _, _),
           read_string(Stream4, end_of_line, _, PolicyTermprobHashValuesS),
           close(Stream4),
           printf(stdout, "successfully.\n", []),
           term_string(PolicyTermprobHashKeys, PolicyTermprobHashKeysS),
           term_string(PolicyTermprobHashValues, PolicyTermprobHashValuesS),
           hash_set_recursively( PolicyTermprobHashKeys, PolicyTermprobHashValues,
                                 PolicyTermprobHashTable )
        ;
           true
        ),
        hash_create( PolicyTreeHashTable ),
        ( exists('trees.hash') ->
           printf(stdout, "Reading in trees.hash ... ", []),
           flush(stdout),
           open('trees.hash', read, Stream5),
           read_string(Stream5, end_of_line, _, _),
           read_string(Stream5, end_of_line, _, PolicyTreeHashKeysS),
           read_string(Stream5, end_of_line, _, _),
           read_string(Stream5, end_of_line, _, _),
           %  Read in all lines that are left, as we stored the list of
           %  trees as byte value, which stretches over several lines.
           read_string(Stream5, end_of_file, _, PolicyTreeHashValuesS),
           close(Stream5),
           printf(stdout, "successfully.\n", []),
           bytes_to_term(PolicyTreeHashValuesS, PolicyTreeHashValues),
           term_string(PolicyTreeHashKeys, PolicyTreeHashKeysS),
%           term_string(PolicyTreeHashValues, PolicyTreeHashValuesS),
           bytes_to_term(PolicyTreeHashValuesS, PolicyTreeHashValues),
           hash_set_recursively( PolicyTreeHashKeys, PolicyTreeHashValues,
                                 PolicyTreeHashTable )
        ;
           true

        ).
        

%  Instantiates the HashTable by iterating through
%  the lists of HashKeys and HashValues and setting each
%  single pair.
:- mode hash_set_recursively(++, ++, ?).
hash_set_recursively( [], [], _HashTable ) :- !.

hash_set_recursively( HashKeys, HashValues, HashTable ) :- 
% printf(stdout, "hash_set_recursively( %w, %w, %w )\n", [HashKeys, HashValues, HashTable]), flush(stdout),
% printf(stdout, "HashKeys: %w\n", [HashKeys]), flush(stdout),
        HashKeys = [Key | RemainingKeys],
% printf(stdout, "Key: %w\n", [Key]), flush(stdout),
        HashValues = [Value | RemainingValues],
% printf(stdout, "hash_set( %w, %w )\n", [Key, Value]), flush(stdout),
        hash_set(HashTable, Key, Value),
        hash_set_recursively(RemainingKeys, RemainingValues,
                             HashTable).
        

%  Replaces all markers containing variables that had been instantiated during
%  planning, by markers containing fresh variables.
%  Note, that the following markers are hard-coded and found through inspection
%  of the ReadyBot code!
:- mode clean_up_markers(++, -).
clean_up_markers(Policy, Result) :-
        %  We use an accumulator and tail-recursion
        %  for higher efficiency.
        clean_up_markers(Policy, [], ResultReversed),
        %( ResultReversed = [] ->
           ResultFlat = ResultReversed,
        %;
% Flattening destroys if(Cond, [], Prog).
%        flatten(ResultReversed, ResultFlat),
        %),
        %  Since we want to avoid using append, we
        %  construct the clean policy in reverse
        %  order in the accumulator and now reverse it.
        reverse(ResultFlat, Result).

:- mode clean_up_markers(++, ++, -).
clean_up_markers([], Result, Result) :- !.

%  For the ReadyBots leave in the following custom markers.
clean_up_markers([marker(f_SawOpponent=false, true) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(f_SawOpponent=false, true) | Rest], Result).

clean_up_markers([marker(f_SawOpponent=true, false) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(f_SawOpponent=true, false) | Rest], Result).

clean_up_markers([marker(f_SawOpponent, true) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(f_SawOpponent, true) | Rest], Result).
 
clean_up_markers([marker(f_SawOpponent, false) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(f_SawOpponent, false) | Rest], Result).

clean_up_markers([marker(f_BotHasGoodWeapon=false, true) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(f_BotHasGoodWeapon=false, true) | Rest], Result).

clean_up_markers([marker(f_BotHasGoodWeapon=true, false) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(f_BotHasGoodWeapon=true, false) | Rest], Result).

clean_up_markers([marker(f_BotHasGoodWeapon, true) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(f_BotHasGoodWeapon, true) | Rest], Result).

clean_up_markers([marker(f_BotHasGoodWeapon, false) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(f_BotHasGoodWeapon, false) | Rest], Result).

clean_up_markers([marker(f_ItemTypeAvailable(Item), true) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(f_ItemTypeAvailable(Item), true) | Rest], Result).

clean_up_markers([marker(f_ItemTypeAvailable(Item), false) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(f_ItemTypeAvailable(Item), false) | Rest], Result).

clean_up_markers([marker(f_ItemTypeAvailable(Item)=false, true) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(f_ItemTypeAvailable(Item)=false, true) | Rest], Result).

clean_up_markers([marker(f_ItemTypeAvailable(Item)=true, false) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(f_ItemTypeAvailable(Item)=true, false) | Rest], Result).

clean_up_markers([marker(and([f_BotHasGoodWeapon=false, f_ItemTypeAvailable(weapon)=true]), true) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(and([f_BotHasGoodWeapon=false, f_ItemTypeAvailable(weapon)=true]), true) | Rest], Result).

clean_up_markers([marker(and([f_BotHasGoodWeapon=false, f_ItemTypeAvailable(weapon)=true]), false) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(and([f_BotHasGoodWeapon=false, f_ItemTypeAvailable(weapon)=true]), false) | Rest], Result).

clean_up_markers([marker(neg(0=0), true) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(neg(0=0), true) | Rest], Result).

clean_up_markers([marker(neg(0=0), false) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(neg(0=0), false) | Rest], Result).

clean_up_markers([marker(neg(1=0), true) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(neg(1=0), true) | Rest], Result).

clean_up_markers([marker(neg(1=0), false) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(neg(1=0), false) | Rest], Result).

clean_up_markers([marker(neg(0=1), true) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(neg(0=1), true) | Rest], Result).

clean_up_markers([marker(neg(0=1), false) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(neg(0=1), false) | Rest], Result).

%  Marker for test in collect(Type, Horizon)
clean_up_markers([marker(and( [ Loc = epf_BotLocation, getNextVisNavNodes( Loc, Horizon, Type, 0.9, TmpVisList ),
                                lif( TmpVisList = [], getNextVisNavNodes( Loc, Horizon, Type, 0.5, VisList ),
                                VisList = TmpVisList ), lif( neg( VisList = [] ), VisList = [HeadElement|_TailElements],
                                HeadElement = nothing ), NewHorizon = Horizon - 1 ] ),
                         true) | PolTail], Rest, Result) :- !,
%  Replace instantiated Variables by new variables and write those variable names to the Policy.
% printf(stdout, "\n\n ***** \n Found a collect test marker with Loc = %w\n", [Loc]),
% printf(stdout, "\n\n ***** \n Replaced it with the variable MarkerVarLoc\n ***** \n \n", []),
% flush(stdout),
        clean_up_markers(PolTail, [marker(and( [ MarkerVarLoc = epf_BotLocation, getNextVisNavNodes( MarkerVarLoc, Horizon, Type, 0.9, MarkerVarTmpVisList ),
                                                 lif( MarkerVarTmpVisList = [], getNextVisNavNodes( MarkerVarLoc, Horizon, Type, 0.5, MarkerVarVisList ),
                                                 MarkerVarVisList = MarkerVarTmpVisList ), lif( neg( MarkerVarVisList = [] ), MarkerVarVisList = [MarkerVarHeadElement|_MarkerVarTailElements],
                                                 MarkerVarHeadElement = nothing ), MarkerVarNewHorizon = Horizon - 1 ] ),
                                          true) | Rest], Result).

%  Marker for test in collect(Type, Horizon)
clean_up_markers([marker(and( [ Loc = epf_BotLocation, getNextVisNavNodes( Loc, Horizon, Type, 0.9, TmpVisList ),
                                lif( TmpVisList = [], getNextVisNavNodes( Loc, Horizon, Type, 0.5, VisList ),
                                VisList = TmpVisList ), lif( neg( VisList = [] ), VisList = [HeadElement|_TailElements],
                                HeadElement = nothing ), NewHorizon = Horizon - 1 ] ),
                         false) | PolTail], Rest, Result) :- !,
%  Replace instantiated Variables by new variables and write those variable names to the Policy.
        clean_up_markers(PolTail, [marker(and( [ MarkerVarLoc = epf_BotLocation, getNextVisNavNodes( MarkerVarLoc, Horizon, Type, 0.9, MarkerVarTmpVisList ),
                                                 lif( MarkerVarTmpVisList = [], getNextVisNavNodes( MarkerVarLoc, Horizon, Type, 0.5, MarkerVarVisList ),
                                                 MarkerVarVisList = MarkerVarTmpVisList ), lif( neg( MarkerVarVisList = [] ), MarkerVarVisList = [MarkerVarHeadElement|_MarkerVarTailElements],
                                                 MarkerVarHeadElement = nothing ), MarkerVarNewHorizon = Horizon - 1 ] ),
                                          false) | Rest], Result).

%%
clean_up_markers([marker(f_SawOpponent=true, false) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(f_SawOpponent=true, false) | Rest], Result).

clean_up_markers([marker(true, false) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(true, false) | Rest], Result).

clean_up_markers([marker(false, true) | PolTail], Rest, Result) :- !,
        clean_up_markers(PolTail, [marker(false, true) | Rest], Result).

clean_up_markers([marker(C, T) | PolTail], Rest, Result) :- !,
%        clean_up_markers(PolTail, [marker(true, true) | Rest], Result).
% printf(stdout, "\n clean_up_markers[marker(_C, _T)]: PolTail %w\n", [PolTail]), flush(stdout),
        clean_up_markers(PolTail, [marker(C, T) | Rest], Result).

clean_up_markers([Term | PolTail], Rest, Result) :-
        not(compound(Term)), !,
        %  Term is neither a list nor a structure.
        clean_up_markers(PolTail, [Term | Rest], Result).

clean_up_markers([Term | PolTail], Rest, Result) :-
        %  Term is a compound.
        is_list(Term), !,
% printf(stdout, "\n clean_up_markers[LIST]: Term = %w\n", [Term]), flush(stdout),
        clean_up_markers(Term, TermClean),
% printf(stdout, "\n clean_up_markers[LIST]: TermClean = %w\n", [TermClean]), flush(stdout),
        clean_up_markers(PolTail, [TermClean | Rest], Result).

clean_up_markers([Term1=Term2 | PolTail], Rest, Result) :-
        not(is_list(Term1)),
        not(is_list(Term2)), !,
        clean_up_markers([Term1], Term1Clean),
        clean_up_markers([Term2], Term2Clean),
        Term1Clean = [Term1Final],
        Term2Clean = [Term2Final],
        clean_up_markers(PolTail, [Term1Final=Term2Final | Rest], Result).

clean_up_markers([Term1=Term2 | PolTail], Rest, Result) :-
        not(is_list(Term1)), !,
        clean_up_markers([Term1], Term1Clean),
        clean_up_markers(Term2, Term2Clean),
        Term1Clean = [Term1Final],
        clean_up_markers(PolTail, [Term1Final=Term2Clean | Rest], Result).

clean_up_markers([Term1=Term2 | PolTail], Rest, Result) :-
        not(is_list(Term2)), !,
        clean_up_markers(Term1, Term1Clean),
        clean_up_markers([Term2], Term2Clean),
        Term2Clean = [Term2Final],
        clean_up_markers(PolTail, [Term1Clean=Term2Final | Rest], Result).

clean_up_markers([Term1=Term2 | PolTail], Rest, Result) :-
        !,
        clean_up_markers(Term1, Term1Clean),
        clean_up_markers(Term2, Term2Clean),
        clean_up_markers(PolTail, [Term1Clean=Term2Clean | Rest], Result).

clean_up_markers([Term | PolTail], Rest, Result) :-
        %  As Term is no list and it is a compound,
        %  it must be a structure (e.g., Term=if(A,B,C)).
        %  We have to recurse into the arguments.
        Term =.. [Functor | Args],
% printf(stdout, "\n clean_up_markers[COMPOUND]: Term =.. [%w | %w]\n", [Functor, Args]), flush(stdout),
        clean_up_markers(Args, CleanArgs),
        CleanTerm =.. [Functor | CleanArgs],
% printf(stdout, "\n clean_up_markers[COMPOUND]: CleanTerm = %w\n", [CleanTerm]), flush(stdout),
        clean_up_markers(PolTail, [CleanTerm | Rest], Result).



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
        concat_string([FullPath, ".tree"], FullPathTree),
        ( not(exists(FullPathTree)) ->
            printf(stdout, "Error: %w not found!\n", [FullPathTree])
        ;
            true
        ),
        %  Run the C4.5/Prolog interface as another process.
        %  When using with the ReadyBots, please comment the next 8 lines,
        %  and uncomment the lines marked with %% below!
        ( not(exists("../../libraries/c45_lib/consultobj/ConsultObjectTest2")) ->
            printf(stdout, "Error: ../../libraries/c45_lib/consultobj/ConsultObjectTest2 not found!\n", [])
        ;
            true
        ),
        exec(["../../libraries/c45_lib/consultobj/ConsultObjectTest2", "-f",
              FullPath],
             [in, out, err], Pid),

%%        ( not(exists("../golog/ipl_agent/libraries/c45_lib/consultobj/ConsultObjectTest2")) ->
%%            printf(stdout, "Error: ../golog/ipl_agent/libraries/c45_lib/consultobj/ConsultObjectTest2 not found!\n", [])
%%        ;
%%            true
%%        ),
%%        exec(["../golog/ipl_agent/libraries/c45_lib/consultobj/ConsultObjectTest2", "-f",
%%              FullPath],
%%             [in, out, err], Pid),

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
        ( nonvar(DecisionString) ; nonvar(Success) ),
        !,
        close(in),
        close(out),
        close(err),
        wait(Pid, _Stat),

        ( ( Success == false ) ->
           true
        ;
            printf(stdout, "[Consultation Phase]: Success == true.. extract...\n", []),
           extract_consultation_results( DecisionString,
                                         Policy, Value, TermProb, Tree ),
            printf(stdout, "[Consultation Phase]: Success\n", []),
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
%        print_skip( out, "####", IndicatorString ),
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
        printf(stdout, "--------\n", []),
        printf(stdout, "[PROLOG] C4.5 asked for the value of attribute: ", []),
        printf(stdout, "%w\n", [AttributeNameS]),
        flush(stdout),
%        exog_fluent_getValue(AttributeName, ValTmp, S),
        %  Check if the fluent is a projection of an
        %  n-dimensional fluent
        ( substring(AttributeNameS, 1, 4, "dim_") ->
           %  Extract the current dimension, the total dimension,
           %  and the filestem.
           %  The fluent name has the format:
           %  dim_CurrDim_of_TotalDim_FileStem.
           
           string_length(AttributeNameS, FStringLength),
           substring(AttributeNameS, OfPos, 4, "_of_"),
           OfPosRight is (OfPos + 4),
           CurrentDimLength is (OfPos - 5),
           substring(AttributeNameS, 5, CurrentDimLength, CurrentDimS),
           printf(stdout, "CurrentDimS: %w.\n", [CurrentDimS]),

           RestLength is (FStringLength - OfPosRight + 1),
           substring(AttributeNameS, OfPosRight, RestLength, RestString),
           substring(RestString, BeforeStemPos, 1, "_"), !,  % Only match the
                                                             % first finding of
                                                             % the pattern.
           BeforeStemPosLeft is (BeforeStemPos - 1),
           substring(RestString, 1, BeforeStemPosLeft, TotalDimS),
           printf(stdout, "TotalDimS: %w.\n", [TotalDimS]),

           StemBegin is (BeforeStemPos + 1),
           StemLength is (RestLength - StemBegin + 1),
           substring(RestString, StemBegin, StemLength, FluentStemString),
           printf(stdout, "FluentStemString: %w.\n", [FluentStemString]),
           flush(stdout),

           %  Converting the FluentStemString to a term with term_string/2
           %  does not result in the correct term, if the term contains
           %  quotation marks. That is why we have to try for
           %  all projected ipl_fluents, if the conversion to a string
           %  would lead to FluentStemString.

           ipl_get_all_fluent_names(S, IPLFluents),

           findall( Fluent,
                    ( member(Fluent, IPLFluents),
                      bytes_to_term(Fluent, ProjectedFluent),
                      ProjectedFluent = projected_fluent(I, N, FluentND),
                      bytes_to_term(FluentND, FluentNDT),
                      term_string(FluentNDT, FluentNDTS),
                      term_string(I, IS),
                      term_string(N, NS),
                      IS = CurrentDimS,
                      NS = TotalDimS,
                      FluentNDTS = FluentStemString ),
                    DegeneratedList ),

           printf(stdout, "DegeneratedList: %w.\n", [DegeneratedList]),
           flush(stdout),

           DegeneratedList = [Fluent1D],
           get_value_from_n_dim_fluent(Fluent1D, S, ValTmp)
        ;
           %  Fluent is not a projection of an n-dimensional fluent.
           term_string(AttributeName, AttributeNameS),
           %  Fluent is instantiated and 1-dimensional.
           ( get_mvar_value(AttributeName, S, ValTmp) ->
              true 
           ;
           ( exog_fluent(AttributeName) ->
              exog_fluent_getValue(AttributeName, ValTmp, S)
           ;
              subf(AttributeName, ValTmp, S)
           )                   
           )
        ),
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
        read_string(out, end_of_line, _, DecisionString),
%        string_length(Decision, DecisionLength),
%        RawDecisionLength is (DecisionLength - 2),
%        substring(Decision, 2, RawDecisionLength, DecisionString),
        printf(stdout, "[C4.5] Decision: %w\n", [DecisionString]),
        printf(stdout, "--------\n", []),
        flush(stdout),
        ( ( stream_ready(out), not(at_eof(out) ) ) ->
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
        printf(stdout, "[C4.5] IndicatorString: %w\n", [IndicatorString]),
        printf(stdout, "[C4.5] This attribute value has not appeared ", []),
        printf(stdout, "during training.\n", []),
        printf(stdout, "       Will use planning instead.\n", []),
        printf(stdout, "--------\n", []),
        Success = false,
        flush(stdout),
        ( ( stream_ready(out), not(at_eof(out) ) ) ->
           read_string(out, end_of_file, _, _Rest)
        ;
           true
        ).

ask_c45_for_decision_aux( in, out, err, _S, IndicatorString,
                          _DecisionString, Success ) :-
        %  Happens when the attribute value has not been seen during
        %  training.
        substring(IndicatorString, "#### Error. Stream is at_eof. ####", _), !,
        printf(stdout, "--------\n", []),
        printf(stdout, "[C4.5] IndicatorString: %w\n", [IndicatorString]),
        printf(stdout, "Will use planning instead.\n", []),
        printf(stdout, "--------\n", []),
        Success = false,
        flush(stdout),
        ( ( stream_ready(out), not(at_eof(out) ) ) ->
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
        ( ( stream_ready(out), not(at_eof(out) ) ) ->
           read_string(out, end_of_file, _, _Rest)
        ;
           true
        ).


%  From the classification DecisionString, extract the policy.
%  The DecisionString is "Policy_x", where x is the hash key of
%  the policy.
:- mode extract_consultation_results(++, -, -, -, -).
extract_consultation_results( DecisionString,
                              Policy, Value, TermProb, Tree ) :-
         %%%  Cut out hash key for Policy.  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         printf(stdout, "DecisionString: %w\n", [DecisionString]), flush(stdout),
         append_strings("Policy_", PolicyHashKeyString, DecisionString),
         printf(stdout, "PolicyHashKeyString: %w\n", [PolicyHashKeyString]), flush(stdout),
         term_string(PolicyHashKey, PolicyHashKeyString),
         printf(stdout, "PolicyHashKey: %w\n", [PolicyHashKey]), flush(stdout),
         %%%  Get the necessary information from the hash tables.  %%%%%%%%%
         getval(policy_hash_table, PolicyHashTable),
         ( not(hash_contains(PolicyHashTable, PolicyHashKey)) ->
            printf(stdout, "Error: No hash entry for this PolicyHashKey in PolicyHashTable!\n", []), flush(stdout)
         ;
            hash_get(PolicyHashTable, PolicyHashKey, PolicyB),
            bytes_to_term(PolicyB, Policy),
            printf(stdout, "Policy: %w\n", [Policy]), flush(stdout)
         ),

         getval(policy_value_hash_table, PolicyValueHashTable),
         hash_get(PolicyValueHashTable, PolicyHashKey, Value),
         printf(stdout, "Value: %w\n", [Value]), flush(stdout),

         getval(policy_termprob_hash_table, PolicyTermprobHashTable),
         hash_get(PolicyTermprobHashTable, PolicyHashKey, TermProb),
         printf(stdout, "TermProb: %w\n", [TermProb]), flush(stdout),

         getval(policy_tree_hash_table, PolicyTreeHashTable),
         hash_get(PolicyTreeHashTable, PolicyHashKey, TreeBytes),
         bytes_to_term(TreeBytes, Tree),
         ( dtdebug ->
            printf(stdout, "Tree: %w\n", [Tree]), flush(stdout)
         ;
            true
         ).

%  Evaluates a custom multivariate fluent.
:- mode get_mvar_value(++, ++, -).

get_mvar_value(_MVar, _S, _Val) :-
        not(multivariate),
        !,
        false.

%%  WUMPUS WORLD MULTIVARIATE ATTRIBUTES  %%

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_exploration,
        !,
        %  Compute the correlated value from the reward function.
        printf(stdout, "Computing value of multivariate fluent mvar_rew_exploration...\n", []),
        exog_fluent_getValue( epf_distance_to_closest_safe_cell, Val1, S),
        exog_fluent_getValue( epf_num_visited, Val2, S),
        exog_fluent_getValue( epf_num_facedShades, Val3, S),
        Val1Tmp is (Val1 * -10),
        Val2Tmp is (Val2 * 20),
        Val is (Val1Tmp + Val2Tmp + Val3),
        printf(stdout, "Value of multivariate fluent mvar_rew_exploration: %w\n", [Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_sum,
        !,
        %  Compute the correlated final value from the reward function.
        printf(stdout, "Computing value of multivariate fluent mvar_rew_sum...\n", []),
        exog_fluent_getValue( epf_wumpus_alive, WA, S ),
        exog_fluent_getValue( epf_sense_gold, SG, S ),
        exog_fluent_getValue( epf_distance_to_closest_safe_cell, DTCSC, S),
        exog_fluent_getValue( epf_num_visited, NV, S),
        exog_fluent_getValue( epf_num_facedShades, NFS, S),
        exog_fluent_getValue( epf_turning_back_again, TBA, S),
        exog_fluent_getValue( epf_remaining_time, RT, S),
        exog_fluent_getValue( epf_returned_safely, RS, S),
        ( WA = true ->
            RewardKillWumpus = 0
        ;
            RewardKillWumpus = 50
        ),
        ( SG = true ->
            RewardFoundGold = 100
        ;
            RewardFoundGold = 0
        ),
        RewardExplorationTmp1 is (DTCSC * -10),
        RewardExplorationTmp2 is (NV * 20),
        RewardExploration is (RewardExplorationTmp1 + RewardExplorationTmp2 + NFS),
        ( TBA = true ->
            RewardDoNotTurnInCirclesStupidly = -100
        ;
            RewardDoNotTurnInCirclesStupidly = 0
        ),
        ( RT > 0 ->
           ( RS = true ->
               RewardTime = 0
           ;
               RewardTime = 0
           )
        ;
           ( RS = true ->
               RewardTime = 0
           ;
               RewardTime = -500
           )
        ),
        Val is ( RewardKillWumpus +
                 RewardFoundGold +
                 RewardExploration +
                 RewardDoNotTurnInCirclesStupidly +
                 RewardTime ),
        printf(stdout, "Value of multivariate fluent mvar_rew_sum: %w\n", [Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_home,
        !, %  We insert a cut here to make the predicate fail, if it is called
           %  with a univariate fluent.
        %  Compute the correlated final value from the reward function.
        printf(stdout, "Computing value of multivariate fluent mvar_rew_home...\n", []),
        exog_fluent_getValue( epf_start_pos_X, SX, S),
        exog_fluent_getValue( epf_start_pos_Y, SY, S),
        exog_fluent_getValue( epf_agent_pos_X, AX, S),
        exog_fluent_getValue( epf_agent_pos_Y, AY, S),
        ( SX = AX ->
           Val = 100
        ;
           PreDiffX is (SX - AX),
           ( PreDiffX > 0 ->
              DiffX is -PreDiffX
           ;
              DiffX = PreDiffX
           ),
           PreDiffY is (SY - AY),
           ( PreDiffY > 0 ->
              DiffY is -PreDiffY
           ;
              DiffY = PreDiffY
           ),
           Val is (DiffX + DiffY)
        ),
        printf(stdout, "Value of multivariate fluent mvar_rew_home: %w\n", [Val]).


%%  READYBOT MULTIVARIATE ATTRIBUTES  %%
 
get_mvar_value(MVar, S, Val) :-
        MVar = mvar_prog_saw_opponent,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        ( subf(f_SawOpponent, ValTmp, S) ->
           Val = ValTmp
        ; 
           Val = false
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_prog_item_type_available_doubledamage,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        ( subf(f_ItemTypeAvailable(doubledamage), ValTmp, S) ->
           Val = ValTmp
        ;
           Val = false
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_prog_item_type_available_supershield,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        ( subf(f_ItemTypeAvailable(supershield), ValTmp, S) ->
           Val = ValTmp
        ;
           Val = false
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_prog_item_type_available_shield,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        ( subf(f_ItemTypeAvailable(shield), ValTmp, S) ->
           Val = ValTmp
        ;
           Val = false
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_prog_item_type_available_health,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        % subf(f_ItemTypeAvailable(health), Val, S) fails in practice in
        % level Albatross. Maybe, because the item is never available in
        % this level!? In this case, we assign Val = false.
        ( subf(f_ItemTypeAvailable(health), ValTmp, S) ->
           Val = ValTmp
        ;
           Val = false
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_prog_item_type_available_weapon,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        % subf(f_ItemTypeAvailable(weapon), Val, S) fails in practice in
        % level Albatross. Maybe, because the item is never available in
        % this level!? In this case, we assign Val = false.
        ( subf(f_ItemTypeAvailable(weapon), ValTmp, S) ->
           Val = ValTmp
        ;
           Val = false
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_bot_has_good_weapon,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        ( subf(f_BotHasGoodWeapon, ValTmp, S) ->
           Val = ValTmp
        ;
           Val = false
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_get_my_score,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        ( subf(f_GetMyScore, ValTmp, S) ->
           Val = ValTmp
        ;
           Val = 0
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_get_player_max_score,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        ( subf(f_GetPlayerMaxScore, ValTmp, S) ->
           Val = ValTmp
        ;
           Val = 0
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_has_double_damage,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        ( (getInventorySlot( doubledamage, DoubleDamageSlot ),
           exog_fluent_getValue(epf_BotInventoryAvailable( DoubleDamageSlot ), ValTmp, S) ) ->
           Val = ValTmp
        ;
           Val = false
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_weapon1,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        ( subf(f_BotHasGoodWeapon, HasGoodWeaponTmp, S) ->
           HasGoodWeapon = HasGoodWeaponTmp
        ;
           HasGoodWeapon = false
        ),
        ( (HasGoodWeapon = true) ->
           Val = 50
        ;
           Val = -50
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_health1,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        exog_fluent_getValue(epf_BotHealth, Health, S),
        ( (Health < 150) ->
           Val = -1
        ;
           Val = 0
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_health2,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        exog_fluent_getValue(epf_BotHealth, Health, S),
        ( (Health < 100) ->
           Val = -5
        ;
           Val = 0
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_health3,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        exog_fluent_getValue(epf_BotHealth, Health, S),
        ( (Health < 50) ->
           Val = -30
        ;
           Val = 0
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_armor1,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        exog_fluent_getValue(epf_BotArmor, Armor, S),
        ( (Armor > 20) ->
           Val = 10
        ;
           Val = 0
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_armor2,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        exog_fluent_getValue(epf_BotArmor, Armor, S),
        ( (Armor > 45) ->
           Val = 10
        ;
           Val = 0
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_armor3,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        exog_fluent_getValue(epf_BotArmor, Armor, S),
        ( (Armor > 95) ->
           Val = 20
        ;
           Val = 0
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_armor4,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        exog_fluent_getValue(epf_BotArmor, Armor, S),
        ( (Armor > 135) ->
           Val = 20
        ;
           Val = 0
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_double,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        ( ( getInventorySlot( doubledamage, DoubleDamageSlot ),
            exog_fluent_getValue(epf_BotInventoryAvailable( DoubleDamageSlot ), HasDoubleDamageTmp, S) ) ->
           HasDoubleDamage = HasDoubleDamageTmp
        ;
           HasDoubleDamage = 0
        ),
        ( (HasDoubleDamage = 0) ->
           Val = 0
        ;
           Val = 30
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_score1,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        ( ( subf(f_GetMyScore, MyScoreTmp, S),
            subf(f_GetPlayerMaxScore, MaxScoreTmp, S) ) ->
          MaxScore = MaxScoreTmp,
          MyScore = MyScoreTmp
        ;
          MaxScore = 0,
          MyScore = 0
        ),
        Diff is (MaxScore - MyScore),
        Val is (Diff * -200).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_score2,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        ( ( subf(f_GetMyScore, MyScoreTmp, S),
            subf(f_GetPlayerMaxScore, MaxScoreTmp, S) ) ->
          MaxScore = MaxScoreTmp,
          MyScore = MyScoreTmp
        ;
          MaxScore = 0,
          MyScore = 0
        ),
        ( (MaxScore = MyScore ; (pf_Action = attack, pf_Action = charge, pf_Action = moveattack)) ->
           Val = 500
        ;
           Val = 0
        ).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_dm,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent %w...\n", [MVar]),
        get_mvar_value(mvar_rew_weapon1, S, RewardWeapon1),
        get_mvar_value(mvar_rew_health1, S, RewardHealth1),
        get_mvar_value(mvar_rew_health2, S, RewardHealth2),
        get_mvar_value(mvar_rew_health3, S, RewardHealth3),
        get_mvar_value(mvar_rew_score1, S, RewardScore1),
        get_mvar_value(mvar_rew_score2, S, RewardScore2),
        get_mvar_value(mvar_rew_armor1, S, RewardArmor1),
        get_mvar_value(mvar_rew_armor2, S, RewardArmor2),
        get_mvar_value(mvar_rew_armor3, S, RewardArmor3),
        get_mvar_value(mvar_rew_armor4, S, RewardArmor4),
        get_mvar_value(mvar_rew_double, S, RewardDouble),
        Val is (RewardWeapon1 +
                RewardHealth1 +
                RewardHealth2 +
                RewardHealth3 +
                RewardScore1 +
                RewardScore2 +
                RewardArmor1 +
                RewardArmor2 +
                RewardArmor3 +
                RewardArmor4 +
                RewardDouble).
%        printf(stdout, "Value of multivariate fluent %w: %w\n", [MVar, Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_exploration,
        !,
        %  Compute the correlated value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent mvar_rew_exploration...\n", []),
        exog_fluent_getValue( epf_distance_to_closest_safe_cell, Val1, S),
        exog_fluent_getValue( epf_num_visited, Val2, S),
        exog_fluent_getValue( epf_num_facedShades, Val3, S),
        Val1Tmp is (Val1 * -10),
        Val2Tmp is (Val2 * 20),
        Val is (Val1Tmp + Val2Tmp + Val3).
%        printf(stdout, "Value of multivariate fluent mvar_rew_exploration: %w\n", [Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_sum,
        !,
        %  Compute the correlated final value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent mvar_rew_sum...\n", []),
        exog_fluent_getValue( epf_wumpus_alive, WA, S ),
        exog_fluent_getValue( epf_sense_gold, SG, S ),
        exog_fluent_getValue( epf_distance_to_closest_safe_cell, DTCSC, S),
        exog_fluent_getValue( epf_num_visited, NV, S),
        exog_fluent_getValue( epf_num_facedShades, NFS, S),
        exog_fluent_getValue( epf_turning_back_again, TBA, S),
        exog_fluent_getValue( epf_remaining_time, RT, S),
        exog_fluent_getValue( epf_returned_safely, RS, S),
        ( WA = true ->
            RewardKillWumpus = 0
        ;
            RewardKillWumpus = 50
        ),
        ( SG = true ->
            RewardFoundGold = 100
        ;
            RewardFoundGold = 0
        ),
        RewardExplorationTmp1 is (DTCSC * -10),
        RewardExplorationTmp2 is (NV * 20),
        RewardExploration is (RewardExplorationTmp1 + RewardExplorationTmp2 + NFS),
        ( TBA = true ->
            RewardDoNotTurnInCirclesStupidly = -100
        ;
            RewardDoNotTurnInCirclesStupidly = 0
        ),
        ( RT > 0 ->
           ( RS = true ->
               RewardTime = 0
           ;
               RewardTime = 0
           )
        ;
           ( RS = true ->
               RewardTime = 0
           ;
               RewardTime = -500
           )
        ),
        Val is ( RewardKillWumpus +
                 RewardFoundGold +
                 RewardExploration +
                 RewardDoNotTurnInCirclesStupidly +
                 RewardTime ).
%        printf(stdout, "Value of multivariate fluent mvar_rew_sum: %w\n", [Val]).

get_mvar_value(MVar, S, Val) :-
        MVar = mvar_rew_home,
        !, %  We insert a cut here to make the predicate fail, if it is called
           %  with a univariate fluent.
        %  Compute the correlated final value from the reward function.
%        printf(stdout, "Computing value of multivariate fluent mvar_rew_home...\n", []),
        exog_fluent_getValue( epf_start_pos_X, SX, S),
        exog_fluent_getValue( epf_start_pos_Y, SY, S),
        exog_fluent_getValue( epf_agent_pos_X, AX, S),
        exog_fluent_getValue( epf_agent_pos_Y, AY, S),
        ( SX = AX ->
           Val = 100
        ;
           PreDiffX is (SX - AX),
           ( PreDiffX > 0 ->
              DiffX is -PreDiffX
           ;
              DiffX = PreDiffX
           ),
           PreDiffY is (SY - AY),
           ( PreDiffY > 0 ->
              DiffY is -PreDiffY
           ;
              DiffY = PreDiffY
           ),
           Val is (DiffX + DiffY)
        ).
%        printf(stdout, "Value of multivariate fluent mvar_rew_home: %w\n", [Val]).


%% DEPRECATED %%
/*        
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
%%%         term_string(Policy, PolicyString),
         %  Recover the policy from the hash table.
         getval(policy_hash_table, PolicyHashTable),
         term_string(PolicyHashKey, PolicyString),
         hash_get(PolicyHashTable, PolicyHashKey, Policy),
         term_string(Value, ValueString),
         term_string(TermProb, TermProbString),
         term_string(Tree, TreeString).

%% /DEPRECATED %%
*/

% }}}
