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
 *           $Id: iplpreprocessor.pl 177 2008-11-20 11:55:04Z dp $
 *         @date: 20.11.08
 *       @author: Dennis Pannhausen <Dennis.Pannhausen@rwth-aachen.de>
 *   description: preprocessor for (i)nductive (p)olicy (l)earning
 *
 * **************************************************************************/

% ================================================================== %
%  PREPROCESSOR                                                      %
% ================================================================== %

%  The IPL preprocessor works as a prepreprocessor on the READYLOG
%  code. Its task is to transform all solve statements in the code
%  to a standard form, where they only consist of a single nondet.
%  The intention behind this is to maximise the number of matching
%  solve statements, which then can be learned by the decision tree
%  learning algorithm, and eventually be replaced by decision trees.


:- write("** loading iplpreprocessor.pl\n"). 

% --------------------------------------------------------- %
%  Header + Flags                                           %
% --------------------------------------------------------- %
% {{{ header + flags

:- ensure_loaded('readylog.pl').
:- ensure_loaded("../../utils/utils.pl").
:- ensure_loaded('iplutils.pl').

%  Needed for converting var names to unique strings.
%  We will use this to bind domain variables for pickBest.
:- lib(var_name).
local_var(L) :- set_var_name(L, "IPLPreVar").

% }}}

% ---------------------------------------------------------- %
%  Procedures                                                %
% ---------------------------------------------------------- %
% {{{ procedures
% >>>>

%  Recurses through the code of a Readylog procedure, and
%  returns the transformed proc Processed.
%  Processed only contains solves in standard form.
:- mode process_proc(++, -).
process_proc( [], Processed ) :- !,
        Processed = [].

process_proc( Proc, Processed ) :-
        not(is_list(Proc)),
        !,
        NewProc = [Proc],
        process_proc( NewProc, Processed ).
        
process_proc( [if(Cond, Sigma1, Sigma2) | RestProgram], Processed ) :-
        %  Test, if Sigma1 is a nondeterministic program. If so, we must
        %  first transform this subprogram.
        term_string( Sigma1, Sigma1S ),
        ( substring( Sigma1S, "solve", _Pos ) ->
           %  Sigma1 contains a solve context.
           ContainsSolveSigma1 = true
        ;
           ContainsSolveSigma1 = false
        ),
        %  Test, if Sigma2 is a nondeterministic program. If so, we must
        %  first transform this subprogram.
        term_string( Sigma2, Sigma2S ),
        ( substring( Sigma2S, "solve", _Pos ) ->
           %  Sigma2 contains a solve context.
           ContainsSolveSigma2 = true
        ;
           ContainsSolveSigma2 = false
        ),
        process_proc_if_aux( [if(Cond, Sigma1, Sigma2) | RestProgram], 
                             ContainsSolveSigma1, ContainsSolveSigma2,
                             Processed ).

%  Shortcut, if there is no else-branch.
process_proc( [if(Cond, Sigma) | RestProgram], Processed ) :-
        process_proc( [if(Cond, Sigma, []) | RestProgram], Processed ).

process_proc( [while(Cond, Sigma) | RestProgram], Processed ) :- !,
        %  Test, if Sigma is a nondeterministic program. If so, we must
        %  first transform this subprogram.
        term_string( Sigma, SigmaS ),
        ( substring( SigmaS, "solve", _Pos ) ->
           %  While contains a solve context.
           flush(stdout),
           process_proc( Sigma, ProcessedSigma ),
           process_proc( RestProgram, ProcessedRest ),
           Processed = [ while(Cond, ProcessedSigma) | ProcessedRest ]
        ;
           %  While contains no solve context.
           process_proc( RestProgram, ProcessedRest ),
           Processed = [ while(Cond, Sigma) | ProcessedRest ]
        ).

process_proc( [solve(Prog, Horizon, RewardFunction) | RestProgram],
              Processed ) :- !,
%        printf(stdout, "\n*** Encountered solve ***\n", []),
%        printf(stdout, "\n*** with Horizon %w. ***\n", [Horizon]),
%        printf(stdout, "\n*** and Prog %w. ***\n", [Prog]),
        process_solve( Prog, Horizon, RewardFunction, ProcessedSolve ),
%        printf(stdout, "\n*** Rest_Program: %w. ***\n", [RestProgram]),
        process_proc( RestProgram, ProcessedRest ),
%        printf(stdout, "\n*** ProcessedRest: %w. ***\n", [ProcessedRest]),
        append( ProcessedSolve, ProcessedRest, Processed ).

process_proc( [Term | RestProgram], Processed ) :- !,
        process_proc( RestProgram, ProcessedRest ),
        Processed = [ Term | ProcessedRest ].

process_proc( Program, Processed ) :-
        Program \= [_Action],
        %  If the proc only consists of a single action without [], we
        %  add those brackets.
        Processed = [Program].

%  Helper predicate for the transformation of an if-expression.
%  This predicate helps to keep apart the different cases of
%  deterministic/nondeterministic subprograms.
:- mode process_proc_if_aux(++, ++, ++, -).
process_proc_if_aux( [if(Cond, Sigma1, Sigma2) | RestProgram], false, false,
                     Processed ) :- !,
        % The subprograms do not contain a solve-context. We can safely
        % skip the whole if statement.
        process_proc( RestProgram, ProcessedRest ),
        Processed = [if(Cond, Sigma1, Sigma2) | ProcessedRest].

%  The following three clauses take care of the cases, where
%  at least one subprogram of the if-expression contains a solve-context.
%  We first have to process the subprograms that contain a
%  solve context.
process_proc_if_aux( [if(Cond, Sigma1, Sigma2) | RestProgram], true, false,
                     Processed ) :- !,
        %  Only Sigma1 contains a solve.
        process_proc( Sigma1, ProcessedSigma1 ),
        process_proc( RestProgram, ProcessedRest ),
        Processed = [ if(Cond, ProcessedSigma1, Sigma2) |
                      ProcessedRest ].

process_proc_if_aux( [if(Cond, Sigma1, Sigma2) | RestProgram], false, true,
                     Processed ) :- !,
        %  Only Sigma2 contains a solve.
        process_proc( Sigma2, ProcessedSigma2 ),
        process_proc( RestProgram, ProcessedRest ),
        Processed = [ if(Cond, Sigma1, ProcessedSigma2) |
                      ProcessedRest ].

process_proc_if_aux( [if(Cond, Sigma1, Sigma2) | RestProgram], true, true,
                     Processed ) :- !,
        %  Both subprograms contain a solve.
        process_proc( Sigma1, ProcessedSigma1 ),
        process_proc( Sigma2, ProcessedSigma2 ),
        process_proc( RestProgram, ProcessedRest ),
        Processed = [ if(Cond, ProcessedSigma1, ProcessedSigma2) |
                      ProcessedRest ].


%  Processes all Readylog procedures proc that are found
%  in the compiled knowledge base, and writes the
%  transformed ipl_procs into the file Stream.
:- mode process_all_proc(++).
process_all_proc( Stream ) :-
%        set_flag(syntax_option, not(doubled_quote_is_quote)),
%        set_flag(syntax_option, not(nl_in_quotes)),
        findall( (ProcName, ProcBody),
                 proc(ProcName, ProcBody),
                 ProcList), !,
%        set_flag(syntax_option, not(doubled_quote_is_quote)),
%        set_flag(syntax_option, not(nl_in_quotes)),
        process_all_proc_aux(ProcList, Stream).

%  Recurse through the list of all procs, and
%  process each single one with process_proc_aux/3.
:- mode process_all_proc_aux(++, ++).
process_all_proc_aux([], _Stream) :- !.

process_all_proc_aux([(ProcName, ProcBody)|List_rest], Stream) :-
        process_proc_aux( ProcName, ProcBody, Stream ), !,
        process_all_proc_aux( List_rest, Stream ).

%  Helper predicate for process_all_proc_aux/2.
%  Writes the proc ProcName with its transformed Body to
%  the file ipl_preprocessed_agent.pl (Stream).
%  Those ipl_procs will be used by the Readylog interpreter
%  if iplearning is active.
:- mode process_proc_aux(++, ++, ++).
process_proc_aux( ProcName, ProcBody, Stream ) :-        
        set_variables( ProcName ),
%%%  Probably due to a mistake in set_variables/1,
%%%  there had appeared a problem that parameterised
%%%  procs were considered twice - once with the
%%%  Prolog variable name, and once with the local var
%%%  name. However, this problem doesn't appear now any
%%%  more. If it should return, uncomment the lines marked
%%%  with "%%-%%" and remove the full stop.
%%-%%        %  Do not consider parameterised procs more than
%%-%%        %  once. Only process those with local vars in
%%-%%        %  the argument list.
%%-%%        contains_only_local_vars(ProcName, Result),
%%-%%        ( Result = true ->
%           printf(Stream, "ipl_proc( %w ).\n", [ProcName]),
           printf(Stream, "ipl_proc( %w, ", [ProcName]),
           %  Make the arguments of cout strings again.
           fix_couts( ProcBody, ProcBodyNew ),
           set_variables( ProcBodyNew ),
           process_proc( ProcBodyNew, ProcessedTmp ),
           %  If the procedure body only contains a
           %  single action, we remove the list brackets.
           ( (is_list(ProcessedTmp), length(ProcessedTmp, 1)) ->
                ProcessedTmp = [Processed]
           ;
                Processed = ProcessedTmp
           ),
           %  Write the transformed proc to the file.
           printf( Stream, "%w ).\n", [Processed] ).
%%-%%        ;
%%-%%           printf( stdout, "Skipping Proc %w\n", [ProcName]), flush(stdout)
%%-%%        ).

%  Give unique (and referable) string names to variables.
%  We search for all variables in some depth-first fashion:
%  We look at the first term, its first argument, the next
%  argument, ..., its last argument, and then at the next
%  term... until we find a var.
:- mode set_variables(++, -).
set_variables( [] ).

set_variables( List ) :-
        is_list(List),
        %  Avoid problems with List=[IPLPreVar0, IPLPreVar1].
        List \= [_Action1 | []],
        List \= [[] | _Action2],
        !,
        List = [Term | Rest],
        set_variables(Term),
        set_variables(Rest).

set_variables( Term ) :-
        var( Term ),
        !,
        local_var(Term).

set_variables( Term ) :-
        Term =.. List,
        length(List, 1), %  Term is a zero-arity functor.
        var(List),
        !,
        ( 
           local_var(List)
        ;
           %  This is the case, when X already is set as local_var.
           true
        ).

set_variables( Term ) :-
        Term =.. List,
        length(List, 1), %  Term is a zero-arity functor.
        !.

set_variables( Term ) :-
        Term =.. [Functor | _Args],
        var(Functor),
        !,
        ( 
           local_var(Functor)
        ;
           %  This is the case, when X already is set as local_var.
           true
        ). %  Do not consider _Args, as vars cannot be functors.

set_variables( Term ) :-
        Term =.. [_Functor | Args],
        ( foreach(X, Args)
          do
             ( var(X) ->
                ( 
                  local_var(X)
                ;
                  %  This is the case, when X already is set as local_var.
                  true
                )
             ;
                set_variables(X)
             )
        ).

%  Tests, if the proc name or proc, contains only local variables
%  and no more Prolog vars.
:- mode contains_only_local_vars(++, -).     
contains_only_local_vars( Term, Result ) :-
        %  Without the next line, we somehow get the error message
        %  "instantiation fault in IPLPreVar#0 =.. List".
        true,
%        printf(stdout, "Term %w\n", [Term]), flush(stdout),
        var(Term),
        !,
%        printf(stdout, "Term %w is var!\n", [Term]), flush(stdout),
        ( get_var_name(Term, _VarName) ->
           Result = true
        ;
%           printf(stdout, "Term %w is *NO* local var!\n", [Term]), flush(stdout),
           Result = false
        ).

contains_only_local_vars( Term, Result ) :-
        Term =.. List,
        length(List, 1), %  Term is a zero-arity functor.
        Result = true.

contains_only_local_vars( Term, Result ) :-
        Term =.. [_Functor | Args],
        ( foreach(X, Args),
          param(Result)
          do
             contains_only_local_vars(X, ResultTmp),
             ( ResultTmp = false ->
                Result = false
             ;
                true
             )
        ),
        ( var(Result) ->
           Result = true
        ;
           true
        ).



% --------------------------------------------------------- %
%  solve statements                                         %
% --------------------------------------------------------- %

%  Preprocesses a general solve statement of the form
%  solve([alpha; nondet(p_1,...,p_n); omega], H, rew),
%  where alpha is the deterministic part before the first
%  nondet (if any) in the code,
%  the p_i are deterministic programs,
%  and omega is an arbitrary Readylog program (not containing
%  another solve).
%  First the operator rho is applied to transform the
%  argument list of the nondet to a set,
%  then the operator tau_prime_H is applied until the omega
%  is integrated into the nondet, leading to
%  solve([alpha; nondet(q_1,...,q_m)], H, rew).
%  Finally the operator tau is applied until the alpha is
%  pulled out of the solve context, leading to
%  alpha; solve(nondet(q_1,...,q_m), H, rew).
:- mode process_solve(++, ++, ++, -).
process_solve( [], _Horizon, _RewardFunction, Processed ) :- !,
        Processed = [].

%  If the solve program is enclosed my more than one pair of brackets [],
%  we flatten it.
process_solve( Program, Horizon, RewardFunction, Processed ) :-
        Program = [[NewProgram]], !,
        process_solve( [NewProgram], Horizon, RewardFunction, Processed ).

%  If the solve program only consists of a single action without [], we
%  add those brackets.
process_solve( Program, Horizon, RewardFunction, Processed ) :-
%        Program \= [_Action],
%        is_atom(Action),
        not(is_list(Program)),
        !,
        NewProgram = [Program],
        process_solve( NewProgram, Horizon, RewardFunction, Processed ).

%  If the solve program contains a call to a proc, we have to follow that trail.
%  Otherwise we might lose a solve context. Consider this example:
%  proc( procOne, [solve(procTwo, H, R)] ).
%  proc( procTwo, [nondet[a,b]] ).
%  The procTwo would be treated as a primitive action, pulled outside the solve
%  and the solve would be deleted. */
process_solve( Program, Horizon, RewardFunction, Processed ) :- 
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%  solve([alpha; nondet(p_1,...,p_n); omega], H, rew)  %%%%%%%%%%%

        %%%  Rho  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        printf(stdout, "\n", []),
%        printf(stdout, "--- process_solve_begin -------------------\n", []),
%        printf(stdout, "apply_rho([solve(%w, Horizon, RewardFunction)], ",
%               [Program]),
%        printf(stdout, "[], Rho_Program)\n", []),
        apply_rho( [solve(Program, Horizon, RewardFunction)],
                   [], Rho_Program ),
%        printf(stdout, "Rho_Program: %w\n", [Rho_Program]),
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%  => solve([alpha; {p_1,...,p_n}; omega], H, rew)  %%%%%%%%%%%%%%

        %%%  Tau_Prime_H  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        printf(stdout, "apply_tau_prime_H(%w, [], Tau_Prime_H_Program))\n",
%                       [Rho_Program]),
	apply_tau_prime_H( Rho_Program, [], Tau_Prime_H_Program ),
%        printf(stdout, "Tau_Prime_H_Program: %w\n", [Tau_Prime_H_Program]),
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%  => solve([alpha; nondet(q_1,...,q_m)], H, rew)  %%%%%%%%%%%%%%%

        %%%  Tau  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        printf(stdout, "\n apply_tau(solve(%w, %w, %w), Tau_Program)\n",
%                       [Tau_Prime_H_Program, Horizon, RewardFunction]),
        apply_tau( solve(Tau_Prime_H_Program, Horizon, RewardFunction),
                         Tau_Program ),
%        printf(stdout, "\n Tau_Program: %w\n", [Tau_Program]),
%        printf(stdout, "--- process_solve_end ---------------------\n", []),
%        printf(stdout, "\n", []),
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%  => alpha; solve(nondet(q_1,...,q_m), H, rew)  %%%%%%%%%%%%%%%%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        Processed = Tau_Program.                                


% ---------------------------------------------------------- %
%  Solve Transformation Operators                            %
% ---------------------------------------------------------- %

% ---------------------------------------------------------- %
%                            Rho                             %
% ---------------------------------------------------------- %

%  Rho finds the first nondeteministic statement
%  (nondet, pickBest, or star) in the code. In case of
%  pickBest or star, the operator transforms it into
%  a nondet. Afterwards, the argument list of the nondet
%  is transformed into a set.

:- mode apply_rho(++, ++, -).
%%%  Empty procedure  %%%
apply_rho( [], Program, Rho_Program ) :- !,
        Rho_Program = Program.

%%%  Prog is enclosed my more than one pair of brackets []. Flatten it.  %%%
apply_rho( [solve(Prog, Horizon, RewardFunction)], Program,
           Rho_Program ) :- 
        Prog = [[NewProg]],
        !,
        apply_rho( [solve(NewProg, Horizon, RewardFunction)], Program,
                   Rho_Program ).

%%%  Prog is a single action without brackets. Add program brackets.  %%%
apply_rho( [solve(Prog, Horizon, RewardFunction)], Program,
           Rho_Program ) :- 
%        Prog \= [_Action],
        not(is_list(Prog)),
        !,
        apply_rho( [solve([Prog], Horizon, RewardFunction)], Program,
                   Rho_Program ).

apply_rho( [solve(Prog, Horizon, RewardFunction)], Program,
           Rho_Program ) :- !,
        apply_rho(Prog, Program, Rho_Program_Tmp),
        Rho_Program = [solve(Rho_Program_Tmp, Horizon, RewardFunction)].

%  In the following, for each possible expression in the Prog,
%  we use four clauses for differentiating between the following four
%  cases:
%  1) Omega  = [] and Prog  = []
%  2) Omega  = [] and Prog \= []
%  3) Omega \= [] and Prog  = []
%  4) Omega \= [] and Prog \= [].
%
%  This is necessary, because we use the | operator
%  instead of append/3 (which has a time complexity of O(n^2)).
%  When the first argument is the empty list, the two behave
%  differently:
%  append([], List, Result) -> Result=List
%  Result=[[] | List]       -> Result=[[] | List]
%  We want to achieve the first result, but with the | operator.
%
%  We could also use ( if -> then; else ) constructs inside one
%  clause, but having different clauses is more efficient.

%%%  Prog = ...  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%  Rho has been applied to program before. Do not change anything.  %%%
apply_rho( [{P_List}], [], Rho_Program ) :- !,
        %  Rho had already been applied to this program before. Thus,
        %  do not change anything.
        Rho_Program = [{P_List}].

apply_rho( [{P_List}], Program, Rho_Program ) :- !,
        %  Rho had already been applied to this program before. Thus,
        %  do not change anything.
        Rho_Program = [Program | [{P_List}]].

apply_rho( [{P_List} | Omega], [], Rho_Program ) :- !,
        %  Rho had already been applied to this program before. Thus,
        %  do not change anything.
        Rho_Program = [{P_List} | Omega].

apply_rho( [{P_List} | Omega], Program, Rho_Program ) :- !,
        %  Rho had already been applied to this program before. Thus,
        %  do not change anything.
        Rho_Program = [Program | [{P_List} | Omega]].

%%%  PickBest  %%%
%  As we don't know the Domain of the pickBest at preprocessing time,
%  we introduce a hard-coded threshold for the domain size and
%  introduce this many unbound variables. In front of the solve
%  we add code to bind those variables to the domain at runtime.
%  This pre-solve code is kept in mind during application of 
%  tau_prime_H, and later, in tau_prime, pulled out of the solve.
%  In the beginning of each nondeterministic choice we test if the
%  variable is bound, which won't be the case if the previous
%  variables already covered the Domain.
apply_rho( [pickBest(F, Domain, Delta)], [], Rho_Program ) :- !,
        %  There are two possible cases:
        %  1) Domain is var. That means, Domain is a variable specified
        %     before somewhere in the current proc, or in some parameter
        %     of the current proc, or it has been defined as a global
        %     variable in some part of the agent code that we didn't
        %     compile before the call of the iplpreprocessor (we don't
        %     handle that here).
        %  2) Domain is nonvar. Then Domain is directly instantiated
        %     in the pickBest call, or it has been defined as a global
        %     variable in the knowledge base.
        ( var(Domain) ->
%           printf(stdout, "\n pickBest! Domain %w is var\n", [Domain]),
%           flush(stdout),
           % Case 1) Try to find the Variable name in the proc argument
           %         list, or in the proc body.
           get_var_name(Domain, DomainS),
%           printf(stdout, "\n DomainS: %w\n", [DomainS]), flush(stdout),
           create_pickBest_code(F, DomainS, Delta, PreSolveProg, ChoiceList)
        ;
%           printf(stdout, "\n pickBest! Domain %w is *non*var\n", [Domain]),
%           flush(stdout),
           % Case 2) Easy case. Just work with the instantiated variable.
           term_string(Domain, DomainS),
           create_pickBest_code(F, DomainS, Delta, PreSolveProg, ChoiceList)
        ),
        %  End recursion to make sure that only the first nondeterministic
        %  statement is replaced.
        Rho_Program = [pickBestBindDomainVariables(PreSolveProg),
                       {ChoiceList}].

apply_rho( [pickBest(F, Domain, Delta)], Program, Rho_Program ) :- !,
        ( var(Domain) ->
           get_var_name(Domain, DomainS),
           create_pickBest_code(F, DomainS, Delta, PreSolveProg, ChoiceList)
        ;
           term_string(Domain, DomainS),
           create_pickBest_code(F, DomainS, Delta, PreSolveProg, ChoiceList)
        ),
        %  End recursion to make sure that only the first nondeterministic
        %  statement is replaced.
        Rho_Program = [Program |
                  [pickBestBindDomainVariables(PreSolveProg), {ChoiceList}]].

apply_rho( [pickBest(F, Domain, Delta) | Omega], [], Rho_Program ) :- !,
        ( var(Domain) ->
           get_var_name(Domain, DomainS),
           create_pickBest_code(F, DomainS, Delta, PreSolveProg, ChoiceList)
        ;
           term_string(Domain, DomainS),
           create_pickBest_code(F, DomainS, Delta, PreSolveProg, ChoiceList)
        ),
        Program_New = [pickBestBindDomainVariables(PreSolveProg), {ChoiceList}],
        %  End recursion to make sure that only the first nondeterministic
        %  statement is replaced.
        Rho_Program = [Program_New | Omega].

apply_rho( [pickBest(F, Domain, Delta) | Omega], Program, Rho_Program ) :- !,
        ( var(Domain) ->
           get_var_name(Domain, DomainS),
           create_pickBest_code(F, DomainS, Delta, PreSolveProg, ChoiceList)
        ;
           term_string(Domain, DomainS),
           create_pickBest_code(F, DomainS, Delta, PreSolveProg, ChoiceList)
        ),
        Program_New = [Program |
                  [pickBestBindDomainVariables(PreSolveProg), {ChoiceList}]],
        %  End recursion to make sure that only the first nondeterministic
        %  statement is replaced.
        Rho_Program = [Program_New | Omega].

%%%  Star  %%%
apply_rho( [star(Alpha)], [], Rho_Program ) :- !,
        %  We delay the listing of the programs [], Alpha, Alpha^2,...
        %  for later (when we will know the horizon for termination),
        %  and now just put the star-statement inside the set brackets {}.
        list_to_string([star(Alpha)], Star_String),
        %  End recursion to make sure that only the first nondeterministic
        %  statement is replaced.
        Rho_Program = [{Star_String}].

apply_rho( [star(Alpha)], Program, Rho_Program ) :- !,
        %  We delay the listing of the programs [], Alpha, Alpha^2,...
        %  for later (when we will know the horizon for termination),
        %  and now just put the star-statement inside the set brackets {}.
        list_to_string([star(Alpha)], Star_String),
        %  End recursion to make sure that only the first nondeterministic
        %  statement is replaced.
        Rho_Program = [Program | {Star_String}].

apply_rho( [star(Alpha) | Omega], [], Rho_Program ) :- !,
        %  We delay the listing of the programs [], Alpha, Alpha^2,...
        %  for later (when we will know the horizon for termination),
        %  and now just put the star-statement inside the set brackets {}.
        list_to_string([star(Alpha)], Star_String),
        %  End recursion to make sure that only the first nondeterministic
        %  statement is replaced.
        %  As we end the application of rho here, and the next operator
        %  apply_tau_prime_H is first matched with the star string in
        %  the nondeterministic set, we do not need to remember Omega
        %  now.
        Rho_Program = [{Star_String} | Omega].

apply_rho( [star(Alpha) | Omega], Program, Rho_Program ) :- !,
        %  We delay the listing of the programs [], Alpha, Alpha^2,...
        %  for later (when we will know the horizon for termination),
        %  and now just put the star-statement inside the set brackets {}.
        list_to_string([star(Alpha)], Star_String),
        Program_New = [Program | {Star_String}],
        %  End recursion to make sure that only the first nondeterministic
        %  statement is replaced.
        %  As we end the application of rho here, and the next operator
        %  apply_tau_prime_H is first matched with the star string in
        %  the nondeterministic set, we do not need to remember Omega
        %  now.
        Rho_Program = [Program_New | Omega].

%%%  Conditional  %%%
%  If Program = [], this will be matched in apply_rho_if_aux/5.
apply_rho( [if(Cond, Sigma1, Sigma2)], Program, Rho_Program ) :- !,
        is_deterministic( Sigma1, Sigma1Det ),
%        printf( stdout, "Sigma1 is deterministic is %w.\n", [Sigma1Det] ),
        is_deterministic( Sigma2, Sigma2Det ),
%        printf( stdout, "Sigma2 is deterministic is %w.\n", [Sigma2Det] ),
        apply_rho_if_aux( [if(Cond, Sigma1, Sigma2)], Program,
                          Sigma1Det, Sigma2Det, Rho_Program ).

apply_rho( [if(Cond, Sigma1, Sigma2) | Omega], Program, Rho_Program ) :- !,
        is_deterministic( Sigma1, Sigma1Det ),
%        printf( stdout, "Sigma1 is deterministic is %w.\n", [Sigma1Det] ),
        is_deterministic( Sigma2, Sigma2Det ),
%        printf( stdout, "Sigma2 is deterministic is %w.\n", [Sigma2Det] ),
        apply_rho_if_aux( [if(Cond, Sigma1, Sigma2) | Omega], Program,
                          Sigma1Det, Sigma2Det, Rho_Program ).

%  Shortcut, if else-branch is not provided.
apply_rho( [if(Cond, Sigma)], Program, Rho_Program ) :- !,
        apply_rho( [if(Cond, Sigma, [])], Program, Rho_Program ).

apply_rho( [if(Cond, Sigma) | Omega], Program, Rho_Program ) :- !,
        apply_rho( [if(Cond, Sigma, []) | Omega], Program, Rho_Program ).

%%%  Loop  %%%
apply_rho( [while(Cond, Sigma)], [], Rho_Program ) :- !,
%        printf(stdout, "is_deterministic( %w, Result )\n", [Sigma]),
        flush(stdout),
        is_deterministic( Sigma, Result ),
%        printf( stdout, "Sigma is deterministic is %w.\n", [Result] ),
%        flush(stdout),
        ( Result = true ->
           %  Skip while. 
           Rho_Program = [while(Cond, Sigma)]
        ;
           apply_rho( Sigma, [], Rho_Program_Sigma ),
           Rho_Program = [while(Cond, Rho_Program_Sigma)]
        ).

apply_rho( [while(Cond, Sigma)], Program, Rho_Program ) :- !,
        is_deterministic( Sigma, Result ),
%        printf( stdout, "Sigma is deterministic is %w.\n", [Result] ),
%        flush(stdout),
        ( Result = true ->
           %  Skip while. 
           Rho_Program = [Program | while(Cond, Sigma)]
        ;
           apply_rho( Sigma, [], Rho_Program_Sigma ),
           Rho_Program = [Program | while(Cond, Rho_Program_Sigma)]
        ).

apply_rho( [while(Cond, Sigma) | Omega], [], Rho_Program ) :- !,
        is_deterministic( Sigma, Result ),
%        printf( stdout, "Sigma is deterministic is %w.\n", [Result] ),
%        flush(stdout),
        ( Result = true ->
           %  Skip while. 
           Program_New = [while(Cond, Sigma)],
           apply_rho( Omega, Program_New, Rho_Program_Tmp ),
           Rho_Program = Rho_Program_Tmp
        ;
           %  The first nondeterministic statement is found
           %  inside Sigma. But as we do not allow nested solve
           %  statements, the solve context inside Sigma will
           %  be independent of a possible solve in Omega. That
           %  is why me may apply rho recursively.
           apply_rho( Sigma, [], Rho_Program_Sigma ),
           Program_New = [while(Cond, Rho_Program_Sigma)],
           apply_rho( Omega, Program_New, Rho_Program_Tmp ),
           Rho_Program = Rho_Program_Tmp
        ).

apply_rho( [while(Cond, Sigma) | Omega], Program, Rho_Program ) :- !,
        is_deterministic( Sigma, Result ),
%        printf( stdout, "Sigma is deterministic is %w.\n", [Result] ),
%        flush(stdout),
        ( Result = true ->
           %  Skip while. 
           Program_New = [Program | while(Cond, Sigma)],
           apply_rho( Omega, Program_New, Rho_Program_Tmp ),
           Rho_Program = Rho_Program_Tmp
        ;
           %  The first nondeterministic statement is found
           %  inside Sigma. But as we do not allow nested solve
           %  statements, the solve context inside Sigma will
           %  be independent of a possible solve in Omega. That
           %  is why me may apply rho recursively.
           apply_rho( Sigma, [], Rho_Program_Sigma ),
           Program_New = [Program | while(Cond, Rho_Program_Sigma)],
           apply_rho( Omega, Program_New, Rho_Program_Tmp ),
           Rho_Program = Rho_Program_Tmp
        ).

%%%  Nondeterministic Choice  %%%
apply_rho( [nondet(ProgList)], [], Rho_Program ) :- !,
%        printf(stdout, "ProgList: %w.\n", [ProgList]),
        %  Replace all commas inside the programs by placeholders,
        %  so that we won't mix up consecutive actions and alternative
        %  programs later.
        ( foreach(Prog, ProgList),
          foreach(NewProgS, NewProgList)
          do
             term_string(Prog, ProgTmpS),
             replace_character( ProgTmpS, ",", "__COMMA__", NewProgSNoComma ),
             replace_character( NewProgSNoComma, "\"", "__QUOTE__",
                                NewProgSClean ),
             NewProgS = NewProgSClean
        ),
        list_to_string(NewProgList, ReducedStringTmp),
        %  Remove superfluous ".
        replace_character( ReducedStringTmp, "\"", "", ReducedString ),
        replace_character( ReducedString, "__QUOTE__", "\"",
                           ReducedStringClean ),
%        printf(stdout, "ReducedString: %w.\n", [ReducedString]),
        %  End recursion to make sure that only the first nondet
        %  is replaced.       
        Rho_Program = [{ReducedStringClean}].

apply_rho( [nondet(ProgList)], Program, Rho_Program ) :- !,
%        printf(stdout, "ProgList: %w.\n", [ProgList]),
        %  Replace all commas inside the programs by placeholders,
        %  so that we won't mix up consecutive actions and alternative
        %  programs later.
        ( foreach(Prog, ProgList),
          foreach(NewProgS, NewProgList)
          do
             term_string(Prog, ProgTmpS),
             replace_character( ProgTmpS, ",", "__COMMA__", NewProgSNoComma ),
             replace_character( NewProgSNoComma, "\"", "__QUOTE__",
                                NewProgSClean ),
             NewProgS = NewProgSClean
        ),
        list_to_string(NewProgList, ReducedStringTmp),
        %  Remove superfluous ".
        replace_character( ReducedStringTmp, "\"", "", ReducedString ),
        replace_character( ReducedString, "__QUOTE__", "\"",
                           ReducedStringClean ),
%        printf(stdout, "ReducedString: %w.\n", [ReducedString]),
        %  End recursion to make sure that only the first nondet
        %  is replaced.       
        Rho_Program = [Program | {ReducedStringClean}].

apply_rho( [nondet(ProgList) | Omega], [], Rho_Program ) :- !,
%        printf(stdout, "ProgList: %w.\n", [ProgList]),
        %  Replace all commas inside the programs by placeholders,
        %  so that we won't mix up consecutive actions and alternative
        %  programs later.
        ( foreach(Prog, ProgList),
          foreach(NewProgS, NewProgList)
          do
             term_string(Prog, ProgTmpS),
             replace_character( ProgTmpS, ",", "__COMMA__", NewProgSNoComma ),
             replace_character( NewProgSNoComma, "\"", "__QUOTE__",
                                NewProgSClean ),
             NewProgS = NewProgSClean
        ),
        list_to_string(NewProgList, ReducedStringTmp),
        %  Remove superfluous ".
        replace_character( ReducedStringTmp, "\"", "", ReducedString ),
        replace_character( ReducedString, "__QUOTE__", "\"",
                           ReducedStringClean ),
%        printf(stdout, "ReducedString: %w.\n", [ReducedString]),
        %  End recursion to make sure that only the first nondet
        %  is replaced.       
        Rho_Program = [{ReducedStringClean} | Omega].

apply_rho( [nondet(ProgList) | Omega], Program, Rho_Program ) :- !,
%        printf(stdout, "ProgList: %w.\n", [ProgList]),
        %  Replace all commas inside the programs by placeholders,
        %  so that we won't mix up consecutive actions and alternative
        %  programs later.
        ( foreach(Prog, ProgList),
          foreach(NewProgS, NewProgList)
          do
             term_string(Prog, ProgTmpS),
             replace_character( ProgTmpS, ",", "__COMMA__", NewProgSNoComma ),
             replace_character( NewProgSNoComma, "\"", "__QUOTE__",
                                NewProgSClean ),
             NewProgS = NewProgSClean
        ),
        list_to_string(NewProgList, ReducedStringTmp),
        %  Remove superfluous ".
        replace_character( ReducedStringTmp, "\"", "", ReducedString ),
        replace_character( ReducedString, "__QUOTE__", "\"",
                           ReducedStringClean ),
%        printf(stdout, "ReducedString: %w.\n", [ReducedString]),
        Program_New = [Program | {ReducedStringClean}],
        %  End recursion to make sure that only the first nondet
        %  is replaced.       
        Rho_Program = [Program_New | Omega].

%%%  Test Action, Primitive Action, Stochastic Action, or proc  %%%
apply_rho( [Term], [], Rho_Program ) :- !,
        Rho_Program = [Term].

apply_rho( [Term], Program, Rho_Program ) :- !,
        Rho_Program = [Program, Term].

apply_rho( [Term | Omega], [], Rho_Program ) :- !,
        apply_rho( Omega, Term, Rho_Program ).

apply_rho( [Term | Omega], Program, Rho_Program ) :- !,
        Program_New = [Program, Term],
        apply_rho( Omega, Program_New, Rho_Program ).

apply_rho( Term, Program, Rho_Program ) :-
%        Term \= [_Action],
        not(is_list(Term)),
        apply_rho( [Term], Program, Rho_Program ).


%  Helper predicate for the application of rho to an if-expression.
%  This predicate helps to keep apart the different cases of
%  deterministic/nondeterministic subprograms.
:- mode apply_rho_if_aux(++, ++, ++, ++, -).
apply_rho_if_aux( [if(Cond, Sigma1, Sigma2)], [],
                   true, true, Rho_Program ) :- !,
        % The subprograms are deterministic. We can safely skip
        % the whole if-statement.
        Rho_Program = [if(Cond, Sigma1, Sigma2)].

apply_rho_if_aux( [if(Cond, Sigma1, Sigma2)], Program,
                   true, true, Rho_Program ) :- !,
        % The subprograms are deterministic. We can safely skip
        % the whole if-statement.
        Rho_Program = [Program | if(Cond, Sigma1, Sigma2)].

apply_rho_if_aux( [if(Cond, Sigma1, Sigma2) | Omega], [],
                   true, true, Rho_Program ) :- !,
        % The subprograms are deterministic. We can safely skip
        % the whole if-statement.
        Program_New = [if(Cond, Sigma1, Sigma2)],
        apply_rho( Omega, Program_New, Rho_Program_Tmp ),
        Rho_Program = Rho_Program_Tmp.

apply_rho_if_aux( [if(Cond, Sigma1, Sigma2) | Omega], Program,
                   true, true, Rho_Program ) :- !,
        % The subprograms are deterministic. We can safely skip
        % the whole if-statement.
        Program_New = [Program | if(Cond, Sigma1, Sigma2)],
        apply_rho( Omega, Program_New, Rho_Program_Tmp ),
        Rho_Program = Rho_Program_Tmp.

%  The following twelve clauses take care of the three cases (four
%  clauses per case to match empty Omega/Program), where
%  at least one subprogram of the if-expression is nondeterministic.
%  So, the first nondeterministic statement is found inside Sigma1 or Sigma2.
%  But as we do not allow nested solve statements, the solve context inside
%  Sigma1 and Sigma2 will be independent of a possible solve in Omega.
%  That is why me may apply Rho recursively.

%%  Sigma1 is nondeterministic.  %%
apply_rho_if_aux( [if(Cond, Sigma1, Sigma2)], [],
                   true, false, Rho_Program ) :- !,
        apply_rho( Sigma2, [], Rho_Program_Sigma2 ),
        Rho_Program = [if(Cond, Sigma1, Rho_Program_Sigma2)].

apply_rho_if_aux( [if(Cond, Sigma1, Sigma2)], Program,
                   true, false, Rho_Program ) :- !,
        apply_rho( Sigma2, [], Rho_Program_Sigma2 ),
        Rho_Program = [Program | if(Cond, Sigma1, Rho_Program_Sigma2)].

apply_rho_if_aux( [if(Cond, Sigma1, Sigma2) | Omega], [],
                   true, false, Rho_Program ) :- !,
        apply_rho( Sigma2, [], Rho_Program_Sigma2 ),
        Program_New = [if(Cond, Sigma1, Rho_Program_Sigma2)],
        apply_rho( Omega, Program_New, Rho_Program_Tmp ),
        Rho_Program = Rho_Program_Tmp.

apply_rho_if_aux( [if(Cond, Sigma1, Sigma2) | Omega], Program,
                   true, false, Rho_Program ) :- !,
        apply_rho( Sigma2, [], Rho_Program_Sigma2 ),
        Program_New = [Program | if(Cond, Sigma1, Rho_Program_Sigma2)],
        apply_rho( Omega, Program_New, Rho_Program_Tmp ),
        Rho_Program = Rho_Program_Tmp.

%%  Sigma2 is nondeterministic.  %%
apply_rho_if_aux( [if(Cond, Sigma1, Sigma2)], [],
                   false, true, Rho_Program ) :- !,
        apply_rho( Sigma1, [], Rho_Program_Sigma1 ),
        Rho_Program = [if(Cond, Rho_Program_Sigma1, Sigma2)].

apply_rho_if_aux( [if(Cond, Sigma1, Sigma2)], Program,
                   false, true, Rho_Program ) :- !,
        apply_rho( Sigma1, [], Rho_Program_Sigma1 ),
        Rho_Program = [Program | if(Cond, Rho_Program_Sigma1, Sigma2)].

apply_rho_if_aux( [if(Cond, Sigma1, Sigma2) | Omega], [],
                   false, true, Rho_Program ) :- !,
        apply_rho( Sigma1, [], Rho_Program_Sigma1 ),
        Program_New = [if(Cond, Rho_Program_Sigma1, Sigma2)],
        apply_rho( Omega, Program_New, Rho_Program_Tmp ),
        Rho_Program = Rho_Program_Tmp.

apply_rho_if_aux( [if(Cond, Sigma1, Sigma2) | Omega], Program,
                   false, true, Rho_Program ) :- !,
        apply_rho( Sigma1, [], Rho_Program_Sigma1 ),
        Program_New = [Program | if(Cond, Rho_Program_Sigma1, Sigma2)],
        apply_rho( Omega, Program_New, Rho_Program_Tmp ),
        Rho_Program = Rho_Program_Tmp.

%%  Both Sigma1 and Sigma2 are nondeterministic.  %%
apply_rho_if_aux( [if(Cond, Sigma1, Sigma2)], [],
                   false, false, Rho_Program ) :- !,
        apply_rho( Sigma1, [], Rho_Program_Sigma1 ),
        apply_rho( Sigma2, [], Rho_Program_Sigma2 ),
        Rho_Program = [if(Cond, Rho_Program_Sigma1, Rho_Program_Sigma2)].

apply_rho_if_aux( [if(Cond, Sigma1, Sigma2)], Program,
                   false, false, Rho_Program ) :- !,
        apply_rho( Sigma1, [], Rho_Program_Sigma1 ),
        apply_rho( Sigma2, [], Rho_Program_Sigma2 ),
        Rho_Program = [Program |
                       if(Cond, Rho_Program_Sigma1, Rho_Program_Sigma2)].

apply_rho_if_aux( [if(Cond, Sigma1, Sigma2) | Omega], [],
                   false, false, Rho_Program ) :- !,
        apply_rho( Sigma1, [], Rho_Program_Sigma1 ),
        apply_rho( Sigma2, [], Rho_Program_Sigma2 ),
        Program_New = [if(Cond, Rho_Program_Sigma1, Rho_Program_Sigma2)],
        apply_rho( Omega, Program_New, Rho_Program_Tmp ),
        Rho_Program = Rho_Program_Tmp.

apply_rho_if_aux( [if(Cond, Sigma1, Sigma2) | Omega], Program,
                   false, false, Rho_Program ) :- !,
        apply_rho( Sigma1, [], Rho_Program_Sigma1 ),
        apply_rho( Sigma2, [], Rho_Program_Sigma2 ),
        Program_New = [Program |
                       if(Cond, Rho_Program_Sigma1, Rho_Program_Sigma2)],
        apply_rho( Omega, Program_New, Rho_Program_Tmp ),
        Rho_Program = Rho_Program_Tmp.

%  Creates the pre-solve code that binds the domain variables
%  for pickBest at runtime,
%  and creates a string ChoiceList for the nondet that will
%  replace the pickBest call.
:- mode create_pickBest_choice_list(++, ++, ++, -, -).
create_pickBest_code(F, Domain, Delta, PreSolveProg, ChoiceList) :-
        pickBestDomainThreshold(Threshold),
%        printf(stdout, "pickBestDomainThreshold is %w\n", [Threshold]),
%        flush(stdout),
        %  Create a helper list of strings
        %  VariableList = [PickBestVar1, PickBestVar2, ...,
        %                  PickBestVarpickBestDomainThreshold] 
        ( for(I, 1, Threshold),
          foreach(I, IntegerList)
          do
             true
        ),
        findall( PickBestVariable,
                 ( member(Iterator, IntegerList),
                   term_string(Iterator, IteratorS),
                   concat_strings("PickBestVar", IteratorS, PickBestVariable)
                 ),
                 VariableList ),
%        printf(stdout, "VariableList: %w\n", [VariableList]),
        create_pickBest_pre_solve_code( Domain, VariableList, PreSolveProg ),
%        printf(stdout, "PreSolveProg (as String): %w\n", [PreSolveProg]),
        term_string( Delta, DeltaS ),
        replace_character( DeltaS, ",", "__COMMA__", DeltaSNoComma ),
        replace_character( DeltaSNoComma, "\"", "__QUOTE__", DeltaSClean ),
        term_string( F, FS),
        findall( Delta_New,
                 ( member(PickBestVar, VariableList),
                   replace_string( DeltaSClean, FS, PickBestVar, Delta_Tmp ),
                   concat_string(["?(nonvar(", PickBestVar,"))__COMMA__ ",
                                  Delta_Tmp],
                                 Delta_New)
                 ),
                 InstantiatedProgs ),
%        printf(stdout, "InstantiatedProgs: %w.\n", [InstantiatedProgs]),
        list_to_string(InstantiatedProgs, InstantiatedProgsString),
        %  Remove superfluous ".
        remove_character( InstantiatedProgsString, "\"", 
                          ChoiceListTmp ),
        replace_character( ChoiceListTmp, "__QUOTE__", "\"",
                           ChoiceList ).

%  Creates the code segment that will be put into the transformed
%  proc before the solve statement, in order to bind the
%  pickBest variables to the Domain, which is only known at
%  runtime.
:- mode create_pickBest_pre_solve_code(++, ++, -).
create_pickBest_pre_solve_code( Domain, VariableList,
                                PreSolveProg ) :-
   %  Make a copy of the real domain, as we want to remove
   %  elements from it without touching the original domain.
   findall( InstantiatorString,
            ( member(PickBestVar, VariableList),
              concat_string([PickBestVar, "::PickBestDomain, ",
                             "indomain(", PickBestVar, "), ",
                             "dvar_remove_element(PickBestDomain, ",
                             PickBestVar, ")"],
                            InstantiatorString)
            ),
            PreSolveProgramList ),
    list_to_string( PreSolveProgramList, PreSolveProgramListS ),
    concat_string(["[dom_copy(", Domain, ", PickBestDomain), ", 
                  PreSolveProgramListS, "]"],
                  PreSolveProgTmp),
    %  Remove superfluous ".
    remove_character( PreSolveProgTmp, "\"", PreSolveProg ).


% ---------------------------------------------------------- %
%                       Tau_Prime_H                          %
% ---------------------------------------------------------- %

%  Tau_Prime_H integrates the arbitrary program omega
%  following the first nondet statement into the set of
%  nondeterministic choices:
%  solve([alpha; nondet(p_1,...,p_n); omega], H, rew)
%  === tau_prime_H ===>
%  solve([alpha; nondet(q_1,...,q_m)], H, rew).

:- mode apply_tau_prime_H(++, ++, -).
apply_tau_prime_H( [solve(Prog, Horizon, _RewardFunction)], Program,
                   Tau_Prime_H_Program ) :- !,
        apply_tau_prime_H( Prog, Horizon, Program, Tau_Prime_H_Program ). 

%  In the following, for each possible expression in omega,
%  we use four clauses for differentiating between the following four
%  cases:
%  1) OmegaPrime  = [] and Prog  = []
%  2) OmegaPrime  = [] and Prog \= []
%  3) OmegaPrime \= [] and Prog  = []
%  4) OmegaPrime \= [] and Prog \= [].
%
%  This is necessary, because we use the | operator
%  instead of append/3 (which has a time complexity of O(n^2)).
%  When the first argument is the empty list, the two behave
%  differently:
%  append([], List, Result) -> Result=List
%  Result=[[] | List]       -> Result=[[] | List]
%  We want to achieve the first result, but with the | operator.
%
%  We could also use ( if -> then; else ) constructs inside one
%  clause, but having different clauses is more efficient.


%%%  omega = ...  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%  Empty program (without any nondeterministic choice in solve context)  %
apply_tau_prime_H( [], _Horizon, [], Tau_Prime_H_Program ) :- !,
        Tau_Prime_H_Program = [].

apply_tau_prime_H( [], _Horizon, Program, Tau_Prime_H_Program ) :- !,
        Tau_Prime_H_Program = Program.

%  Star and pickBest are implicitly translated into nondet statements.
%  Here they are then directly integrated into the nondeterministic
%  decision set.

%%%  pickBest  %%%
%  As we didn't know the domain of pickBest when applying Rho, we now
%  have to store the deterministic code that should bind the variables
%  at runtime. Therefore, we now keep this code as an argument for the
%  predicate pickBestBindDomainVariables/1 (defined in final_trans.pl)
%  and attach it after the application of tau_prime_H.
%  Later, tau_prime will treat it as usual deterministic code
%  and pull it out of the solve context.
%  Note, that Omega_Prime is never empty, as it contains the
%  nondeterministic choice {}.
apply_tau_prime_H( [pickBestBindDomainVariables(PreSolveProg) | Omega_Prime],
                   Horizon, [], Tau_Prime_H_Program ) :-  !,
        apply_tau_prime_H( Omega_Prime, Horizon, [], Tau_Prime_H_Program_Tmp ),
        Tau_Prime_H_Program = [ pickBestBindDomainVariables(PreSolveProg) |
                                Tau_Prime_H_Program_Tmp ].

%%%  Star  %%%
%  Note that Rho had postponed the expansion of the star that appeared as first
%  nondeterministic statement, by putting it inside the nondeterministic set.
%  Also note that, at this point, the nondeterministic set only consists of the
%  "star".
apply_tau_prime_H( [{Star}], Horizon, [], Tau_Prime_H_Program ) :- 
        ( \+ string(Star) -> 
            %  If Star is not a string yet,
            %  convert it to one.
            term_string(Star, StarString)
        ;
            %  Else simply rename it.
            StarString = Star
        ),
        %  Test, if Star = "star(Alpha)".
        substring( StarString, 1, 4, "star" ),
        !,

        %  Cut out argument of star.
        string_length(StarString, StarLength),
        ReducedStarLength is (StarLength - 6),
        substring( StarString, 6, ReducedStarLength, AlphaString ),
        term_string(Alpha, AlphaString),
         
        %  Construct a list Tau_Prime_H_Program_Tmp of all possible programs
        %  [Alpha],
        %  [Alpha; Alpha],
        %  ...,
        %  [Alpha^k],
        %  where k is the first length of the Alpha sequence that is longer
        %  than the horizon.
        horizon_consumption(Alpha, Consume),
        expand_alpha( Alpha, Consume, [], Horizon, Star_Program ),

        %  Add the choice of Alpha^0 in the beginning.
        Star_Program_Tmp = [[] | Star_Program],

        list_to_string( Star_Program_Tmp, Star_P_List ),
        apply_tau_prime_H( [{Star_P_List}], Horizon, [],
                           Tau_Prime_H_Program ).

apply_tau_prime_H( [{Star}], Horizon, Program,
                   Tau_Prime_H_Program ) :- 
        ( \+ string(Star) -> 
            %  If Star is not a string yet,
            %  convert it to one.
            term_string(Star, StarString)
        ;
            %  Else simply rename it.
            StarString = Star
        ),
        %  Test, if Star = "star(Alpha)".
        substring( StarString, 1, 4, "star" ),
        !,

        %  Cut out argument of star.
        string_length(StarString, StarLength),
        ReducedStarLength is (StarLength - 6),
        substring( StarString, 6, ReducedStarLength, AlphaString ),
        term_string(Alpha, AlphaString),
         
        %  Construct a list Tau_Prime_H_Program_Tmp of all possible programs
        %  [Alpha],
        %  [Alpha; Alpha],
        %  ...,
        %  [Alpha^k],
        %  where k is the first length of the Alpha sequence that is longer
        %  than the horizon.
        horizon_consumption(Alpha, Consume),
        expand_alpha( Alpha, Consume, [], Horizon, Star_Program ),

        %  Add the choice of Alpha^0 in the beginning.
        Star_Program_Tmp = [[] | Star_Program],

        list_to_string( Star_Program_Tmp, Star_P_List ),
        apply_tau_prime_H( [{Star_P_List}], Horizon, Program,
                           Tau_Prime_H_Program ).

apply_tau_prime_H( [{Star} | Omega_Prime], Horizon, [],
                   Tau_Prime_H_Program ) :- 
        ( \+ string(Star) -> 
            %  If Star is not a string yet,
            %  convert it to one.
            term_string(Star, StarString)
        ;
            %  Else simply rename it.
            StarString = Star
        ),
        %  Test, if Star = "star(Alpha)".
        substring( StarString, 1, 4, "star" ),
        !,
%        printf( stdout, "Encountered star in nondet. choice set.\n", []),
%        printf( stdout, "StarString: %w\n", [StarString]),

        %  Cut out argument of star.
        string_length(StarString, StarLength),
        ReducedStarLength is (StarLength - 6),
        substring( StarString, 6, ReducedStarLength, AlphaString ),
        term_string(Alpha, AlphaString),
         
        %  Construct a list Tau_Prime_H_Program_Tmp of all possible programs
        %  [Alpha; Omega_Prime],
        %  [Alpha; Alpha; Omega_Prime],
        %  ...,
        %  [Alpha^k; Omega_Prime],
        %  where k is the first length of the Alpha sequence that is longer
        %  than the horizon.
        horizon_consumption(Alpha, Consume),
        expand_alpha( Alpha, Consume, Omega_Prime, Horizon, Star_Program ),

        %  Add the choice of Alpha^0 in the beginning.
        %  Omega_Prime is appended (implicitely) to the empty program,
        %  because Omega_Prime must be executed afterwards.
        list_to_string( Omega_Prime, Omega_PrimeS ),
        comma_to_semicolon( Omega_PrimeS, Omega_PrimeTmp ),
        string_to_list( Omega_PrimeTmp, Omega_PrimeTmpList ),
        append( Omega_PrimeTmpList, Star_Program, Star_Program_Tmp ),

        list_to_string( Star_Program_Tmp, Star_P_List ),
%        printf( stdout, "Star_P_List: %w\n", [Star_P_List] ),
%        printf( stdout, "Omega_Prime: %w\n", [Omega_Prime] ),
        apply_tau_prime_H( [{Star_P_List}], Horizon, [],
                           Tau_Prime_H_Program ).

%  Note that Rho had postponed the expansion of the star that appeared as first
%  nondeterministic statement, by putting it inside the nondeterministic set.
%  Also note that, at this point, the nondeterministic set only consists of the
%  "star".
apply_tau_prime_H( [{Star} | Omega_Prime], Horizon, Program,
                   Tau_Prime_H_Program ) :- 
        ( \+ string(Star) -> 
            %  If Star is not a string yet,
            %  convert it to one.
            term_string(Star, StarString)
        ;
            %  Else simply rename it.
            StarString = Star
        ),
        %  Test, if Star = "star(Alpha)".
        substring( StarString, 1, 4, "star" ),
        !,
%        printf( stdout, "Encountered star in nondet. choice set.\n", []),
%        printf( stdout, "StarString: %w\n", [StarString]),

        %  Cut out argument of star.
        string_length(StarString, StarLength),
        ReducedStarLength is (StarLength - 6),
        substring( StarString, 6, ReducedStarLength, AlphaString ),
        term_string(Alpha, AlphaString),
         
        %  Construct a list Tau_Prime_H_Program_Tmp of all possible programs
        %  [Alpha; Omega_Prime],
        %  [Alpha; Alpha; Omega_Prime],
        %  ...,
        %  [Alpha^k; Omega_Prime],
        %  where k is the first length of the Alpha sequence that is longer
        %  than the horizon.
        horizon_consumption(Alpha, Consume),
        expand_alpha( Alpha, Consume, Omega_Prime, Horizon, Star_Program ),

        %  Add the choice of Alpha^0 in the beginning.
        %  Omega_Prime is appended (implicitely) to the empty program,
        %  because Omega_Prime must be executed afterwards.
        list_to_string( Omega_Prime, Omega_PrimeS ),
        comma_to_semicolon( Omega_PrimeS, Omega_PrimeTmp ),
        string_to_list( Omega_PrimeTmp, Omega_PrimeTmpList ),
        append( Omega_PrimeTmpList, Star_Program, Star_Program_Tmp ),

        list_to_string( Star_Program_Tmp, Star_P_List ),
%        printf( stdout, "Star_P_List: %w\n", [Star_P_List] ),
%        printf( stdout, "Omega_Prime: %w\n", [Omega_Prime] ),
        apply_tau_prime_H( [{Star_P_List}], Horizon, Program,
                           Tau_Prime_H_Program ).

%%%  This is the case, where a star comes *after* the first  %%%
%%%  nondeterministic statement.                             %%%
apply_tau_prime_H( [{P_List} | star(Alpha)], Horizon,
                   [], Tau_Prime_H_Program ) :-
        !, %  CUT is okay here, as we skip the step of converting star
           %  into a nondet and again applying tau_prime_H to that nondet.
           %  Instead we directly integrate the choices Alpha^0,...,Alpha^i
           %  into the choice set.

        %  Construct a list Star_Program of all possible programs
        %  [Alpha],
        %  [Alpha; Alpha],
        %  ...,
        %  [Alpha^k],
        %  where k is the first length of the Alpha sequence that is longer
        %  than the horizon.
        %  TODO: Different to the star case before, the p_i in the P_List may
        %  already consume horizon. To enhance performance this can be
        %  considered when expanding Alpha. A naive approach would be to
        %  subtract the minimal horizon consumption of all p_i.
        horizon_consumption(Alpha, Consume),
        expand_alpha( Alpha, Consume, [], Horizon, Star_Program ),

        %  Add the choice of Alpha^0 in the beginning.
        Star_Program_Tmp = [[] | Star_Program],
         
        %  Integrate the complete rest program into the nondeterministic set.
        integrate( {P_List}, Star_Program_Tmp, {P_List_New} ),

        apply_tau_prime_H( [{P_List_New}], Horizon, [],
                           Tau_Prime_H_Program ).

apply_tau_prime_H( [{P_List} | star(Alpha)], Horizon,
                   Program, Tau_Prime_H_Program ) :-
        !, %  CUT is okay here, as we skip the step of converting star
           %  into a nondet and again applying tau_prime_H to that nondet.
           %  Instead we directly integrate the choices Alpha^0,...,Alpha^i
           %  into the choice set.

        %  Construct a list Star_Program of all possible programs
        %  [Alpha],
        %  [Alpha; Alpha],
        %  ...,
        %  [Alpha^k],
        %  where k is the first length of the Alpha sequence that is longer
        %  than the horizon.
        %  TODO: Different to the star case before, the p_i in the P_List may
        %  already consume horizon. To enhance performance this can be
        %  considered when expanding Alpha. A naive approach would be to
        %  subtract the minimal horizon consumption of all p_i.
        horizon_consumption(Alpha, Consume),
        expand_alpha( Alpha, Consume, [], Horizon, Star_Program ),

        %  Add the choice of Alpha^0 in the beginning.
        Star_Program_Tmp = [[] | Star_Program],
         
        %  Integrate the complete rest program into the nondeterministic set.
        integrate( {P_List}, Star_Program_Tmp, {P_List_New} ),

        apply_tau_prime_H( [{P_List_New}], Horizon, Program,
                           Tau_Prime_H_Program ).

apply_tau_prime_H( [{P_List} | [star(Alpha) | Omega_Prime]], Horizon,
                   [], Tau_Prime_H_Program ) :-
        !, %  CUT is okay here, as we skip the step of converting star
           %  into a nondet and again applying tau_prime_H to that nondet.
           %  Instead we directly integrate the choices Alpha^0,...,Alpha^i
           %  into the choice set.

        %  Construct a list Star_Program of all possible programs
        %  [Alpha; Omega_Prime],
        %  [Alpha; Alpha; Omega_Prime],
        %  ...,
        %  [Alpha^k; Omega_Prime],
        %  where k is the first length of the Alpha sequence that is longer
        %  than the horizon.
        %  TODO: Different to the star case before, the p_i in the P_List may
        %  already consume horizon. To enhance performance this can be
        %  considered when expanding Alpha. A naive approach would be to
        %  subtract the minimal horizon consumption of all p_i.
        horizon_consumption(Alpha, Consume),
        expand_alpha( Alpha, Consume, Omega_Prime, Horizon, Star_Program ),

        %  Add the choice of Alpha^0 in the beginning.
        %  Omega_Prime is appended (implicitely) to the empty program,
        %  because Omega_Prime must be executed afterwards.
        list_to_string( Omega_Prime, Omega_PrimeS ),
        comma_to_semicolon( Omega_PrimeS, Omega_PrimeTmp ),
        string_to_list( Omega_PrimeTmp, Omega_PrimeTmpList ),
        append( Omega_PrimeTmpList, Star_Program, Star_Program_Tmp ),
         
        %  Integrate the complete rest program into the nondeterministic set.
        integrate( {P_List}, Star_Program_Tmp, {P_List_New} ),

        apply_tau_prime_H( [{P_List_New}], Horizon, [],
                           Tau_Prime_H_Program ).

apply_tau_prime_H( [{P_List} | [star(Alpha) | Omega_Prime]], Horizon,
                   Program, Tau_Prime_H_Program ) :-
        !, %  CUT is okay here, as we skip the step of converting star
           %  into a nondet and again applying tau_prime_H to that nondet.
           %  Instead we directly integrate the choices Alpha^0,...,Alpha^i
           %  into the choice set.

        %  Construct a list Star_Program of all possible programs
        %  [Alpha; Omega_Prime],
        %  [Alpha; Alpha; Omega_Prime],
        %  ...,
        %  [Alpha^k; Omega_Prime],
        %  where k is the first length of the Alpha sequence that is longer
        %  than the horizon.
        %  TODO: Different to the star case before, the p_i in the P_List may
        %  already consume horizon. To enhance performance this can be
        %  considered when expanding Alpha. A naive approach would be to
        %  subtract the minimal horizon consumption of all p_i.
        horizon_consumption(Alpha, Consume),
        expand_alpha( Alpha, Consume, Omega_Prime, Horizon, Star_Program ),

        %  Add the choice of Alpha^0 in the beginning.
        %  Omega_Prime is appended (implicitely) to the empty program,
        %  because Omega_Prime must be executed afterwards.
        list_to_string( Omega_Prime, Omega_PrimeS ),
        comma_to_semicolon( Omega_PrimeS, Omega_PrimeTmp ),
        string_to_list( Omega_PrimeTmp, Omega_PrimeTmpList ),
        append( Omega_PrimeTmpList, Star_Program, Star_Program_Tmp ),
         
        %  Integrate the complete rest program into the nondeterministic set.
        integrate( {P_List}, Star_Program_Tmp, {P_List_New} ),

        apply_tau_prime_H( [{P_List_New}], Horizon, Program,
                           Tau_Prime_H_Program ).

%%%  PickBest  %%%
apply_tau_prime_H( [{P_List} | pickBest(F, Domain, Delta)], 
                   Horizon, [], Tau_Prime_H_Program ) :-
        !, 
        findall( Delta_New,
                 %  Domain will be a list, correct?
                 ( member(Value, Domain), 
                   replace_term( Delta, F, Value, Delta_New ) ),
                 Instantiated_Progs ),
        integrate({P_List}, Instantiated_Progs, {P_List_New}), 
        apply_tau_prime_H( [{P_List_New}], Horizon, [],
                           Tau_Prime_H_Program ).

apply_tau_prime_H( [{P_List} | pickBest(F, Domain, Delta)], 
                   Horizon, Program, Tau_Prime_H_Program ) :-
        !, 
        findall( Delta_New,
                 %  Domain will be a list, correct?
                 ( member(Value, Domain), 
                   replace_term( Delta, F, Value, Delta_New ) ),
                 Instantiated_Progs ),
        integrate({P_List}, Instantiated_Progs, {P_List_New}), 
        apply_tau_prime_H( [{P_List_New}], Horizon, Program,
                           Tau_Prime_H_Program ).

apply_tau_prime_H( [{P_List} | [pickBest(F, Domain, Delta) | Omega_Prime]], 
                   Horizon, [], Tau_Prime_H_Program ) :-
        !, 
        findall( Delta_New,
                 %  Domain will be a list, correct?
                 ( member(Value, Domain), 
                   replace_term( Delta, F, Value, Delta_New ) ),
                 Instantiated_Progs ),
        integrate({P_List}, Instantiated_Progs, {P_List_New}), 
        apply_tau_prime_H( [{P_List_New} | Omega_Prime], Horizon, [],
                           Tau_Prime_H_Program ).

apply_tau_prime_H( [{P_List} | [pickBest(F, Domain, Delta) | Omega_Prime]], 
                   Horizon, Program, Tau_Prime_H_Program ) :-
        !, 
        findall( Delta_New,
                 %  Domain will be a list, correct?
                 ( member(Value, Domain), 
                   replace_term( Delta, F, Value, Delta_New ) ),
                 Instantiated_Progs ),
        integrate({P_List}, Instantiated_Progs, {P_List_New}), 
        apply_tau_prime_H( [{P_List_New} | Omega_Prime], Horizon, Program,
                           Tau_Prime_H_Program ).

%%%  Empty program (with nondeterministic choice in solve context)  %%%
apply_tau_prime_H( [{P_List}], _Horizon, [], Tau_Prime_H_Program ) :-
        !,
        %  To enhance readability we put in some brackets
        %  around the programs to choose from
        concat_string(["[", P_List, "]"], Tmp1),
        replace_string(Tmp1, " ", "", Tmp2 ),
        replace_string(Tmp2, ",", "], [", Tmp3 ),
        %  Before integration, we had exchanged commas in tests,
        %  while-statements, and if-statements by placeholders.
        %  Now we substitute them back.
        replace_string(Tmp3, "__COMMA__", ",", P_List_Clean),
        %  TODO: sort P_List
        Program_New = [nondet([P_List_Clean])],
        Tau_Prime_H_Program = Program_New.

apply_tau_prime_H( [{P_List}], _Horizon, Program, Tau_Prime_H_Program ) :-
        !,
        %  To enhance readability we put in some brackets
        %  around the programs to choose from
        concat_string(["[", P_List, "]"], Tmp1),
        replace_string(Tmp1, " ", "", Tmp2 ),
        replace_string(Tmp2, ",", "], [", Tmp3 ),
        %  Before integration, we had exchanged commas in tests,
        %  while-statements, and if-statements by placeholders.
        %  Now we substitute them back.
        replace_string(Tmp3, "__COMMA__", ",", P_List_Clean),
        %  TODO: sort P_List
        Program_New = [Program | nondet([P_List_Clean])],
        Tau_Prime_H_Program = Program_New.

%%%  Conditional  %%%
apply_tau_prime_H( [{P_List} | if(Cond, Sigma1, Sigma2)],
                   Horizon, [], Tau_Prime_H_Program ) :- !,
        integrate({P_List}, [?(Cond)], {P_List_New1}), 
        integrate({P_List}, [?(not(Cond))], {P_List_New2}),
        apply_tau_prime_H( [{P_List_New1} | Sigma1], Horizon,
                           [], Tau_Prime_H_Program1 ),
        apply_tau_prime_H( [{P_List_New2} | Sigma2], Horizon, 
                           [], Tau_Prime_H_Program2 ),
        %  Cut away nondet() in resulting programs.
        list_to_string( Tau_Prime_H_Program1, P_String1 ),
        string_length( P_String1, Length1 ),
        ReducedLength1 is (Length1 - 10),
        substring( P_String1, 9, ReducedLength1, Final_P_String1),
        list_to_string( Tau_Prime_H_Program2, P_String2 ),
        string_length( P_String2, Length2 ),
        ReducedLength2 is (Length2 - 10),
        substring( P_String2, 9, ReducedLength2, Final_P_String2),
        join_prog_lists( {Final_P_String1}, {Final_P_String2},
                         {P_String_Joined} ),
        %  TODO: sort P_String_Joined
        Tau_Prime_H_Program = [nondet(P_String_Joined)].

apply_tau_prime_H( [{P_List} | if(Cond, Sigma1, Sigma2)],
                   Horizon, Program, Tau_Prime_H_Program ) :- !,
        integrate({P_List}, [?(Cond)], {P_List_New1}), 
        integrate({P_List}, [?(not(Cond))], {P_List_New2}),
        apply_tau_prime_H( [{P_List_New1} | Sigma1], Horizon,
                           Program, Tau_Prime_H_Program1 ),
        apply_tau_prime_H( [{P_List_New2} | Sigma2], Horizon, 
                           Program, Tau_Prime_H_Program2 ),
        %  Cut away nondet() in resulting programs.
        list_to_string( Tau_Prime_H_Program1, P_String1 ),
        string_length( P_String1, Length1 ),
        ReducedLength1 is (Length1 - 10),
        substring( P_String1, 9, ReducedLength1, Final_P_String1),
        list_to_string( Tau_Prime_H_Program2, P_String2 ),
        string_length( P_String2, Length2 ),
        ReducedLength2 is (Length2 - 10),
        substring( P_String2, 9, ReducedLength2, Final_P_String2),
        join_prog_lists( {Final_P_String1}, {Final_P_String2},
                         {P_String_Joined} ),
        %  TODO: sort P_String_Joined
        Program_New = [Program | nondet(P_String_Joined)],
        Tau_Prime_H_Program = Program_New.

apply_tau_prime_H( [{P_List} | [if(Cond, Sigma1, Sigma2) | Omega_Prime]],
                   Horizon, [], Tau_Prime_H_Program ) :- !,
        integrate({P_List}, [?(Cond)], {P_List_New1}), 
        integrate({P_List}, [?(not(Cond))], {P_List_New2}),
        apply_tau_prime_H( [{P_List_New1} | [Sigma1 | Omega_Prime]], Horizon,
                           [], Tau_Prime_H_Program1 ),
        apply_tau_prime_H( [{P_List_New2} | [Sigma2 | Omega_Prime]], Horizon, 
                           [], Tau_Prime_H_Program2 ),
        %  Cut away nondet() in resulting programs.
        list_to_string( Tau_Prime_H_Program1, P_String1 ),
        string_length( P_String1, Length1 ),
        ReducedLength1 is (Length1 - 10),
        substring( P_String1, 9, ReducedLength1, Final_P_String1),
        list_to_string( Tau_Prime_H_Program2, P_String2 ),
        string_length( P_String2, Length2 ),
        ReducedLength2 is (Length2 - 10),
        substring( P_String2, 9, ReducedLength2, Final_P_String2),
        join_prog_lists( {Final_P_String1}, {Final_P_String2},
                         {P_String_Joined} ),
        %  TODO: sort P_String_Joined
        Tau_Prime_H_Program = [nondet(P_String_Joined)].

apply_tau_prime_H( [{P_List} | [if(Cond, Sigma1, Sigma2) | Omega_Prime]],
                   Horizon, Program, Tau_Prime_H_Program ) :- !,
        integrate({P_List}, [?(Cond)], {P_List_New1}), 
        integrate({P_List}, [?(not(Cond))], {P_List_New2}),
        apply_tau_prime_H( [{P_List_New1} | [Sigma1 | Omega_Prime]], Horizon,
                           Program, Tau_Prime_H_Program1 ),
        apply_tau_prime_H( [{P_List_New2} | [Sigma2 | Omega_Prime]], Horizon, 
                           Program, Tau_Prime_H_Program2 ),
        %  Cut away nondet() in resulting programs.
        list_to_string( Tau_Prime_H_Program1, P_String1 ),
        string_length( P_String1, Length1 ),
        ReducedLength1 is (Length1 - 10),
        substring( P_String1, 9, ReducedLength1, Final_P_String1),
        list_to_string( Tau_Prime_H_Program2, P_String2 ),
        string_length( P_String2, Length2 ),
        ReducedLength2 is (Length2 - 10),
        substring( P_String2, 9, ReducedLength2, Final_P_String2),
        join_prog_lists( {Final_P_String1}, {Final_P_String2},
                         {P_String_Joined} ),
        %  TODO: sort P_String_Joined
        Program_New = [Program | nondet(P_String_Joined)],
        Tau_Prime_H_Program = Program_New.

%  Shortcut, if else-branch is not provided.
apply_tau_prime_H( [{P_List} | if(Cond, Sigma)],
                   Horizon, [], Tau_Prime_H_Program ) :- !,
        apply_tau_prime_H( [{P_List} | if(Cond, Sigma, [])],
                           Horizon, [], Tau_Prime_H_Program ).

apply_tau_prime_H( [{P_List} | if(Cond, Sigma)],
                   Horizon, Program, Tau_Prime_H_Program ) :- !,
        apply_tau_prime_H( [{P_List} | if(Cond, Sigma, [])],
                           Horizon, Program, Tau_Prime_H_Program ).

apply_tau_prime_H( [{P_List} | [if(Cond, Sigma) | Omega_Prime]],
                   Horizon, [], Tau_Prime_H_Program ) :- !,
        apply_tau_prime_H( [{P_List} | [if(Cond, Sigma, []) | Omega_Prime]],
                           Horizon, [], Tau_Prime_H_Program ).

apply_tau_prime_H( [{P_List} | [if(Cond, Sigma) | Omega_Prime]],
                   Horizon, Program, Tau_Prime_H_Program ) :- !,
        apply_tau_prime_H( [{P_List} | [if(Cond, Sigma, []) | Omega_Prime]],
                           Horizon, Program, Tau_Prime_H_Program ).

%%%  Loop  %%%
apply_tau_prime_H( [{P_List} | while(Cond, Sigma)],
                   Horizon, [], Tau_Prime_H_Program ) :- !,
         Alpha = [?(Cond) | Sigma],
         %  Use neg as negation, because not destroys the brackets around the
         %  condition. Correct?
         Ending = [?(neg(Cond))],
         %  Construct a list of all sequences of condition tests and Sigma.
         %  Also append the failing condition and Omega_Prime to each
         %  program in the list.
         horizon_consumption(Alpha, Consume),
         expand_alpha( Alpha, Consume, Ending, Horizon, Star_Program ),
         term_string( Cond, CondS ),
         concat_string( ["?(neg(", CondS, "))"], NegCondTestS ),
         %  Add the choice of [?(not(Cond))] in the beginning.
         Star_Program_Tmp = [[NegCondTestS] | Star_Program],
         %  Integrate the complete rest program into the nondeterministic set.
         integrate( {P_List}, Star_Program_Tmp, {P_List_New} ),

         apply_tau_prime_H( [{P_List_New}], Horizon, [],
                            Tau_Prime_H_Program ).

apply_tau_prime_H( [{P_List} | while(Cond, Sigma)],
                   Horizon, Program, Tau_Prime_H_Program ) :- !,
         Alpha = [?(Cond) | Sigma],
         %  Use neg as negation, because not destroys the brackets around the
         %  condition. Correct?
         Ending = [?(neg(Cond))],
         %  Construct a list of all sequences of condition tests and Sigma.
         %  Also append the failing condition and Omega_Prime to each
         %  program in the list.
         horizon_consumption(Alpha, Consume),
         expand_alpha( Alpha, Consume, Ending, Horizon, Star_Program ),
         term_string( Cond, CondS ),
         concat_string( ["?(neg(", CondS, "))"], NegCondTestS ),
         %  Add the choice of [?(not(Cond)); Omega_Prime] in the beginning.
         Star_Program_Tmp = [[NegCondTestS] | Star_Program],
         %  Integrate the complete rest program into the nondeterministic set.
         integrate( {P_List}, Star_Program_Tmp, {P_List_New} ),

         apply_tau_prime_H( [{P_List_New}], Horizon, Program,
                            Tau_Prime_H_Program ).

apply_tau_prime_H( [{P_List} | [while(Cond, Sigma) | Omega_Prime]],
                   Horizon, [], Tau_Prime_H_Program ) :- !,
         Alpha = [?(Cond) | Sigma],
         %  Use neg as negation, because not destroys the brackets around the
         %  condition. Correct?
         Ending = [?(neg(Cond));Omega_Prime],
         %  Construct a list of all sequences of condition tests and Sigma.
         %  Also append the failing condition and Omega_Prime to each
         %  program in the list.
         horizon_consumption(Alpha, Consume),
         expand_alpha( Alpha, Consume, Ending, Horizon, Star_Program ),
         term_string( Cond, CondS ),
         concat_string( ["?(neg(", CondS, "))"], NegCondTestS ),
         %  Add the choice of [?(not(Cond)); Omega_Prime] in the beginning.
         list_to_string( Omega_Prime, Omega_PrimeS ),
         concat_string( [NegCondTestS, ";", Omega_PrimeS], Tmp1S ),
         string_to_list( Tmp1S, Tmp2 ),
         Star_Program_Tmp = [Tmp2 | Star_Program],
         %  Integrate the complete rest program into the nondeterministic set.
         integrate( {P_List}, Star_Program_Tmp, {P_List_New} ),

         apply_tau_prime_H( [{P_List_New} | []], Horizon, [],
                            Tau_Prime_H_Program ).

apply_tau_prime_H( [{P_List} | [while(Cond, Sigma) | Omega_Prime]],
                   Horizon, Program, Tau_Prime_H_Program ) :- !,
         Alpha = [?(Cond) | Sigma],
         %  Use neg as negation, because not destroys the brackets around the
         %  condition. Correct?
         Ending = [?(neg(Cond));Omega_Prime],
         %  Construct a list of all sequences of condition tests and Sigma.
         %  Also append the failing condition and Omega_Prime to each
         %  program in the list.
         horizon_consumption(Alpha, Consume),
         expand_alpha( Alpha, Consume, Ending, Horizon, Star_Program ),
         term_string( Cond, CondS ),
         concat_string( ["?(neg(", CondS, "))"], NegCondTestS ),
         %  Add the choice of [?(not(Cond)); Omega_Prime] in the beginning.
         list_to_string( Omega_Prime, Omega_PrimeS ),
         concat_string( [NegCondTestS, ";", Omega_PrimeS], Tmp1S ),
         string_to_list( Tmp1S, Tmp2 ),
         Star_Program_Tmp = [Tmp2 | Star_Program],
         %  Integrate the complete rest program into the nondeterministic set.
         integrate( {P_List}, Star_Program_Tmp, {P_List_New} ),

         apply_tau_prime_H( [{P_List_New}], Horizon, Program,
                            Tau_Prime_H_Program ).

%%%  Nondeterministic Choice  %%%
apply_tau_prime_H( [{P_List} | nondet(ArgList)], Horizon,
                   [], Tau_Prime_H_Program ) :- !,
        integrate( {P_List}, ArgList, {P_List_New} ), 
        apply_tau_prime_H( [{P_List_New}], Horizon, [],
                           Tau_Prime_H_Program ).

apply_tau_prime_H( [{P_List} | nondet(ArgList)], Horizon,
                   Program, Tau_Prime_H_Program ) :- !,
        integrate( {P_List}, ArgList, {P_List_New} ), 
        apply_tau_prime_H( [{P_List_New}], Horizon, Program,
                           Tau_Prime_H_Program ).

apply_tau_prime_H( [{P_List} | [nondet(ArgList) | Omega_Prime]], Horizon,
                   [], Tau_Prime_H_Program ) :- !,
        integrate( {P_List}, ArgList, {P_List_New} ), 
        apply_tau_prime_H( [{P_List_New} | Omega_Prime], Horizon, [],
                           Tau_Prime_H_Program ).

apply_tau_prime_H( [{P_List} | [nondet(ArgList) | Omega_Prime]], Horizon,
                   Program, Tau_Prime_H_Program ) :- !,
        integrate( {P_List}, ArgList, {P_List_New} ), 
        apply_tau_prime_H( [{P_List_New} | Omega_Prime], Horizon, Program,
                           Tau_Prime_H_Program ).

%%%  Test Action, Primitive Action, or Stochastic Action  %%%
apply_tau_prime_H( [{P_List} | Term], Horizon, [],
                   Tau_Prime_H_Program ) :- !,
        integrate( {P_List}, [Term], {P_List_New} ),
        apply_tau_prime_H( [{P_List_New}], Horizon, [],
                           Tau_Prime_H_Program ).

apply_tau_prime_H( [{P_List} | Term], Horizon, Program,
                   Tau_Prime_H_Program ) :- !,
        integrate( {P_List}, [Term], {P_List_New} ),
        apply_tau_prime_H( [{P_List_New}], Horizon, Program,
                           Tau_Prime_H_Program ).

apply_tau_prime_H( [{P_List} | [Term | Omega_Prime]], Horizon, [],
                   Tau_Prime_H_Program ) :- !,
        integrate( {P_List}, [Term], {P_List_New} ),
        apply_tau_prime_H( [{P_List_New} | Omega_Prime], Horizon, [],
                           Tau_Prime_H_Program ).

apply_tau_prime_H( [{P_List} | [Term | Omega_Prime]], Horizon, Program,
                   Tau_Prime_H_Program ) :- !,
        integrate( {P_List}, [Term], {P_List_New} ),
        apply_tau_prime_H( [{P_List_New} | Omega_Prime], Horizon, Program,
                           Tau_Prime_H_Program ).

%%%  Conditional (before nondeterministic choice)  %%%
apply_tau_prime_H( [if(Cond, Sigma1, Sigma2)], Horizon, _Program,
                   Tau_Prime_H_Program ) :- !,
        %  As Sigma1 or Sigma2 might contain nondeterministic programs,
        %  we have to apply tau_prime_H to them.
        %  TODO: For optimisation, we could test, if they contain { }
        %  and otherwise just skip them.
        apply_tau_prime_H( Sigma1, Horizon, [], Tau_Prime_H_Program_Sigma1 ),
        apply_tau_prime_H( Sigma2, Horizon, [], Tau_Prime_H_Program_Sigma2 ),
        Tau_Prime_H_Program = [if(Cond, Tau_Prime_H_Program_Sigma1,
                                        Tau_Prime_H_Program_Sigma2)].

%%%  Conditional (before nondeterministic choice)  %%%
apply_tau_prime_H( [if(Cond, Sigma1, Sigma2) | Omega], Horizon, Program,
                   Tau_Prime_H_Program ) :- !,
        %  As Sigma1 or Sigma2 might contain nondeterministic programs,
        %  we have to apply tau_prime_H to them.
        %  TODO: For optimisation, we could test, if they contain { }
        %  and otherwise just skip them.
        apply_tau_prime_H( Sigma1, Horizon, [], Tau_Prime_H_Program_Sigma1 ),
        apply_tau_prime_H( Sigma2, Horizon, [], Tau_Prime_H_Program_Sigma2 ),
        apply_tau_prime_H( Omega, Horizon, Program, Tau_Prime_H_Program_Omega ),
        Tau_Prime_H_Program = [if(Cond, Tau_Prime_H_Program_Sigma1,
                                        Tau_Prime_H_Program_Sigma2) |
                               Tau_Prime_H_Program_Omega].

%%%  Loop (before nondeterministic choice)  %%%
apply_tau_prime_H( [while(Cond, Sigma)], Horizon, _Program,
                    Tau_Prime_H_Program ) :- !,
        apply_tau_prime_H( Sigma, Horizon, [], Tau_Prime_H_Program_Sigma ),
        Tau_Prime_H_Program = [while(Cond, Tau_Prime_H_Program_Sigma)].

apply_tau_prime_H( [while(Cond, Sigma) | Omega], Horizon, Program,
                    Tau_Prime_H_Program ) :- !,
        apply_tau_prime_H( Sigma, Horizon, [], Tau_Prime_H_Program_Sigma ),
        apply_tau_prime_H( Omega, Horizon, Program, Tau_Prime_H_Program_Omega ),
        Tau_Prime_H_Program = [while(Cond, Tau_Prime_H_Program_Sigma) |
                               Tau_Prime_H_Program_Omega].


%  Ignore (but store) everything before the first nondet.
%  That means, ignore everything not starting with a
%  set of programs.
%  Note, that if- and while-statements containing solve-
%  statements have been handled differently before.

%%%  Test Action, Primitive Action, Stochastic Action, or proc  %%%
%%%  (before nondeterministic choice)                           %%%
apply_tau_prime_H( [Alpha], _Horizon, _Program,
                   Tau_Prime_H_Program ) :- !,
        Tau_Prime_H_Program = [Alpha].

apply_tau_prime_H( [Alpha | Omega], Horizon, Program,
                   Tau_Prime_H_Program ) :-
        apply_tau_prime_H( Omega, Horizon, Program,
                           Tau_Prime_H_Program_Nondet ),
        Tau_Prime_H_Program = [Alpha | Tau_Prime_H_Program_Nondet].


         
% ---------------------------------------------------------- %
%                            Tau                             %
% ---------------------------------------------------------- %

%  Tau pulls out the deterministic program alpha that
%  comes before the first nondeterministic statement
%  (nondet/pickBest/star) in the solve context:
%  solve([alpha; nondet(q_1,...,q_m)], H, rew)
%  === tau ===>
%  alpha; solve([nondet(q_1,...,q_m)], H, rew).

:- mode apply_tau(++, -).
%%%  Program with double brackets  %%%
apply_tau(solve([[Program]], Horizon, RewardFunction), Tau_Program ) :- !, 
%        printf(stdout, "\n DOUBLE BRACKETS!!!!! \n", []), flush(stdout),
        apply_tau(solve([Program], Horizon, RewardFunction), Tau_Program ).

apply_tau(solve(Program, Horizon, RewardFunction), Tau_Program ) :-
        %  If the proc is parameterised with the horizon, then Horizon
        %  will be not initialised. Instead of backtracking for all calls
        %  to the proc, we will simply fix a pessimistic horizon of 1.
        var(Horizon),
        !,
        apply_tau(solve(Program, 1, RewardFunction), Tau_Program ).

%%%  Empty Program  %%%
apply_tau(solve([], _Horizon, _RewardFunction), Tau_Program ) :- !,
        Tau_Program = [].

%%%  Zero Horizon  %%%
apply_tau(solve(_Anything, Horizon, _RewardFunction), Tau_Program ) :-
        %%  DO NEVER PUT A CUT HERE, OR THE NONDET WILL NOT BE RE-TRIED!  %%
        Horizon =< 0,
        %  If the horizon is less or equal 0, we end the recursion here.
        !,
        Tau_Program = [].

%%%  Nondeterministic Choice  %%%
apply_tau(solve([nondet(P_List)], Horizon, RewardFunction), Tau_Program ) :- !, 
        Tau_Program = [solve([nondet(P_List)], Horizon, RewardFunction)].

%%%  Conditional  %%%
apply_tau(solve([if(Cond, Sigma1, Sigma2)], Horizon, RewardFunction),
          Tau_Program ) :- !,
        apply_tau(solve(Sigma1, Horizon, RewardFunction),
                  Tau_Program_Sigma1),
        apply_tau(solve(Sigma2, Horizon, RewardFunction),
                  Tau_Program_Sigma2),
        ( (Tau_Program_Sigma1 = [], Tau_Program_Sigma2 = [] ) ->
           %  Optimise readability by leaving away if-clause.
           Tau_Program = []
        ;
           list_to_string(Tau_Program_Sigma1, Sigma1_Tmp),
           list_to_string(Tau_Program_Sigma2, Sigma2_Tmp),
           %  Replace the " around the strings by [].
           remove_character( Sigma1_Tmp, "\"", Sigma1_Final ),
           remove_character( Sigma2_Tmp, "\"", Sigma2_Final ),

           %  Put together both sub-results.
           term_string(Cond, CondString),
           concat_string( ["if( ", CondString, ", [", Sigma1_Final,
                           "], [", Sigma2_Final, "] )"], FinalString ),
           string_to_list(FinalString, Tau_Program)
        ).

apply_tau(solve([if(Cond, Sigma1, Sigma2) | Omega], Horizon, RewardFunction),
          Tau_Program ) :- !,
        apply_tau(solve([Sigma1 | Omega], Horizon, RewardFunction),
                  Tau_Program_Sigma1),
        apply_tau(solve([Sigma2 | Omega], Horizon, RewardFunction),
                  Tau_Program_Sigma2),
        ( (Tau_Program_Sigma1 = [], Tau_Program_Sigma2 = [] ) ->
           %  Optimise readability by leaving away if-clause.
           Tau_Program = []
        ;
           list_to_string(Tau_Program_Sigma1, Sigma1_Tmp),
           list_to_string(Tau_Program_Sigma2, Sigma2_Tmp),
           %  Replace the " around the strings by [].
           remove_character( Sigma1_Tmp, "\"", Sigma1_Final ),
           remove_character( Sigma2_Tmp, "\"", Sigma2_Final ),

           %  Put together both sub-results.
           term_string(Cond, CondString),
           concat_string( ["if( ", CondString, ", [", Sigma1_Final,
                           "], [", Sigma2_Final, "] )"], FinalString ),
           string_to_list(FinalString, Tau_Program)
        ).

%  Shortcut, if else-branch is not provided.
apply_tau(solve([if(Cond, Sigma)], Horizon, RewardFunction),
          Tau_Program ) :- !,
        apply_tau(solve([if(Cond, Sigma, [])], Horizon, RewardFunction),
                  Tau_Program ).

apply_tau(solve([if(Cond, Sigma) | Omega], Horizon, RewardFunction),
          Tau_Program ) :- !,
        apply_tau(solve([if(Cond, Sigma, []) | Omega], Horizon, RewardFunction),
                  Tau_Program ).

%%%  Loop  %%%
%  The loop is expressed as if-statements.
apply_tau(solve([while(Cond, Sigma)], Horizon, RewardFunction),
          Tau_Program ) :- !,
        apply_tau(solve([Sigma | while(Cond, Sigma)], Horizon,
                  RewardFunction), Tau_Program1),
        ( ( Tau_Program1 = [] ) ->
           %  Optimise readability by leaving away if-clause. */
           Tau_Program = []
        ;
           list_to_string(Tau_Program1, Tau1_Final),

           %  Put together both sub-results.
           term_string(Cond, CondString),
           concat_string( ["if( ", CondString, ", [", Tau1_Final,
                           "])"], FinalStringTmp ),
           %  Remove superfluous ".
           remove_character( FinalStringTmp, "\"", 
                             FinalString ),
           string_to_list(FinalString, Tau_Program)
        ).

apply_tau(solve([while(Cond, Sigma) | Omega], Horizon, RewardFunction),
          Tau_Program ) :- !,
        apply_tau(solve([Sigma | [while(Cond, Sigma) | Omega]], Horizon,
                  RewardFunction), Tau_Program1),
        apply_tau(solve(Omega, Horizon, RewardFunction), Tau_Program2),
        ( (Tau_Program1 = [], Tau_Program2 = [] ) ->
           %  Optimise readability by leaving away if-clause. */
           Tau_Program = []
        ;
           list_to_string(Tau_Program1, Tau1_Final),
           list_to_string(Tau_Program2, Tau2_Final),

           %  Put together both sub-results.
           term_string(Cond, CondString),
           concat_string( ["if( ", CondString, ", [", Tau1_Final,
                           "], [", Tau2_Final, "] )"], FinalStringTmp ),
           %  Remove superfluous ".
           remove_character( FinalStringTmp, "\"", 
                             FinalString ),
           string_to_list(FinalString, Tau_Program)
        ).

%%%  Test Action  %%%
apply_tau(solve([?(Term)], _Horizon, _RewardFunction), Tau_Program ) :- !,
        Tau_Program = [?(Term)].

apply_tau(solve([?(Term) | Omega], Horizon, RewardFunction), Tau_Program ) :- !,
        %  Note that the test action does not consume horizon.
        apply_tau(solve(Omega, Horizon, RewardFunction), Tau_Program_Tmp),
        Tau_Program = [?(Term) | Tau_Program_Tmp].

%  If the solve program contains a call to a proc, we have to follow that trail,
%  and recursively make sure that the proc is purely deterministic before
%  pulling it out.
%  Otherwise we might loose a solve context. Consider this example:
%  proc( procOne, [solve(procTwo, H, R)] ).
%  proc( procTwo, [nondet[a,b]] ).
%  The procTwo would be treated as a primitive action, pulled outside the solve
%  and the solve would be deleted.
apply_tau(solve([ProcName], Horizon, RewardFunction), Tau_Program ) :- 
        proc( ProcName, ProcBody ), !,
        is_deterministic( ProcBody, Result ),
 %       printf(stdout, "is_deterministic( %w, %w )\n", [ProcBody, Result]),
 %       flush(stdout),
        ( Result = true ->
           %  Proc is deterministic.
           ( Horizon > 0 ->
              Tau_Program = [ProcName]
           ;
              Tau_Program = []
           )
        ;
           %  Proc is nondeterministic.
           %  We leave Proc untouched, and do not extract potential
           %  deterministic parts.
           Tau_Program = [solve([ProcName], Horizon, RewardFunction)]
        ).

apply_tau(solve([ProcName | Omega], Horizon, RewardFunction), Tau_Program ) :- 
        proc( ProcName, ProcBody ), !,
        is_deterministic( ProcBody, Result ),
%        printf(stdout, "is_deterministic( %w, %w )\n", [ProcBody, Result]),
%        flush(stdout),
        ( Result = true ->
           %  Proc is deterministic.
           horizon_consumption( ProcBody, Consume ),
           Horizon_New is (Horizon - Consume),
           apply_tau(solve(Omega, Horizon_New, RewardFunction),
                     Tau_Program_Tmp),
           ( Horizon > 0 ->
              Tau_Program = [ProcName | Tau_Program_Tmp]
           ;
              Tau_Program = Tau_Program_Tmp
           )
        ;
           %  Proc is nondeterministic.
           %  We leave Proc untouched, and do not extract potential
           %  deterministic parts.
           Tau_Program = [solve([ProcName | Omega], Horizon, RewardFunction)]
        ).

%  Binding the variables doesn't cost any horizon.
apply_tau(solve([pickBestBindDomainVariables(PreSolveProg), nondet(ChoiceList)],
                Horizon, RewardFunction), Tau_Program ) :-
        apply_tau(solve([nondet(ChoiceList)], Horizon, RewardFunction),
                  Tau_Program_Tmp),
        Tau_Program = [pickBestBindDomainVariables(PreSolveProg) |
                       Tau_Program_Tmp].

apply_tau(solve([[pickBestBindDomainVariables(PreSolveProg), nondet(ChoiceList)]
                | _Omega],
                Horizon, RewardFunction), Tau_Program ) :-
        %  Forget about Omega (e.g., when converting while to ifs).
        apply_tau(solve([nondet(ChoiceList)], Horizon, RewardFunction),
                  Tau_Program_Tmp),
        Tau_Program = [pickBestBindDomainVariables(PreSolveProg) |
                       Tau_Program_Tmp].

%%%  Primitive Action, Stochastic Action  %%%
apply_tau(solve([Term], Horizon, _RewardFunction), Tau_Program ) :-
        ( Horizon > 0 ->
           Tau_Program = [Term]
        ;
           Tau_Program = []
        ).


apply_tau(solve([Term | Omega], Horizon, RewardFunction), Tau_Program ) :-
        Horizon_New is (Horizon - 1),
        apply_tau(solve(Omega, Horizon_New, RewardFunction), Tau_Program_Tmp),
        ( Horizon > 0 ->
           Tau_Program = [Term | Tau_Program_Tmp]
        ;
           Tau_Program = Tau_Program_Tmp
        ).

% }}}


% ========================================================== %
%   Main                                                     %
% ========================================================== %
% {{{ Main
% >>>>

:- mode iplpreprocess(++).
%iplpreprocess( File ) :-
%        printf("iplpreprocess(%w): Is it just a Readybot?\n", [File]),
%        File = "-botname",
%        !,
%        printf("Oh, just a ReadyBot... nothing to iplpreprocess.\n", []).

iplpreprocess( File ) :- iplpreprocess( File, _NewFile, true, 0).

:- mode iplpreprocess(++, ?, ?, ++).
iplpreprocess( File, NewFile, _Last, Level ) :-
        ( 
          substring(File, ".pl", _Pos)
        ;
          substring(File, ".readylog", _Pos)
        ),
        !,
	printf("\n\t++ level             : %w\n", Level),
	printf(  "\t++ loading           : %w\n", File),
	printf("\t-----------------------------------------\n", []),
	flush(output),
	cputime(Tloadbegin),
	compile(File),
	cputime(Tloadend),
	Tloaddiff is Tloadend-Tloadbegin,
	printf("\t-----------------------------------------\n", []),
	printf("\t++ loading           : DONE (%w sec.)\n",
	       [Tloaddiff]),
	printf("\t++ processing..\n", []),
	flush(output),
	cputime(Tbegin),
	pathname(File, Path, BaseName, _Suffix),
	concat_string( [Path, "ipl_tmp_", BaseName, ".pl"], TmpFile),
	concat_string( [Path, "ipl_processed_", BaseName, ".pl"], NewFile),
	open(TmpFile, write, Stream),
	write_header(Stream),
	%  -- <MAIN PART> --
        process_all_proc(Stream),
	%  -- </MAIN PART> --
	% -- <LAST_FILE PART> --
%	(
%	  Last ->
%	  generate_events_list( Stream )
%	;
%	  true
%	),
	% -- </LAST_FILE PART> -- */
%        read(FileStream, BufferVar),
%        write(Stream, BufferVar),
	close(Stream),
	erase_rhomb(TmpFile, NewFile),
	cputime(Tend),
	Tdiff is Tend - Tbegin,
	printf("\n\t++ processing        : DONE (%w sec.)\n", [Tdiff]),
	printf("\t++ output written to : ", []),
	printf("%w\n\n", [NewFile]), flush(output). 

iplpreprocess( File, _NewFile, _Last, _Level ) :-
        printf("File: %w\n", [File]),
        printf("Oh, just a ReadyBot... nothing to iplpreprocess.\n", []).


:- mode write_header(++).
write_header(Stream) :-
	printf(Stream, "/*******************************\n", []),
	printf(Stream, "* file generated from ReadyLog *\n", []),
	printf(Stream, "*******************************/\n", []),
	printf(Stream, "\n", []),
	printf(Stream, ":- pragma(nodebug).\n", []).

:- mode erase_rhomb(++, ++).
erase_rhomb(FileIn, FileOut) :-
	concat_string( ["tr -d '#' < ", FileIn, " > ", FileOut],
		       TRString),
	system( TRString),
	concat_string( ["rm ", FileIn], RMString),
	system( RMString).

:- mode run_all(++, ++).
runall(M, M).
runall(N, M) :-
	N < M, !,
	printf("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",[]),
	argv(N, FileName),
	Level is N - 3,
	(
	  N =:= M-1 ->
	  %  Notice when last file is processed: here some
	  %  extra stuff may be added.
	  iplpreprocess(FileName, NewFileName, true, Level)
	;
	  iplpreprocess(FileName, NewFileName, false, Level)
	),
	%  Compile the lately processed output before
	%  stepping over to next level.
        %  Will be forgotten, if compiled here. Compile
        %  in preprocessor instead!
%	compile(NewFileName),
	N_next is N+1,
	runall(N_next, M).

autorun :-
	argc(M),
 	(
	  M > 3 ->
	  runall(3, M),
	  exit(0)
	;
	  writeln("******************************"),
	  writeln("*      interactive mode      *"),
	  writeln("******************************")
	).

% }}}

:- writeln("** loading iplpreprocessor.pl\t\t DONE").

%:- autorun.
