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

/* ================================================================== */
/*  PREPROCESSOR                                                      */
/* ================================================================== */

/** The IPL preprocessor works as a prepreprocessor on the READYLOG
 *  code. Its task is to transform all solve statements in the code
 *  to a standard form, where they only consist of a single nondet.
 *  The intention behind this is to maximise the number of matching
 *  solve statements, which then can be learned by the decision tree
 *  learning algorithm, and eventually be replaced by decision trees.
 */

:- write("** loading iplpreprocessor.pl\n"). 

/* --------------------------------------------------------- */
/*  Header + Flags                                           */
/* --------------------------------------------------------- */
% {{{ header + flags

:- ensure_loaded('readylog.pl').
:- ensure_loaded("../../utils/utils.pl").
:- ensure_loaded('iplutils.pl').

% }}}

/* ----------------------------------------------------------
   procedures
---------------------------------------------------------- */
% {{{ procedures
% >>>>

process_proc( [], Processed ) :- !,
%        printf( stdout, "\n******* Empty. ******\n", [] ),
        Processed = [].


process_proc( [if(Cond, Sigma1, Sigma2) | RestProgram], Processed ) :- !,
        /** Test, if Sigma1 is nondeterministic program. If so, we must
         *  first transform this subprogram. */
%        printf( stdout, "Sigma1: %w\n", [Sigma1] ),
        term_string( Sigma1, Sigma1S ),
        ( substring( Sigma1S, "solve", _Pos ) ->
             /** If contains a solve context. */
%             printf( stdout, "\n*~*~SIGMA1_BEGIN*~*\n", [] ),
             process_proc( Sigma1, ProcessedSigma1 )
%             printf( stdout, "\nProcessed Sigma1: %w\n", [ProcessedSigma1] ),
%            printf( stdout, "\n*~*~SIGMA1_END*~*\n", [] )
        ;
             true
        ),
        /** Test, if Sigma2 is nondeterministic program. If so, we must
         *  first transform this subprogram. */
%        printf( stdout, "Sigma2: %w\n", [Sigma2] ),
        term_string( Sigma2, Sigma2S ),
        ( substring( Sigma2S, "solve", _Pos ) ->
             /** If contains a solve context. */
%             printf( stdout, "\n*~*~SIGMA2_BEGIN*~*\n", [] ),
             process_proc( Sigma2, ProcessedSigma2 )
%             printf( stdout, "\nProcessed Sigma2: %w\n", [ProcessedSigma2] ),
%             printf( stdout, "\n*~*~SIGMA2_END*~*\n", [] )
        ;
             true
        ),
        process_proc( RestProgram, ProcessedRest ),
        ( ( ground( ProcessedSigma1 ), ground( ProcessedSigma2 ) ) ->
             /** both subprograms are nondeterministic */
             Processed = [ if(Cond, ProcessedSigma1, ProcessedSigma2) |
                           ProcessedRest ]
        ;
             /** not both subprograms are nondeterministic */
             ( ground( ProcessedSigma1 ) ->
                  /** only Sigma1 is nondeterministic */
                  Processed = [ if(Cond, ProcessedSigma1, Sigma2) |
                                ProcessedRest ]
             ;
                  /** Sigma1 is deterministic */
                  ( ground( ProcessedSigma2 ) ->
                       /** only Sigma2 is nondeterministic */
                       Processed = [ if(Cond, Sigma1, ProcessedSigma2) |
                                     ProcessedRest ]
                  ;
                       /** both subprograms are deterministic */
                       Processed = [ if(Cond, Sigma1, Sigma2) |
                                     ProcessedRest ]
                  )
             )
        ).

process_proc( [if(Cond, Sigma) | RestProgram], Processed ) :-
        process_proc( [if(Cond, Sigma, []) | RestProgram], Processed ).

process_proc( [while(Cond, Sigma) | RestProgram], Processed ) :- !,
        /** Test, if Sigma is nondeterministic program. If so, we must
         *  first transform this subprogram. */
%        printf( stdout, "Sigma: %w\n", [Sigma] ),
 
        term_string( Sigma, SigmaS ),
        ( substring( SigmaS, "solve", _Pos ) ->
             /** While contains a solve context. */
%             printf( stdout, "\n*~*~SIGMA_BEGIN*~*\n", [] ),
             process_proc( Sigma, ProcessedSigma ),
%             printf( stdout, "\nProcessed Sigma: %w\n", [ProcessedSigma] ),
%             printf( stdout, "\n*~*~SIGMA_END*~*\n", [] ),
             
             process_proc( RestProgram, ProcessedRest ),
             Processed = [ while(Cond, ProcessedSigma) | ProcessedRest ]
        ;
              /** While contains no solve context. */
             process_proc( RestProgram, ProcessedRest ),
             Processed = [ while(Cond, Sigma) | ProcessedRest ]
        ).

process_proc( [solve(Prog, Horizon, RewardFunction) | RestProgram],
              Processed ) :- !,
%        printf( stdout, "\n******* Encountered solve. ******\n", [] ),
        process_solve( Prog, Horizon, RewardFunction, ProcessedSolve ),
%        printf( stdout, "\n******* Rest_Program: %w. ******\n", [RestProgram] ),
        process_proc( RestProgram, ProcessedRest ),
%        printf( stdout, "\n******* ProcessedRest: %w. ******\n", [ProcessedRest] ),
        append( ProcessedSolve, ProcessedRest, Processed ).
%        printf( stdout, "\n******* Processed: %w. ******\n", [Processed] ).

process_proc( [Term | RestProgram], Processed ) :- 
%        prim_action(Term),
        !,
%        printf( stdout, "\n******* Encountered Term: %w. ******\n", [Term] ),
%        printf( stdout, "\n******* Rest_Program: %w. ******\n", [RestProgram] ),
        process_proc( RestProgram, ProcessedRest ),
        Processed = [ Term | ProcessedRest ].
%        printf( stdout, "\n******* Processed: %w. ******\n", [Processed] ).

/** If the proc only consists of a single action without [], we
 *  add those brackets. */
process_proc( Program, Processed ) :-
%        printf( stdout, "\n******* Encountered Single Action: %w. ******\n", [Program] ),
        Program \= [_Action],
        !,
        Processed = [Program].
%        NewProgram = [Program],
%        process_proc( NewProgram, Processed ).


process_proc_aux( ProcName, ProcBody, Stream ) :-        
        printf(Stream, "ipl_proc( %w, ", [ProcName]),
%        printf( stdout, "Processing ProcBody: %w\n", [ProcBody] ),
        /** Make the arguments of cout strings again. */
        fix_couts( ProcBody, ProcBodyNew ),
%        printf( stdout, "ProcBodyNew: %w\n", [ProcBodyNew] ),
%        printf( stdout, "process_proc( %w, Processed )\n", [ProcBodyNew] ),
        process_proc( ProcBodyNew, Processed ),
%        printf( stdout, "process_proc( %w, %w )\n", [ProcBodyNew, Processed] ),
%        /** cut away superfluous outer brackets [].
%         *  Doesn't work :/ replaces variables by 
%         *  Prolog placeholders... not nice. */
%        list_to_string( Processed, ProcessedClean ),
        /** write the transformed proc to the file */
        printf( Stream, "%w ).\n", [Processed] ).

process_all_proc( Stream ) :-
        findall( (ProcName, ProcBody),
                 proc(ProcName, ProcBody),
                 ProcList), !,
%        findall( ProcName,
%                 proc(ProcName, ProcBody),
%                 ProcNames), !,
%        printf( stdout, "All procs: %w\n", [ProcNames] ),
        process_all_proc_aux(ProcList, Stream).

process_all_proc_aux([], _Stream) :- !.
process_all_proc_aux([(ProcName, ProcBody)|List_rest], Stream) :-
%        printf( stdout, "process_proc_aux(%w, %w, Stream)\n", [ProcName, ProcBody] ),
        process_proc_aux( ProcName, ProcBody, Stream ), !,
        process_all_proc_aux( List_rest, Stream ).


/* ----------------------------------------------------------
   solve statements
--------------------------------------------------------- */

/** process_solve( Program, Horizon, RewardFunction )
 *  preprocesses a general solve statement of the form
 *  solve([alpha; nondet(p_1,...,p_n); omega], H, rew),
 *  where alpha is the deterministic part before the first
 *  nondet (if any) in the code,
 *  the p_i are deterministic programs,
 *  and omega is an arbitrary READYLOG program (not containing
 *  another solve).
 *  First the operator rho is applied to transform the
 *  argument list of the nondet to a set,
 *  then the operator tau_prime_H is applied until the omega
 *  is integrated into the nondet, leading to
 *  solve([alpha; nondet(q_1,...,q_m)], H, rew).
 *  Finally the operator tau is applied until the alpha is
 *  pulled out of the solve context, leading to
 *  alpha; solve(nondet(q_1,...,q_m), H, rew).
 */

process_solve( [], _Horizon, _RewardFunction, Processed ) :-
        Processed = [].

/** If the solve program only consists of a single action without [], we
 *  add those brackets. */
process_solve( Program, Horizon, RewardFunction, Processed ) :-
        Program \= [_Action],
        NewProgram = [Program],
        process_solve( NewProgram, Horizon, RewardFunction, Processed ).

/** If the solve program contains a call to a proc, we have to follow that trail.
 *  Otherwise we might loose a solve context. Consider this example:
 *  proc( procOne, [solve(procTwo, H, R)] ).
 *  proc( procTwo, [nondet[a,b]] ).
 *  The procTwo would be treated as a primitive action, pulled outside the solve
 *  and the solve would be deleted. */
process_solve( Program, Horizon, RewardFunction, Processed ) :-
%        printf( stdout, "Applying rho.\n", [] ),
        Initial_Rho_Program = [],
%        printf( stdout, "apply_rho(%w, [], Rho_Program)\n,", [Program] ),
        apply_rho( [solve(Program, Horizon, RewardFunction)],
                   Initial_Rho_Program, Rho_Program ),
%        printf(stdout, "Rho_Program: %w\n", [Rho_Program]),
        Initial_Tau_Prime_H_Program = [],
%        printf(stdout, "apply_tau_prime_H(%w, %w, Tau_Prime_H_Program)\n)",
%                       [Rho_Program, Initial_Tau_Prime_H_Program]),
	apply_tau_prime_H( Rho_Program, Initial_Tau_Prime_H_Program,
                           Tau_Prime_H_Program ),
%        printf(stdout, "Tau_Prime_H_Program: %w\n", [Tau_Prime_H_Program]),
        apply_tau( solve(Tau_Prime_H_Program, Horizon, RewardFunction),
                         Tau_Program ),
%        printf(stdout, "Tau_Program: %w", [Tau_Program]),
        Processed = Tau_Program.


/* ----------------------------------------------------------
   transformation operators
---------------------------------------------------------- */

/* ----------------------------------------------------------
                            rho                             
---------------------------------------------------------- */

/** rho finds the first nondeteministic statement
 *  (nondet, pickBest, or star) in the code. In case of
 *  pickBest or star, the operator transforms it into
 *  a nondet. Afterwards, the argument list of the nondet
 *  is transformed into a set.
 */

apply_rho( [], Program, Rho_Program ) :- !,
        Rho_Program = Program.

apply_rho( [solve(Prog, Horizon, RewardFunction)], Program,
           Rho_Program ) :- !,
        apply_rho(Prog, Program, Rho_Program_Tmp),
        Rho_Program = [solve(Rho_Program_Tmp, Horizon, RewardFunction)].

apply_rho( [{P_List} | Omega], Program, Rho_Program ) :- !,
        /** Rho had already been applied to this program before. Thus,
         *  do not change anything. */
        /** TODO: Check if this is correct, or if append has to be used.
         *  Also appears in other places! */
        Rho_Program = [ Program | [{P_List} | Omega] ].

apply_rho( [pickBest(F, Domain, Delta) | Omega], Program, Rho_Program ) :- !,
        findall( Delta_New,
                 /** Domain will be a list, correct? */
                 ( member(Value, Domain), 
                   replace_term( Delta, F, Value, Delta_New ) ),
                 Instantiated_Progs ),
%        printf( stdout, "Instantiated_Progs: %w.", [Instantiated_Progs]),
        list_to_string(Instantiated_Progs, Instantiated_Progs_String),
        Program_New = [ Program | {Instantiated_Progs_String} ],
        /** end recursion to make sure that only the first nondeterministic
         *  statement is replaced */
        Rho_Program = [ Program_New, Omega ].
               
apply_rho( [star(Alpha) | Omega], Program, Rho_Program ) :- !,
        /** we delay the listing of the programs [], Alpha, Alpha^2,...
         *  for later (when we will know the horizon), and now just put
         *  the star-statement inside the set brackets {}.
         *  It is important to store Omega with the star statement
         *  as in the following process programs q_i might be
         *  integrated into the choice set.
         */
        list_to_string([star(Alpha)], Star_String),
        Program_New = [ Program | {Star_String} ],
        /** end recursion to make sure that only the first nondeterministic
         *  statement is replaced */
        /** As we end the application of rho here, and the next operator
         *  apply_tau_prime_H is first matched with the star string in
         *  the nondeterministic set, we do not need to remember Omega
         *  now. */
        Rho_Program = [ Program_New | Omega ].

apply_rho( [if(Cond, Sigma1, Sigma2) | Omega], [], Rho_Program ) :- !,
        apply_rho( [if(Cond, Sigma1, Sigma2) | Omega], [DUMMY], Rho_Program_Tmp ),
        Rho_Program_Tmp = [DUMMY | Rho_Program].

% If Program = [], the result becomes "[ [] | if... ]". :(
% [--] Program_New = [ Program | if(Cond, Rho_Program_Sigma1, Rho_Program_Sigma2) ]
% Append works fine and leads to "[ if... ]"
% but append should be avoided.
% [--] append(Program, if(Cond, Rho_Program_Sigma1, Rho_Program_Sigma2), Program_New)
% This is why above there is a second predicate that matches Program = [].
apply_rho( [if(Cond, Sigma1, Sigma2) | Omega], Program, Rho_Program ) :- !,
        is_deterministic( Sigma1, Result1 ),
%        printf( stdout, "Sigma1 is deterministic is %w.\n", [Result1] ),
        is_deterministic( Sigma2, Result2 ),
%        printf( stdout, "Sigma2 is deterministic is %w.\n", [Result2] ),
        ( (Result1 = true, Result2 = true) ->
                           /** Skip if statement. */
                           Program_New = [ Program | if(Cond, Sigma1, Sigma2) ],
                           apply_rho( Omega, Program_New, Rho_Program_Tmp ),
                           Rho_Program = Rho_Program_Tmp
        ;
                           /** The first nondeterministic statement is found
                            *  inside Sigma1 or Sigma2. But as we do not allow
                            *  nested solve statements, the solve context inside
                            *  Sigma1 and Sigma2 will be independent of a possible
                            *  solve in Omega. That is why me may apply rho
                            *  recursively. */
                           ( (Result1 = true) ->
                                    apply_rho( Sigma2, [], Rho_Program_Sigma2 ),
                                    Program_New = [ Program | if(Cond, Sigma1, Rho_Program_Sigma2) ],
                                    apply_rho( Omega, Program_New, Rho_Program_Tmp ),
                                    Rho_Program = Rho_Program_Tmp
                           ;
                                    ( (Result2 = true) ->
                                             apply_rho( Sigma1, [], Rho_Program_Sigma1 ),
                                             Program_New = [ Program |
                                                             if(Cond, Rho_Program_Sigma1, Sigma2) ],
                                                    apply_rho( Omega, Program_New, Rho_Program_Tmp ),
                                             Rho_Program = Rho_Program_Tmp
                                    ;
                                             /** Both Sigma1 and Sigma2 are nondeterministic */
%                                             printf( Stream, "Both Sigma1 and Sigma2 are nondeterministic.\n", [] ),
                                             apply_rho( Sigma1, [], Rho_Program_Sigma1 ),
                                             apply_rho( Sigma2, [], Rho_Program_Sigma2 ),
                                             Program_New = [ Program |
                                                             if(Cond, Rho_Program_Sigma1, Rho_Program_Sigma2) ],
                                             apply_rho( Omega, Program_New, Rho_Program_Tmp ),
                                             Rho_Program = Rho_Program_Tmp
                                    )
                           )
        ).

apply_rho( [if(Cond, Sigma) | Omega], Program, Rho_Program ) :-
        apply_rho( [if(Cond, Sigma, []) | Omega], Program, Rho_Program ).

apply_rho( [while(Cond, Sigma) | Omega], Program, Rho_Program ) :- !,
        is_deterministic( Sigma, Result ),
%        printf( stdout, "Sigma is deterministic is %w.\n", [Result] ),
        ( Result = true -> /** Skip while. */
                           Program_New = [ Program | while(Cond, Sigma) ],
                           apply_rho( Omega, Program_New, Rho_Program_Tmp ),
                           Rho_Program = Rho_Program_Tmp
        ;
                           /** The first nondeterministic statement is found
                            *  inside Sigma. But as we do not allow nested solve
                            *  statements, the solve context inside Sigma will
                            *  be independent of a possible solve in Omega. That
                            *  is why me may apply rho recursively. */
                           apply_rho( Sigma, [], Rho_Program_Sigma ),
                           Program_New = [ Program | while(Cond, Rho_Program_Sigma) ],
                           apply_rho( Omega, Program_New, Rho_Program_Tmp ),
                           Rho_Program = Rho_Program_Tmp
        ).

apply_rho( [nondet(ProgList) | Omega], [], Rho_Program ) :- !,
%        printf(stdout, "ProgList: %w.\n", [ProgList]),
        list_to_string(ProgList, ReducedString),
%        printf(stdout, "ReducedString: %w.\n", [ReducedString]),
        Program_New = {ReducedString},
        /** end recursion to make sure that only the first nondet
          * is replaced */
        Rho_Program = [ Program_New | Omega ].

apply_rho( [nondet(ProgList) | Omega], Program, Rho_Program ) :- !,
%        printf(stdout, "ProgList: %w.\n", [ProgList]),
        list_to_string(ProgList, ReducedString),
%        printf(stdout, "ReducedString: %w.\n", [ReducedString]),
        Program_New = [ Program | {ReducedString} ],
        /** end recursion to make sure that only the first nondet
          * is replaced */
        Rho_Program = [ Program_New | Omega ].

/** Program is []. This is the case, when the Prog in the
 *  solve only consists of one Term. We want to avoid the
 *  Rho_Program to look like [ []|Term ]. */
apply_rho( [Term | Omega], [], Rho_Program ) :- !,
%        printf(stdout, "Program: %w\n", [Program]),
        Program_New = [ Term ],
%        printf(stdout, "Appending: [%w]\n", [Term]),        
%        printf(stdout, "Program_New: %w\n", [Program_New]),
%        printf(stdout, "Omega: %w\n", [Omega]),
        apply_rho( Omega, Program_New, Rho_Program_Tmp ),
%        printf( stdout, "Rho_Program_Tmp=%w\n", [Rho_Program_Tmp]),
        Rho_Program = Rho_Program_Tmp.

apply_rho( [Term | Omega], Program, Rho_Program ) :- !,
%        printf(stdout, "Program: %w\n", [Program]),
        Program_New = [ Program | Term ],
%        printf(stdout, "Appending: [%w]\n", [Term]),        
%        printf(stdout, "Program_New: %w\n", [Program_New]),
%        printf(stdout, "Omega: %w\n", [Omega]),
        apply_rho( Omega, Program_New, Rho_Program_Tmp ),
%        printf( stdout, "Rho_Program_Tmp=%w\n", [Rho_Program_Tmp]),
        Rho_Program = Rho_Program_Tmp.
%        printf(stdout, "Rho_Program_After_Recursion: %w\n", [Rho_Program]).


/* ----------------------------------------------------------
                        tau_prime_H                             
---------------------------------------------------------- */

/** tau_prime_H integrates the arbitrary program omega
 *  following the first nondet statement into the set of
 *  nondeterministic choices:
 *  solve([alpha; nondet(p_1,...,p_n); omega], H, rew)
 *  === tau_prime_H ===>
 *  solve([alpha; nondet(q_1,...,q_m)], H, rew).
 */

apply_tau_prime_H( [solve(Prog, Horizon, _RewardFunction)], Program,
                   Tau_Prime_H_Program ) :- !,
        apply_tau_prime_H( [Prog], Horizon, Program, Tau_Prime_H_Program ). 

/** omega = ... */

/** Empty program (without nondeterministic choice in solve context) */
apply_tau_prime_H( [], _Horizon, Program, Tau_Prime_H_Program ) :- !,
%        printf(stdout, "Encountered Empty Program.\n", []),
        Tau_Prime_H_Program = Program.

/** Star and pickBest are implicitly translated into nondet statements.
 *  Here they are then directly integrated into the nondeterministic
 *  decision set.
 */

/** Star */
/** Note that rho had postponed the expansion of the star that appeared as first
 *  nondeterministic statement, by putting it inside the nondeterministic set,
 *  together with the program Omega.
 *  Also note that, at this point, the nondeterministic set only consists of the
 *  "star". */
apply_tau_prime_H( [{Star} | Omega_Prime], Horizon, Program, Tau_Prime_H_Program ) :- 
         ( \+ string(Star) -> /** if Star is not a string yet,
                                * convert it to one */
                              term_string(Star, StarString)
         ;
                              /** else simply rename it */
                              StarString = Star
         ),
         /** Test, if Star = "star(Alpha)" */
         substring( StarString, 1, 4, "star" ),
         !,
%         printf( stdout, "Encountered star in nondeterministic choice set.\n", []),
%         printf( stdout, "StarString: %w\n", [StarString]),

         /** cut out argument of star */
         string_length(StarString, StarLength),
         ReducedStarLength is (StarLength - 6),
         substring( StarString, 6, ReducedStarLength, AlphaString ),
%         printf( stdout, "Alpha: %w\n", [AlphaString]),
         term_string(Alpha, AlphaString),
         
         /** Construct a list Tau_Prime_H_Program_Tmp of all possible programs
          *  [Alpha; Omega_Prime],
          *  [Alpha; Alpha; Omega_Prime],
          *  ...,
          *  [Alpha^k; Omega_Prime],
          *  where k is the first length of the Alpha sequence that is longer
          *  than the horizon. */
         Program_Initial = [],
%         printf( stdout, "expand_alpha(%w, %w, %w, [], Star_Program )\n",
%                 [Alpha, Omega, Horizon] ),
         horizon_consumption(Alpha, Consume),
         expand_alpha( Alpha, Consume, Omega_Prime, Horizon, Program_Initial,
                       Star_Program ),
%         printf( stdout, "Star_Program: %w\n", [Star_Program] ),

         /** Add the choice of Alpha^0 in the beginning.
          *  Omega_Prime is appended (implicitely) to the empty program as Omega_Prime
          *  must be executed afterwards. */
         list_to_string( Omega_Prime, Omega_PrimeS ),
         comma_to_semicolon( Omega_PrimeS, Omega_PrimeTmp ),
%         printf( stdout, "Omega_Prime with semicolon: %w\n", [Omega_PrimeTmp] ),
         string_to_list( Omega_PrimeTmp, Omega_PrimeTmpList ),
         append( Omega_PrimeTmpList, Star_Program, Star_Program_Tmp ),
%         printf( stdout, "Star_Program and Alpha^0: %w\n", [Star_Program_Tmp] ),

         list_to_string( Star_Program_Tmp, Star_P_List ),
%         printf( stdout, "Star_P_List: %w\n", [Star_P_List] ),
%         printf( stdout, "Omega_Prime: %w\n", [Omega_Prime] ),
         apply_tau_prime_H( [{Star_P_List} | []], Horizon, Program, Tau_Prime_H_Program ).

/** This is the case, where a star comes after the first nondeterministic statement */
apply_tau_prime_H( [{P_List} | [star(Alpha) | Omega_Prime]], Horizon,
                   Program, Tau_Prime_H_Program ) :-
         !, /** CUT is okay here, as we skip the step of converting star into a nondet
             *  and again applying tau_prime_H to that nondet.
             *  Instead we directly integrate the choices Alpha^0,...,Alpha^i into
             *  the choice set. */
%         printf( stdout, "Encountered star.\n", []),

         /** Construct a list Tau_Prime_H_Program_Tmp of all possible programs
          *  [Alpha; Omega_Prime],
          *  [Alpha; Alpha; Omega_Prime],
          *  ...,
          *  [Alpha^k; Omega_Prime],
          *  where k is the first length of the Alpha sequence that is longer
          *  than the horizon. */
         /** TODO: Different to the star case before, the p_i in the P_List may
          *  already consume horizon. To enhance performance this can be considered
          *  when expanding Alpha. A naive approach would be to subtract the minimal
          *  horizon consumption of all p_i. */
         Program_Initial = [],
         horizon_consumption(Alpha, Consume),
         expand_alpha( Alpha, Consume, Omega_Prime, Horizon, Program_Initial,
                       Star_Program ),
%         apply_tau_prime_H_aux( [{P_List} | [star(Alpha) | Omega_Prime]],
%                                Horizon, Program_Initial, Tau_Prime_H_Program_Tmp ),
         /** Add the choice of Alpha^0 in the beginning.
          *  Omega_Prime is appended (implicitely) to the empty program as Omega_Prime
          *  must be executed afterwards. */
         list_to_string( Omega_Prime, Omega_PrimeS ),
         comma_to_semicolon( Omega_PrimeS, Omega_PrimeTmp ),
         string_to_list( Omega_PrimeTmp, Omega_PrimeTmpList ),
         append( Omega_PrimeTmpList, Star_Program, Star_Program_Tmp ),
         
         /** Integrate the complete rest program into the nondeterministic set. */
         integrate( {P_List}, Star_Program_Tmp, {P_List_New} ),

         apply_tau_prime_H( [{P_List_New} | []], Horizon, Program, Tau_Prime_H_Program ).

/** PickBest */
apply_tau_prime_H( [{P_List} | [pickBest(F, Domain, Delta) | Omega_Prime]], Horizon,
                   Program, Tau_Prime_H_Program ) :-
        !, 
        findall( Delta_New,
                 /** Domain will be a list, correct? */
                 ( member(Value, Domain), 
                   replace_term( Delta, F, Value, Delta_New ) ),
                 Instantiated_Progs ),
%        printf( stdout, "Instantiated_Progs: %w.", [Instantiated_Progs]),
%        list_to_string(Instantiated_Progs, Instantiated_Progs_String),
         integrate({P_List}, Instantiated_Progs, {P_List_New}), 
         apply_tau_prime_H( [{P_List_New} | Omega_Prime], Horizon, Program, Tau_Prime_H_Program ).

apply_tau_prime_H( [{P_List} | []], Horizon, [], Tau_Prime_H_Program ) :- !,
        apply_tau_prime_H( [{P_List} | []], Horizon, [DUMMY], Tau_Prime_H_Program_Tmp ),
        Tau_Prime_H_Program_Tmp = [DUMMY | Tau_Prime_H_Program].

/** Empty program (with nondeterministic choice in solve context) */
apply_tau_prime_H( [{P_List} | []], _Horizon, Program, Tau_Prime_H_Program ) :- !,
%        printf(stdout, "Encountered Empty Program.\n", []),
        /** To enhance readability we put in some brackets
         *  around the programs to choose from */
        concat_string(["[", P_List, "]"], Tmp1),
        replace_string(Tmp1, " ", "", Tmp2 ),
        replace_string(Tmp2, ",", "], [", Tmp3 ),
        /** Before integration, we exchanged commas in tests,
         *  while statements, and if statements by placeholders.
         *  Now substitute them back. */
        replace_string(Tmp3, "__COMMA__", ",", P_List_Clean),
        /** TODO: sort P_List */
        Program_New = [ Program | nondet(P_List_Clean) ],
        Tau_Prime_H_Program = Program_New.

/** Conditional */
apply_tau_prime_H( [{P_List} | [if(Cond, Sigma1, Sigma2) | Omega_Prime]],
                   Horizon, [], Tau_Prime_H_Program ) :- !,
        apply_tau_prime_H( [{P_List} | [if(Cond, Sigma1, Sigma2) | Omega_Prime]],
                           Horizon, [DUMMY], Tau_Prime_H_Program_Tmp ),
        Tau_Prime_H_Program_Tmp = [DUMMY | Tau_Prime_H_Program].

apply_tau_prime_H( [{P_List} | [if(Cond, Sigma1, Sigma2) | Omega_Prime]],
                   Horizon, Program, Tau_Prime_H_Program ) :- !,
%        printf( stdout, "integrate({%w}, %w, P_List_New1) ", [P_List, [? | (Cond)]]), 
        integrate({P_List}, [?(Cond)], {P_List_New1}), 
%        printf( stdout, "P_List_New1: %w", [P_List_New1]), 
        integrate({P_List}, [?(not(Cond))], {P_List_New2}),
%        printf( stdout, "P_List_New2: %w", [P_List_New2]),
        apply_tau_prime_H( [{P_List_New1} | [Sigma1 | Omega_Prime]], Horizon, Program,
                           Tau_Prime_H_Program1 ),
        apply_tau_prime_H( [{P_List_New2} | [Sigma2 | Omega_Prime]], Horizon, Program,
                           Tau_Prime_H_Program2 ),
        /** cut away nondet() in resulting programs */
        list_to_string( Tau_Prime_H_Program1, P_String1 ),
        string_length( P_String1, Length1 ),
        ReducedLength1 is (Length1 - 10),
        substring( P_String1, 9, ReducedLength1, Final_P_String1),
        list_to_string( Tau_Prime_H_Program2, P_String2 ),
        string_length( P_String2, Length2 ),
        ReducedLength2 is (Length2 - 10),
        substring( P_String2, 9, ReducedLength2, Final_P_String2),
        join_prog_lists( {Final_P_String1}, {Final_P_String2}, {P_String_Joined} ),
        /** TODO: sort P_String_Joined */
        Program_New = [ Program | nondet(P_String_Joined) ],
        Tau_Prime_H_Program = Program_New.

apply_tau_prime_H( [{P_List} | [if(Cond, Sigma) | Omega_Prime]],
                   Horizon, Program, Tau_Prime_H_Program ) :- !,
        apply_tau_prime_H( [{P_List} | [if(Cond, Sigma, []) | Omega_Prime]],
                           Horizon, Program, Tau_Prime_H_Program ).

/** Loop */
apply_tau_prime_H( [{P_List} | [while(Cond, Sigma) | Omega_Prime]],
                   Horizon, Program, Tau_Prime_H_Program ) :- !,
%         printf( stdout, "Loop encountered\n", [] ),
         Program_Initial = [],
         Alpha = [?(Cond) | Sigma],
         /** Use neg as negation, because not destroys the brackets around the
          *  condition. Correct? */
         Ending = [?(neg(Cond));Omega_Prime],
         /** Construct a list of all sequences of condition tests and Sigma. Also
          *  append the failing condition and Omega_Prime to each program in
          *  the list. */
         horizon_consumption(Alpha, Consume),
         expand_alpha( Alpha, Consume, Ending, Horizon, Program_Initial, Star_Program ),
%         printf( stdout, "Star_Program: %w\n", [Star_Program] ),
         term_string( Cond, CondS ),
         concat_string( ["?(neg(", CondS, "))"], NegCondTestS ),
         /** Add the choice of [?(not(Cond)); Omega_Prime] in the beginning. */
         list_to_string( Omega_Prime, Omega_PrimeS ),
         concat_string( [NegCondTestS, ";", Omega_PrimeS], Tmp1S ),
         string_to_list( Tmp1S, Tmp2 ),
         Star_Program_Tmp = [ Tmp2 | Star_Program ],
         /** Integrate the complete rest program into the nondeterministic set. */
         integrate( {P_List}, Star_Program_Tmp, {P_List_New} ),

         apply_tau_prime_H( [{P_List_New} | []], Horizon, Program, Tau_Prime_H_Program ).

/** Nondeterministic Choice */
apply_tau_prime_H( [{P_List} | [nondet(ArgList) | Omega_Prime]], Horizon,
                   Program, Tau_Prime_H_Program ) :- !,
        %printf( stdout, "nondet encountered\n", []),
        integrate({P_List}, ArgList, {P_List_New}), 
        apply_tau_prime_H( [{P_List_New} | Omega_Prime], Horizon, Program, Tau_Prime_H_Program ).

/** Test Action, Primitive Action, or Stochastic Action */
apply_tau_prime_H( [{P_List} | [Term | Omega_Prime]], Horizon, Program,
                   Tau_Prime_H_Program ) :- !,
%        printf(stdout, "Encountered Primitive Action.\n", []),
%        printf(stdout, "Integrating %w into %w.\n", [Term, {P_List}]),
        integrate({P_List}, [Term], {P_List_New}),
%        printf(stdout, "New Program List: %w.\n", [{P_List_New}]),
        apply_tau_prime_H( [{P_List_New} | Omega_Prime], Horizon, Program, Tau_Prime_H_Program ).

apply_tau_prime_H( [if(Cond, Sigma1, Sigma2) | Omega], Horizon, Program,
                   Tau_Prime_H_Program ) :- !,
        /** As Sigma1 or Sigma2 might contain nondeterministic programs,
         *  we have to apply tau_prime_H to them.
         *  TODO: For optimisation, we could test, if they contain { }
         *  and otherwise just skip them. */
%        printf( stdout, "apply_tau_prime_H( %w, %w, [], Tau_Prime_H_Program_Sigma1)\n", [Sigma1, Horizon]),
        apply_tau_prime_H( Sigma1, Horizon, [], Tau_Prime_H_Program_Sigma1 ),
%        printf( stdout, "Tau_Prime_H_Program_Sigma1: %w\n", [Tau_Prime_H_Program_Sigma1]),
        apply_tau_prime_H( Sigma2, Horizon, [], Tau_Prime_H_Program_Sigma2 ),
        apply_tau_prime_H( Omega, Horizon, Program, Tau_Prime_H_Program_Omega ),
        Tau_Prime_H_Program = [ if(Cond, Tau_Prime_H_Program_Sigma1,
                                          Tau_Prime_H_Program_Sigma2) |
                                Tau_Prime_H_Program_Omega ].

/** A single action or proc */
apply_tau_prime_H( [Alpha | []], _Horizon, _Program,
                   Tau_Prime_H_Program ) :- !,
        Tau_Prime_H_Program = Alpha.

/** Ignore (but store) everything before the first nondet,
 *  that means, ignore everything not starting with a
 *  set of programs.
 *  Note that if and while statements containing solve
 *  statements have been handled differently before. */
apply_tau_prime_H( [Alpha | Omega], Horizon, Program,
                   Tau_Prime_H_Program ) :- !,
%        printf(stdout, "Encountered alpha = %w.\n", [Alpha]),
%        printf(stdout, "apply_tau_prime_H( %w, %w, %w, %w ).\n",
%                       [[{P_List} | Omega], Horizon, Program,
%                       Tau_Prime_H_Program]),
        apply_tau_prime_H( Omega, Horizon, Program, Tau_Prime_H_Program_Nondet ),
        Tau_Prime_H_Program = [ Alpha | Tau_Prime_H_Program_Nondet ].


         
/* ----------------------------------------------------------
                            tau                             
---------------------------------------------------------- */

/** tau pulls out the deterministic program alpha that
 * comes before the first nondeterministic statement
 * (nondet, pickBest, star) in the solve context:
 * solve([alpha; nondet(q_1,...,q_m)], H, rew)
 * === tau ===>
 * alpha; solve([nondet(q_1,...,q_m)], H, rew).
 */

/** Empty Program */
apply_tau(solve([], _Horizon, _RewardFunction), Tau_Program ) :- !,
%        printf(stdout, "Empty solve reached!\n", []),
        Tau_Program = [].

/** Zero Horizon */
apply_tau(solve(_Anything, Horizon, _RewardFunction), Tau_Program ) :-
        /* DO NEVER PUT A CUT HERE, OR THE NONDET WILL NOT BE RE-TRIED */ 
%        printf(stdout, "CHECKING HORIZON, Horizon: %w\n", [Horizon]),
        Horizon =< 0,
        /** use CUT to say: Horizon > 0 in all following clauses */
        /** TODO: doesn't work... why? */
        !,
%        printf(stdout, "Horizon less/equal 0\n", []),
        Tau_Program = [].
%        printf(stdout, "Tau_Program: %w\n", [Tau_Program]).

/** Nondeterministic Choice */
apply_tau(solve([nondet(P_List)], Horizon, RewardFunction), Tau_Program ) :- !, 
%        printf(stdout, "nondet encountered!\n", []),
        Tau_Program = [solve([nondet([P_List])], Horizon, RewardFunction)].

/** Conditional */
apply_tau(solve([if(Cond, Sigma1, Sigma2) | Omega], Horizon, RewardFunction),
          Tau_Program ) :- !,
%        printf(stdout, "if condition encountered, Horizon: %w\n", [Horizon]),
%        printf(stdout, "apply_tau(solve([%w | Omega], %w, RewardFunction), %w )\n",
%               [Sigma1, Horizon, Tau_Program_Sigma1]),
        apply_tau(solve([Sigma1 | Omega], Horizon, RewardFunction), Tau_Program_Sigma1),
%        printf(stdout, "\n ********* Tau_Program_Sigma1: %w\n", [Tau_Program_Sigma1]),
        apply_tau(solve([Sigma2 | Omega], Horizon, RewardFunction), Tau_Program_Sigma2),
        ( (Tau_Program_Sigma1 = [], Tau_Program_Sigma2 = [] ) ->
                /** optimise readability by leaving away if-clause */
                Tau_Program = []
        ;
                list_to_string(Tau_Program_Sigma1, Sigma1_Tmp),
                list_to_string(Tau_Program_Sigma2, Sigma2_Tmp),
                /** replace the " around the strings by [] */
                remove_character( Sigma1_Tmp, "\"", Sigma1_Final ),
                remove_character( Sigma2_Tmp, "\"", Sigma2_Final ),

                /** put together both sub-results */
                term_string(Cond, CondString),
                concat_string( ["if( ", CondString, ", [", Sigma1_Final,
                                "], [", Sigma2_Final, "] )"], FinalString ),
                string_to_list(FinalString, Tau_Program)
        ).

apply_tau(solve([if(Cond, Sigma) | Omega], Horizon, RewardFunction), Tau_Program ) :- !,
        apply_tau(solve([if(Cond, Sigma, []) | Omega], Horizon, RewardFunction),
                  Tau_Program ).

/** Loop */
apply_tau(solve([while(Cond, Sigma) | Omega], Horizon, RewardFunction), Tau_Program ) :- !,
%        printf(stdout, "loop before nondet encountered\n", []),
        apply_tau(solve([Sigma | [while(Cond, Sigma) | Omega]], Horizon, RewardFunction),
                  Tau_Program1),
        apply_tau(solve(Omega, Horizon, RewardFunction), Tau_Program2),
        ( (Tau_Program1 = [], Tau_Program2 = [] ) ->
                /** optimise readability by leaving away if-clause */
                Tau_Program = []
        ;
                list_to_string(Tau_Program1, Tau1_Final),
                list_to_string(Tau_Program2, Tau2_Final),

                /** put together both sub-results */
                term_string(Cond, CondString),
                concat_string( ["if( ", CondString, ", [", Tau1_Final,
                                "], [", Tau2_Final, "] )"], FinalString ),
                string_to_list(FinalString, Tau_Program)
        ).

/** Test Action */
apply_tau(solve([?(Term) | Omega], Horizon, RewardFunction), Tau_Program ) :- !,
%        printf(stdout, "test action encountered\n", []),
        /** Note that the test action does not consume horizon. */
        apply_tau(solve(Omega, Horizon, RewardFunction), Tau_Program_Tmp),
        Tau_Program = [ ?(Term) | Tau_Program_Tmp ].

/** If the solve program contains a call to a proc, we have to follow that trail, and
 *  recursively make sure that the proc is purely deterministic before pulling it out.
 *  Otherwise we might loose a solve context. Consider this example:
 *  proc( procOne, [solve(procTwo, H, R)] ).
 *  proc( procTwo, [nondet[a,b]] ).
 *  The procTwo would be treated as a primitive action, pulled outside the solve
 *  and the solve would be deleted. */
apply_tau(solve([ProcName | Omega], Horizon, RewardFunction), Tau_Program ) :- 
%        printf( stdout, "\nEncountered Proc. ProcName: %w\n", [ProcName] ),
        proc( ProcName, ProcBody ), !,
%        printf( stdout, "ProcBody: %w is deterministic is ", [ProcBody] ),
        is_deterministic( ProcBody, Result ),
%        printf( stdout, "%w\n", [Result] ),
        ( Result = true -> /** Proc is deterministic */
                           horizon_consumption( ProcBody, Consume ),
                           Horizon_New is (Horizon - Consume),
                           apply_tau(solve(Omega, Horizon_New, RewardFunction),
                                     Tau_Program_Tmp),
                           ( Horizon > 0 ->
                                     Tau_Program = [ ProcName | Tau_Program_Tmp ]
                           ;
                                     Tau_Program = Tau_Program_Tmp
                           )
        ;
                           /** Proc is nondeterministic */
                           /** We leave Proc untouched, and do not extract potential
                            *  deterministic parts. */
                           Tau_Program = [solve([ProcName | Omega], Horizon, RewardFunction)]
        ).

/** Primitive Action, Stochastic Action */
apply_tau(solve([Term | Omega], Horizon, RewardFunction), Tau_Program ) :- !,
%        printf(stdout, "simple/stochastic action %w encountered\n", [Term]),
        Horizon_New is (Horizon - 1),
%        printf(stdout, "apply_tau(solve(%w, %w, RewardFunction), Tau_Program_Tmp)\n", [Omega, Horizon_New]),
        apply_tau(solve(Omega, Horizon_New, RewardFunction), Tau_Program_Tmp),
%        printf(stdout, "apply_tau(solve(Omega, %w, RewardFunction), Tau_Program_Tmp) ***SUCCEEDED***\n", [Horizon_New]),
        ( Horizon > 0 ->
              Tau_Program = [ Term | Tau_Program_Tmp ]
        ;
%              printf(stdout, "This should never happen, because of CUT above!\n", []),
              Tau_Program = Tau_Program_Tmp
        ).

% }}}


/* ==========================================================
   MAIN
========================================================== */
% {{{ MAIN
% >>>>

iplpreprocess( File ) :- iplpreprocess( File, _NewFile, true, 0).
/** TODO: Last needed? */
iplpreprocess( File, NewFile, _Last, Level ) :-
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
	/* -- <MAIN PART> -- */
        process_all_proc(Stream),
	/* -- </MAIN PART> -- */
	/* -- <LAST_FILE PART> -- */
%	(
%	  Last ->
%	  generate_events_list( Stream )
%	;
%	  true
%	),
	/* -- </LAST_FILE PART> -- */
%        read(FileStream, BufferVar),
%        write(Stream, BufferVar),
	close(Stream),
	erase_rhomb(TmpFile, NewFile),
	cputime(Tend),
	Tdiff is Tend - Tbegin,
	printf("\n\t++ processing        : DONE (%w sec.)\n", [Tdiff]),
	printf("\t++ output written to : ", []),
	printf("%w\n\n", [NewFile]), flush(output). 

write_header(Stream) :-
	printf(Stream, "/*******************************\n", []),
	printf(Stream, "* file generated from ReadyLog *\n", []),
	printf(Stream, "*******************************/\n", []),
	printf(Stream, "\n", []),
	printf(Stream, ":- pragma(nodebug).\n", []).

erase_rhomb(FileIn, FileOut) :-
	concat_string( ["tr -d '#' < ", FileIn, " > ", FileOut],
		       TRString),
	system( TRString),
	concat_string( ["rm ", FileIn], RMString),
	system( RMString).

runall(M, M).
runall(N, M) :-
	N < M, !,
	printf("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",[]),
	argv(N, FileName),
	Level is N - 3,
	(
	  N =:= M-1 ->
	  /* notice when last file is processed: here some
	  extra stuff is added */
	  iplpreprocess(FileName, NewFileName, true, Level)
	;
	  iplpreprocess(FileName, NewFileName, false, Level)
	),
	/* compile the lately processed output before
	stepping over to next level */
        /** TODO: Will be forgotten, if compiled here. Compile
         *  in preprocessor instead! */
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

:- autorun.
