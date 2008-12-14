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
 *           $Id: iplpreprocessor.pl 01 2008-11-20 11:55:04Z dp $
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

:- ensure_loaded('readylog.pl').
:- ensure_loaded("../../utils/utils.pl").

/* ----------------------------------------------------------
   procedures
---------------------------------------------------------- */
% {{{ procedures
% >>>>

process_proc( [solve(Prog, Horizon, RewardFunction) | RestProgram], Stream ) :-
        process_solve( Prog, Horizon, RewardFunction, Stream ),
        process_proc( RestProgram, Stream ).

process_proc( Proc, Stream ) :-
%         printf( Stream, "SEARCHING FOR SUBSTRING\n", []),
%         type_of( ProcString, X ),
%         printf( Stream, "ProcString: %w is of type %w.\n", [ProcString, X]),
%         printf( Stream, "ProcLength: %w\n", [ProcLength]),
         term_string( Proc, ProcString ),
         ( substring( ProcString, "solve", SolvePos ) ->
                string_length(ProcString, ProcLength),
%               printf( Stream, "SolvePos: %w\n", [SolvePos]),
                BeforeLength is SolvePos - 1,
                substring( ProcString, 1, BeforeLength, BeforeString ),
                RestLength is 1+(ProcLength-SolvePos),
                substring( ProcString, SolvePos, RestLength, SolveAndAfterString ),
                printf( Stream, "%w", [BeforeString]),
                /** Using term_string(-,+) cut away the rest of the (Prolog term) proc,
                 *  so that after a type conversion it would match the string
                 *  SolveAndAfterString. This way we keep the term structure.
                 *  TODO: To get a Prolog term, we have to add "[". This currently
                 *  leads to superfluous left brackets after the solve context. */
                concat_strings( "[", SolveAndAfterString, SolveAndAfterStringPlusBracket ),
                term_string( RestProc, SolveAndAfterStringPlusBracket ),
                process_proc( RestProc, Stream )
            ;
                printf( Stream, "%w", [ProcString] )
         ).

process_proc( ProcName, ProcBody, Stream ) :-        
        printf(Stream, "prolog_ipl_proc( %w ).\n", [ProcName]),
        printf(Stream, "prolog_ipl_proc( %w ) :-", [ProcName] ),
        process_proc( ProcBody, Stream ),
        printf(Stream, ".\n", []).

process_all_proc( Stream ) :-
        findall( (ProcName, ProcBody),
                 proc(ProcName, ProcBody),
                 ProcList), !,
        process_all_proc_aux(ProcList, Stream).

process_all_proc_aux([], _Stream) :- !.
process_all_proc_aux([(ProcName, ProcBody)|List_rest], Stream) :-
        process_proc( ProcName, ProcBody, Stream), !,
        process_all_proc_aux(List_rest, Stream).

/* ----------------------------------------------------------
   solve statements
--------------------------------------------------------- */

/** process_solve( Program, Horizon, RewardFunction, Stream )
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

process_solve( [], _Horizon, _RewardFunction, _Stream ).

process_solve( Program, Horizon, RewardFunction, Stream ) :-
%        printf( Stream, "Applying rho.\n", [] ),
        Initial_Rho_Program = [],
        apply_rho( Program, Initial_Rho_Program, Rho_Program, Stream ),
%       printf( Stream, "rho successfully applied.\n", [] ),
%        printf(Stream, "%w\n", [Rho_Program]),
        Initial_Tau_Prime_H_Program = [],
%        printf(Stream, "apply_tau_prime_H(%w, %w, %w, %w, %w\n)",
%                       [Rho_Program, Horizon, Initial_Tau_Prime_H_Program,
%                        Tau_Prime_H_Program, Stream]),
	apply_tau_prime_H( Rho_Program, Horizon, Initial_Tau_Prime_H_Program,
                           Tau_Prime_H_Program, Stream ),
%        printf(Stream, "%w\n", [Tau_Prime_H_Program]),
        apply_tau( solve(Tau_Prime_H_Program, Horizon, RewardFunction),
                         Tau_Program, Stream),
%        printf(Stream, "solve(%w, %w, %w)", [Tau_Program, Tau_Horizon,
%               RewardFunction]).
        printf(Stream, "%w", [Tau_Program]).

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

apply_rho( [], Program, Rho_Program, _Stream ) :- !,
        Rho_Program = Program.

apply_rho( [pickBest(F, Domain, Delta) | Omega], Program, Rho_Program, Stream ) :- !,
        findall( Delta_New,
                 /** Domain will be a list, correct? */
                 ( member(Value, Domain), 
                   replace_term( Delta, F, Value, Delta_New, Stream ) ),
                 Instantiated_Progs ),
%        printf( Stream, "Instantiated_Progs: %w.", [Instantiated_Progs]),
        list_to_string(Instantiated_Progs, Instantiated_Progs_String),
        append(Program, [{Instantiated_Progs_String}], Program_New),
        /** end recursion to make sure that only the first nondeterministic
         *  statement is replaced */
        append(Program_New, Omega, Rho_Program).
               
apply_rho( [star(Alpha) | Omega], Program, Rho_Program, Stream ) :- !,
        /** we delay the listing of the programs [], Alpha, Alpha^2,...
         *  for later (when we will know the horizon), and now just put
         *  the star-statement inside the set brackets {}.
         *  It is important to store Omega with the star statement
         *  as in the following process programs q_i might be
         *  integrated into the choice set.
         */
        list_to_string([star(Alpha)], Star_String),
        append(Program, [{Star_String}], Program_New),
        /** end recursion to make sure that only the first nondeterministic
         *  statement is replaced */
        /** As we end the application of rho here, and the next operator
         *  apply_tau_prime_H is first matched with the star string in
         *  the nondeterministic set, we do not need to remember Omega
         *  now. */
        append(Program_New, Omega, Rho_Program).

apply_rho( [nondet(ProgList) | Omega], Program, Rho_Program, Stream ) :- !,
%        printf(Stream, "ProgList: %w.\n", [ProgList]),
        list_to_string(ProgList, ReducedString),
%        printf(Stream, "ReducedString: %w.\n", [ReducedString]),
        append(Program, [{ReducedString}], Program_New),
        /** end recursion to make sure that only the first nondet
          * is replaced */
        append(Program_New, Omega, Rho_Program).
%        apply_rho( Omega, Program_New, Rho_Program, Stream ).

apply_rho( [Term | Omega], Program, Rho_Program, Stream ) :- !,
%        printf(Stream, "Program: %w\n", [Program]),
        append(Program, [Term], Program_New),
%        printf(Stream, "Appending: [%w]\n", [Term]),        
%        printf(Stream, "Program_New: %w\n", [Program_New]),
%        printf(Stream, "Omega: %w\n", [Omega]),
        apply_rho( Omega, Program_New, Rho_Program_Tmp, Stream ),
%        printf( Stream, "Rho_Program_Tmp=%w\n", [Rho_Program_Tmp]),
        Rho_Program = Rho_Program_Tmp.
%        printf(Stream, "Rho_Program_After_Recursion: %w\n", [Rho_Program]).


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


/** omega = ... */

/** Empty program (without nondeterministic choice in solve context) */
apply_tau_prime_H( [], _Horizon, Program,
                   Tau_Prime_H_Program, _Stream ) :- !,
%        printf(Stream, "Encountered Empty Program.\n", []),
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
apply_tau_prime_H( [{Star} | Omega_Prime], Horizon,
                   Program, Tau_Prime_H_Program, Stream ) :- 
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
%         printf( Stream, "Encountered star in nondeterministic choice set.\n", []),
%         printf( Stream, "StarString: %w\n", [StarString]),

         /** cut out argument of star */
         string_length(StarString, StarLength),
         ReducedStarLength is (StarLength - 6),
         substring( StarString, 6, ReducedStarLength, AlphaString ),
%         printf( Stream, "Alpha: %w\n", [AlphaString]),
         term_string(Alpha, AlphaString),
         
         /** Construct a list Tau_Prime_H_Program_Tmp of all possible programs
          *  [Alpha; Omega_Prime],
          *  [Alpha; Alpha; Omega_Prime],
          *  ...,
          *  [Alpha^k; Omega_Prime],
          *  where k is the first length of the Alpha sequence that is longer
          *  than the horizon. */
         Program_Initial = [],
%         printf( Stream, "expand_alpha(%w, %w, %w, [], Star_Program, Stream)\n",
%                 [Alpha, Omega, Horizon] ),
         expand_alpha( Alpha, Omega_Prime, Horizon, Program_Initial,
                       Star_Program, Stream ),
%         printf( Stream, "Star_Program: %w\n", [Star_Program] ),

         /** Add the choice of Alpha^0 in the beginning.
          *  Omega_Prime is appended (implicitely) to the empty program as Omega_Prime
          *  must be executed afterwards. */
         list_to_string( Omega_Prime, Omega_PrimeS ),
         comma_to_semicolon( Omega_PrimeS, Omega_PrimeTmp, Stream ),
%         printf( Stream, "Omega_Prime with semicolon: %w\n", [Omega_PrimeTmp] ),
         string_to_list( Omega_PrimeTmp, Omega_PrimeTmpList ),
         append( Omega_PrimeTmpList, Star_Program, Star_Program_Tmp ),
%         printf( Stream, "Star_Program and Alpha^0: %w\n", [Star_Program_Tmp] ),

         list_to_string( Star_Program_Tmp, Star_P_List ),
%         printf( Stream, "Star_P_List: %w\n", [Star_P_List] ),
%         printf( Stream, "Omega_Prime: %w\n", [Omega_Prime] ),
         apply_tau_prime_H( [{Star_P_List} | []], Horizon, Program,
                            Tau_Prime_H_Program, Stream ).

/** This is the case, where a star comes after the first nondeterministic statement */
apply_tau_prime_H( [{P_List} | [star(Alpha) | Omega_Prime]], Horizon,
                   Program, Tau_Prime_H_Program, Stream ) :-
         !, /** CUT is okay here, as we skip the step of converting star into a nondet
             *  and again applying tau_prime_H to that nondet.
             *  Instead we directly integrate the choices Alpha^0,...,Alpha^i into
             *  the choice set. */
%         printf( Stream, "Encountered star.\n", []),

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
         expand_alpha( Alpha, Omega_Prime, Horizon, Program_Initial, Star_Program, Stream ),
%         apply_tau_prime_H_aux( [{P_List} | [star(Alpha) | Omega_Prime]],
%                                Horizon, Program_Initial, Tau_Prime_H_Program_Tmp,
%                                Stream ),
         /** Add the choice of Alpha^0 in the beginning.
          *  Omega_Prime is appended (implicitely) to the empty program as Omega_Prime
          *  must be executed afterwards. */
         list_to_string( Omega_Prime, Omega_PrimeS ),
         comma_to_semicolon( Omega_PrimeS, Omega_PrimeTmp, Stream ),
         string_to_list( Omega_PrimeTmp, Omega_PrimeTmpList ),
         append( Omega_PrimeTmpList, Star_Program, Star_Program_Tmp ),
         
         /** Integrate the complete rest program into the nondeterministic set. */
         integrate( {P_List}, Star_Program_Tmp, {P_List_New}, Stream ),

         apply_tau_prime_H( [{P_List_New} | []],
	                    Horizon, Program, Tau_Prime_H_Program, Stream ).

/** PickBest */
apply_tau_prime_H( [{P_List} | [pickBest(F, Domain, Delta) | Omega_Prime]], Horizon,
                   Program, Tau_Prime_H_Program, Stream ) :-
        !, 
        findall( Delta_New,
                 /** Domain will be a list, correct? */
                 ( member(Value, Domain), 
                   replace_term( Delta, F, Value, Delta_New, Stream ) ),
                 Instantiated_Progs ),
%        printf( Stream, "Instantiated_Progs: %w.", [Instantiated_Progs]),
%        list_to_string(Instantiated_Progs, Instantiated_Progs_String),
         integrate({P_List}, Instantiated_Progs, {P_List_New}, Stream), 
         apply_tau_prime_H( [{P_List_New} | Omega_Prime], Horizon, Program,
                           Tau_Prime_H_Program, Stream ).


/** Empty program (with nondeterministic choice in solve context) */
apply_tau_prime_H( [{P_List} | []], _Horizon, Program,
                   Tau_Prime_H_Program, _Stream ) :- !,
%        printf(Stream, "Encountered Empty Program.\n", []),
        /** To enhance readability we put in some brackets
         *  around the programs to choose from */
        concat_strings("[", P_List, Tmp1),
        concat_strings(Tmp1, "]", Tmp2),
        replace_string(Tmp2, " ", "", Tmp3, Stream),
        replace_string(Tmp3, ",", "], [", Tmp4, Stream),
        /** Before integration, we exchanged commas in tests,
         *  while statements, and if statements by placeholders.
         *  Now substitute them back. */
        replace_string(Tmp4, "__COMMA__", ",", P_List_Clean, Stream),
        /** TODO: sort P_List */
        append(Program, [nondet(P_List_Clean)], Program_New),
        Tau_Prime_H_Program = Program_New.

/** Conditional */
apply_tau_prime_H( [{P_List} | [if(Cond, Sigma1, Sigma2) | Omega_Prime]],
                   Horizon, Program, Tau_Prime_H_Program, Stream ) :- !,
%        printf( Stream, "integrate({%w}, %w, P_List_New1) ", [P_List, [? | (Cond)]]), 
        integrate({P_List}, [?(Cond)], {P_List_New1}, Stream), 
%        printf( Stream, "P_List_New1: %w", [P_List_New1]), 
        integrate({P_List}, [?(not(Cond))], {P_List_New2}, Stream),
%        printf( Stream, "P_List_New2: %w", [P_List_New2]),
        apply_tau_prime_H( [{P_List_New1} | [Sigma1 | Omega_Prime]], Horizon, Program,
                           Tau_Prime_H_Program1, Stream ),
        apply_tau_prime_H( [{P_List_New2} | [Sigma2 | Omega_Prime]], Horizon, Program,
                           Tau_Prime_H_Program2, Stream ),
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
        append(Program, [nondet(P_String_Joined)], Program_New),
        Tau_Prime_H_Program = Program_New.

apply_tau_prime_H( [{P_List} | [if(Cond, Sigma) | Omega_Prime]],
                   Horizon, Program, Tau_Prime_H_Program, Stream ) :- !,
        apply_tau_prime_H( [{P_List} | [if(Cond, Sigma, []) | Omega_Prime]],
                           Horizon, Program, Tau_Prime_H_Program, Stream ).

/** Loop */
apply_tau_prime_H( [{P_List} | [while(Cond, Sigma) | Omega_Prime]],
                   Horizon, Program, Tau_Prime_H_Program, Stream ) :- !,
%         printf( Stream, "Loop encountered\n", [] ),
         Program_Initial = [],
         Alpha = [?(Cond) | Sigma],
         /** Use neg as negation, because not destroys the brackets around the
          *  condition. Correct? */
         Ending = [?(neg(Cond));Omega_Prime],
         /** Construct a list of all sequences of condition tests and Sigma. Also
          *  append the failing condition and Omega_Prime to each program in
          *  the list. */
         expand_alpha( Alpha, Ending, Horizon, Program_Initial, Star_Program, Stream ),
%         printf( Stream, "Star_Program: %w\n", [Star_Program] ),
         term_string( Cond, CondS ),
         concat_strings( "?(neg(", CondS, NegCondPart1S ),
         concat_strings( NegCondPart1S, "))", NegCondTestS ),
%         findall( X,
%                  ( member(Y, Star_Program),
%                    term_string(Y, YS),
%                    concat_strings( YS, ";", S1 ),
%                    concat_strings( S1, NegCondTestS, S2 ),
%                    term_string(X, S2)
%                  ),
%                  Star_Program_Ext ),
         /** Add the choice of [?(not(Cond)); Omega_Prime] in the beginning. */
         list_to_string( Omega_Prime, Omega_PrimeS ),
         concat_strings( NegCondTestS, ";", Tmp1S ),
         concat_strings( Tmp1S, Omega_PrimeS, Tmp2S ),
         string_to_list( Tmp2S, Tmp2 ),
         append( Tmp2, Star_Program, Star_Program_Tmp ),
         /** Integrate the complete rest program into the nondeterministic set. */
         integrate( {P_List}, Star_Program_Tmp, {P_List_New}, Stream ),

         apply_tau_prime_H( [{P_List_New} | []], Horizon, Program,
                            Tau_Prime_H_Program, Stream ).

%         /** Add the choice of [?(not(Cond))] and [?(Cond);Sigma)] in the beginning. */
%         NegCondTestS = "?(not(Cond))",
%         CondTestSigmaS = "?(Cond);Sigma",
%         string_to_list( NegCondTestS, NegCondTest ),
%         string_to_list( CondTestSigmaS, CondTestSigma ),
%         append( NegCondTest, Star_Program, Star_Program_Tmp1 ),
%         append( CondTestSigma, Star_Program, Star_Program_Tmp2 ),
%         
%         /** Integrate the complete rest programs into the nondeterministic set. */
%         integrate( {P_List}, Star_Program_Tmp1, {P_List_New1}, Stream ),
%         integrate( {P_List}, Star_Program_Tmp2, {P_List_New2}, Stream ),
%
%         apply_tau_prime_H( [{P_List_New1} | Omega_Prime], Horizon, Program,
%                            Tau_Prime_H_Program1, Stream ),
%         apply_tau_prime_H( [{P_List_New2} | Omega_Prime], Horizon, Program,
%                            Tau_Prime_H_Program2, Stream ),
%
%         append( Tau_Prime_H_Program1, Tau_Prime_H_Program2, Tau_Prime_H_Program).

/** Nondeterministic Choice */
apply_tau_prime_H( [{P_List} | [nondet(ArgList) | Omega_Prime]], Horizon,
                   Program, Tau_Prime_H_Program, Stream ) :- !,
        %printf( Stream, "nondet encountered\n", []),
        integrate({P_List}, ArgList, {P_List_New}, Stream), 
        apply_tau_prime_H( [{P_List_New} | Omega_Prime], Horizon, Program,
                           Tau_Prime_H_Program, Stream ).


/** Test Action, Primitive Action, or Stochastic Action */
apply_tau_prime_H( [{P_List} | [Term | Omega_Prime]], Horizon, Program,
                   Tau_Prime_H_Program, Stream ) :- !,
%        printf(Stream, "Encountered Primitive Action.\n", []),
%        printf(Stream, "Integrating %w into %w.\n", [Term, {P_List}]),
        integrate({P_List}, [Term], {P_List_New}, Stream),
%        printf(Stream, "New Program List: %w.\n", [{P_List_New}]),
        apply_tau_prime_H( [{P_List_New} | Omega_Prime], Horizon, Program,
                           Tau_Prime_H_Program, Stream ).

/** Ignore (but store) everything before the first nondet,
 *  that means, ignore everything not starting with a
 *  set of programs. */
apply_tau_prime_H( [Alpha | Omega], Horizon, Program,
                   Tau_Prime_H_Program, Stream ) :- !,
%        printf(Stream, "Encountered alpha = %w.\n", [Alpha]),
%        printf(Stream, "apply_tau_prime_H( %w, %w, %w, %w, %w ).\n",
%                       [[{P_List} | Omega], Horizon, Program,
%                       Tau_Prime_H_Program, Stream]),
        apply_tau_prime_H( Omega, Horizon, Program,
                           Tau_Prime_H_Program_Nondet, Stream ),
        append([Alpha], Tau_Prime_H_Program_Nondet, Tau_Prime_H_Program_Solve),
        Tau_Prime_H_Program = Tau_Prime_H_Program_Solve.

expand_alpha( Alpha, Omega_Prime, Horizon, Program, Star_Program, Stream ) :-
         /** Determine recursively, how many horizon consuming actions Alpha
          *  will execute at least.
          *  If this number is =< Horizon, then append another Alpha.
          *  A too long final sequence will only be considered up to
          *  the horizon by the Readylog interpreter.
          */
         horizon_consumption(Alpha, Consume, Stream),
%         printf( Stream, "Horizon consumption of %w: %w\n", [Alpha, Consume] ),
         ( (Consume > Horizon) -> /** End recursion. */
                                  Star_Program = []
         ;
                                  /** Else keep appending Alpha */
                                  append(Program, Alpha, P_w_Alpha),
                                  /** Conversion from comma to semicolon necessary to
                                   *  mark consecutive actions. */
                                  list_to_string(P_w_Alpha, P_w_AlphaS),
                                  comma_to_semicolon( P_w_AlphaS, P_w_AlphaFinalS, Stream ),
                                  string_to_list( P_w_AlphaFinalS, P_w_AlphaFinal ),
%                                  printf(Stream, "Program with Alpha: %w\n", [P_w_AlphaFinal]),

                                  append(P_w_Alpha, Omega_Prime, P_w_Alpha_Omega),
                                  /** Conversion from comma to semicolon necessary to
                                   *  mark consecutive actions. */
                                  list_to_string(P_w_Alpha_Omega, P_w_Alpha_OmegaS),
                                  comma_to_semicolon( P_w_Alpha_OmegaS, P_w_Alpha_OmegaFinalS, Stream ),
                                  string_to_list( P_w_Alpha_OmegaFinalS, P_w_Alpha_OmegaFinal ),
%                                  printf(Stream, "Program with Alpha and Omega_Prime: %w\n", [P_w_Alpha_OmegaFinal]),

                                  Horizon_New is (Horizon - Consume),
                                  /** Continue appending Alpha. Note, that
                                   *  the parameter P_w_AlphaFinal does not
                                   *  contain Omega_Prime. */
                                  expand_alpha( Alpha, Omega_Prime, Horizon_New, P_w_AlphaFinal,
                                                Star_Program_Tmp, Stream ),
                                  append( P_w_Alpha_OmegaFinal, Star_Program_Tmp, Star_Program )
         ).

         
/* ----------------------------------------------------------
                            tau                             
---------------------------------------------------------- */

/** tau_prime pulls the deterministic program alpha that
 * comes before the first nondeterministic statement
 * (nondet, pickBest, star) in the solve context:
 * solve([alpha; nondet(q_1,...,q_m)], H, rew)
 * === tau_prime ===>
 * alpha; solve([nondet(q_1,...,q_m)], H, rew).
 */

/** Empty Program */
apply_tau(solve([], _Horizon, _RewardFunction), Tau_Program,
          Stream ) :- !,
%        printf(Stream, "Empty solve reached!\n", []),
        Tau_Program = [].

/** Zero Horizon */
apply_tau(solve(_Anything, Horizon, _RewardFunction), Tau_Program,
          Stream ) :- /* DO NEVER PUT A CUT HERE, OR THE NONDET WILL NOT BE RE-TRIED */ 
%        printf(Stream, "CHECKING HORIZON, Horizon: %w\n", [Horizon]),
        Horizon =< 0,
        /** use CUT to say: Horizon > 0 in all following clauses */
        /** TODO: doesn't work... why? */
        !,
%        printf(Stream, "Horizon less/equal 0\n", []),
        Tau_Program = [].
%        printf(Stream, "Tau_Program: %w\n", [Tau_Program]).

/** Nondeterministic Choice */
apply_tau(solve([nondet(P_List)], Horizon, RewardFunction), Tau_Program,
          Stream ) :- !, 
%        printf(Stream, "nondet encountered!\n", []),
        Tau_Program = [solve([nondet(P_List)], Horizon, RewardFunction)].
%        Tau_Program = [].

/** Conditional */
apply_tau(solve([if(Cond, Sigma1, Sigma2) | Omega], Horizon, RewardFunction),
          Tau_Program, Stream ) :- !,
%        printf(Stream, "if condition encountered, Horizon: %w\n", [Horizon]),
%        printf(Stream, "apply_tau(solve([%w | Omega], %w, RewardFunction),
                  %w, Stream)\n", [Sigma1, Horizon, Tau_Program_Sigma1]),
        apply_tau(solve([Sigma1 | Omega], Horizon, RewardFunction),
                  Tau_Program_Sigma1, Stream),
%        printf(Stream, "\n ********* Tau_Program_Sigma1: %w\n", [Tau_Program_Sigma1]),
        apply_tau(solve([Sigma2 | Omega], Horizon, RewardFunction),
                  Tau_Program_Sigma2, Stream),
        ( (Tau_Program_Sigma1 = [], Tau_Program_Sigma2 = [] ) ->
                /** optimise readability by leaving away if-clause */
                Tau_Program = []
        ;
                list_to_string(Tau_Program_Sigma1, Sigma1_Final),
                list_to_string(Tau_Program_Sigma2, Sigma2_Final),

                /** put together both sub-results */
                term_string(Cond, CondString),
                concat_strings( "if( ", CondString, S1 ),
                concat_strings( S1, ", [", S2 ),
                concat_strings( S2, Sigma1_Final, S3 ),
                concat_strings( S3, "], [", S4 ),
                concat_strings( S4, Sigma2_Final, S5 ),
                concat_strings( S5, "] )", S6 ),
                string_to_list(S6, Tau_Program)
        ).

apply_tau(solve([if(Cond, Sigma) | Omega], Horizon, RewardFunction), Tau_Program,
          Stream ) :- !,
        apply_tau(solve([if(Cond, Sigma, []) | Omega], Horizon, RewardFunction),
                  Tau_Program, Stream ).

/** Loop */
apply_tau(solve([while(Cond, Sigma) | Omega], Horizon, RewardFunction),
          Tau_Program, Stream ) :- !,
%        printf(Stream, "loop before nondet encountered\n", []),
        apply_tau(solve([Sigma | [while(Cond, Sigma) | Omega]], Horizon, RewardFunction),
                  Tau_Program1, Stream),
        apply_tau(solve(Omega, Horizon, RewardFunction),
                  Tau_Program2, Stream),
        ( (Tau_Program1 = [], Tau_Program2 = [] ) ->
                /** optimise readability by leaving away if-clause */
                Tau_Program = []
        ;
                list_to_string(Tau_Program1, Tau1_Final),
                list_to_string(Tau_Program2, Tau2_Final),

                /** put together both sub-results */
                term_string(Cond, CondString),
                concat_strings( "if( ", CondString, S1 ),
                concat_strings( S1, ", [", S2 ),
                concat_strings( S2, Tau1_Final, S3 ),
                concat_strings( S3, "], [", S4 ),
                concat_strings( S4, Tau2_Final, S5 ),
                concat_strings( S5, "] )", S6 ),
                string_to_list(S6, Tau_Program)
        ).

/** Test Action */
apply_tau(solve([?(Term) | Omega], Horizon, RewardFunction), Tau_Program, Stream ) :- !,
%        printf(Stream, "test action encountered\n", []),
        /** Note that the test action does not consume horizon. */
        apply_tau(solve(Omega, Horizon, RewardFunction), Tau_Program_Tmp, Stream),
        append( [?(Term)], Tau_Program_Tmp, Tau_Program ).

/** Primitive Action, Stochastic Action */
apply_tau(solve([Term | Omega], Horizon, RewardFunction), Tau_Program, Stream ) :- !,
%        printf(Stream, "simple/stochastic action %w encountered\n", [Term]),
        Horizon_New is (Horizon - 1),
%        printf(Stream, "apply_tau(solve(%w, %w, RewardFunction), Tau_Program_Tmp, Stream)\n", [Omega, Horizon_New]),
        apply_tau(solve(Omega, Horizon_New, RewardFunction), Tau_Program_Tmp, Stream),
%        printf(Stream, "apply_tau(solve(Omega, %w, RewardFunction), Tau_Program_Tmp, Stream) ***SUCCEEDED***\n", [Horizon_New]),
        ( Horizon > 0 ->
%              printf(Stream, "append( [%w], %w, Tau_Program )\n", [Term, Tau_Program_Tmp]),
              append( [Term], Tau_Program_Tmp, Tau_Program )
        ;
%              printf(Stream, "This should never happen, because of CUT above!\n", []),
              Tau_Program = Tau_Program_Tmp
        ).

/* ----------------------------------------------------------
   utilities                           
---------------------------------------------------------- */

/** replace commas that separate consecutive actions by semicolons,
 *  but ignores commas in tests, if-conditions, and while statements
 *  by commas */
comma_to_semicolon( "", String_New, Stream ) :- !,
        String_New = "".

comma_to_semicolon( String, String_New, Stream ) :-
        substring( String, 1, 3, "if(" ),
%        printf( Stream, "if( pattern found\n", [] ),
        !,
        comma_to_semicolon_aux( String, String_New, Stream ).
        
comma_to_semicolon( String, String_New, Stream ) :-
        substring( String, 1, 6, "while(" ),
%        printf( Stream, "while( pattern found\n", [] ),
        !,
        comma_to_semicolon_aux( String, String_New, Stream ).
        
comma_to_semicolon( String, String_New, Stream ) :-
        substring( String, 1, 2, "?(" ),
%        printf( Stream, "?( pattern found\n", [] ),
        !,
        comma_to_semicolon_aux( String, String_New, Stream ).

comma_to_semicolon( String, String_New, Stream ) :- !, 
%        printf( Stream, "no special statement found\n", [] ),
        ( ( substring( String, IfPos, 3, "if(" ) ;
            substring( String, WhilePos, 6, "while(" ) ;
            substring( String, TestPos, 2, "?(" )
          ) ->
              /** If there still is any such term in the String */
              /** Check, if positions are instantiated. */
              ( ground(IfPos) -> ( ground(WhilePos) -> min(IfPos, WhilePos, MinTmp),
                                                         ( ground(TestPos) -> /** All variables are defined */
                                                                               min(TestPos, MinTmp, MinPos)
                                                         ;
                                                                               /** Only IfPos and WhilePos are defined */
                                                                               MinPos = MinTmp
                                                         )
                                  ;
                                  /** WhilePos is not defined */                        
                                                         ( ground(TestPos) -> /** Only IfPos and MinPos are defined */
                                                                               min(IfPos, TestPos, MinPos)
                                                         ;
                                                                               /** Only IfPos is defined */
                                                                               MinPos = IfPos
                                                         )
                                  )
              ;
                                  /** IfPos is not defined */
                                  ( ground(WhilePos) -> ( ground(TestPos) -> /** Only WhilePos and TestPos are defined */
                                                                               min(WhilePos, TestPos, MinPos)
                                                         ;
                                                                               /** Only WhilePos is defined */
                                                                               MinPos = WhilePos
                                                         )
                                  ;
                                                         /** TestPos must be defined (only) as a substring was found */
                                                         MinPos = TestPos
                                  )
              ),
%              printf( Stream, "********** MinPos: %w\n", [MinPos] ),
              string_length( String, StringLength ),
              /** String left from statement */
              /** Note that MinPos > 0. Otherwise, a previous clause would have matched. */
              MinPosLeft is (MinPos - 1),
              substring( String, 1, MinPosLeft, Left ),
              replace_character( Left, ",", ";", LeftResult ),

              /** Rest string */
              MinPosRight is (MinPos + 1),
              RestLength is (StringLength - MinPosRight + 2),
              substring( String, MinPos, RestLength, Rest ),
%              printf( Stream, "********** Rest: %w\n", [Rest] ),
              comma_to_semicolon( Rest, RestResult, Stream ),
              concat_strings( LeftResult, RestResult, String_New )
%              printf( Stream, "********** concat_strings( %w, %w, String_New )\n", [LeftResult, RestResult] )
        ;
              /** No special term found in the String */                                    
              /** Commas can all be replaced */
              replace_character(String, ",", ";", String_New)
        ).
        
comma_to_semicolon_aux( String, String_New, Stream ) :-
        string_length( String, StringLength ),
        substring( String, ClosingBracketPos, 1, ")" ),

        /** Statement string */
        ClosingBracketPosLeft is (ClosingBracketPos - 1),
        substring( String, 1, ClosingBracketPosLeft, Left ),
        /** Replace commas in the statement by placeholder.
         *  Otherwise integrate clause would interpret commas
         *  as separators for different programs.
         *  After integration, when converting the
         *  nondeterministic set into a vector, we replace the
         *  placeholders by commas again */
        replace_character( Left, ",", "__COMMA__", LeftResult ),

        /** Rest string */
        RightLength is (StringLength - ClosingBracketPos + 1),
        substring( String, ClosingBracketPos, RightLength, Right ),
        comma_to_semicolon( Right, RightResult, Stream ),

        concat_strings( LeftResult, RightResult, String_New ).

/** compute *minimal* horizon consumption for *deterministic* programs */
/** TODO: What about nondeterministic programs inside star? */
horizon_consumption([], Consume, _Stream) :- !,
        Consume = 0.

horizon_consumption([if(_Cond, Sigma1, Sigma2) | Rest], Consume, Stream) :- !, 
        horizon_consumption( [Sigma1 | Rest], Consume1, Stream ),
%         printf( Stream, "Horizon consumption of Sigma1=%w: %w\n", [Sigma1, Consume1] ),
        horizon_consumption( [Sigma2 | Rest], Consume2, Stream ),
%         printf( Stream, "Horizon consumption of Sigma2=%w: %w\n", [Sigma2, Consume2] ),
        min( Consume1, Consume2, Consume ).

horizon_consumption([if(Cond, Sigma) | Rest], Consume, Stream) :-
        horizon_consumption([if(Cond, Sigma, []) | Rest], Consume, Stream).

horizon_consumption([while(_Cond, Sigma) | Rest], Consume, Stream) :- !,
        horizon_consumption([Sigma | Rest], Consume, Stream).

horizon_consumption([?(_Test) | Rest], Consume, Stream) :- !,
        horizon_consumption(Rest, Consume, Stream).

horizon_consumption([_Action | Rest], Consume, Stream) :- !,
        horizon_consumption(Rest, Consume_Rest, Stream),
        Consume is (1 + Consume_Rest).

subsequence([],_RestY).
subsequence([Item | RestX], [Item | RestY]) :-
        subsequence(RestX,RestY).

join_prog_lists( {P_String1}, {P_String2}, {P_String_Joined} ) :-
        /** add brackets around first argument */
        concat_strings( "[", P_String1, RS1Left),
        concat_strings( RS1Left, "], ", ReducedString1 ),
        /** add brackets around second argument */
        concat_strings( "[", P_String2, RS2Left),
        concat_strings( RS2Left, "]", ReducedString2 ),
        /** join argument lists */
        concat_strings( ReducedString1, ReducedString2, P_String_Joined ).

integrate({P1_String}, {P2_String}, {P_String_New}, Stream) :- 
        string(P2_String),
        !,
        string_to_list(P2_String, P2_List),
        integrate({P1_String}, P2_List, {P_String_New}, Stream).

integrate({P_String}, List, {P_String_New}, Stream) :- !,
%        printf(Stream, "*** Checking if there are brackets in %w...\n ", [P_String]),
        /** cut away [ and ]. Note that [] are not counted in StringLength */
        string_length(P_String, StringLength),
        /** check if first character is [ and last character is ] */
        ( (substring( P_String, 1, 1, "["),
           substring( P_String, StringLength, 1, "]") ) ->
%                printf(Stream, "There are brackets ***\n", []),
                 /** cut away brackets */
                 StringLengthReduced is (StringLength-2),
                 substring( P_String, 2, StringLengthReduced, ReducedString ),
                 integrate({ReducedString}, List, {P_String_New}, Stream)
           ;
%                printf(Stream, "No brackets found ***\n", []),
                 integrate_aux({P_String}, List, {P_String_New}, Stream)
        ).

integrate_aux({P_String}, [], {P_String_New}, Stream) :- !,
%        printf(Stream, "*integrate case 1*\n", []),
        P_String_New = P_String.

integrate_aux({""}, List, {P_String_New}, Stream) :- !,
%        printf(Stream, "*integrate case 4* with [Term]=[%w]\n", [Term]),
        P_List = [],
%        printf(Stream, "P_String: "" --> P_List: %w\n", [P_List]),
        append(P_List, List, P_List_New),
        list_to_string(P_List_New, P_String_New).
%        printf(Stream, "P_List_New: %w --> P_String_New: %w\n", [P_List_New, P_String_New]).

integrate_aux({P_String}, List, {P_String_New}, Stream) :- !,
        /** test, if List really is a list */ 
%        printf(Stream, "*integrating a program list*\n", []),
%        printf(Stream, "%w ---integrate--> {%w}\n", [List, P_String]),
        string_to_list(P_String, P_List),
%        printf(Stream, "P_List: %w\n", [P_List]),
        /** create some kind of cross-product */
        findall([X;Y],
                ( member(X, P_List),               
                  member(Y, List) ),
                P_List_Cross),
        list_to_string(P_List_Cross, P_String_Cross),
        /** Clean up string by removing unnecessary brackets.
         *  Programs are separated by commas while actions
         *  are separated by semicolons. */
        /** Remove empty program (note that the empty program
         *  has been considered in the cross product already */
        replace_string(P_String_Cross, "[];", "", Tmp1, Stream),
        replace_string(Tmp1, ";[]", "", Tmp2, Stream),
        /* remove brackets */
        remove_character(Tmp2, "[", Tmp3),
        remove_character(Tmp3, "]", P_String_Cross_Clean),
%        printf(Stream, "P_String_Cross_Clean: %w\n", [P_String_Cross_Clean]),

%        printf(Stream, "P_List_Cross: %w --> P_String_Cross: %w\n", [P_List_Cross, P_String_Cross]),
        P_String_New = P_String_Cross_Clean.

list_to_string( [], ResultString ) :- !,
        ResultString = "".

list_to_string( List, ResultString ) :- !,
        /** cut away brackets around the argument list */
        term_string(List, String),
        string_length(String, StringLength),
        ReducedLength is (StringLength - 2),
        substring( String, 2, ReducedLength, TmpString ),
        /** remove all "'" that Prolog builds in when
         *  converting term->string */
        remove_character( TmpString, "'", ResultString ).        
        
string_to_list( "", List ) :- !,
         List = [].

string_to_list( String, List ) :- !,
        split_string( String, ",", " \t", StringList),
        findall( X,
                 ( member(Y, StringList), atom_string(X,Y) ),
                 List ).

remove_character( String, Character, ResultString ) :- !,
        ( substring( String, Character, PrimePos ) ->
              /** If there still is such a character in the string */
              PrimePosRight is (PrimePos + 1),
              string_length( String, StringLength ),
              RightLength is (StringLength - PrimePos),
              ( PrimePos = 1 ->
                     /** If Character is the first character */
                     substring( String, 2, RightLength, RightFromPrime),
                     remove_character(RightFromPrime, Character, ResultString)
              ;
                     /** Else */
                     PrimePosLeft is (PrimePos - 1),
                     substring( String, 1, PrimePosLeft, LeftFromPrime),
                     ( PrimePos = StringLength ->
                            /** If Character is the last character */
                            remove_character(LeftFromPrime, Character, ResultString)
                     ;
                            /** Character is found somewhere in the middle */
                            substring( String, PrimePosRight,
                                       RightLength, RightFromPrime),
                            remove_character(LeftFromPrime, Character, LeftResult),
                            remove_character(RightFromPrime, Character, RightResult),
                            concat_strings(LeftResult, RightResult, ResultString)
                     )
              )
        ;
              /** Otherwise end recursion */
              ResultString = String
        ).

replace_character( String, OldChar, NewChar, ResultString ) :- !,
        ( substring( String, OldChar, PrimePos ) ->
              /** If there still is such a character in the string */
              PrimePosRight is (PrimePos + 1),
              string_length( String, StringLength ),
              RightLength is (StringLength - PrimePos),
              ( PrimePos = 1 ->
                     /** If Character is the first character */
                     substring( String, 2, RightLength, RightFromPrime),
                     replace_character(RightFromPrime, OldChar, NewChar, ResultStringTmp),
                     concat_strings(NewChar, ResultStringTmp, ResultString)
              ;
                     /** Else */
                     PrimePosLeft is (PrimePos - 1),
                     substring( String, 1, PrimePosLeft, LeftFromPrime),
                     ( PrimePos = StringLength ->
                            /** If Character is the last character */
                            replace_character(LeftFromPrime, OldChar, NewChar, ResultStringTmp),
                            concat_strings(ResultStringTmp, NewChar, ResultString)
                     ;
                            /** Character is found somewhere in the middle */
                            substring( String, PrimePosRight,
                                       RightLength, RightFromPrime),
                            replace_character(LeftFromPrime, OldChar, NewChar, LeftResult),
                            replace_character(RightFromPrime, OldChar, NewChar, RightResult),
                            concat_strings(LeftResult, NewChar, ResultStringTmp),
                            concat_strings(ResultStringTmp, RightResult, ResultString)
                     )
              )
        ;
              /** Otherwise end recursion */
              ResultString = String
        ).

replace_string( "", _Pattern, _Value, String_New, Stream ) :-!,
        String_New = "".

replace_string( String, Pattern, Value, String_New, Stream ) :-!,
        ( substring( String, Pattern, PatternPos ) ->
              string_length( String, StringLength ),
              string_length( Pattern, PatternLength ),
              PatternPosRight is (PatternPos + PatternLength),
              ( PatternPos = 1 ->
                    /** If Pattern is at the beginning */
%                    printf( Stream, "%w found at the beginning.\n", [Pattern] ),
                    RestLength is (StringLength - PatternPosRight + 1),
                    substring( String, PatternPosRight, RestLength, Right ),
%                    printf( Stream, "replace_string( %w, %w, %w, String_Tmp, Stream)\n",
%                                    [Right, Pattern, Value] ),
                    replace_string( Right, Pattern, Value, String_Tmp, Stream ),
%                    printf( Stream, "String_Tmp: %w\n", [String_Tmp] ),
                    concat_strings( Value, String_Tmp, String_New )
              ;
                    /** Else */
                    PatternPosLeft is (PatternPos - 1),
                    substring( String, 1, PatternPosLeft, Left ),
                    replace_string( Left, Pattern, Value, String_Left_Tmp, Stream),
                    RestLength is (StringLength - PatternPosRight + 1),
                    substring( String, PatternPosRight, RestLength, Right ),
                    replace_string( Right, Pattern, Value, String_Right_Tmp, Stream),
                    concat_strings( String_Left_Tmp, Value, String_Tmp1 ),
                    concat_strings( String_Tmp1, String_Right_Tmp, String_New )
              )
        ;
              /** If Pattern is not found in String */
              String_New = String
        ).

replace_term( Program, Term, Value, Program_New, Stream ) :-!,
        term_string(Term, TermS),
        term_string(Value, ValueS),
%        list_to_string(Program, ProgramS),
        term_string(Program, ProgramS),
%        printf( Stream, "*1*replace_term_aux( %w, %w, %w, %w, Stream)\n",
%                [ProgramS, TermS, ValueS, Program_NewS]),
        replace_term_aux( ProgramS, TermS, ValueS, Program_NewS, Stream ),
%        split_string(ProgramS, ValueS, "", Program_Tmp),
%        string_to_list(Program_NewS, Program_New).
        term_string( Program_New, Program_NewS ). /** Correct? Or string_to_list w/o []? */

replace_term_aux( "", _Term, _Value, Program_New, Stream ) :- !,
        Program_New = "".

replace_term_aux( Program, Term, Value, Program_New, Stream ) :- !,
%        printf( Stream, "*2* replace_term_aux(%w, %w, %w, Program_New, Stream)\n",
%                [Program, Term, Value] ),
        ( substring( Program, Term, TermPos ) ->
              /** If Term is found in the string Program */
%              printf( Stream, "Term is found...\n", [] ),
              string_length( Program, ProgramLength ),
              string_length( Term, TermLength ),
              TermPosRight is (TermPos + TermLength),
              ( TermPos = 1 ->
                    /** If Term is at the beginning */
%                    printf( Stream, "... at the beginning.\n", [] ),
                    substring( Program, TermPosRight, TermLength, Right ),
                    replace_term_aux( Right, Term, Value, Program_Tmp, Stream ),
                    concat_strings( Value, Program_Tmp, Program_New )
              ;
                    /** Else */
%                    printf( Stream, "... in the middle.\n", [] ),
                    TermPosLeft is (TermPos - 1),
                    substring( Program, 1, TermPosLeft, Left ),
                    replace_term_aux( Left, Term, Value, Program_Left_Tmp, Stream),
%                    printf( Stream, "Left string successfully replaced by %w\n", [Program_Left_Tmp] ),
                    RestLength is (ProgramLength - TermPosRight + 1),
                    substring( Program, TermPosRight, RestLength, Right ),
%                    printf( Stream, "Right string: %w\n", [Right] ),
                    replace_term_aux( Right, Term, Value, Program_Right_Tmp, Stream),
%                    printf( Stream, "Right string successfully replaced by %w\n", [Program_Right_Tmp] ),
                    concat_strings( Program_Left_Tmp, Value, Program_Tmp1 ),
                    concat_strings( Program_Tmp1, Program_Right_Tmp, Program_New )
              )
        ;
              /** If Term is not found in the string Program */
%              printf( Stream, "Term is *not* found...\n", [] ),
              Program_New = Program
        ).
        

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        

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
	compile(NewFileName),
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
