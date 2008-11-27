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
 * code. Its task is to transform all solve statements in the code
 * to a standard form, where they only consist of a single nondet.
 * The intention behind this is to maximise the number of matching
 * solve statements, which then can be learned by the decision tree
 * learning algorithm, and eventually be replaced by decision trees.
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
        %process_solve( Prog, Horizon, RewardFunction, Stream ),
        term_string( solve(Prog, Horizon, RewardFunction), SolveString ),
        string_length( SolveString, SolveLength ),
        process_solve( Prog, Horizon, RewardFunction, Stream ),
%        printf(Stream, "Here had been a solve of length %w!", [SolveLength]), !,
        process_proc( RestProgram, Stream ).

%process_proc( [Term | RestProgram], Stream ) :-
%        printf(Stream, "%w", [Term]), !,
%        process_proc( RestProgram, Stream ).

%process_proc( [], Stream).
        
%process_proc( ProcString, Stream ).

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
                % Using term_string(-,+) cut away the rest of the (Prolog term) proc,
                % so that after a type conversion it would match the string
                % SolveAndAfterString. This way we keep the term structure.
                % TODO: To get a Prolog term, we have to add "[". This currently
                % leads to superfluous left brackets after the solve context.
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
        %process_proc( ProcName, ProcBody, Stream), !,
        process_proc( ProcName, ProcBody, Stream), !,
        process_all_proc_aux(List_rest, Stream).

/* ----------------------------------------------------------
   solve statements
--------------------------------------------------------- */

/** process_solve( Program, Horizon, RewardFunction, Stream )
 * preprocesses a general solve statement of the form
 * solve([alpha; nondet(p_1,...,p_n); omega], H, rew),
 * where alpha is the deterministic part before the first
 * nondet (if any) in the code,
 * the p_i are deterministic programs,
 * and omega is an arbitrary READYLOG program (not containing
 * another solve).
 * First the operator rho is applied to transform the
 * argument list of the nondet to a set,
 * then the operator tau_prime_H is applied until the omega
 * is integrated into the nondet, leading to
 * solve([alpha; nondet(q_1,...,q_m)], H, rew).
 * Finally the operator tau is applied until the alpha is
 * pulled out of the solve context, leading to
 * alpha; solve(nondet(q_1,...,q_m), H, rew).
 */

process_solve( [], Horizon, RewardFunction, _Stream ).

process_solve( Program, Horizon, RewardFunction, Stream ) :-
%        printf( Stream, "Applying rho.\n", [] ),
        Initial_Rho_Program = [],
        apply_rho( Program, Initial_Rho_Program, Rho_Program, Stream ),
        Initial_Tau_Prime_H_Program = [],
%        printf(Stream, "apply_tau_prime_H(%w, %w, %w, %w, %w\n)",
%                       [Rho_Program, Horizon, Initial_Tau_Prime_H_Program,
%                        Tau_Prime_H_Program, Stream]),
	apply_tau_prime_H( Rho_Program, Horizon, Initial_Tau_Prime_H_Program,
                           Tau_Prime_H_Program, Stream ),
%        apply_tau( Tau_Prime_H_Program, Horizon, Reward,
%                   Tau_Prime_Program_Alpha, Tau_Prime_Program_Nondet,
%                   Tau_Prime_Horizon ), !,
%        printf(Stream, "%w; solve(%w, %w, %w)", [Tau_Prime_Program_Alpha,
%                                                 Tau_Prime_Program_Nondet,
%                                                 Tau_Prime_Horizon,
%                                                 Reward]).
        printf(Stream, "solve(%w, %w, %w)", [Tau_Prime_H_Program, Horizon, RewardFunction]).

/* ----------------------------------------------------------
   transformation operators
---------------------------------------------------------- */

/** rho finds the first nondeteministic statement
 * (nondet, pickBest, or star) in the code. In case of
 * pickBest or star, the operator transforms it into
 * a nondet. Afterwards, the argument list of the nondet
 * is transformed into a set.
 */

apply_rho( [], Program, Rho_Program, Stream ) :-
        Rho_Program = Program.

/** TODO rho(pickBest) */
%apply_rho( [pickBest(F, Domain, Delta) | Omega], Program, Rho_Program, Stream ) :- !,
%        flatten(Delta, Delta_flat),
%        findall(Value,
%                (Value::Domain, indomain(Value)),
%                Value_list),
%        domain(numbers),
%        printf( Stream, "numbers is a domain\n", []),
%        printf( Stream, "Value_list: %w\n", [Value_list]),
%        findall(Delta_sub,
%                (Value::Domain, indomain(Value), subvl( F, Value, Delta_flat, Delta_sub )),
%                Delta_sub_list),
%        append(Program, [nondet(Delta_sub_list)], Program_New),
%        apply_rho( Omega, Program_New, Rho_Program, Stream ).
               
/** TODO rho(star) */
%apply_rho( [Alpha*|Omega], Stream ) :- !,
%        printf(Stream, "%w%w", [ProgramSet, Omega]).

apply_rho( [nondet(ProgList) | Omega], Program, Rho_Program, Stream ) :- !,
%        printf(Stream, "ProgList: %w.\n", [ProgList]),
        list_to_string(ProgList, ReducedString),
%        printf(Stream, "ReducedString: %w.\n", [ReducedString]),
        append(Program, [{ReducedString} ], Program_New),
        apply_rho( Omega, Program_New, Rho_Program, Stream ).

apply_rho( [Term | Omega], Program, Rho_Program, Stream ) :- !,
%        printf(Stream, "Program: %w\n", [Program]),
        append(Program, [Term], Program_New),
%        printf(Stream, "Appending: [%w]\n", [Term]),        
%        printf(Stream, "Program_New: %w\n", [Program_New]),
%        printf(Stream, "Omega: %w\n", [Omega]),
        apply_rho( Omega, Program_New, Rho_Program, Stream ).
%        printf(Stream, "Rho_Program_After_Recursion: %w\n", [Rho_Program]).

%print_list_as_set([], Stream) :- printf(Stream, "}", []).
%print_list_as_set([L1], Stream) :- printf(Stream, "%w}", [L1]).
%print_list_as_set([L1 | L], Stream) :-
%        printf(Stream, "%w, ", [L1]),
%        print_list_as_set(L, Stream).

/** tau_prime_H integrates the arbitrary program omega
 * following the first nondet statement into the set of
 * nondeterministic choices:
 * solve([alpha; nondet(p_1,...,p_n); omega], H, rew)
 * === tau_prime_H ===>
 * solve([alpha; nondet(q_1,...,q_m)], H, rew).
 */

/** ignore everything before the first nondet */
apply_tau_prime_H( [Alpha | [{P_List} | Omega]], Horizon, Program,
                   Tau_Prime_H_Program, Stream ) :- !,
%        printf(Stream, "Encountered alpha = %w.\n", [Alpha]),
%        printf(Stream, "apply_tau_prime_H( %w, %w, %w, %w, %w ).\n",
%                       [[{P_List} | Omega], Horizon, Program,
%                       Tau_Prime_H_Program, Stream]),
        apply_tau_prime_H( [{P_List} | Omega], Horizon, Program,
                           Tau_Prime_H_Program, Stream ).

/** omega = ... */

/** Empty program */
apply_tau_prime_H( [{P_List} | []], Horizon, Program,
                   Tau_Prime_H_Program, Stream ) :- !,
%        printf(Stream, "Encountered Empty Program.\n", []),
        /** TODO: sort P_List */
        append(Program, [nondet(P_List)], Program_New),
        Tau_Prime_H_Program = Program_New.


/** Test Action */

/** Primitive Action */
apply_tau_prime_H( [{P_List} | [Term | Omega_Prime]], Horizon, Program,
                   Tau_Prime_H_Program, Stream ) :- !,
%        printf(Stream, "Encountered Primitive Action.\n", []),
%        printf(Stream, "Integrating %w into %w.\n", [Term, {P_List}]),
        integrate({P_List}, [Term], {P_List_New}),
%        printf(Stream, "New Program List: %w.\n", [{P_List_New}]),
        apply_tau_prime_H( [{P_List_New} | Omega_Prime], Horizon, Program,
                           Tau_Prime_H_Program, Stream ).

integrate({P_String}, [], {P_String_New}) :-
        P_String_New = P_String.

integrate({P_String}, [Term], {P_String_New}) :-
        string_to_list(P_String, P_List),
        findall(X,
                ( member(Y, P_List), append([Y], [Term], X) ),
               P_List_New),
        list_to_string(P_List_New, P_String_New).

list_to_string( [], String ) :- !, 
        String = "".

list_to_string( List, ReducedString ) :- !,
        /** cut away brackets around the argument list */
        term_string(List, String),
        string_length(String, StringLength),
        ReducedLength is (StringLength - 2),
        substring( String, 2, ReducedLength, ReducedString ).

string_to_list( "", List ) :- !,
         List = [].

string_to_list( String, List ) :- !,
        split_string( String, ",", " \t", StringList),
        findall( X,
                 ( member(Y, StringList), atom_string(X,Y) ),
                 List ).

/** Stochastic Action */

/** Conditional */

/** Loop */

/** Nondeterministic Choice */

/** PickBest */

/** Star */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        

% }}}


/* ==========================================================
   MAIN
========================================================== */
% {{{ MAIN
% >>>>

iplpreprocess( File ) :- iplpreprocess( File, _NewFile, true, 0).
iplpreprocess( File, NewFile, Last, Level ) :-
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
