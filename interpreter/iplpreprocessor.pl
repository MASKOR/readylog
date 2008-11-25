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
        printf(Stream, "Here had been a solve!", []), !,
        process_proc( RestProgram, Stream ).

%process_proc( [Term | RestProgram], Stream ) :-
%        printf(Stream, "%w", [Term]), !,
%        process_proc( RestProgram, Stream ).

process_proc( [], Stream).

process_proc( [Term | RestProgram], Stream ) :-
        printf( Stream, "%w", [Term] ),
        process_proc( RestProgram, Stream ).

%%process_proc( E, Stream ) :-
%%        E \= [_E|_L],
%%        printf(Stream, "E = %w, and E is atomic.", [E]).
%        printf(Stream, "%w", [E]).

%%process_proc( [E | E1], Stream ) :-
%        printf(Stream, "Before Test E=%w, E1=%w", [E,E1]),
%%        E1 =.. [solve|args],
%        printf(Stream, "After Test", []),
%%        process_proc(E, Stream), %!,
%%        process_proc(E1, Stream).

%%process_proc( [E | E1], Stream ) :-
%%        process_proc(E, Stream), %!,
%%        process_proc(E1, Stream).


%process_proc( E, Stream ) :-
%        process_proc([E], Stream).

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
	apply_rho( Program, Rho_Program, Stream ),
%	apply_tau_prime_H( Program, Horizon, Reward, Tau_Prime_H_Program ),
%        apply_tau( Tau_Prime_H_Program, Horizon, Reward,
%                   Tau_Prime_Program_Alpha, Tau_Prime_Program_Nondet,
%                   Tau_Prime_Horizon ), !,
%        printf(Stream, "%w; solve(%w, %w, %w)", [Tau_Prime_Program_Alpha,
%                                                 Tau_Prime_Program_Nondet,
%                                                 Tau_Prime_Horizon,
%                                                 Reward]).
        printf(Stream, "solve(%w, %w, %w)", [Program, Horizon, RewardFunction]).

/* ----------------------------------------------------------
   transformation operators
---------------------------------------------------------- */

/** rho finds the first nondeteministic statement
 * (nondet, pickBest, or star) in the code. In case of
 * pickBest or star, the operator transforms it into
 * a nondet. Afterwards, the argument list of the nondet
 * is transformed into a set.
 */
%apply_rho( [], _Stream ).
%%apply_rho( Program, Stream ) :-
%%        printf(Stream, "%w", [Program]).

%apply_rho( [pickBest(Value, Domain, Delta) | Omega], Stream ) :- !,
%        ProgramList = makeSetFrom[DeltaWithValueReplacedByAllValuesFromDomain],
%        printf(Stream, "nondet(%w)%w", [ProgramList, Omega]).

%apply_rho( [Alpha*|Omega], Stream ) :- !,
%        printf(Stream, "%w%w", [ProgramSet, Omega]).

apply_rho( [nondet(ProgList) | Omega], Stream ) :- !,
        printf(Stream, "{", []),
        print_list_as_set(ProgList, Stream),
        % print out the rest of the program unchanged
        % as rho only affects the first nondet
        printf(Stream, "%w", [Omega]). % need a function to print out whole program

apply_rho( [], Stream ).

apply_rho( [Term | Omega], Stream ) :-
        printf(Stream, "%w", [Term]),
        Rho_Program = [Term | Omega],
        apply_rho( Omega, Stream ).

print_list_as_set([], Stream) :- printf(Stream, "}", []).
print_list_as_set([L1], Stream) :- printf(Stream, "%w}", [L1]).
print_list_as_set([L1 | L], Stream) :-
        printf(Stream, "%w, ", [L1]),
        print_list_as_set(L, Stream).



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
