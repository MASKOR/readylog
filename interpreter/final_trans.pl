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
 *           $Id$
 *       @author: Christian Fritz <Christian.Fritz@rwth-aachen.de>
 *   description: TRANS and FINAL of Readylog Interpreter 
 * last modified: $Date$
 *            by: $Author$
 *
 * **************************************************************************/

/* ================================================================== */
/*                        TRANS and FINAL                             */
/* ================================================================== */

/* --------------------------------------------------------- */
/*  Final                                                    */
/* --------------------------------------------------------- */
% {{{ Final

/* axioms for final configuration. 
 * final(P, S) states that program P is finished in situation S.
 * (nothing left to do) 
 */
final([],_). 
final(pconc(E1,E2),S) :-   final(E1,S), !; final(E2,S).  
final([E|L],S) :- 	   final(E,S), final(L,S). 
final(if(Cond,E1,E2),S) :-
	holds(Cond,S) -> final(E1,S) ; final(E2,S).
final(while(Cond,E),S) :-  not holds(Cond,S),! ; final(E,S). 
final(withCtrl(_F,E),S) :- final(E,S).
final(applyPolicy(P), S) :-
	final(P,S),
	printColor(yellow, "** POLICY COMPLETED **\n", []).
% <DP was here>
final(applyLearnedPolicy(P, _Solve, _S_solve), S) :-
	final(P,S),
	printColor(yellow, "** POLICY COMPLETED **\n", []).
% </DP was here>

/* -- Macros -- */
final(tryAll(E1,E2),S) :-   final(pconc(E1,E2),S).
final(withPol(_E1,E2),S) :-  final(E2,S).
final(loop(E),S) :- 	    final(E,S).
final(if(Cond,E),S) :- 	    final(if(Cond,E,[]),S).
final(interrupt(E1, E2), S) :- final(E1, S); final(E2, S).
final(interrupt(E1, E2, E3), S) :- final(E1, S); final(E2, S); final(E3, S).
/* ------------ */

final(E,S) :- proc(E,E1), final(E1,S). 
% <DP was here>
final(E,S) :- ipl_proc(E,E1), final(E1,S).
% </DP was here>

% }}}

/* --------------------------------------------------------- */
/*  transPr                                                  */
/* --------------------------------------------------------- */
% {{{ transPr

/* the old ConGolog trans/4 is a transPr with probability 1 */
trans(E,S,E1,S1) :- transPr(E,S,E1,S1,1).

/* Sequence.
 * in a final configuration a transition with the tail of the program
 * is executed, otherwhise a transition with the head is executed
 */
transPr([E|L],S,R,S1,P) :- 
	(
	  final(E,S) -> 
	  (
	    length(L,1) ->
	    L=[E1],
	    transPr(E1,S,R,S1,P)
	  ;
	    transPr(L,S,R,S1,P)
	  )
	;
	  transPr(E,S,R1,S1,P),
	  R=[R1|L]
	).

/* prob; probabilistic execution of program e1 with prob p.
 * note: to distinguish both branches of the program the
 * dummy action tossHead, tossTail is introduced. 
 */
transPr(prob(P,E1,_E2),S1,E1,[tossHead|S1],Q) :-
	Q is P.
transPr(prob(P,_E1,E2),S1,E2,[tossTail|S1],Q) :- 
	Pnum is P,
	Q is 1-Pnum.

/* event-based execution.
 * online case: the time fluent start is evaluated and
 *   the tform cond is tested (holdsTForm defined in readylog.pl)
 * offline case: the least time point ltp is calculated.
 *   (ltp is defined in readylog.pl) and time is advanced to
 *   the least time point where Cond holds (setTime action)
 */
transPr(waitFor(Cond),S,[],SS,1) :- !,
	(
	  holds(online=true,S) ->
	  has_val(start,T,S), holdsTForm(Cond,T,S), SS = S
	;
	  ltp(Cond,T,S), SS=[setTime(T)|S]
	).

/* concurrency.
 * the action which can be performed first ist executed.
 * if E1 can perform a transition which does not consume
 * time (simulataneous) we perform this ation.
 * Otherwise we look for a legal successor transition for
 * E2 and check which of both has an earlier time
 * (simultaneous defined in final_trans.pl)
 * (earliereq defined in final_trans.pl)
 */
transPr(pconc(E1,E2),S,pconc(EE1,EE2),SS,P) :- 
	not final(E1,S), not final(E2,S), !,
	(
	  /* case 1: E1 can perform a step */
	  transPr(E1,S,E3,S3,P3), S3=[Action|_],
	  (Action\=tossHead -> ! ; true),
	  ( /*  if no time consumed by action,
	        no doubt about executing it */
	  simultaneous(S,S3) ->
	      EE1=E3, EE2=E2, SS=S3, P=P3
	  ;
	      (transPr(E2,S,E4,S4,P4) ->
		  (earliereq(S3,S4) ->
		      EE1=E3, EE2=E2, SS=S3, P=P3
		  ;
		      EE1=E1, EE2=E4, SS=S4, P=P4
		  )
	      ;
		  EE1=E3, EE2=E2, SS=S3, P=P3
	      )
	  )
	;
	  /* case 2: no action possible in E1, try E2 */
	  \+ transPr(E1,S,_,_,_),
	  transPr(E2,S,EE2,SS,P), EE1=E1
	).

/* test.
 * if cond holds, we transition towards a final configuration (trans([]...)
 * (holds defined in readylog.pl)
 */
transPr(?(Cond),S,[],S,1) :- holds(Cond,S), !. 

/* conditional.
 * if the condition holds in S we transition towards E1
 * otherwise to E2.
 */
transPr(if(Cond,E1,E2),S,E,S1,P) :- !,
	(
	  holds(Cond,S) ->
	  transPr(E1,S,E,S1,P)
	;
	  transPr(E2,S,E,S1,P)
	).

/* loop.
 * as long as the condition holds, we perform program E1.
 * the successor configuration is the program after executing
 * the first action in E with the loop appended
 */
transPr(while(Cond,E),S,[E1,while(Cond,E)],S1,P) :- !,
	( holds(Cond,S) -> transPr(E,S,E1,S1,P) ).

/* guarded execution.
 * as long as the condition F holds, we perform steps
 * in program E. The difference to while is, that
 * the body is performed only once.
 */
transPr(withCtrl(F,E),S,EE,SS,P) :- !,
	holds(F,S), transPr(E,S,E1,SS,P), EE = withCtrl(F,E1). 


/* --------------------------------------------------------- */
/*  Interrupts                                               */
/* --------------------------------------------------------- */

/* interrupts.
 * parallel condition-bounded execution
 */
transPr( interrupt([], ENorm), S, EE, SS, P) :-
	transPr( ENorm, S, EE, SS, P).
transPr( interrupt([[Phi,EInt]|Rest], ENorm), S, EE, SS, P) :-
	transPr(interrupt(Phi, EInt, interrupt(Rest, ENorm)), S, EE, SS, P).		 

transPr( interrupt(Phi, EInt, ENorm), S, EE, SS, P) :-
	(
	  holds(Phi, S) ->
	  transPr([EInt, interrupt(Phi, EInt, ENorm)], S, EE, SS, P)
	;
	  transPr(ENorm, S, ENormE, SS, P),
	  EE = interrupt(Phi, EInt, ENormE)
	).



/* --------------------------------------------------------- */
/*  decision-theoretic planning                              */
/* --------------------------------------------------------- */

/*
<deprecated> has to be tested
% 	printf(" --------\t solve \t-------------
% 	      \nHorizon: %w\nProg: %w\nS: %w", [Horizon, Prog, S]),
% 	flush(output),
% 	% STF was here:
% 	cancel_after_event(event_exogUpdate),
% 	% till here!
% 	statistics(times, [CPUT, SYST, RealT]), !,
% 	bestDoM(Prog, [clipOnline|S], Horizon, Policy,
% 	        Value, TermProb, checkEvents, Tree, reward),
% 	statistics(times, [CPUT2, SYST2, RealT2]), !,
% 	% STF next 2 me again
% 	param_cycletime(CycleTime),
% 	event_after_every(event_exogUpdate, CycleTime),
% 	% till here!
%  	!,
%  	CPUDiff is CPUT2 - CPUT,
% 	SYSDiff is SYST2 - SYST,
% 	RealDiff is RealT2 - RealT,
% 	printf(" --------\t solve DONE \t-------------\n", []),
% 	printf("Policy:\n", []),
% 	printPol(stdout, Policy),
% 	printf("\n\nTimes: %w\nValue: %w\nTermProb: %w\n",
% 	       [[CPUDiff, SYSDiff, RealDiff], Value, TermProb]), !,
% 				% 	transPr(applyPolicy(Policy), S, Policy_r, S_r, _Prob).
% 	(
% 	  dtdebug ->
% 	  printf("Tree: %w\n", [Tree])
% 	;
% 	  true
% 	),
% 	Policy_r = applyPolicy(Policy),
% 	S_r = S.
</deprecated> 
*/

/* solve; decision theoretic optimizing.
 * start optimization of a program.
 * the program is handed to bestDoM
 * if the program Prog is optimized the calculated policy
 * Policy is executed using the the special transition
 * applyPolicy
 * the first predicate is a shortcut for using the standard
 * function reward as reward function
 * (bestDoM defined in decisionTheoretic.pl)
 */
transPr( solve(Prog, Horizon), S, Policy_r, S_r, 1) :- !,
	transPr( solve( Prog, Horizon, reward), S, Policy_r, S_r, 1).

transPr( solve(Prog, Horizon, RewardFunction), S, Policy_r, S_r, 1) :- !,
	printf(" --------\t solve \t-------------\
              \n Horizon: %w\n    Prog: %w\n       S: %w\n", [Horizon, Prog, S]),
	flush(output),
	% STF was here:
	%cancel_after_event(event_exogUpdate), % this predicate is deprecated!
	cancel_after_event(event_exogUpdate, CancelledEvent),
	printf(" CnclEvt: %w\n", [CancelledEvent]),
	% till here!
	statistics(times, [CPUT, SYST, RealT]), !,
        % <DP was here>
        ( iplearn ->
            %  Determine, whether we are still preparing for the training phase,
            %  or, for this solve, we are in the training phase or in the
            %  consultation phase.
            create_hash_key( solve(Prog, Horizon, RewardFunction), HashKey ),
            determine_ipl_phase( HashKey, S, Phase),
            ( (Phase = "pre_train" ; Phase = "train") ->
                 %  Compute policy through DT planning.
	         bestDoM(Prog, [clipOnline|S], Horizon, Policy,
	         Value, TermProb, checkEvents, Tree, RewardFunction)
            ;
                 %  Phase = "consult"
                 %  Consult learned decision tree to get policy.
	         statistics(times, [CPUT_BC, SYST_BC, RealT_BC]),
                 consult_dtree(Prog, Horizon, RewardFunction, [clipOnline|S], 
                               PolicyConsult, ValueConsult, TermProbConsult,
                               TreeConsult,
                               ConsultSuccess),
	         
                 statistics(times, [CPUT_AC, SYST_AC, RealT_AC]),
                 %  statistics
 	         CPUDiffC is CPUT_AC - CPUT_BC,
   	         SYSDiffC is SYST_AC - SYST_BC,
	         RealDiffC is RealT_AC - RealT_BC,
                 getval(consultation_time_cpu, CTcpu),
                 getval(consultation_time_sys, CTsys),
                 getval(consultation_time_real, CTreal),
                 CTcpuNew is (CTcpu + CPUDiffC),
                 CTsysNew is (CTsys + SYSDiffC),
                 CTrealNew is (CTreal + RealDiffC),
                 setval(consultation_time_cpu, CTcpuNew),
                 setval(consultation_time_sys, CTsysNew),
                 setval(consultation_time_real, CTrealNew),
                      getval(learned_policy_consultations_total, LPCTtimesOld),
                      LPCTtimes is (LPCTtimesOld + 1),
                 ( exists('consult_times.log') ->
                    open('consult_times.log', append, TimesLog),
                    printf(TimesLog, "--------------- CONSULTATION TIMES --------------\n", []),
                    printf(TimesLog, "[# of consultations, CPU, SYS, REAL]\n", []),
                    printf(TimesLog, "%w %w %w %w.\n", [LPCTtimes, CTcpuNew, CTsysNew, CTrealNew]),
                    printf(TimesLog, "-------------------------------------------------\n\n", []),
                    close(TimesLog)
                 ;
                    open('consult_times.log', write, TimesLog),
                    printf(TimesLog, "--------------- CONSULTATION TIMES --------------\n", []),
                    printf(TimesLog, "[# of consultations, CPU, SYS, REAL]\n", []),
                    printf(TimesLog, "%w %w %w %w.\n", [LPCTtimes, CTcpuNew, CTsysNew, CTrealNew]),
                    printf(TimesLog, "-------------------------------------------------\n\n", []),
                    close(TimesLog)
                 ),

                 ( ConsultSuccess ->
                      %  If the decision tree returned a valid policy, use it.
                      Policy = PolicyConsult,
                      Value = ValueConsult,
                      TermProb = TermProbConsult,
                      Tree = TreeConsult,

                      %  statistics
                      getval(learned_policy_consultations_successful, LPCS),
                      LPCS_new is (LPCS + 1),
                      setval(learned_policy_consultations_successful, LPCS_new),
                      getval(learned_policy_consultations_total, LPCT),
                      LPCT_new is (LPCT + 1),
                      setval(learned_policy_consultations_total, LPCT_new)
                 ;
                      %  Otherwise, stick to DT planning.
	              bestDoM(Prog, [clipOnline|S], Horizon, PolicyConv,
		              ValueConv, TermProbConv, checkEvents, TreeConv,
                              RewardFunction),
                      Policy = PolicyConv,
                      Value = ValueConv,
                      TermProb = TermProbConv,
                      Tree = TreeConv,

                      %  statistics
                      getval(learned_policy_consultations_total, LPCT),
                      LPCT_new is (LPCT + 1),
                      setval(learned_policy_consultations_total, LPCT_new)
                 )

            )
        ;
            %  Inductive policy learning is turned off. ->
            %  Compute policy through DT planning.
	    bestDoM(Prog, [clipOnline|S], Horizon, Policy,
		    Value, TermProb, checkEvents, Tree, RewardFunction)
        ),
        % </DP was here>
	statistics(times, [CPUT2, SYST2, RealT2]), !,
	% STF next 2 me again
	param_cycletime(CycleTime),
	event_after_every(event_exogUpdate, CycleTime),
	% till here!
 	!,
 	CPUDiff is CPUT2 - CPUT,
	SYSDiff is SYST2 - SYST,
	RealDiff is RealT2 - RealT,
	printf(" --------\t solve DONE \t-------------\n", []),
	printf("Policy:\n", []),
	printPol(stdout, Policy),
	printf("\n\nTimes: %w\nValue: %w\nTermProb: %w\n",
	       [[CPUDiff, SYSDiff, RealDiff], Value, TermProb]), !,
				% 	transPr(applyPolicy(Policy), S, Policy_r, S_r, _Prob).
        getval(avg_solve_time, AST),
        ASTTmp is (AST + CPUDiff),
        ASTNew is (ASTTmp/2.0),
        setval(avg_solve_time, ASTNew),
        open('avg_solve_time.log', write, SolveTimeLog),
        printf(SolveTimeLog, "--------------- AVG SOLVE TIME --------------\n", []),
        printf(SolveTimeLog, "%w\n", [ASTNew]),
        close(SolveTimeLog),
	(
	  dtdebug ->
	  printf("Tree: %w\n", [Tree])
	;
	  true
	),
        % <DP was here>
        (
          iplearn -> 
             printf("*********** PHASE: %w.\n", [Phase]),
             ( Phase = "pre_train" ->
                %  Use DT planning.
                Policy_r = applyPolicy(Policy)
             ;
               ( Phase = "train" ->
                  write_learning_instance(solve(Prog, Horizon, RewardFunction),
                                          Policy, Value, TermProb, Tree,
                                          [clipOnline|S]),
                  printf("Learning instance written successfully.\n", []),
                  Policy_r = applyPolicy(Policy)
               ;
                  %  Phase = "consult"
                  Policy_r = applyLearnedPolicy(Policy, solve(Prog, Horizon,
                                                RewardFunction), S),
                  %  Print and write out statistics in log file
                  getval(learned_policy_consultations_total, LPCTcurr),
                  getval(learned_policy_consultations_successful, LPCScurr),
                  setval(learned_policy_applications_total, LPCScurr),
                  LPATcurr = LPCScurr,
                  LPCFcurr is (LPCTcurr - LPCScurr),
                  LPCratio is (LPCScurr / LPCTcurr),

                  getval(learned_policy_applications_failed, LPAFcurr),
                  LPAScurr is (LPCScurr - LPAFcurr),
                  LPAratio is (LPAScurr / LPCScurr),

                  ( exists('learned_policy.log') ->
                     open('learned_policy.log', append, StreamLog),
                     printf(StreamLog, "--------------- CONSULTATION --------------\n", []),
                     printf(StreamLog, "%w total consultations of learned policies.\n", [LPCTcurr]),
                     printf(StreamLog, "%w successful consultations of learned policies.\n", [LPCScurr]),
                     printf(StreamLog, "%w failed consultations of learned policies.\n", [LPCFcurr]),
                     printf(StreamLog, "%w ratio of consultation success to total consultations.\n", [LPCratio]),
                     printf(StreamLog, "%w %w << consultations vs. ratio.\n", [LPCTcurr, LPCratio]),
                     printf(StreamLog, "--------------- APPLICATION ---------------\n", []),
                     printf(StreamLog, "%w total applications of learned policies.\n", [LPATcurr]),
                     printf(StreamLog, "%w successful (complete) applications of learned policies.\n", [LPAScurr]),
                     printf(StreamLog, "%w failed (complete) applications of learned policies.\n", [LPAFcurr]),
                     printf(StreamLog, "%w ratio of application success to total applications.\n", [LPAratio]),
                     printf(StreamLog, "%w %w << applications vs. ratio.\n", [LPATcurr, LPAratio]),
                     printf(StreamLog, "-------------------------------------------\n\n", []),
                     close(StreamLog)
                  ;
                     open('learned_policy.log', write, StreamLog),
                     printf(StreamLog, "--------------- CONSULTATION --------------\n", []),
                     printf(StreamLog, "%w total consultations of learned policies.\n", [LPCTcurr]),
                     printf(StreamLog, "%w successful consultations of learned policies.\n", [LPCScurr]),
                     printf(StreamLog, "%w failed consultations of learned policies.\n", [LPCFcurr]),
                     printf(StreamLog, "%w ratio of consultation success to total consultations.\n", [LPCratio]),
                     printf(StreamLog, "%w %w << consultations vs. ratio.\n", [LPCTcurr, LPCratio]),
                     printf(StreamLog, "--------------- APPLICATION ---------------\n", []),
                     printf(StreamLog, "%w total applications of learned policies.\n", [LPATcurr]),
                     printf(StreamLog, "%w successful (complete) applications of learned policies.\n", [LPAScurr]),
                     printf(StreamLog, "%w failed (complete) applications of learned policies.\n", [LPAFcurr]),
                     printf(StreamLog, "%w ratio of application success to total applications.\n", [LPAratio]),
                     printf(StreamLog, "%w %w << applications vs. ratio.\n", [LPATcurr, LPAratio]),
                     printf(StreamLog, "-------------------------------------------\n\n", []),
                     close(StreamLog)
                  )
               )
             )
        ;
            %  Inductive policy learning is turned off.
            Policy_r = applyPolicy(Policy)
        ),
        % </DP was here>
	S_r = S.


/* --------------------------------------------------------- */
/*  nondeterminism                                           */
/* --------------------------------------------------------- */

/* non-deterministic choice.
 * online simply choose randomly (always the first). 
 * Usually this construct should never be hit online. 
 */
transPr( nondet(L), S, EE, SS, 1) :- !,
	L = [_E|_LR], /* L is a list */
	member_index( EE, I_choice, L),
	SS = [toss(I_choice)|S].

/* pickBest: online simply choose randomly (always the first).
 * This construct should never be hit online. 
 */
transPr( pickBest(F, R, E), S, E_choice, S_choice, 1) :- !,
	flatten(E,E_flat),
	(
	  /* F is a (yet unbound) variable: directly work with this */
	  var(F) ->    
	  F::R,
	  indomain(F), /* bind variable F to a value in the domain */
 	  E_choice = E,
	  BestValue = F
	;
	  /* substitute all occurences of F by a still free variable X */
	  subvl( F, X, E_flat, E_sub), !,  
  	  X::R,
	  indomain(X), /* bind variable X to a value in the domain */
	  E_choice = E_sub,
	  BestValue = X
	),
	S_choice = [toss(BestValue)|S].

%  <DP was here>
transPr( [pickBestBindDomainVariables(E)|Rest], S, EE, SS, 1) :- !,
        %  Execute pre-solve code that has been prepared
        %  by rho-operator in the iplpreprocessor.
        E,
        transPr( Rest, S, EE, SS, 1).
%  </DP was here>


/* --------------------------------------------------------- */
/*  applyPolicy                                              */
/* --------------------------------------------------------- */

/* applyPolicy; case: marker.
 * if a marker was found as next statement in the policy
 * it's thruth value is evaluated again. If the truth
 * values at planning time are the same as at execution
 * time, we pop the marker from the policy and proceed with the
 * rest of the policy (done by applyPolicy(PolR) in the clause head)
 * otherwise PolR is set to the empty program which terminated the
 * execution of the policy
 */
transPr( applyPolicy([marker(Cond, TruthValue)|PolTail]), S, applyPolicy(PolR), S, 1) :- !,
	printColor(yellow, "applyPolicy: \tPolHead = %w\n", [marker(Cond, TruthValue)]),
	flush(output),
	(
	  holds(Cond, S) ->
	  (
	    TruthValue -> PolR = PolTail
	  ;
	    printColor(red, "BREAKING POLICY (true-case)\n", []),
	    %printColor(yellow, "CONDITION:%w\n", [Cond]),
	    printColor(cyan, "SITUATION:%w\n", [S]),
	    %printColor(blue, "PolR::%w\n", [PolR]),
	    PolR = []
	  )
	;
	  (
	    not(TruthValue) -> PolR = PolTail
	  ;
	    printColor(red, "BREAKING POLICY (false-case)\n", []),
	    %printColor(yellow, "CONDITION:%w\n", [Cond]),
	    printColor(cyan, "SITUATION:%w\n", [S]),
	    %printColor(blue, "PolR::%w\n", [PolR]),
	    PolR = []
	  )
	).	

/* applyPolicy; case: conditional.
 * if the condition Cond holds, we proceed with executing the policy
 * [Pol1, PolTail], otherwise we proceed with [Pol2, PolTail]. We need
 * this kind of combination not to leave applyPol transitions
 */
transPr( applyPolicy([if(Cond, Pol1, Pol2) | PolTail]), S, ProgR, SNew, 1) :- !,
	printColor(yellow, "applyPolicy: \tPolHead = %w\n", [if(Cond, Pol1, Pol2)]),
	flush(output),
	(
	  holds(Cond, S) -> append( Pol1, PolTail, PolNew)
	;
	  append( Pol2, PolTail, PolNew)
	),
	transPr( applyPolicy(PolNew), S, ProgR, SNew, 1).

/* applyPolicy; case: normal program step.
 * we check for final of next action and perform transition to the rest of
 * the program, otherwise we simply make a transtion with PolHead.
 */
transPr( applyPolicy([PolHead | PolTail]), S, ProgR, SNew, 1) :-
	printColor(yellow, "applyPolicy: general case \tPolHead = %w\n", [PolHead]),
	printf("PolTail = %w\n", [PolTail]),
	flush(output),
	(
	  final(PolHead, S) ->
	  transPr( applyPolicy(PolTail), S, ProgR, SNew, 1)
	;
	  transPr( PolHead, S, PolHeadR, SNew, 1),  
	  append(PolHeadR, PolTail, PolR),
	  ProgR = applyPolicy(PolR)
	).

/* comment in only for debugging purposes.
 */
% transPr( applyPolicy(E), S, E, S, 1) :-
% 	printf("applyPolicy: DEBUG: no case/trans for \t %w\n", [E]),
% 	flush(output), !, fail.

% <DP was here>
/* --------------------------------------------------------- */
/*  applyLearnedPolicy                                       */
/* --------------------------------------------------------- */
/* applyLearnedPolicy; case: marker.
 * if a marker was found as next statement in the policy
 * it's thruth value is evaluated again. If the truth
 * values at planning time are the same as at execution
 * time, we pop the marker from the policy and proceed with the
 * rest of the policy (done by applyPolicy(PolR) in the clause head)
 * otherwise PolR is re-planned through DT planning
 */
transPr( applyLearnedPolicy([marker(Cond, TruthValue)|PolTail],
         solve(Prog, Horizon, RewardFunction), S_solve), S, PolR, S, 1) :- !,
	printColor(yellow, "applyLearnedPolicy: \tPolHead = %w\n", [marker(Cond, TruthValue)]),
	flush(output),
	(
	  holds(Cond, S) ->
	  (
	    TruthValue -> PolR = applyLearnedPolicy(PolTail,
                                             solve(Prog, Horizon, RewardFunction), S_solve)
	  ;
            %  statistics
            getval(learned_policy_applications_failed, LPAF),
            LPAF_new is (LPAF + 1),
            setval(learned_policy_applications_failed, LPAF_new),

	    printColor(red, "BREAKING POLICY (true-case)\n", []),
	    %printColor(yellow, "CONDITION:%w\n", [Cond]),
	    printColor(cyan, "SITUATION:%w\n", [S]),
   	    printColor(red, "LEARNED POLICY WAS BROKEN -> re-planning with DT planning\n", []),
	    bestDoM(Prog, [clipOnline|S_solve], Horizon, PolicyReplanned,
	            _Value, _TermProb, checkEvents, _Tree, RewardFunction),
	    printf(" --------\t re-planning DONE \t-------------\n", []),
   	    printf("Policy:\n", []),
	    printPol(stdout, PolicyReplanned),
            % use applyPolicy and NOT applyLearnedPolicy
            PolR = applyPolicy(PolicyReplanned)

            %%  In case of the ReadyBots we do not suggest re-planning.
            %%  Instead, please comment out the above lines for the
            %%  else case and uncomment the following lines instead.
%            printColor(red, "BREAKING POLICY (true-case)\n", []),
%            %printColor(yellow, "CONDITION:%w\n", [Cond]),
%            printColor(cyan, "SITUATION:%w\n", [S]),
%            PolR = []
	  )
	;
	  (
	    not(TruthValue) -> PolR = applyLearnedPolicy(PolTail,
                                                  solve(Prog, Horizon, RewardFunction), S_solve)
	  ;
            %  statistics
            getval(learned_policy_applications_failed, LPAF),
            LPAF_new is (LPAF + 1),
            setval(learned_policy_applications_failed, LPAF_new),

	    printColor(red, "BREAKING POLICY (false-case)\n", []),
	    %printColor(yellow, "CONDITION:%w\n", [Cond]),
	    printColor(cyan, "SITUATION:%w\n", [S]),
   	    printColor(red, "LEARNED POLICY WAS BROKEN -> re-planning with DT planning\n", []),
	    bestDoM(Prog, [clipOnline|S_solve], Horizon, PolicyReplanned,
	            _Value, _TermProb, checkEvents, _Tree, RewardFunction),
	    printf(" --------\t re-planning DONE \t-------------\n", []),
   	    printf("Policy:\n", []),
	    printPol(stdout, PolicyReplanned),
            % use applyPolicy and NOT applyLearnedPolicy
            PolR = applyPolicy(PolicyReplanned)

            %%  In case of the ReadyBots we do not suggest re-planning.
            %%  Instead, please comment out the above lines for the
            %%  else case and uncomment the following lines instead.
%            printColor(red, "BREAKING LEARNED POLICY (false-case)\n", []),
%            %printColor(yellow, "CONDITION:%w\n", [Cond]),
%            printColor(cyan, "SITUATION:%w\n", [S]),
%            PolR = []
	  )
	).	

/* applyLearnedPolicy; case: conditional.
 * if the condition Cond holds, we proceed with executing the policy
 * [Pol1, PolTail], otherwise we proceed with [Pol2, PolTail]. We need
 * this kind of combination not to leave applyPol transitions
 */
transPr( applyLearnedPolicy([if(Cond, Pol1, Pol2) | PolTail],
         solve(Prog, Horizon, RewardFunction), S_solve), S, ProgR, SNew, 1) :- !,
	printColor(yellow, "applyLearnedPolicy: \tPolHead = %w\n", [if(Cond, Pol1, Pol2)]),
	flush(output),
	(
	  holds(Cond, S) -> append( Pol1, PolTail, PolNew)
	;
	  append( Pol2, PolTail, PolNew)
	),
	transPr( applyLearnedPolicy(PolNew,
                             solve(Prog, Horizon, RewardFunction), S_solve), S, ProgR, SNew, 1).

/* applyLearnedPolicy; case: normal program step.
 * we check for final of next action and perform transition to the rest of
 * the program, otherwise we simply make a transtion with PolHead.
 */
transPr( applyLearnedPolicy([PolHead | PolTail],
         solve(Prog, Horizon, RewardFunction), S_solve), S, ProgR, SNew, 1) :-
	printColor(yellow, "applyLearnedPolicy: general case \tPolHead = %w\n", [PolHead]),
	printf("PolTail = %w\n", [PolTail]),
	flush(output),
	(
	  final(PolHead, S) ->
	  transPr( applyLearnedPolicy(PolTail,
                   solve(Prog, Horizon, RewardFunction), S_solve), S, ProgR, SNew, 1)
	;
          %  Wumpus World
          %  Since the learning might propose an impossible action,
          %  we have to check if the goal transPr can be fulfilled.
          %  Otherwise we re-plan.
	  ( transPr( PolHead, S, PolHeadR, SNew, 1) ->
             append(PolHeadR, PolTail, PolR),
	     ProgR = applyLearnedPolicy(PolR, solve(Prog, Horizon, RewardFunction), S_solve)
          ;
             %  statistics
             getval(learned_policy_applications_failed, LPAF),
             LPAF_new is (LPAF + 1),
             setval(learned_policy_applications_failed, LPAF_new),

	     printColor(red, "BREAKING POLICY (impossible action)\n", []),
	     printColor(cyan, "SITUATION:%w\n", [S]),
   	     printColor(red, "LEARNED POLICY WAS BROKEN -> re-planning with DT planning\n", []),
	     bestDoM(Prog, [clipOnline|S], Horizon, PolicyReplanned,
	             _Value, _TermProb, checkEvents, _Tree, RewardFunction),
	     printf(" --------\t re-planning DONE \t-------------\n", []),
   	     printf("Policy:\n", []),
	     printPol(stdout, PolicyReplanned),
             % use applyPolicy and NOT applyLearnedPolicy
             ProgR = applyPolicy(PolicyReplanned),
             SNew = S
          )
          %  /Wumpus World

          %%  In case of the ReadyBots we do not suggest re-planning.
          %%  Instead, please comment out the above lines for the
          %%  else case and uncomment the following lines instead.
%          transPr( PolHead, S, PolHeadR, SNew, 1),
%          append(PolHeadR, PolTail, PolR),
%          ProgR = applyLearnedPolicy(PolR,
%                              solve(Prog, Horizon, RewardFunction), S_solve)
	).
% </DP was here>
	


/* --------------------------------------------------------- */
/*  Macros/Abbreviations                                     */
/* --------------------------------------------------------- */

transPr(tryAll(E1,E2),S,EE,SS,P) :- 
	transPr(pconc(E1,E2),S,EE,SS,P).
transPr(withPol(E1,E2),S,EE,SS,P) :- 
	transPr(pconc([E1,?(false)],E2),S,EE,SS,P).
transPr(loop(E),S,EE,SS,P) :- 
	transPr(while(true,E),S,EE,SS,P).
transPr(if(Phi,E),S,EE,SS,P) :-	
	E2=if(Phi,E,[]), transPr(E2,S,EE,SS,P).
transPr(prob(P,E),S,EE,SS,Q) :- 
	transPr(prob(P,E,[]),S,EE,SS,Q).
transPr(whenever(Phi,E),S,EE,SS,P) :- 
	transPr(while(true,[waitFor(Phi),E]),S,EE,SS,P).

/* ------------------------------------------------------- */



/* primitive action.
 * if E is a primitive action we look up if it is possible
 * either from preprocessed axioms or standard poss axioms.
 * case 1: If the current action E is a send action (to communicate
 *    with registers), the register is evaluated, as well
 *    as the value of this register (B).
 *    There exists 2 flags: eval_register and eval_exog_functions
 *    set to false, the values are not substituted by a call to subf
 *    (cf subf clause in readylog.pl)
 * case 2: reply actions. here, the register fluent is not
 *    substituted, but the reply-value
 * case 3: ordinary actions; substitute fluents by its values
 *
 * (prolog_poss defined in preprocessed file/preprocessor.pl)
 * (fluent eval_registers in defined in readylog.pl)
 * (fluent eval_exog_functions defined in readylog.pl)
 */
transPr( E, S, [], [E_sub|S], 1) :- 
	prim_action(E), !,
	(
	  prolog_poss(E) -> prolog_poss(E, S) 
	; poss(E,Cond), holds(Cond,S) ),
	!,
	(
	  E = send(A,B) ->
	  subf(A,A1,[set(eval_registers, false)|S]),
	  subf(B,B1,[set(eval_exog_functions, false)|S]),
	  E_sub = send(A1,B1)
	; 
	  (
	    E = reply(A,B) ->
	    subf(A,A1,[set(eval_registers, false)|S]),
	    subf(B,B1,S), E_sub = reply(A1,B1)
	  ;                
	    /* substitute current values for all fluents
	    before appending to situation */
	    subf(E,E_sub,S)
	  )
	).

/* procedure calls: these include stochastic procedures. 
 * Recall that stochastic procedures are defined as usual procs, 
 * but have only additionally a model defined 
 */
transPr( E, S, EE, SS, P) :- 
% 	/* - evaluate parameters: call-by-value - */
% 	E =.. [ProcName|Args_s],
% 	subfl( Args_s, Args_eval_s, S),
% 	E_sub =.. [ProcName|Args_eval_s],
% 	/* - look up procedure with evaluated actual parameters - */
% 	proc( E_sub, E_body), !, 
% <DP was here>
        ( iplearn ->
           /* use transformed proc with solve contexts in standard form */
           /* do not evaluate args */
           ipl_proc( E, E_body), !, transPr( E_body, S, EE, SS, P)
        ;
	   /* do not evaluate args */
	   proc( E, E_body), !, transPr( E_body, S, EE, SS, P)
        ).
%	/* do not evaluate args */
%	proc( E, E_body), !, 
%	transPr( E_body, S, EE, SS, P).
% </DP was here>

% }}}


/* --------------------------------------------------------- */
/*  AUXILIARY                                                */
/* --------------------------------------------------------- */
% {{{ AUXILIARY

/* used for X in toss(X) */
member_index( X, 0, [X|_L]).
member_index( X, I, [_Y|L]) :- member_index(X, I2, L), I is I2+1.

/* determine which situation has an earlier starting time */
earliereq(H1,H2) :-
     has_val(start,T1,H1), has_val(start,T2,H2), !, T1 =< T2.
earlier(H1,H2) :-
     has_val(start,T1,H1), has_val(start,T2,H2), !, T1 < T2.

/* do H1 and H2 have the same starting time? */
simultaneous(H1,H2) :-
     has_val(start,T,H1), !, has_val(start,T,H2).

% }}}
