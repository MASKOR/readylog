/************************************************************
    bestDoM
************************************************************/

/*.................................................................
* This program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 2 of the License, or
* (at your option) any later version.
* 
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
* 
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
.................................................................*/


/* ----------------------------------------------------------
   NOTES

   This file is not actually implementing decision
   theoretic planning in that it only assigns rewards
   to situations at leaf nodes. This is comparable to
   a utility function in classical planning and seems
   more appropriate here, as intermediate rewards would
   make the agent tend to prefer longer plans to shorter
   ones, even if the final reward (at the leaf) is lower
   in comparison to a shorter path. However, it is not
   a good idea to only provide reward in a 'goal case'
   since it often may happen that neither branch of the
   decision tree reaches such a goal state. (cf 8.3.04)

   ---------------------------------------------------------- */


:- write("********** Loading decisionTheoretic.pl ...\n"). 

/* ----------------------------------------------------------
   Header + Flags
---------------------------------------------------------- */
% {{{ header + flags
% >>>>

:- lib(scattered).       /* to avoid error: non consecutive */
:- set_flag(print_depth,500).


/* ------------------------------------------------------ */
/* DTDebug: turn on for debug output while decision
   theoretic planning is in progress
*/
:- setval(dtdebug, false).
dtdebug :- getval(dtdebug, X), !, X=true.
toggle_dtdebug :- getval(dtdebug, X),
	(
	  X ->
	  setval(dtdebug, false),
	  printf("DTDebug turned OFF\n", [])
	;
	  setval(dtdebug, true),
	  printf("DTDebug turned ON\n", [])
	).

/* ------------------------------------------------------ */
/* DTRewards: if turned on, real decision theroetic planning
   is done, that is, a reward is given in each visited state.
   If turned off, it works more like a search with cut-off:
   a reward is only assigned in leaf-nodes: this seems much
   more reasonable for RoboCup where you usually cannot
   plan until reaching a goal-situation.
*/
:- setval(dtrewards, true).
dtrewards :- getval(dtrewards, X), !, X=true.
toggle_dtrewards :- getval(dtrewards, X),
	(
	  X ->
	  setval(dtrewards, false),
	  printf("DTRewards turned OFF(now giving rewards only in leaves\n", [])
	;
	  setval(dtrewards, true),
	  printf("DTRewards turned ON\n(now giving rewards in each state)", [])
	).


% }}}


/* ----------------------------------------------------------
   bestDoM
---------------------------------------------------------- */
% {{{ bestDoM
% >>>>

/** former bestDoM/6 is a bestDoM where events are still
to be checked */
bestDoM(Program, S, H, Pol, V, Prob) :-
	bestDoM(Program, S, H, Pol, V, Prob, checkEvents).

bestDoM(Program, S, H, Pol, V, Prob, Events) :-
	bestDoM(Program, S, H, Pol, V, Prob, Events, _Tree).

bestDoM(Program, S, H, Pol, V, Prob, Events, Tree) :-
	bestDoM(Program, S, H, Pol, V, Prob, Events, Tree, reward).

	 
/* --- explicit event models --- */
% {{{ explicit event models

/** check if any explicit event is defined for this situation */
/* get list of all events defined for this situation */
bestDoM( Program, S, H, Pol, V, Prob, checkEvents, Tree, RewardFunction) :-
	H >= 0,
	events_list(Eventlist), !,
 	(
	  not Eventlist = [] ->
 	  /* at least one event defined */
 	  bestDoM_event( Program, Eventlist, S, H, Pol, V, Prob, Tree, RewardFunction)
 	;
	  /* else: just go ahead with program */
	  bestDoM( Program, S, H, Pol, V, Prob, ignoreEvents, Tree, RewardFunction)
	).

/** execute all possible events in order of definition [chaining] */
bestDoM_event( Program, [], S, H, Pol, V, Prob, Tree, RewardFunction) :-
	/* end of chain: do next action */
	/* these are the leaves of the events-tree */
	bestDoM( Program, S, H, Pol, V, Prob, ignoreEvents, Tree, RewardFunction).

bestDoM_event( Program, [Event| Rest], S, H, Pol, V, Prob, Tree, RewardFunction) :-
	/** <state_abstraction> */
	(
	  use_caching ->
	  sit2state(S, State)
	;
	  State = S
	),
	/** </state_abstraction> */
	
	(
	  /* check if event possible */
	  prolog_event_poss(Event, S) ->
	  (
	    event_outcomes(Event,S,NatOutcomesList,SenseEffect), !
	  ;
	    /* this is guard and only for debugging purposes: event_outcomes should never fail */
	    printColor(red, "\n\n***!!! EVENT OUTCOMES OF %w FAIL IN PROLOG -- THIS MUST NOT HAPPEN !!!***\n\n", [Event]),
	    flush(output), !, fail
	  ),
	  bestDoM_event_Aux( Program, Rest, NatOutcomesList,
			     S, H, Pol_list, V_list, Prob,
			     Tree_list, RewardFunction),
	  /* if costs are defined for this stoch_proc,
	  use them, else 0.0 */
	  (
	    prolog_event_costs(Event, Costs, S)
	  ;
	    Costs = 0.0
	  ), !,
	  (
	    dtrewards ->
	    /* give reward at any time: */
	    subf(RewardFunction, Reward, S),
	    V is Reward - Costs + V_list
	  ;
	    /** give reward only at leaves */
	    V is V_list - Costs
	  ),
	  flatten( SenseEffect, SenseEffect_flat),
	  append( SenseEffect_flat, Pol_list, Pol),
	  Tree = [info(V, Prob), event(Event, Tree_list)]
	;
	  bestDoM_event( Program, Rest, S, H, Pol, V, Prob, Tree, RewardFunction)
	).

/** execute a certain event: create all outcomes,
go on with remaining events [branching] */
bestDoM_event_Aux( Program, RestEvents, [(Prog,Pr,SenseCond)], S,
		     H, Pol, V, Prob, Tree, RewardFunction) :-
	/* #REVIEW this: if several outcomes are not possible, we
	reinit the process several times at the same point (S) */
	transStar(Prog, S, _Prog_remain, NewSit),
	bestDoM_event(Program,RestEvents,NewSit,H,
			Pol_rest,V_rest,Prob_rest, Tree_rest, RewardFunction), !,
	V is Pr*V_rest,
	Pol = [if(SenseCond,Pol_rest,[])],
	Prob is Pr*Prob_rest,
	Tree = [info(V_rest, Prob_rest),
		event_outcome(Prog, Pr, SenseCond) | Tree_rest].

bestDoM_event_Aux( Program, RestEvents,
		   [(Prog,Pr,SenseCond) | OtherOutcomes],
		   S, H, Pol, V, Prob, Tree, RewardFunction) :-
	not OtherOutcomes = [],
	transStar(Prog, S, _Prog_remain, NewSit),
	bestDoM_event_Aux(Program, RestEvents, OtherOutcomes,
			  S, H, Pol_tree, V_tree, Prob_tree,
			  Tree_tree, RewardFunction),
	bestDoM_event(Program, RestEvents, NewSit, H,
		      Pol_rest, V_rest, Prob_rest, Tree_rest, RewardFunction), !,
	V is Pr*V_rest + V_tree,
	Pol = [if(SenseCond,Pol_rest,Pol_tree)],
	Prob is Pr*Prob_rest + Prob_tree,
	Tree = [[info(V_rest, Prob_rest),
		 event_outcome(Prog, Pr, SenseCond) | Tree_rest], Tree_tree].

% }}}
/** ----------------------------- */

bestDoM([?(C)| E], S, H, Pol, V, Prob, ignoreEvents, Tree, RewardFunction) :-
	H >= 0, 
	(
	  holds(C,S) ->
	  bestDoM(E,S,H,Pol_rest,V,Prob, ignoreEvents, Tree_rest, RewardFunction),
	  Pol = [marker(C, true)|Pol_rest],
	  Tree = [?(C)|Tree_rest]
	;
	  Prob is 0.0,
	  Pol = [],
	  subf(RewardFunction, V, S),
	  leaf_debug(V, S),
	  Tree = [not_poss(?(C)), leaf(S, H, V, Prob)]
	).

bestDoM([nondet([E1]) | E],S,H,Pol,V,Prob, ignoreEvents, Tree, RewardFunction) :-
	H >= 0, 
	bestDoM([E1 | E],S,H,Pol_rest,V,Prob, ignoreEvents, Tree_rest, RewardFunction),
	(
	  dtdebug ->
	  set_textcolor( green ),
	  printf("MAX %w\n%w\t%w\t%w\n", [Tree_rest, V, Prob, Pol_rest]),
	  set_textcolor( normal )
	;
	  true
	),
	Pol = Pol_rest,
	Tree = Tree_rest.

bestDoM([nondet([E1 | E2]) | E],S,H,Pol,V,Prob, ignoreEvents, Tree, RewardFunction) :-
	H >= 0, not E2 = [],
	bestDoM([E1 | E],S,H,Pol1,V1,Prob1, ignoreEvents, Tree_rest, RewardFunction), 
      	(
	  dtdebug ->
	  set_textcolor( green ),
	  printf("MAX %w\n%w\t%w\t%w\n", [Tree_rest, V1, Prob1, Pol1]),
	  set_textcolor( normal )
	;
	  true
	),
	bestDoM([nondet(E2) | E],S,H,Pol2,V2,Prob2, ignoreEvents, Tree_tree, RewardFunction), 
	(
	  greatereq(V1,Prob1,V2,Prob2) ->
	  Pol = Pol1, Prob = Prob1, V = V1 ,
	  Tree = [nondet( policy([info(V1, Prob1)|Tree_rest]), [info(V2, Prob2)| Tree_tree] )]
	; 
	  Pol = Pol2, Prob = Prob2, V=V2,
	  Tree = [nondet( [info(V1, Prob1)|Tree_rest], policy([info(V2, Prob2)| Tree_tree]) )]
	).

% {{{ >>>>>>>> bestDoM pickBest <<<<<<<<

% >>>>

/** pickBest picks the 'best' value for F from the domain R and
perform program E with that. R can be a list of atoms, integers,
an interval of integers (e.g. [3..9]) or a mixture of both (e.g.
[3,6,5.6,mayday,7..12,one,two]). It is treated as a set:
duplicates are ignored */
bestDoM([pickBest(F, R, E) | E_rest],S,H,Pol,V,Prob, ignoreEvents, Tree, RewardFunction) :-
	H >= 0,
	subf(R, Rsub, S),
	flatten(E,E_flat),
	(
	  var(F) ->
	  F::Rsub,
	  findall((Pol1,V1,Prob1, Tree1),
		  bestDoM_pickBest_Aux(F,E_flat,E_rest,S,H,Pol1,
					 V1,Prob1, Tree1, RewardFunction),
		  ResultList)
	;
	  subvl( F, X, E_flat, E_sub),
	  X::Rsub,
	  findall((Pol2,V2,Prob2, Tree2),
		  bestDoM_pickBest_Aux(X,E_sub,E_rest,S,H,Pol2,
					 V2,Prob2, Tree2, RewardFunction),
		  ResultList)
	),
	bestDoM_pickBest_getBest(ResultList, Pol, V, Prob, Tree).

bestDoM_pickBest_Aux(F,E,E_rest,S,H,Pol,V,Prob, Tree, RewardFunction) :-
	indomain(F),
 	bestDoM([E|E_rest],[toss(F)|S],H,Pol_rest,V,Prob,
		ignoreEvents, Tree, RewardFunction),
	(
	  dtdebug ->
	  set_textcolor( green ),
	  printf("BEST F: %w\n%w\t%w\t%w\t%w\n",
		 [Tree, F, V, Prob, Pol_rest]),
          printColor(red, "BESTDOF SIT: %w",[S]),
	  set_textcolor( normal )
	;
	  true
	),
	Pol = Pol_rest.

bestDoM_pickBest_getBest([(Pol, V, Prob, Tree)], Pol, V, Prob, Tree).
bestDoM_pickBest_getBest([(Pol_local, V_local, Prob_local, Tree_local)|Rest],
			 Pol, V, Prob, Tree) :-
	Rest \= [],
	bestDoM_pickBest_getBest(Rest, Pol_rest, V_rest, Prob_rest, Tree_rest),
 	(
 	  greatereq(V_local,Prob_local,V_rest,Prob_rest) ->
 	  Pol=Pol_local, Prob=Prob_local, V=V_local,
	  Tree = [pickBest( policy([info(V_local, Prob_local)| Tree_local]),
			    [info(V_rest, Prob_rest) | Tree_rest] )]

 	; 
 	  Pol=Pol_rest, Prob=Prob_rest, V=V_rest,
	  Tree = [pickBest( [info(V_local, Prob_local) | Tree_local],
			    policy([info(V_rest, Prob_rest) | Tree_rest]) )]
 	).

% }}}
	
bestDoM([if(C,E1,E2) | E],S,H,Pol,V,Prob, ignoreEvents, Tree, RewardFunction) :-
	H >= 0, 
	(
	  holds(C,S) ->
	  bestDoM([E1|E],S,H,Pol1,V,Prob,ignoreEvents, Tree_1, RewardFunction),
	  Pol = [marker(C, true) | Pol1],
	  Tree = [marker(C, true) | Tree_1]
	;
	  bestDoM([E2|E],S,H,Pol2,V,Prob,ignoreEvents, Tree_2, RewardFunction),
	  Pol = [marker(C, false) | Pol2],
	  Tree = [marker(C, false) | Tree_2]
	).
bestDoM([if(C,E1) | E],S,H,Pol,V,Prob,Events,Tree, RewardFunction) :-
	bestDoM([if(C,E1,[])|E],S,H,Pol,V,Prob,Events,Tree, RewardFunction).

bestDoM([while(C,E1) | E],S,H,Pol,V,Prob, ignoreEvents, Tree, RewardFunction) :-
	H >= 0,
	(
	  holds(C,S) ->
	  bestDoM([E1,while(C,E1)|E],S,H,Pol1,V,Prob,ignoreEvents, Tree_1, RewardFunction),
	  Pol = [marker(C, true) | Pol1],
	  Tree = [marker(C, true) | Tree_1]
	;
	  bestDoM(E,S,H,Pol2,V,Prob,ignoreEvents, Tree_2, RewardFunction),
	  Pol = [marker(C, false) | Pol2],
	  Tree = [marker(C, false) | Tree_2]
	).
bestDoM([loop(E1) | E],S,H,Pol,V,Prob,Events,Tree, RewardFunction) :-
	bestDoM([while(true,E1)|E],S,H,Pol,V,Prob,Events,Tree, RewardFunction).

/** waitfor(Cond) has to be possible (there is a least time point)
and has to be executed online (or course) */
bestDoM([waitFor(Cond) | E],S,H,Pol,V,Prob,ignoreEvents,Tree, RewardFunction) :-
	H >= 0,
	(
	  ltp(Cond,T,S) ->
	  bestDoM(E, [setTime(T)|S], H, Pol_rest, V,
		  Prob, checkEvents,Tree_rest, RewardFunction),
	  Pol = [waitFor(Cond) | Pol_rest],
	  Tree = [waitFor(Cond)| Tree_rest]	
	;
	  Pol = [],
	  Prob is 0.0,
	  subf( RewardFunction, V, S),
	  leaf_debug(V, S),
	  Tree = [not_poss(waitFor(Cond)), leaf(S, H, V, Prob)]
	).
bestDoM([whenever(Phi,E1) | E],S,H,Pol,V,Prob,Events,Tree,RewardFunction) :-
	bestDoM([while(true,[waitfor(Phi),E1])|E],S,H,Pol,V,
		Prob,Events,Tree,RewardFunction).

bestDoM([pi(X,E1) | E],S,H,Pol,V,Prob,ignoreEvents,Tree,RewardFunction) :-
	H >= 0, 
 	subvl(X,_,E1,E1_X),
	bestDoM([E1_X | E],S,H,Pol,V,Prob,ignoreEvents,Tree,RewardFunction).


% {{{ >>>>>>>> bestDoM stoch_procs <<<<<<<<
% >>>>

/** stoch_procs are procedures with associated stochastic
outcomes which are completely independant from the
procedures body. As such, it is an extension of the
stochastic actions of DTGolog which remain as a special
case of these stoch_procs */
bestDoM([A | E],S,H,Pol,V,Prob,ignoreEvents,Tree,RewardFunction) :-
	H > 0, 
	stoch_proc(A), !,
 	A =.. [ProcName|Args_s],
 	subfl(Args_s, Args_eval_s, S),
 	A_sub =.. [ProcName|Args_eval_s],
	(
	  /* is it possible in the current situation? */
	  stoch_proc_poss(A_sub, S) -> 
				%	  printColor(yellow, "stoch_proc %w is poss\n", A_sub),
	  (
	    stoch_proc_outcomes(A_sub,S,NatOutcomesList,SenseEffect) ->
				%	  printColor(yellow, "stoch_proc %w has outcomes\n", A_sub),
	    Hor is H - 1,
	    bestDoM_stoch_Aux(NatOutcomesList,E,S,Hor,RestPol,VF,Prob,
			      Tree_list,RewardFunction),
				%	  printColor(yellow, "stoch_proc %w outcomes have been treated\n", A_sub),
	    /* if costs are defined for this stoch_proc, use them,
	    else 0.0 */
	    ( stoch_proc_costs(A_sub, Costs, S) ; Costs = 0.0 ), !,
				%	    printf(" stoch_proc: %w Costs = %w\n", [A_sub, Costs]),
	    (
	      dtrewards ->
	      /* give reward at any time: */
	      subf(RewardFunction, R, S),
	      V is R - Costs + VF
	    ;
	      /** give reward only at leaves */
	      V is VF - Costs
	    ),
	    flatten( SenseEffect, SenseEffect_flat),
	    append( SenseEffect_flat, RestPol, Sense_RestPol),
	    Pol = [ A | Sense_RestPol ],
	    Tree = [info(V, Prob), stoch_proc((A), Costs, Tree_list)]
	  ;
	    /* this is guard and only for debugging purposes: stoch_proc_outcomes should never fail */
	    printColor(red, "\n\n***!!! STOCH PROC OUTCOMES OF %w FAIL IN PROLOG -- THIS MUST NOT HAPPEN !!!***\n\n", [A_sub]),
	    flush(output), !,
	    V = 0.0,
	    Pol = [], Prob = 0.0,
	    Tree = [fail(A_sub, S), leaf(S, H, V, Prob)]
	  )
	;
	  subf( RewardFunction, V, S),
	  leaf_debug(V, S),
	  Pol = [], Prob = 0.0,
	  Tree = [not_poss(A_sub), leaf(S, H, V, Prob)]
	).

/** shouldn't usually occur, but for nonterminating options */
bestDoM_stoch_Aux([],E,S,H,Pol,V,Prob, Tree,RewardFunction) :-
	H >= 0, bestDoM(E,S,H,Pol,V,Prob,checkEvents, Tree,RewardFunction).

bestDoM_stoch_Aux([(Prog,Pr,SenseCond)],E,S,H,Pol,V,Prob, Tree,RewardFunction) :-
	H >= 0, 
	( 
 	  transStar(Prog, S, _Prog_remain, S2) ->
 	  bestDoM(E,S2,H,Pol1,V1,Prob1,checkEvents, Tree_rest,RewardFunction), !,
	  (
	    dtdebug ->
	    set_textcolor( red ),
	    printf("AVG %w\nV:%w\tPr:%w\tProg:%w\tPol:%w\n",
		   [Tree_rest, V1, Pr, Prog, Pol1]),
	    set_textcolor( normal )
	  ;
	    true
	  ),
	  Pol = [ if(SenseCond, Pol1, []) ],
	  V is Pr*V1, 
	  Prob is Pr*Prob1,
	  Tree = [info(V1, Prob1), stoch_proc_outcome((Prog), Pr, (SenseCond))|Tree_rest]
	;
	  Pol = [], (Prob is 0.0) , V = 0,
	  Tree = [not_poss(stoch_proc_outcome(Prog)), leaf(S, V, H, Prob)]
	).

bestDoM_stoch_Aux([(Prog,Pr,SenseCond)| OtherOutcomes],E,S,H,Pol,
		  V,Prob, Tree,RewardFunction) :-
	H >= 0, 
 	not OtherOutcomes = [],
	( 
	  transStar(Prog, S, _Prog_remain, S2) ->
 	  bestDoM(E,S2,H,Pol1,V1,Prob1,checkEvents, Tree_1,RewardFunction), !,
	  (
	    dtdebug ->
	    set_textcolor( red ),
	    printf("AVG %w\n V:%w\tPr:%w\tProg:%w\tPol:%w\n",
		   [Tree_1, V1, Pr, Prog, Pol1]),
	    set_textcolor( normal )
	  ;
	    true
	  ),
	  bestDoM_stoch_Aux(OtherOutcomes,E,S,H,PolT,VT,ProbT, TreeT,RewardFunction),
	  Pol = [if(SenseCond, Pol1, PolT)],
	  V is VT + Pr*V1, 
	  Prob is ProbT + Pr*Prob1,
	  Tree = [[info(V1, Prob1), stoch_proc_outcome((Prog), Pr, (SenseCond))|Tree_1], TreeT]
	;
	  bestDoM_stoch_Aux(OtherOutcomes,E,S,H,Pol,V,Prob, TreeT,RewardFunction),
	  Tree = [[not_poss(stoch_proc_outcome(Prog)), leaf(S, 0, H, 0.0)], TreeT]	  
	).

% }}}


/** no (logical) constants allowed as formal parameters of procs:
this saves us from having to evaluate possible fluents as actual
parameters. These instead just get substituted for the variable
formal parameters */
bestDoM([Proc | E],S,H,Pol,V,Prob,ignoreEvents, Tree,RewardFunction) :-
	H > 0,
	Proc =.. [ProcName|Args_s],
	subfl(Args_s, Args_eval_s, S),
	Proc_sub =.. [ProcName|Args_eval_s],
	proc(Proc_sub,Body),
	not(stoch_proc(ProcName)),
	!,
	bestDoM([Body | E],S,H,Pol,V,Prob,ignoreEvents, Tree_rest,RewardFunction),
	Tree = [proc( Proc_sub)|Tree_rest].


bestDoM([A | E],S,H,Pol,V,Prob,ignoreEvents, Tree,RewardFunction) :-
	H > 0, 
	prim_action(A), !,
	(
	  (
	    prolog_poss(A) -> prolog_poss(A, S) 
	  ; poss(A,Cond), holds(Cond,S)	) ->
	  Hor is H - 1, 
	  subf(A,A_sub,S),
	  bestDoM(E,[A_sub|S],Hor,RestPol,VF,Prob,checkEvents, Tree_rest,RewardFunction),
	  (
	    dtrewards ->
	    /* give reward at any time: */
	    subf(RewardFunction, R, S),
	    V is R + VF
	  ;
	    /** give reward only at leaves */
	    V = VF
	  ),
	  Pol = [ A | RestPol ],
	  Tree = [prim_action(A)|Tree_rest]
	;
	  Pol = [], Prob is 0.0,
	  subf( RewardFunction, V, S),
	  leaf_debug(V, S),
	  Tree = [not_poss(prim_action(A)), leaf(S, V, H, Prob)]
	).

% {{{ >>>>>>>> bestDoM finals <<<<<<<<
% >>>>

bestDoM([],S,H,Pol,V,Prob,_Events, Tree,RewardFunction) :-
	Pol=[],
	subf( RewardFunction, V, S),
	leaf_debug(V, S),
	Prob is 1.0,
	Tree = [leaf(S, V, H, Prob)].

bestDoM([[] | E],S,H,Pol,V,Prob,Events, Tree, RewardFunction) :-
	H >= 0,
	bestDoM(E,S,H,Pol,V,Prob,Events, Tree, RewardFunction).

/* make simple terms a list */
bestDoM(E,S,H,Pol,V,Prob,Events, Tree, RewardFunction) :-
	H >= 0,
	E \= [_E|_L],
	bestDoM([E],S,H,Pol,V,Prob,Events, Tree, RewardFunction).

/* make list in list a simple list */
bestDoM([E|_E_rest],S,H,Pol,V,Prob,Events, Tree, RewardFunction) :-
	H >= 0,
	E = [_E|_L],
	flatten([E|_E_rest], E_flat),
	bestDoM(E_flat,S,H,Pol,V,Prob,Events, Tree, RewardFunction).

/** (must be last clause):
horizon is zero and nothing demands to be done: leaf. */
bestDoM(_E,S,H,[],V,1.0,_Events, Tree, RewardFunction) :-
	H =:= 0,
	subf( RewardFunction, V, S),
	leaf_debug(V, S),
	Tree = [leaf(S, V, H, 1.0)].

% }}}


% }}}


/* ----------------------------------------------------------
   Auxiliary
---------------------------------------------------------- */

leaf_debug( V, S) :-
	(
	  dtdebug ->
	  set_textcolor( blue ),
	  printf("LEAF: V: %w\tS: %w\n", [V, S]),
      printColor(red, "LEAF DEBUG SIT: %w",[S]),
	  set_textcolor( normal )
	;
	  true
	).


% {{{ DTGolog Auxiliary
% >>>>

/** predicates taken over from DTGolog */

lesseq(V1,Prob1,V2,Prob2) :-  Pr1 is float(Prob1), (Pr1 = 0.0) ,
         Pr2 is float(Prob2), 
         ( (Pr2 \= 0.0) ; 
           (Pr2 = 0.0) ,  V1 =< V2
          ).

lesseq(V1,Prob1,V2,Prob2) :-  
            (Prob1 \= 0.0) , (Prob2 \= 0.0) , V1 =< V2.

greatereq(_,Prob1,_,Prob2) :-   (Prob1 \= 0.0) , (Prob2 = 0.0) .

greatereq(V1,Prob1,V2,Prob2) :-
            (Prob1 \= 0.0) , (Prob2 \= 0.0) , V2 =< V1.

% }}}


:- write("**********   ... decisionTheoretic.pl done.\n"). 
