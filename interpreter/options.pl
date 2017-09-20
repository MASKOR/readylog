/***********************************************************
    Options
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


/* This file contains all predicates relevant for automatic
option creation via local MDPs. */

:- write("  ----> Loading options.pl \t ...\n").

:- discontiguous([
	bestDoMOpt/10
]).

/* ==========================================================
   Automatic Option Creation
========================================================== */
% {{{ Automatic Option Creation
% >>>>

/*
  These predicates are used to solve a local MDP and to compute a
  model for its behavior. This will later be transformed by the
  preprocessor to obtain a new stochastic procedure.
*/

/* ----------------------------------------------------------
   The Iteration
---------------------------------------------------------- */
/*
Iterate until the value function at all states has converged */
create_option(Name, Prog, L, Epsilon, H) :-
	H2 is H + 1,
	create_option_Aux( Name, Prog, L, Epsilon, H2, Converged),
	write("+"), flush(output), !,
	(
	  Converged ->
	  true
	;
	  create_option(Name, Prog, L, Epsilon, H2)
	).

create_option_Aux( _, _, [], _, _, true).
create_option_Aux( Name, Prog, [S|L], Epsilon, H, Converged) :-
	opt_state_tmp( Name, D, S),
	(
	  H_old is H - 1,
	  opt_exp_reward(Name, D, H_old, (_,V_old))
	;
	  /* at first run there is no value available */
	  V_old = -10000.0
	), !,

	/* - body - */
	bestDoMOpt( Name, Prog, checkEvents, S, H, _, _, 0, _, _), !,
	opt_exp_reward(Name, D, H, (_,V_new)),
	V_diff is abs(V_new - V_old),

	/* - recursion - */
	create_option_Aux( Name, Prog, L, Epsilon, H, Converged_rest),

	/* - result */
	(
	  V_diff =< Epsilon, Converged_rest ->
	  Converged = true
	;
	  Converged = false
	).


/* ----------------------------------------------------------
   bestDoMOpt
---------------------------------------------------------- */
/* Look-up values if possible */
bestDoMOpt( Name, _Prog, _Events, S, H, R, PL, 0, _, PseudoV) :-
	opt_state_tmp( Name, D, S),
	opt_prob( Name, D, H, PL),
	opt_exp_reward( Name, D, H, (R, PseudoV)).


/* Exit state reached, assert values for all H */
bestDoMOpt( Name, _Prog, _Events, S, _, R, PL, 0, _, PseudoReward) :-
	not init_tmp(Name, S) ->
	(
	  option_beta( Name, Cond, PseudoReward), holds(Cond, S)
	;
	  PseudoReward = 0.0
	), 
	(
	  R is 0.0,		% no more rewards to earn from here
	  opt_state_tmp( Name, D, S),
	  PL = [(D, 1.0)],
	  assert( opt_prob( Name, D, _, PL) ),
	  assert( opt_exp_reward( Name, D, _, (R, PseudoReward)) )
	).


/* ----------------------------------------------------------
   Explicit Events
*/
% {{{ Explicit Events

% >>>>

/* check if any explicit event is defined for this situation */
/* get list of all events defined for this situation */
bestDoMOpt(Name, Action, checkEvents, S, H, R, PL,
	   Depth, Pol, PseudoV) :-   H > 0, 
	(
	  setof( Event, event_aux(Event), Eventlist) ->
	  /* at least one event defined */
	  bestDoMOpt_event(Name, Action, Eventlist, S, H, R, PL,
			   Depth, PseudoV),
	  Pol = []
	;
	  /* else: just go ahead with next action */
	  bestDoMOpt(Name,Action,ignoreEvents,S,H,R,PL,Depth,
		     Pol,PseudoV)
	).

/* execute all possible events in order of definition [chaining] */
bestDoMOpt_event(Name, Action, [], S, H, R, PL, Depth, PseudoV) :-
	bestDoMOpt(Name,Action,ignoreEvents,S,H,R,PL,Depth,_,PseudoV).

bestDoMOpt_event(Name, Action, [Event| Rest], S, H, R, PL,
		 Depth, PseudoV) :-
	(
	  /* check if event possible */
	  prolog_event_poss(Event, S) ->
	  event_outcomes(Event,S,NatOutcomesList,_SenseEffect), !,
	  bestDoMOpt_event_Aux(Name, Action, Rest, NatOutcomesList, S,
			       H, R_list, PL, Depth, PseudoV_list),
	  prolog_function(reward, Reward, S),
	  /* if costs are defined for this stoch_proc, use them,
	  else 0.0 */
	  ( prolog_event_costs(Event, Costs, S) ; Costs = 0.0 ),
	  option(Name, _Body, Gamma),
	  R is Reward - Costs + Gamma*R_list,
	  PseudoV is Reward - Costs + Gamma*PseudoV_list,
	  opt_update_Optionbase(Depth, Name, S, H, PL, R, PseudoV, [])
	;
	  bestDoMOpt_event(Name, Action, Rest, S, H,
			   R, PL, Depth, PseudoV)
	).
	  
/* execute a certain event: create all outcomes, go on with
remaining events [branching] */
bestDoMOpt_event_Aux(Name, Action, RestEvents, [(Prog,Pr,_SenseCond)],
		     S, H, R, PL, Depth, PseudoV) :-
	H >= 0, 
	transStar(Prog, S, _Prog_remain, NewSit),
	bestDoMOpt_event(Name,Action,RestEvents,NewSit,H,R_rest,
			 _PL_rest,Depth,PseudoV_rest), !,
	R is Pr*R_rest,
	PseudoV is Pr*PseudoV_rest,
	opt_state_tmp(Name, SucState, NewSit),
	PL = [(SucState, Pr)].

bestDoMOpt_event_Aux(Name, Action, RestEvents,
		     [(Prog,Pr,_SenseCond)|OtherOutcomes],
		     S,H,R,PL, Depth, PseudoV) :-
	H >= 0, 
	not OtherOutcomes = [],
	transStar(Prog, S, _Prog_remain, NewSit),
	bestDoMOpt_event_Aux(Name, Action, RestEvents, OtherOutcomes,
			     S, H, R_tree, PL_tree, Depth,
			     PseudoV_tree),
	bestDoMOpt_event(Name,Action,RestEvents,NewSit,H,R_rest,
			 _PL_rest,Depth,PseudoV_rest), !,
	R is R_tree + Pr*R_rest,
	PseudoV is PseudoV_tree + Pr*PseudoV_rest,
	opt_state_tmp(Name, SucState, NewSit),
	TempPL = [(SucState, Pr)],
	opt_PL_add( PL_tree, TempPL, PL).

% }}}
% <<<<

/* ----------------------------------------------------------
   If and nondet
*/
/* ____ if ____ */
bestDoMOpt( Name, if(C,E1,E2), ignoreEvents, S, H, R, PL, Depth,
	    Pol, PseudoV ) :-
	H >= 0,
	(
	  holds( C, S) ->
	  bestDoMOpt( Name, E1, ignoreEvents, S, H, R, PL, Depth,
		      Pol, PseudoV)
	;
	  bestDoMOpt( Name, E2, ignoreEvents, S, H, R, PL, Depth,
		      Pol, PseudoV)
	).

/* ____ nondet ____ */
bestDoMOpt( Name, nondet(L), ignoreEvents, S, H, R, PL, Depth,
	    Pol, PseudoV ) :-
	H >= 0,
	NDepth is Depth+1,
	bestDoMOpt_nondet_Aux( Name, L, S, H, R, PL, NDepth,
			       Pol, PseudoV ),
	opt_update_Optionbase(Depth, Name, S, H, PL, R, PseudoV, Pol).

bestDoMOpt_nondet_Aux( Name, [E1], S, H, R, PL, Depth,
		       Pol, PseudoV ) :-
	bestDoMOpt( Name, E1, ignoreEvents, S, H, R, PL, Depth,
		    Pol, PseudoV).

bestDoMOpt_nondet_Aux( Name, [E1|L_rest], S, H, R, PL, Depth, Pol,
		       PseudoV ) :-
	L_rest \= [],
	bestDoMOpt( Name, E1, ignoreEvents, S, H, R1, PL1, Depth,
		    Pol1, PseudoV1),
	bestDoMOpt_nondet_Aux( Name, L_rest, S, H, R2, PL2, Depth,
			       Pol2, PseudoV2 ),
	(
	  PseudoV1 >= PseudoV2,
	  PL = PL1, R = R1, Pol=Pol1, PseudoV = PseudoV1
	;
	  PseudoV2 > PseudoV1,
	  PL = PL2, R = R2, Pol=Pol2, PseudoV = PseudoV2
	).


/* ____ primitive action ____ */
bestDoMOpt( Name, A, ignoreEvents, S, H, R, PL, Depth, Pol, PseudoV) :-
	H > 0,
	prim_action(A), !,
	(
	  (
	    prolog_poss(E) -> prolog_poss(E, S) 
	  ; poss(E,Cond), holds(Cond,S)	) ->
	  Hor is H - 1,
	  Pol=A,
	  option(Name, Prog, _Gamma ),
	  bestDoMOpt( Name, Prog, checkEvents, [A|S], Hor, RR, _PL,
		      0, _, PseudoVR),
	  prolog_function(reward, R1, S),
	  option(Name, _Body, Gamma),
	  R is R1 + Gamma*RR,
	  PseudoV is R1 + Gamma*PseudoVR,
	  opt_state_tmp(Name, SucState, [A|S])
	;
	  prolog_function(reward, R, S),
	  opt_state_tmp(Name, SucState, S),
	  Pol=[], PseudoV = 0.0
	),
	PL = [(SucState, 1.0)],
	opt_update_Optionbase(Depth, Name, S, H, PL, R, PseudoV, Pol).


/* ____ stochastic procedure ____ */
bestDoMOpt(Name, A, ignoreEvents, S, H, R, PL, Depth, Pol, PseudoV) :-
	H > 0, 
	stoch_proc(A), !,
	(
	  stoch_proc_poss(A, S) ->
  	  stoch_proc_outcomes(A,S,NatOutcomesList,_SenseEffect),
	  Hor is H - 1,
	  bestDoMOpt_stoch_Aux(Name, NatOutcomesList,S,Hor,R_list,
			       PL,PseudoV_list),
	  prolog_function(reward, R1, S),
	  ( stoch_proc_costs(A, Costs, S) ; Costs = 0.0 ),
	  option(Name, _Body, Gamma),
	  R is R1 - Costs + Gamma*R_list,
	  PseudoV is R1 - Costs + Gamma*PseudoV_list,
	  Pol=A
	;
	  prolog_function(reward, R, S),
	  Pol=[], PseudoV = 0.0, PL = []
	),
	opt_update_Optionbase(Depth, Name, S, H, PL, R, PseudoV, Pol).
	  
/* this case usually does not happen: only in case the user
really defined an empty list of outcomes */
bestDoMOpt_stoch_Aux(Name, [], S, H, R, PL, PseudoV) :-
	option(Name, OptionBody, _Gamma ),
	bestDoMOpt(Name,OptionBody,checkEvents,S,H,R,
		   PL,0, _, PseudoV).

bestDoMOpt_stoch_Aux(Name, [(Prog,Pr,_SenseCond)], S, H, R,
		     PL, PseudoV) :-
	( transStar(Prog, S, _Prog_remain, NS) ; NS = S),
	option(Name, OptionBody, _Gamma ),
	bestDoMOpt(Name,OptionBody,checkEvents,NS,H,R_rest,_PL_rest,0,
		   _, PseudoV_rest), !,
	R is Pr*R_rest,
	PseudoV is Pr*PseudoV_rest,
	opt_state_tmp(Name, SucState, NS),
	PL = [(SucState, Pr)].

bestDoMOpt_stoch_Aux(Name, [(Prog,Pr,_SenseCond) | OtherOutcomes],
		     S,H,R,PL, PseudoV) :-
	not OtherOutcomes = [],
	( transStar(Prog, S, _Prog_remain, NS) ; NS = S),
	bestDoMOpt_stoch_Aux(Name, OtherOutcomes,S,H,R_tree,
			     PL_tree, PseudoV_tree),
	option(Name, OptionBody, _Gamma ),
	bestDoMOpt(Name,OptionBody,checkEvents,NS,H,R_rest,
		   _PL_rest,0,_,PseudoV_rest), !,
	R is R_tree + Pr*R_rest,
	PseudoV is PseudoV_tree + Pr*PseudoV_rest,
	opt_state_tmp(Name, SucState, NS),
	TempPL = [(SucState, Pr)],
	opt_PL_add( PL_tree, TempPL, PL).



/* ____ horizon zero ____ 
assert zeros and basic reward */
/* should not be needed in current implementation */
bestDoMOpt( Name, _, _Events, S, H, R, PL, Depth, _, PseudoV) :-
	H =:= 0,
	prolog_function(reward, R, S),
	PseudoV is R,
	PL = [],
	opt_update_Optionbase(Depth, Name, S, H, PL, R, PseudoV, []).

% }}}



/* ==========================================================
   Automatic Model Creation
========================================================== */
% {{{ Automatic Model Creation
% >>>>

/* for each state we need a model of how the world will evolve
when taking/continuing the option in this state. A model consists
of the probabilities for leaving the options state-sub-space by a
certain exit-state and the assigned expected value. This is needed
for using the option in planning.
*/

/* ----------------------------------------------------------
   The Iteration
---------------------------------------------------------- */
/*
Iterate until the value function at all states has converged */
create_model(Name, Lstates, Epsilon, H) :-
	H2 is H + 1,
	create_model_Aux( Name, Lstates, Lstates,
			  Epsilon, H2, Converged),
	write(">"), flush(output), !,
	(
	  Converged ->
	  true
	;
	  create_model(Name, Lstates, Epsilon, H2)
	).

create_model_Aux( _, [], _, _, _, true).
create_model_Aux( Name, [State|L], Istates,
		  Epsilon, H, Converged) :-
	(
	  H_old is H - 1,
	  opt_exit_prob( Name, State, H_old, EPL_old)
	;
	  EPL_old = []
	), !,

	/* - body - */
	bestDoMModel( Name, State, Istates, H, EPL), !,
	opt_PL_sum(EPL, ExitSum),
	opt_PL_sum(EPL_old, ExitSum_old),

	V_diff is abs(ExitSum - ExitSum_old),

	/* - recursion - */
	create_model_Aux( Name, L, Istates,
			  Epsilon, H, Converged_rest),

	/* - result */
	(
	  V_diff =< Epsilon, Converged_rest ->
	  Converged = true
	;
	  Converged = false
	).


bestDoMModel( Name, State, Istates, H, EPL) :-
	opt_prob_max( Name, State, PL), /* one step transitions */
	extract_exit_states( PL, Istates, EPL_local, IPL_local),
	H_last is H-1,
	bestDoMModel_Aux( Name, IPL_local, H_last, EPL_last),
	opt_PL_add( EPL_last, EPL_local, EPL),
	H_last_last is H-2,
	retractall( opt_exit_prob( Name, State, H_last_last, _) ),
	assert( opt_exit_prob( Name, State, H, EPL) ).

extract_exit_states( [], _, [], []).
extract_exit_states( [(D, Pr)|PL_rest], Istates, EPL, IPL) :-
	extract_exit_states( PL_rest, Istates, EPL_rest, IPL_rest),
	(
	  member( D, Istates) ->
	  EPL = EPL_rest,
	  IPL = [(D,Pr)|IPL_rest]
	;
	  EPL = [(D,Pr)|EPL_rest],
	  IPL = IPL_rest
	).

bestDoMModel_Aux( _Name, [], _H, []).
bestDoMModel_Aux( _Name, _IPL, 0, []) :- !.
bestDoMModel_Aux( Name, [(D,Pr)|IPL_rest], H, EPL) :-
	bestDoMModel_Aux( Name, IPL_rest, H, EPL_rest),
	opt_exit_prob( Name, D, H, EPL_D),
	opt_PL_multiply( EPL_D, Pr, EPL_D_weighted),
	opt_PL_add(EPL_D_weighted, EPL_rest, EPL).

% }}}


/* ==========================================================
   Auxiliary
========================================================== */
% {{{ Auxiliary
% >>>>


/* -- Probability list (PL) auxiliary methods */

/* multiply a PL by a scalar */
%                PLin,        Scalar,   PLout
opt_PL_multiply( [],          _,        [] ).
opt_PL_multiply(  L,          1.0,       L ).
opt_PL_multiply( [(D,PA)|L],  S,        [(D,PNA)|NL] ) :-
	PNA is PA*S, opt_PL_multiply( L, S, NL).


/* add two PLs */
%           Ain,    Bin,    Cout
opt_PL_add( [],     [],     []).
opt_PL_add( [A|AL], [],     [A|AL]).
opt_PL_add( [],     [B|BL], [B|BL]).
/* both heads describe same outcome */
opt_PL_add( [(D,PA)|AL], [(D,PB)|BL], [(D,PC)|CL]) :- 
	PC is PA+PB, opt_PL_add( AL, BL, CL).
/* heads describe different outcomes, skip second head for now,
 * and add it when coming back */
opt_PL_add( [(DA,PA)|AL], [(DB,PB)|BL], CL) :-
	DA \= DB, opt_PL_add( [(DA,PA)|AL], BL, ZL),
	opt_PL_add_simple( (DB,PB), ZL, CL).

opt_PL_add_simple( A, [], [A]).
opt_PL_add_simple( (D,PA), [(D,PB)|BL], [(D,PC)|BL]) :-
	PC is PA+PB.
opt_PL_add_simple( (DA,PA), [(DB,PB)|BL], [(DB,PB)| ZL]) :-
	DA \= DB, opt_PL_add_simple( (DA,PA), BL, ZL).

/* calculate the sum of all elements of a probability list */
opt_PL_sum([], 0).
opt_PL_sum([(_D,P)|L], X) :-
	opt_PL_sum(L, Xrest), X is P + Xrest.



/* retract all but the maximal horizon */
opt_retract( _, [] ).
opt_retract( Name, [D|L] ) :-
	opt_retract_Aux(Name, D),
	opt_retract( Name, L).

opt_retract_Aux( Name, D ) :-
	opt_retract_Aux_prob(Name, D),
	opt_retract_Aux_exp_reward(Name, D),
	opt_retract_Aux_pol(Name, D).

opt_retract_Aux_prob(Name, D):-
	opt_prob( Name, D, H1, Prob), H2 is H1+1,
	not opt_prob( Name, D, H2, _),
	retractall( opt_prob(Name, D, _, _) ),
	assert( opt_prob_max( Name, D, Prob) ).	

opt_retract_Aux_exp_reward(Name, D):-
	opt_exp_reward( Name, D, H1, (ER,_)), H2 is H1+1,
	not opt_exp_reward( Name, D, H2, _),
	retractall( opt_exp_reward(Name, D, _, _) ),
	assert( opt_exp_reward_max( Name, D, ER) ).	

opt_retract_Aux_pol(Name, D):-
	(
	  opt_pol( Name, D, H1, P),
	  H2 is H1+1,
	  not opt_pol( Name, D, H2, _), !,
	  retractall( opt_pol(Name, D, _, _) ),
	  assert( opt_pol_max( Name, D, P) )
	;
	  /* if no policy exists for this state, it must be
	  unreachable (and events beaming away from it).
	  Then just write the empty program, as it will never
	  ever be relevant afterall. */
	  assert( opt_pol_max( Name, D, []) )
	).
	  
opt_retract_model( _, [] ).
opt_retract_model( Name, [D|L] ) :-
	opt_exit_prob( Name, D, H1, Prob), H2 is H1+1,
	not opt_exit_prob( Name, D, H2, _),
	retractall( opt_exit_prob(Name, D, _, _) ),
	assert( opt_exit_prob_max( Name, D, Prob) ),
	opt_retract_model( Name, L).


/* provide opt_prob to call with S-term */
opt_prob( Name, PL, S) :-
	opt_state_tmp( Name, D, S),
	opt_prob_max( Name, D, PL).

/* provide opt_exp_reward to call with S-term */
opt_exp_reward( Name, R, S) :-
	opt_state_tmp( Name, D, S),
	opt_exp_reward_max( Name, D, R).

/* provide opt_pol to call with S-term */
opt_pol( Name, Pol, S) :-
	opt_state_tmp( Name, D, S),
	opt_pol_max( Name, D, Pol).


/* opt_makeBody(Name):
 * construct an executable policy we can plug in,
 * when using option 'Name', and save it in the
 * KB under opt_Body(Name)
*/
opt_makeBody(Name) :-
	setof( (Description, LocalPolicy),
	       opt_pol_max( Name, Description, LocalPolicy),
	       PolicyMap ),
	opt_makeBody_Aux( Name, PolicyMap, Cases),
	option_sense(Name, SenseProgram),
	flatten( [Cases, SenseProgram], Prog),
	Pol1 = [SenseProgram, while( is_possible(Name), Prog )],
	flatten( Pol1, Policy),
	assert( opt_Body(Name, Policy)).

opt_makeBody_Aux( Option, [(Description, LocalPolicy)], Case ) :-
	option_mapping_tmp( Option, Description, _, Condition),
	Case = (if(Condition, LocalPolicy, [])).
opt_makeBody_Aux( Option, [(Description, LocalPolicy)| L], Case ) :-
	not L=[],
	opt_makeBody_Aux( Option, L, Cases_rest ),
	option_mapping_tmp( Option, Description, _, Condition),
	Case = (if(Condition, LocalPolicy, Cases_rest)).


opt_update_Optionbase(Depth, Name, S, H, PL, R, PseudoV, Pol) :-
	Depth =:= 0, !,
	opt_state_tmp( Name, D, S),
	H_last_last is H-2,
	retractall( opt_prob( Name, D, H_last_last, _) ),
	assert( opt_prob( Name, D, H, PL) ),
	retractall( opt_exp_reward( Name, D, H_last_last, _) ),
	assert( opt_exp_reward( Name, D, H, (R, PseudoV)) ),
	retractall( opt_pol( Name, D, H_last_last, _) ),
	assert( opt_pol( Name, D, H, Pol)).	
opt_update_Optionbase(Depth, _Name, _S, _H, _PL, _R, _PseudoV, _Pol) :-
	Depth > 0, !.


/* (7.9.03) probability list to outcomes: */
/* (needed at projection-time) */
opt_PL_to_Outcomes( _O, [], _S, [] ).
opt_PL_to_Outcomes( Option, [(D,P)|PL], S, [Out|Outcomes] ) :-
	option_mapping( Option, D, Prog, SenseCond),
	Out = (Prog, P, SenseCond),
	opt_PL_to_Outcomes( Option, PL, S, Outcomes ).


% }}}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- write("  <---- Loading options.pl \t[DONE]\n"). 

