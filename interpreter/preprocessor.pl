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
 *         @date: 12.5.03
 *       @author: Christian Fritz <Christian.Fritz@rwth-aachen.de>
 *   description: preprocessor
 * last modified: $Date$
 *            by: $Author$
 *
 * **************************************************************************/

/* ================================================================== */
/*  PREPROCESSOR                                                      */
/* ================================================================== */

/* In this file we provide the functionality to preprocess parts
 * of READYLOG program files, generating executable prolog code. The
 * intention behind this is to save time online where possible. Also
 * options and stochastic procedures and events are computed here.
 */

:- write("** loading preprocessor.pl\n"). 

:- ensure_loaded('readylog.pl').
:- ensure_loaded("../utils/utils.pl").

/* ----------------------------------------------------------
   utilities
---------------------------------------------------------- */
% {{{ utilities

/* needed to create fresh variable names (otherwise they clash in
concatenated bodies) */
:- lib(var_name).
local_var(L) :- set_var_name(L, "PreVar").

/* conjunct two bodys:
intended for beautifying bodies, e.g.: (true, A) -> A */ 
conjunct( true, B, B).
conjunct( A, true, A).
conjunct( false, _B, false).
conjunct( _A, false, false).
conjunct( A, B, (A, B)).

/* disjunct two bodys:
intended for beautifying bodies, e.g.: (true; A) -> true */ 
disjunct( true, _B, true).
disjunct( _A, true, true).
disjunct( false, B, B).
disjunct( A, false, A).
disjunct( A, B, (A; B)).

/* generate implication but rule out yet evaluated cases
if they are impossible (e.g. "false -> and([..])") */
imply( false, _Body, false).
imply( _Cond, true, true).
imply( _Cond, false, false).
imply( Cond, Body, ((Cond) -> Body)).


% }}}



/* ----------------------------------------------------------
   conditions
---------------------------------------------------------- */
% {{{ conditions
% >>>>

/* process_condition( C, S, Body)
(cf. holds(C, S) in readylog)
states, that Body is the Prolog body which holds iff the
condition C holds in situation S.
process_condition_not(C, S, Body) analogous for negated
conditions.
*/


% {{{ >>> process_condition <<<
% >>>>

process_condition( and(C1,C2), S, Body ) :-
	process_condition( C1, S, C1New),
	process_condition( C2, S, C2New),
	conjunct(C1New, C2New, Body).
process_condition( and([]), _S, true ).
process_condition( and([C|C_rest]), S, Body ) :-
	process_condition( C, S, CNew),
	process_condition(and(C_rest), S, C_rest_new),
	conjunct(CNew, C_rest_new, Body).
process_condition( or(C1,C2), S, Body ) :-
	process_condition( C1, S, C1New),
	process_condition( C2, S, C2New),
	disjunct(C1New, C2New, Body).
process_condition( or([C]), S, CNew ) :-
	process_condition( C, S, CNew).
process_condition( or([C|C_rest]), S, Body ) :-
	process_condition( C, S, CNew),
	process_condition( or(C_rest), S, C_rest_new),
	disjunct(CNew, C_rest_new, Body).
	
process_condition( some(V,P), S, New ) :-
	subv(V,_,P,P_sub), process_condition(P_sub, S, New).
process_condition( exists(V,P), S, New) :-  /* synonm for some */
	subv(V,_,P,P_sub), process_condition(P_sub, S, New).

/* bind a variable to a finite domain:
IMPORTANT: lib(fd) must be loaded in first place */
process_condition( domain(Var, Domain), _S, Body) :-
	(
	  local_var([Var]) ->
	  Body = (Var::Domain, indomain(Var))
	;
	  /* a Var got from local_var cannot be passed to
	  local_var again, also non-variables shall be
   	  considered */
	  (
	    var(Var) ->
	    Body = (Var::Domain, indomain(Var))
	  ;
	    Body = (Var::Domain)
	  )
	).
	  

process_condition( false, _S, fail ).
process_condition( true, _S, true ).

process_condition( know(Phi), S,
		   holds(bel(Phi)=1,S) ).
process_condition( lookahead(P,T,E,LL), S,
		   (cache(limitedProjTest(P,T,E,LL,Erg,S)),
		       Erg=true) ).
process_condition( neg(P), S, New) :-
	process_condition( not(P), S, New).  /* synonym for not */
process_condition( not(P), S, New) :-
	(
	  nonground(P) -> process_condition_not(P, S, New)
	;
	  process_condition( P, S, NotBody ),
	  New = (not(NotBody))
	).

process_condition( is_possible(A), S, New) :-
	process_subf(A, A_sub, Body_sub, _Type, S),
	Body = 	((
		  stoch_proc(A_sub) ->
		  stoch_proc_poss(A_sub, S)
		;
		  prolog_poss( A_sub ) ->
		  prolog_poss( A_sub, S)
		;
		  poss(A_sub, Cond) ->
		  holds(Cond, S)
		)),
	conjunct(Body_sub, Body, New).

process_condition( lif(Cond, Cond1, Cond2), S, New) :-
	process_condition(Cond, S, Cond_body),
	process_condition(Cond1, S, Cond1_body),
	process_condition(Cond2, S, Cond2_body),
	New = ((
		Cond_body ->
		(Cond1_body)
	      ;
		(Cond2_body)
	      )).

/* for debugging: we allow printing in conditions */
process_condition( writeln(Text), _S, writeln(Text)).
process_condition( write(Text), _S, write(Text)).

process_condition( P, S, New) :- /* processing comparisions */ 
	local_var([A,B]),
	P=..[Op,A,B], (Op=(=);Op=(<);(Op=(>);(Op=(=<);Op=(>=)))), !,
	process_comparison(false,Op,A,B,New,S).
process_condition( P, S, New) :-
	proc(P,_), !, process_subf( P, P1, Body_args, _Type, S),
	proc(P1,P2), process_condition( P2, S, Body_proc),
	conjunct(Body_args, Body_proc, New).

/* rule for functions */
process_condition( P, S, New) :-
	/* generate a dummy list of arguments before testing
	if function: if some args are vars they would get
	bound otherwise */
	P=..[F|L1],
	(
	  length(L1, ArgC),
	  generate_dummy_list( ArgC, L_dummy),
	  P_dummy=..[F|L_dummy],
	  (
	    function(P_dummy,_,_) ->
	    /* evaluate parameters */
	    process_subf_list( L1, L2, Body_list, S),
	    P_sub=..[F|L2],  
	    conjunct( Body_list, prolog_function(P_sub, P_res, S), Body1),
	    conjunct( Body1, holds(P_res, [s0]), New)
	  ;
	    exog_function(P_dummy) ->
	    process_subf_list( L1, L2, Body_list, S),
	    P_sub=..[F|L2],  
	    conjunct( Body_list, exog_function_getValue(P_sub, P_res), Body1),
	    conjunct( Body1, holds(P_res, [s0]), New)
	  )
	).

/* usual prolog call (calling a predicate) */
/* #think: add some test like pred(P) here ? */
process_condition( P, S, New) :-
	process_subf( P, P_call, Body_args, _Type, S),
	conjunct( Body_args, P_call, New).


% }}}

% {{{ >>> process_condition_not <<<
% >>>>

/* the not cases: move negation inside (as holds_not/2 does) */
process_condition_not( and(C1,C2), S, Body ) :-
	process_condition( or(not(C1), not(C2)), S, Body).
process_condition_not( and(C), S, CNew ) :-
	not_list(C, NotC), process_condition( or(NotC), S, CNew).
process_condition_not( or(C1,C2), S, Body ) :-
	process_condition( and(not(C1), not(C2)), S, Body).
process_condition_not( or(C), S, CNew ) :-
	not_list(C, NotC), process_condition( and(NotC), S, CNew).

process_condition_not( false, _S, true ).
process_condition_not( true, _S, fail ).

/* double negation */
process_condition_not( neg(P), S, New) :-
	process_condition( P, S, New).  /* synonym for not */
process_condition_not( not(P), S, New) :-
	process_condition( P, S, New).

process_condition_not( is_possible(A), S, New) :-
	process_subf(A, A_sub, Body_sub, _Type, S),
	Body = 	((
		  stoch_proc(A_sub) ->
		  not(stoch_proc_poss(A_sub, S))
		;
		  prolog_poss( A_sub ) ->
		  not(prolog_poss( A_sub, S))
		;
		  poss(A_sub, Cond) ->
		  holds(not(Cond), S)
		)),
	conjunct(Body_sub, Body, New).

process_condition_not( lif(Cond, Cond1, Cond2), S, New) :-
	process_condition(Cond, S, Cond_body),
	process_condition(not(Cond1), S, Cond1_not_body),
	process_condition(not(Cond2), S, Cond2_not_body),
	New = ((
		Cond_body ->
		Cond1_not_body
	      ;
		Cond2_not_body
	      )).

process_condition_not( P, S, New) :- /* processing comparisions */ 
	local_var([A,B]),
	P=..[Op,A,B], (Op=(=);Op=(<);(Op=(>);(Op=(=<);Op=(>=)))), !,
	process_comparison(true,Op,A,B,New,S).

process_condition_not( P, S, New) :-
	proc(P,_), !, process_subf( P, P1, Body_args, _Type, S),
	proc(P1,P2), process_condition( not(P2), S, Body_proc),
	conjunct(Body_args, Body_proc, New).

process_condition_not( P, S, New) :-
	process_condition( P, S, Body_positive),
	New = not(Body_positive).

% }}}

% {{{ >>> process_comparison <<<

% >>>>

/* corresponding to eval_comparison of icpGolog. Note: we take a
shift from the semantics in icpGolog as we do never compare
t_functions syntactically but only by their current value.
Further we added some simple type-checking mechanism, mainly used
to find out what already at compile time can be evaluated */
process_comparison( Not_flag, Op, A, B, New, S) :-
	local_var([A1,B1]),
	process_subf(A,A1,BodyA,TypeA,S),
	process_subf(B,B1,BodyB,TypeB,S),
	Expression_tmp =.. [Op,A1,B1],
	(
	  Not_flag -> Expression = not(Expression_tmp)
	; Expression = Expression_tmp),
	(
	  TypeA = eval, TypeB = eval ->
	  /* seldom case where things can already at compile
	  time be tested */
	  (
	    call(Expression),
	    New = Expression
	  ;
	    printf("** process_comparison: unsatisfiable ", []),
	    printf("condition (	%w) found\n - was that ", []),
	    printf("intended? ruling out\n", [Expression]),
	    New = false
	  )
	;
	  /* default: */
	  conjunct(BodyA, BodyB, Tmp1),
	  conjunct(Tmp1, Expression, New) /* maybe cut in between */
	).

% }}}

/* negate all elements in a list */
not_list( [], []).
not_list( [A|B], [not(A)|B2]) :-  not_list(B,B2).

% }}}

/* ----------------------------------------------------------
   subf (expression evaluation)
---------------------------------------------------------- */
% {{{ subf
% >>>>

/* process_subf(A, A_value, Body, Type, S)
(cf. subf(A, V, S) in readylog)
states, that A has the value A_value in situation S after performing
Body in prolog and having type Type (var, eval, number, fluent_value,
constant, list(Arity), predicate, arithmetic_expression ).
Note, that in general A_value will still be variable which only
bound when Body is executed in Prolog at runtime of the program
(i.e. not yet at preprocessing time, except when Type is 'eval')
*/
process_subf(A, A, true, var, _S) :- var(A), !.
process_subf(A, A, true, eval, _S) :- number(A), !.
process_subf(A, A, true, const, _S) :- const(A), !.

/* order of these arithmetic rules are important, as they
implicitly define precedence of "*" and "/" before "+" and "-" */
process_subf(A+B, C, Body, Type, S) :-
	local_var([ValA,ValB]),
	process_subf(A, ValA, BodyA, TypeA, S),
	process_subf(B, ValB, BodyB, TypeB, S),
	process_subf_aux( +, ValA, BodyA, TypeA, ValB, BodyB,
			  TypeB, C, Body, Type).
process_subf(A-B, C, Body, Type, S) :-
	local_var([ValA,ValB]),
	process_subf(A, ValA, BodyA, TypeA, S),
	process_subf(B, ValB, BodyB, TypeB, S),
	process_subf_aux( -, ValA, BodyA, TypeA, ValB, BodyB,
			  TypeB, C, Body, Type). 
process_subf(A*B, C, Body, Type, S) :-
	local_var([ValA,ValB]),
	process_subf(A, ValA, BodyA, TypeA, S),
	process_subf(B, ValB, BodyB, TypeB, S),
	process_subf_aux( *, ValA, BodyA, TypeA, ValB, BodyB,
			  TypeB, C, Body, Type). 
process_subf(A/B, C, Body, Type, S) :-
	local_var([ValA,ValB]),
	process_subf(A, ValA, BodyA, TypeA, S),
	process_subf(B, ValB, BodyB, TypeB, S),
	process_subf_aux( /, ValA, BodyA, TypeA, ValB, BodyB,
			  TypeB, C, Body, Type). 

/* explicit rule for lists: analogous rule is missing in icpGolog */
process_subf([H|L], P_res, Body, list, S) :-
	process_subf_list( [H|L], P_res, Body, S).

process_subf(P, P_res, Body, fluent_value, S)  :-
	local_var([P2,P3,T1]),
	fluent(P), !, 
	(
	  special_fluent(P) -> P2=P 
	; P =..[F|L1],  /* evaluate parameters */
	  process_subf_list(L1,L2,Body_args,S),
	  P2 =..[F|L2]
	),
	(
	  is_prim_fluent(P) ->
	  conjunct( Body_args, has_val(P2,P_res,S), Body)
	;
	  /* is_cont_fluent: evaluate at current time if
	  it's a t_function */
	  conjunct( Body_args, has_val(P2,P3,S), Tmp1),
	  Body = ( Tmp1,
		     (
		       t_function(P3) ->
		       has_val(sit_start_time,T1,S), t_function(P3,P_res,T1)
		     ;
		       P_res=P3
		     )		     
		 )
	).

/* function call */
process_subf(P, P_res, Body, fluent_value, S )  :-
	/* generate a dummy list of arguments before testing
	if function: if some args are vars they would get
	bound otherwise */
	P=..[F|L1],
	(
	  F = idstring, L1=[Arg] ->
	  Body = term_string( Arg, P_res)
	;
	  length(L1, ArgC),
	  generate_dummy_list( ArgC, L_dummy),
	  P_dummy=..[F|L_dummy],
	  (
	    function(P_dummy,_,_) ->
	    /* evaluate parameters */
	    process_subf_list( L1, L2, Body_list, S),
	    P_sub=..[F|L2],  
	    conjunct( Body_list, prolog_function(P_sub, P_res, S), Body)
	  ;
	    exog_function(P_dummy) ->
	    process_subf_list( L1, L2, Body_list, S),
	    P_sub=..[F|L2],  
	    conjunct( Body_list, exog_function_getValue(P_sub, P_res), Body)
	  )
	).

/* special rule for set(fluentname, Value)-action:
the fluentname must not be subf-ed */
process_subf(P, P_res, Body_args, action, S) :-
	P=..[set,Fluentname|L],
	process_subf_list(L, L_sub, Body_args, S),
	P_res=..[set,Fluentname|L_sub].

/* usual prolog call (calling a predicate) */
/* I don't believe this makes sense: predicates
don't return anything!
Of course it does: subf is not only used in comparisons:
e.g. ?(goalWidth(GoalWidth))... no,
but: consider X=sqrt(4) - has to be transformed to X is sqrt(4).
(23.09.03) restricted to built_in()s: otherwise function
overlaoding in Readylog could lead to problems.
*/
process_subf(P, P_res, Body, Type, S) :-
	P=..[F|L1],
	process_subf_list(L1, L2, Body_args, S),
	length(L1, ArgC), ArgC_plus is ArgC + 1,
	(
	  string(F),
	  P_res =.. [F|L2]
	;
	  is_predicate(F/ArgC_plus),
	  is_built_in(F/ArgC_plus) -> /* arithmetic predicate call */
	  P_tmp=..[F|L2],
	  Type = number,
	  conjunct( Body_args, (P_res is P_tmp), Body)
	;
	  P_res=..[F|L2],
	  Body = Body_args,
	  Type = predicate
	).

/* else: nothing to evaluate (constant, e.g. pm_playon) */
process_subf(P, P, true, predicate, _S).


/* -------- aux ---------- */

/* typechecking: arguments in arithmetics have to be numbers or eval */
process_subf_aux(Op, ValA, BodyA, TypeA, ValB, BodyB, TypeB,
		 ValC, BodyC, TypeC) :-
%	local_var([ValC]),
	Expression =.. [Op, ValA, ValB],
	(
	  /* best case: everything can be evaluated at compile time */
	  TypeA = eval, TypeB = eval ->
	  ValC is Expression,
	  BodyC = true,
	  TypeC = eval
	;
	  /* if cannot be evaluated right now: leave ValC
	  uninstantiated and put evaluation into Body */
	  (
	    metaType(TypeA, meta_number), TypeB = eval ->
	    concat_string(["(",ValB,")"], StringB),
	    /* ^ needed e.g. for X is Y*(-10) instead of
	    X is Y*-10 (which wouldn't compile) */
	    Expression2 =.. [Op, ValA, StringB],
	    conjunct( BodyA, (ValC is Expression2), BodyC)
	  ;
	    TypeA = eval, metaType(TypeB, meta_number) ->
	    concat_string(["(",ValA,")"], StringA),
	    Expression2 =.. [Op, StringA, ValB],
	    conjunct( BodyB, (ValC is Expression2), BodyC)
	  ;
	    metaType( TypeA, meta_number),
	    metaType( TypeB, meta_number) ->
	    conjunct( BodyA, BodyB, Tmp1),
	    conjunct( Tmp1, (ValC is Expression), BodyC)
	  ),
	  TypeC = number
	;
	  /* error case */
	  printf("** error in process_subf_aux (%w): TypeA %w\tTypeB %w\n",
		 [Expression, TypeA, TypeB])
	).

/* important for things like X = [Y|V] where V is a free variable: */
process_subf_list( V, V, true, _S) :- var(V), !.
process_subf_list( [], [], true, _S).
process_subf_list( [I1|L1], [I2|L2], Body, S) :-
	local_var([I2]),
	process_subf( I1, I2, Body_local, _Type, S),
	process_subf_list( L1, L2, Body_rest, S),
	conjunct( Body_local, Body_rest, Body).


metaType(number, meta_number).
metaType(fluent_value, meta_number).
metaType(var, meta_number).
%metaType(var, meta_arithmetic).
%metaType(arithmetic_expression, meta_arithmetic).

/* -> better type concept? like e.g. fluent( Name, Type).. ?
already there: prim / cont (only cont can be t_functions)
maybe add lists, but it's not essential if we can stay without
componentwise comparison of lists
*/

generate_dummy_list( 0, []).
generate_dummy_list( N, [_|Rest]) :-
	N>0, N2 is N-1, generate_dummy_list( N2, Rest).


% }}}


/* ----------------------------------------------------------
   functions
---------------------------------------------------------- */
% {{{ functions
% >>>>

/* process_function( F, V, Cond, Sream)
preprocesses function F and writes the resulting prolog
clause (prolog_function(F, Val, S)) to Stream.
This is reather trivial, since we only have to preprocess
the condition (given in the function definitions) and
the value and conjoin the two resulting prolog bodies.
*/

process_function( FunctionName, Value, Cond, Stream ) :-
	process_condition(Cond, S, NewCondBody), !,
	process_subf( Value, Value_eval, Body_val, _Type, S),
	conjunct( NewCondBody, Body_val, Body),
	printf(Stream, "prolog_function( %w ).\n", [FunctionName]),
	printf(Stream, "prolog_function( %w, %w, %w ) :- %w.\n",
	       [FunctionName, Value_eval, S, Body] ).

process_all_functions(Stream) :-
	findall( (FunctionName, Value, Cond),
		 (
		   function(FunctionName, Value, Cond),
		   not prolog_function(FunctionName)
		 ), FunctionsList), !,
	process_all_functions_aux(FunctionsList, Stream).

process_all_functions(File, FileMode) :-
	findall( (FunctionName, Value, Cond),
		 (
		   function(FunctionName, Value, Cond),
		   not prolog_function(FunctionName)
		 ), FunctionsList), !,
	open( File, FileMode, Stream),
	process_all_functions_aux(FunctionsList, Stream),
	close(Stream).

process_all_functions_aux([], _Stream) :- !.
process_all_functions_aux([(FunctionName, Value, Cond)|List_rest],
			  Stream) :-
	process_function( FunctionName, Value, Cond, Stream), !,
	process_all_functions_aux(List_rest, Stream).	


% }}}

/* ----------------------------------------------------------
   action preconditions
---------------------------------------------------------- */
% {{{ action preconditions
% >>>>

/* process_poss( A, C, Stream)
preprocesses the precondition of action A and writes
the resulting prolog clause ('prolog_poss(A, S)') to
Stream. Again, this gets rather trivial by using
process_condition on the defined action precondition.
*/

process_poss( Action, Cond, Stream ) :-
	process_condition(Cond, S, NewCondBody), !,
	/* add arity one predicate to indicate: yes there is a
	preprocessed poss predicate. Use:
	"prolog_poss(A), !, prolog_poss(A, S)"  in Code */
	printf(Stream, "prolog_poss( %w ).\n", [Action]),
	printf(Stream, "prolog_poss( %w, %w ) :- %w.\n",
	       [Action, S, NewCondBody] ).

process_all_poss(Stream) :-
	findall( (Action, Cond),
		 ( poss(Action, Cond), not prolog_poss(Action)),
		 PossList), !,
	process_all_poss_aux(PossList, Stream).

process_all_poss(File, FileMode) :-
	findall( (Action, Cond),
		 ( poss(Action, Cond), not prolog_poss(Action)),
		 PossList), !,
	open( File, FileMode, Stream),
	process_all_poss_aux(PossList, Stream),
	close(Stream).

process_all_poss_aux([], _Stream) :- !.
process_all_poss_aux([(Action, Cond)|List_rest], Stream) :-
	process_poss( Action, Cond, Stream), !,
	process_all_poss_aux(List_rest, Stream).	


% }}}

/* ----------------------------------------------------------
   generateSSAs
---------------------------------------------------------- */
% {{{ generateSSAs
% >>>>

/* automatically generate successor state axioms (SSAs) from
effect axioms (causes_val's). The intentions behind this is that
SSAs in practice appear much faster, while on the other hand
causes_val's are more intuitve to implement, are strictly within
the language, and are the basis for the present algorithm for
progressing the knowledge base.
The applied conversion from effect axioms to SSAs is as described
in Reiter's solution to the frame problem.
*/

all_causes(L) :-
	setof(
		(Action, Fluent, NewValue, Condition)
		, (
			causes_val(Action, Fluent, NewValue, Condition)
			, not ssa(Fluent, _, [Action|_])
		)
		, L
	)
	; L = []
.


generateSSAs(Stream) :-
	all_causes(CausesVal_s),
	generateSSAs_aux( CausesVal_s, Stream ),
	printf(Stream, "\n/* \t___ dummy SSAs ___ */\n",[]),
	generate_dummy_SSAs(Stream).
	
generateSSAs(File, FileMode) :-
	all_causes(CausesVal_s),
	open( File, FileMode, Stream),
	generateSSAs_aux( CausesVal_s, Stream ),
	generate_dummy_SSAs(Stream),
	close(Stream).

generateSSAs_aux( [], _Stream ).
generateSSAs_aux( [(Action, Fluent, NewValue, Condition)| Rest_s],
		  Stream ) :-
	process_condition(Condition, Srest, Body_cond),
	process_subf(NewValue, EvalVal, Body_subf, _Type, Srest),
	conjunct( Body_cond, Body_subf, NewBody),
	printf(Stream, "ssa( %w, %w, %w ) :- !, %w.\n",
	       [Fluent, EvalVal, [Action|Srest], NewBody] ),
	generateSSAs_aux( Rest_s, Stream ).


compile_SSAs :-
	writeln(">>> Compiling SSAs...")
	, all_causes(L)
	, compile_SSAs(L)
	, writeln("<<< Done compiling SSAs.")
.

compile_SSAs( [] ).
compile_SSAs( [(Action, Fluent, NewValue, Condition) | Rest_s] ) :-
	process_condition(Condition, Srest, Body_cond)
	, process_subf(NewValue, EvalVal, Body_subf, _Type, Srest)
	, conjunct( Body_cond, Body_subf, NewBody)
	, SSA =.. [
		:-
		, ssa(Fluent, EvalVal, [Action|Srest])
		, NewBody
	]
	, writeln(SSA)
	, compile_term(SSA)
	, compile_SSAs(Rest_s)
.


/* finally: for all fluents write dummy ssa and
for all exog_fluent corresponding lookup rule */
generate_dummy_SSAs(Stream) :-
	(setof( F,
		(prim_fluent(F), not ssa(F,_,_)),
		PrimFluents);PrimFluents=[]),
	(setof( F,
		(cont_fluent(F), not ssa(F,_,_)),
		ContFluents);ContFluents=[]),
	(setof( F,
		(exog_prim_fluent(F), not ssa(F,_,_)),
		ExogPrimFluents);ExogPrimFluents=[]),
	(setof( F,
		(exog_cont_fluent(F), not ssa(F,_,_)),
		ExogContFluents);ExogContFluents=[]),
	!,
	generate_dummy_SSAs_aux_set(PrimFluents, Stream),
	generate_dummy_SSAs_aux_set(ContFluents, Stream),
	generate_dummy_SSAs_aux_set(ExogPrimFluents, Stream),
	generate_dummy_SSAs_aux_set(ExogContFluents, Stream),
	generate_dummy_SSAs_aux_exog(ExogPrimFluents, Stream),
	generate_dummy_SSAs_aux_exog(ExogContFluents, Stream),
	generate_dummy_SSAs_aux(PrimFluents, Stream),
	generate_dummy_SSAs_aux(ContFluents, Stream),
	generate_dummy_SSAs_aux(ExogPrimFluents, Stream),
	generate_dummy_SSAs_aux(ExogContFluents, Stream).

generate_dummy_SSAs_aux([], _Stream).
generate_dummy_SSAs_aux([Fluent|Rest], Stream) :-
	printf(Stream, "ssa( %w, %w, %w ) :- %w.\n",
	       [Fluent, NewValue, [_UnknownAction|Srest],
		(!, has_val(Fluent, NewValue, Srest))] ),
	generate_dummy_SSAs_aux( Rest, Stream).

generate_dummy_SSAs_aux_exog([], _Stream).
generate_dummy_SSAs_aux_exog([Fluent|Rest], Stream) :-
	printf(Stream, "ssa( %w, %w, %w ) :- %w.\n",
	       [Fluent, NewValue, [exogf_Update|Srest],
		(!, exog_fluent_getValue(Fluent, NewValue, Srest))] ),
	generate_dummy_SSAs_aux_exog( Rest, Stream).

/* add axiom for set(fluent, NewValue) */
generate_dummy_SSAs_aux_set([], _Stream).
generate_dummy_SSAs_aux_set([Fluent|Rest], Stream) :-
	printf(Stream, "ssa( %w, %w, [set(%w, %w)|_] ) :- !.\n",
	       [Fluent, NewValue, Fluent, NewValue] ),
	generate_dummy_SSAs_aux_set( Rest, Stream).


% }}}




/* ----------------------------------------------------------
   stochastic procedures
---------------------------------------------------------- */
% {{{ stochastic procedures
% >>>>

generate_all_stoch_proc_outcomes( Stream ) :-
	findall( (ProcName, ModelBody),
		 (
		   proc_model(ProcName, ModelBody),
		   not stoch_proc(ProcName)
		 ), ProcList), !,
	findall( (ProcName, PossCond),
		 (
		   proc_poss(ProcName, PossCond),
		   not stoch_proc(ProcName)
		 ), ProcPossList), !,
	findall( (ProcName, Value, Cond),
		 (
		   proc_costs(ProcName, Value, Cond),
		   not stoch_proc(ProcName)
		 ), ProcCostsList), !,
	generate_all_stoch_proc_aux( ProcList, Stream),
	generate_all_stoch_proc_outcomes_aux( ProcList, Stream),
	generate_all_stoch_proc_poss_aux( ProcPossList, Stream),
	generate_all_stoch_proc_costs_aux( ProcCostsList, Stream).

generate_all_stoch_proc_outcomes( File, FileMode ) :-
	findall( (ProcName, ModelBody),
		 (
		   proc_model(ProcName, ModelBody),
		   not stoch_proc(ProcName)
		 ), ProcList), !,
	findall( (ProcName, PossCond),
		 (
		   proc_poss(ProcName, PossCond),
		   not stoch_proc(ProcName)
		 ), ProcPossList), !,
	findall( (ProcName, Value, Cond),
		 (
		   proc_costs(ProcName, Value, Cond),
		   not stoch_proc(ProcName)
		 ), ProcCostsList), !,
	open( File, FileMode, Stream),
	generate_all_stoch_proc_aux( ProcList, Stream),
	generate_all_stoch_proc_outcomes_aux( ProcList, Stream),
	generate_all_stoch_proc_poss_aux( ProcPossList, Stream),
	generate_all_stoch_proc_costs_aux( ProcCostsList, Stream),
	close(Stream).


/* additional marker, that stoch_proc exists
(compare to prolog_poss/1) */
generate_all_stoch_proc_aux( [], _Stream) :- !.
generate_all_stoch_proc_aux( [(ProcName, _)|Rest], Stream) :- !,
	printf(Stream, "stoch_proc(%w).\n", [ProcName] ),
	generate_all_stoch_proc_aux( Rest, Stream).	

generate_all_stoch_proc_outcomes_aux( [], _Stream) :- !.
generate_all_stoch_proc_outcomes_aux( [(ProcName, ModelBody)|Rest],
				      Stream) :- !,
	generate_stoch_proc_outcome( ProcName, ModelBody, Stream),
	generate_all_stoch_proc_outcomes_aux( Rest, Stream).

/* stoch_proc_poss states the conditions under
which a stoch_proc is possible */
generate_all_stoch_proc_poss_aux( [], _Stream) :- !.
generate_all_stoch_proc_poss_aux( [(ProcName, PossCond)|Rest],
				  Stream) :- !,
	process_condition( PossCond, S, Body_cond),
	printf(Stream, "stoch_proc_poss(%w, %w) :- %w, !.\n",
	       [ProcName, S, Body_cond] ),
	generate_all_stoch_proc_poss_aux( Rest, Stream).


/* stoch_proc_costs states the costs arising from execution */
generate_all_stoch_proc_costs_aux( [], _Stream) :- !.
generate_all_stoch_proc_costs_aux( [(ProcName, Value, Cond)|Rest],
				   Stream) :- !,
	process_condition( Cond, S, Body_cond),
	printf(Stream, "stoch_proc_costs(%w, %w, %w) :- %w, !.\n",
	       [ProcName, Value, S, Body_cond] ),
	generate_all_stoch_proc_costs_aux( Rest, Stream).


/*
generate_stoch_proc_outcome( simProc, S, Body) generates a prolog
body for modeling the outcomes of program simProc. Use it with a
prototype like: stoch_proc_outcomes( ProcName, S, Outcomes,
SenseEffect ) (with exactly these names for the last two
parameters)
*/
generate_stoch_proc_outcome( Proc, ModelBody, Stream ) :-
 	var(Outcomes), var(SenseEffect),
	generate_stoch_proc_outcome( ModelBody, S, [], [], Outcomes,
                                     SenseEffect, NewBody ),
	printf(Stream,
	       "stoch_proc_outcomes(%w, %w, %w, %w) :- %w, !.\n",
	       [Proc, S, Outcomes, SenseEffect, NewBody] ).

/*
generate_stoch_proc_outcome( simProc, S, OutcomesIn,
SenseEffectIn, Body)

generates for the given simulation procedure simProc and the
symbolic situation S the precompiled stoch_proc_outcomes clause.
The actual outcome can be sensed by performing the program
senseEffect. Note that by the following clauses we rule out some
constructs, more precisely we only allow a couple of the usual
set (see [fritzthesis03]). The task is realized
in a forward/backward way: the next entry in a sequence gets the
outcomes and senseEffect until now. The overall result is the
Body which will be generated on the way back and in which
variables Outcomes and SenseEffect will be set. These will then
be mentioned as formal parameters in the prototype. */

generate_stoch_proc_outcome( [], _S, Outs, Sense, Outcomes,
                             SenseEffect, Body ) :- !,
        Body = ( Outcomes = Outs, SenseEffect = Sense).

generate_stoch_proc_outcome( [?(Cond)|Rest], S, Outs, Sense, Outcomes,
                             SenseEffect, Body ) :- !,
 	process_condition( Cond, S, Body_cond),
	generate_stoch_proc_outcome( Rest, S, Outs, Sense, Outcomes,
                                     SenseEffect, Body_rest ),
	conjunct( Body_cond, Body_rest, Body).

generate_stoch_proc_outcome( [if(Cond, A, B)|Rest], S, Outs, Sense,
                             Outcomes, SenseEffect, Body ) :- !,
 	process_condition( Cond, S, Body_cond),
	flatten(A, A_flat), append( A_flat, Rest, A_rest),
	flatten(B, B_flat), append( B_flat, Rest, B_rest),
	generate_stoch_proc_outcome( A_rest, S, Outs, Sense, Outcomes,
                                     SenseEffect, Body_A ),
	generate_stoch_proc_outcome( B_rest, S, Outs, Sense, Outcomes,
                                     SenseEffect, Body_B ),
 	Body = ( (Body_cond) -> Body_A ; Body_B ).

generate_stoch_proc_outcome( [sprob(L, SenseProg)|Rest], S, Outs,
                             Sense, Outcomes, SenseEffect, Body ) :- !,
	/* check that probabilities sum up to 1.0 */
	(
	  checkProbabilities(L)
	;
	  printf("** warning: probabilities in sprob-statement", []),
	  printf("do not sum up to 1.0 in [ %w]\n", [L])
	),
	combineOutcomes( Outs, L, Outs_new),
 	/* sense effect */
 	flatten(Sense, Sense_flat),
 	flatten(SenseProg, SenseProg_flat),
 	append( Sense_flat, SenseProg_flat, Sense_new),
	/* require Rest to be empty:
	sprob has to be the last statement */
	(
	  Rest \= [] ->
	  printf("** syntax warning: unreachable code", []),
	  printf("[%w]: sprob( ., .) has to be the last statement\n",
		 [Rest])
	;
	  true
	),
	/* finalize */
	generate_stoch_proc_outcome( [], S, Outs_new, Sense_new,
                                     Outcomes, SenseEffect, Body ).

generate_stoch_proc_outcome( [A|Rest], S, Outs, Sense, Outcomes, SenseEffect, Body ) :-
	!,
	(
	  prim_action( A )
	;
	  printf("** ERROR in generate_stoch_proc_outcome:",[]),
	  printf(" unknown action or construct [ %w] found\n",
		 [A]), !, fail
	), !,
        /* we don't have to check or worry about wether A is
	  possible or not: that is done online in the transStar */
	A =.. [F|Args],
	(
	  F = set ->
	  Args = [Fluent, Value],
	  process_subf_list( [Value], Value_eval, Body_args, S),
	  Args_eval = [Fluent|Value_eval]	
	;	  
	  process_subf_list( Args, Args_eval, Body_args, S)
	),
	A2 =.. [F|Args_eval],
	combineOutcomes( Outs, [([A2], 1.0, true)], Outs_new),
	generate_stoch_proc_outcome( Rest, [A2|S], Outs_new,
				     Sense, Outcomes, SenseEffect, Body_rest ),
	conjunct( Body_args, Body_rest, Body).

/* make simple statement a list */
generate_stoch_proc_outcome( E, S, Outs, Sense, Outcomes, SenseEffect, Body ) :-
	E \= [_X|_Y], /* E is not a list */
	generate_stoch_proc_outcome( [E], S, Outs, Sense, Outcomes, SenseEffect, Body ).
	
	


/* -------- aux ---------- */

combineOutcomes( [], L, L).
combineOutcomes( L, [], L).

/* only single outcomes in both */
combineOutcomes( [(Out_E, Pr_E, Cond_E)], [(Out_R, Pr_R, Cond_R)],
		 [(Out, Pr, Cond)]) :-
	append( Out_E, Out_R, Out),
	Pr is Pr_E * Pr_R,
	(
	  Cond_E = true ->
	  Cond = Cond_R
	;
	  Cond_R = true ->
	  Cond = Cond_E
	;
	  Cond = and(Cond_E, Cond_R)
	).

combineOutcomes( [E_head|E_rest], [Rest], Outcomes) :-
	E_rest \= [],
	combineOutcomes( [E_head], [Rest], Outcome_local ),
	combineOutcomes( E_rest, [Rest], Outcomes_list ),
	append( Outcome_local, Outcomes_list, Outcomes).	

combineOutcomes( E, [Rest_head|Rest_rest], Outcomes) :-
	Rest_rest \= [],
	combineOutcomes( E, [Rest_head], Outcomes_list ),
	combineOutcomes( E, Rest_rest, Outcomes_list_list ),
	append( Outcomes_list, Outcomes_list_list, Outcomes).

/* check that probabilities in an outcomes list sum to 1.0.
The probabilities can be set dynamically, using variables in
the list. Then this check cannot be made yet and we have to
assume the user knows what he is doing. */
checkProbabilities(L) :- checkProbabilities(L, 0.0).
% checkProbabilities([], 1.0). 
% SJ, 12.11.04: checkProbabilities([(A, 0.7, X), (B, 0.2, Y), (X, 0.1, Z)]).
%  Leads to a added prob of 0.99999999999999989. Thats not 1.0...
%  Therefore the fix:
checkProbabilities([], FinalProb) :- 
        Tmp is 1.0-FinalProb,
        abs( Tmp, IsNearZero ),
        IsNearZero < 0.00001.

checkProbabilities([(_Prog, Pr, _Sense)|_L], _X) :- var(Pr), !.
checkProbabilities([(_Prog, Pr, _Sense)|L], X) :-
	X2 is X + Pr,
	checkProbabilities(L, X2).

% }}}

/* ----------------------------------------------------------
   explicit events
---------------------------------------------------------- */
% {{{ explicit events
% >>>>

/* explicit events are syntactically the same as stochastic
procedures, but are called 'by nature' whenever possible
(instead of being called by the user, as stoch_procs are).
*/


/* only called once at deepest level to memorize the list of
events */
generate_events_list( Stream) :-
	findall( Event, event_model(Event, _Model), Events_s),
	printf(Stream, "events_list(%w).\n", [Events_s]).


generate_all_event_outcomes( Stream ) :-
	findall( (EventName, ModelBody),
		 ( event_model(EventName, ModelBody),
		     not event_aux(EventName)), EventList), !,
	findall( (EventName, PossCond),
		 ( event_poss(EventName, PossCond),
		     not event_aux(EventName)), EventPossList), !,
	findall( (EventName, Value, Cond),
		 ( event_costs(EventName, Value, Cond),
		     not event_aux(EventName)), EventCostsList), !,
	generate_all_event_aux( EventList, Stream),
	generate_all_event_outcomes_aux( EventList, Stream),
	generate_all_event_poss_aux( EventPossList, Stream),
	generate_all_event_costs_aux( EventCostsList, Stream).


generate_all_event_outcomes( File, FileMode ) :-
	findall( (EventName, ModelBody),
		 ( event_model(EventName, ModelBody),
		     not event_aux(EventName)), EventList), !,
	findall( (EventName, PossCond),
		 ( event_poss(EventName, PossCond),
		     not event_aux(EventName)), EventPossList), !,
	findall( (EventName, Value, Cond),
		 ( event_costs(EventName, Value, Cond),
		     not event_aux(EventName)), EventCostsList), !,
	open( File, FileMode, Stream),
	generate_all_event_aux( EventList, Stream),
	generate_all_event_outcomes_aux( EventList, Stream),
	generate_all_event_poss_aux( EventPossList, Stream),
	generate_all_event_costs_aux( EventCostsList, Stream),
	close(Stream).

/* generate event dummy to enumerate all possible events */
generate_all_event_aux( [], _Stream) :- !.
generate_all_event_aux( [(EventName, _ModelBody)|Rest], Stream) :- !,
	printf(Stream, "event_aux(%w).\n", [EventName] ),
	generate_all_event_aux( Rest, Stream).

/* generate event outcome lists definition in prolog predicates */
generate_all_event_outcomes_aux( [], _Stream) :- !.
generate_all_event_outcomes_aux( [(EventName, ModelBody)|Rest],
				 Stream) :- !,
	generate_event_outcome( EventName, ModelBody, Stream),
	generate_all_event_outcomes_aux( Rest, Stream).

/* event_poss states the conditions under which an event is possible */
generate_all_event_poss_aux( [], _Stream) :- !.
generate_all_event_poss_aux( [(EventName, PossCond)|Rest], Stream) :-
	!, process_condition( PossCond, S, Body_cond),
	printf(Stream, "prolog_event_poss(%w, %w) :- %w, !.\n",
	       [EventName, S, Body_cond] ),
	generate_all_event_poss_aux( Rest, Stream).


/* event_costs states the costs arising from execution */
generate_all_event_costs_aux( [], _Stream) :- !.
generate_all_event_costs_aux( [(EventName, Value, Cond)|Rest],
			      Stream) :- !,
	process_condition( Cond, S, Body_cond),
	printf(Stream, "prolog_event_costs(%w, %w, %w) :- %w, !.\n",
	       [EventName, Value, S, Body_cond] ),
	generate_all_event_costs_aux( Rest, Stream).


/* generate_event_outcome( simProc, S, Body) generates a prolog
body for modeling the outcomes of program simProc. Use it with a
prototype like: event_outcomes( EventName, S, Outcomes,
SenseEffect ) (with exactly these names for the last two
parameters) ! Note: since this is essentially the same as
stoch_procs, we can use the same predicate
(generate_stoch_proc_outcome) to generate the outcomes ! */
generate_event_outcome( Event, ModelBody, Stream ) :-
%% STFIXME
%	generate_stoch_proc_outcome( ModelBody, S, [], [], NewBody ),
	generate_stoch_proc_outcome( ModelBody, S, [], [], Outcomes, SenseEffect, NewBody ),
%	var(Outcomes), var(SenseEffect),
	printf(Stream, "event_outcomes(%w, %w, %w, %w) :- %w, !.\n",
	       [Event, S, Outcomes, SenseEffect, NewBody] ).

% }}}



/* ----------------------------------------------------------
   options
---------------------------------------------------------- */
% {{{ options
% >>>>

/* see fritzthesis03 and options.pl to understand this. */

/* ----------------------------------------------------------
   prepare option
   some preparations are necessary for options calculation
---------------------------------------------------------- */
prepare_option( Option, Stream ) :-
	open( ".tmpfile", write, StreamTmp),
	prepare_option_mappings( Option, Stream, StreamTmp),
	prepare_option_init( Option, Stream, StreamTmp),
	close(StreamTmp),
	concat_string( ["tr -d '#' < .tmpfile > .tmpfile2"], TRString),
	system( TRString),
	compile(".tmpfile2"),
 	concat_string( ["rm .tmpfile .tmpfile2"], RMString),
 	system( RMString).

prepare_option_mappings( Option, Stream, StreamTmp) :-
	(
	  /* mapping defined via list of state variabled */
	  option_variables( Option, VarList) ->
	  options_varlist2state( VarList, S, State, Sit,
				 Cond, CondBody), 
	  printf(Stream, "option_mapping( %w, %w, %w, %w).\n",
		 [Option, State, Sit, Cond] ),
	  printf(Stream, "opt_state( %w, %w, %w ) :- %w.\n",
		 [Option, State, S, CondBody] ),
	  printf(StreamTmp, "option_mapping_tmp( %w, %w, %w, %w).\n",
		 [Option, State, Sit, Cond] ),
	  printf(StreamTmp, "opt_state_tmp( %w, %w, %w ) :- %w.\n",
		 [Option, State, S, CondBody] )
	;
	  /* mapping NOT defined via list of state variabled:
	  looking for explicit mapping definition */
	  findall( (State, Cond, Sit),
		   option_mapping( Option, State, Sit, Cond),
		   MappingList),
	  prepare_option_mappings_aux( Option, MappingList, Stream,
				       StreamTmp)
	).

prepare_option_mappings_aux( _Option, [], _Stream, _StreamTmp ).
prepare_option_mappings_aux( Option, [(State, Cond, Sit)|List_rest],
			     Stream, StreamTmp ) :-
	process_condition(Cond, S, CondBody), !,
	printf(Stream, "opt_state( %w, %w, %w ) :- %w.\n",
	       [Option, State, S, CondBody] ),
	printf(StreamTmp, "option_mapping_tmp( %w, %w, %w, %w).\n",
	       [Option, State, Sit, Cond] ),
	printf(StreamTmp, "opt_state_tmp( %w, %w, %w ) :- %w.\n",
	       [Option, State, S, CondBody] ),
	prepare_option_mappings_aux( Option, List_rest, Stream,
				     StreamTmp ).

prepare_option_init( Option, Stream, StreamTmp) :-
	option_init( Option, InitCondition),
	process_condition(InitCondition, S, Body),
	printf(Stream, "stoch_proc_poss( %w, %w ) :- %w.\n",
	       [Option, S, Body] ),
	printf(StreamTmp, "init_tmp( %w, %w ) :- %w.\n",
	       [Option, S, Body] ).



/* (9.10.03) for easy mapping by state variables */
options_varlist2state( [Fluent], S, [(Fluent, FValue)],
		       [set(Fluent, FValue)], Fluent=FValue, Body ) :-
	process_subf( Fluent, FValue, Body, _Type, S).
options_varlist2state( [Fluent|Rest], S, [(Fluent, FValue)|RestState],
		       [set(Fluent, FValue)|RestSit],
		       and(Fluent=FValue, RestCond), Body) :-
	process_subf( Fluent, FValue, Body1, _Type, S),
	options_varlist2state( Rest, S, RestState, RestSit,
			       RestCond, Body2),
	conjunct( Body1, Body2, Body).
		

/* ------------------------------------------------------- */


/* ----------------------------------------------------------
   construct init set
---------------------------------------------------------- */
process_option_constructInitSet(Option, Sinit_s ) :-
 	findall( Snew,
		 (
		   option_mapping_tmp(Option, _D, Action, _C),
		   Snew1 = [Action, s0],
		   flatten(Snew1, Snew),
		   init_tmp(Option, Snew)
		 ),
		 Sinit_s).

process_option_constructInitStates(_Option, [], []).
process_option_constructInitStates(Option, [S|S_rest],
				   [State|State_rest]) :-
	opt_state_tmp(Option, State, S),
	process_option_constructInitStates(Option, S_rest,
					   State_rest).
/* ------------------------------------------------------- */

process_option( Option, Epsilon, Stream ) :-

	/* prepare option processing */
	prepare_option( Option, Stream),
	
	/* construct the init set */
 	process_option_constructInitSet(Option, Sinit_s),
 	process_option_constructInitStates(Option, Sinit_s, IStates_s),
	
	/* compute the option policy and expected value */
	option( Option, Prog, _Gamma ),
	printf("\t '%w': ", [Option]), flush(output),
	create_option( Option, Prog, Sinit_s, Epsilon, 0),
 	opt_retract( Option, IStates_s ),
 	opt_makeBody( Option ),
  	opt_Body(Option, Body),

	/* compute the option  model */
	create_model( Option, IStates_s, Epsilon, 0),
	nl,
 	opt_retract_model( Option, IStates_s ),
	
	/* create new proc from body */
	printf(Stream, "proc(%w, %w).\n", [Option, Body]),

	/* create stoch_proc(Option) */
	printf(Stream, "stoch_proc(%w).\n", [Option]),
	
	/* create opt_value (from exp_reward) */
	/* which will be queried by stoch_proc_costs  */
	findall( (D, R), opt_exp_reward_max(Option, D, R), Values_s),
	process_option_costs(Option, Values_s, Stream),
 	Body_costs = (
		       opt_state(Option, Desc, S),
		       opt_costs(Option, Desc, Costs)
		       ),
 	printf(Stream, "stoch_proc_costs( %w, %w, %w ) :- %w.\n",
 	       [Option, Costs, S, Body_costs] ),

	/* create stoch_proc_outcomes */
	findall( (D, PL), opt_exit_prob_max(Option, D, PL),
		 Probs_s),
	process_option_probs(Option, Probs_s, Stream),
 	Body_outcomes = (
			  opt_state(Option, Desc, S),
			  opt_probability_list(Option, Desc, PL),
			  opt_PL_to_Outcomes(Option, PL, S, Outcomes)
			),
	option_sense(Option, SenseAction), 
 	printf(Stream,
	       "stoch_proc_outcomes( %w, %w, %w, %w ) :- %w.\n",
 	       [Option, S, Outcomes, SenseAction, Body_outcomes] ).
	
/* write down the costs (= -expected values/rewards) for each
possible description (state) */
process_option_costs(_Option, [], _Stream).
process_option_costs(Option, [(D,R)|Values_s], Stream) :-
	C is R*(-1.0),
	printf(Stream, "opt_costs(%w, %w, %w).\n", [Option, D, C]),
	process_option_costs(Option, Values_s, Stream).
	
/* write down the probability list for each
possible description (state) */
process_option_probs(_Option, [], _Stream).
process_option_probs(Option, [(D,PL)|Probs_s], Stream) :-
	printf(Stream, "opt_probability_list(%w, %w, %w).\n",
	       [Option, D, PL]),
	process_option_probs(Option, Probs_s, Stream).
	

/*         ----- process all options -----            */

process_all_options(Stream) :-
	/* find options that were not yet processed */
	findall( Option,
		 (option(Option, _, _), not stoch_proc(Option)),
		 OptionList), !,
	(
	  options_epsilon(Epsilon)
	;
	  Epsilon = 0.01
	),
	printf("\n\t___ computing options (+) and models (>),", []),
	printf(" epsilon = %w ___\n", [Epsilon]),
	process_all_options_aux(Epsilon, OptionList, Stream).

process_all_options(File, FileMode) :-
	/* find options that were not yet processed */
	findall( Option,
		 (option(Option, _, _), not stoch_proc(Option)),
		 OptionList), !,
	open( File, FileMode, Stream),
	(
	  options_epsilon(Epsilon)
	;
	  Epsilon = 0.01
	),
	printf("\n\t___ computing options (+) and models (>),", []),
	printf(" epsilon = %w ___\n", [Epsilon]),
	process_all_options_aux(Epsilon, OptionList, Stream),
	close(Stream).

process_all_options_aux(_Epsilon, [], _Stream) :- !.
process_all_options_aux(Epsilon, [Option | List_rest], Stream) :-
	process_option( Option, Epsilon, Stream), !,
	process_all_options_aux(Epsilon, List_rest, Stream).	

% }}}



/* ==========================================================
   MAIN
========================================================== */
% {{{ MAIN
% >>>>

preprocess( File ) :- preprocess( File, _NewFile, true, 0).
preprocess( File, NewFile, Last, Level ) :-
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
	concat_string( [Path, "tmp_", BaseName, ".pl"], TmpFile),
	concat_string( [Path, "processed_", BaseName, ".pl"], NewFile),
	open(TmpFile, write, Stream),
	write_header(Stream),
	/* -- <MAIN PART> -- */
	process_all_functions( Stream ),
 	generate_all_stoch_proc_outcomes( Stream ),
 	generate_all_event_outcomes( Stream ),
  	generateSSAs( Stream ),
  	process_all_poss( Stream ), !,
  	process_all_options( Stream ),
	/* -- </MAIN PART> -- */
	/* -- <LAST_FILE PART> -- */
	(
	  Last ->
	  generate_events_list( Stream )
	;
	  true
	),
	/* -- </LAST_FILE PART> -- */
	close(Stream),
	erase_rhomb(TmpFile, NewFile),
	cputime(Tend),
	Tdiff is Tend - Tbegin,
	printf("\n\t++ processing        : DONE (%w sec.)\n", [Tdiff]),
	printf("\t++ output written to : ", []),
	printf("%w\n\n", [NewFile]), flush(output). 

write_header(Stream) :-
	printf(Stream, "/******************************\n", []),
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
	Level is N - 4,
	(
	  N =:= M-1 ->
	  /* notice when last file is processed: here some
	  extra stuff is added */
	  preprocess(FileName, NewFileName, true, Level)
	;
	  preprocess(FileName, NewFileName, false, Level)
	),
	/* compile the lately processed output before
	steping over to next level */
	compile(NewFileName),
	N_next is N+1,
	runall(N_next, M).

autorun :-
	argc(M),
	printf(" argcount is %w.\n", [M]),
 	(
	  M > 4 ->
	  runall(4, M),
	  exit(0)
	;
	  writeln("******************************"),
	  writeln("*      interactive mode      *"),
	  writeln("******************************")
	).

% }}}

:- writeln("** loading preprocessor.pl\t\t DONE").

:- autorun.	      
