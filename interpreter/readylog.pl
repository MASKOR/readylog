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
 *         @date: 06.03.03
 *       @author: Christian Fritz <Christian.Fritz@rwth-aachen.de>
 *         @date: 05/11/29 new revision
 *       @author: Alexander Ferrein <ferrein@cs.rwth-aachen.de>
 *   description: Readylog Interpreter 
 *                extension of ipcGolog (by Norman Jansen)
 * last modified: $Date$
 *            by: $Author$
 *
 * **************************************************************************/

:- write(" -----> Loading readylog.pl \t ... \n").

/* ================================================================== */
/*  INCLUDES                                                          */
/* ================================================================== */

/** load debugging utils.
 *
 */
:- ensure_loaded('debug.pl').

/** load definition file.
 * it loads the required libs and defines numerous dynamic predicates
 * needed for readylog
 */
:- ensure_loaded('definitions.pl').

/** load the configuration file.
 * several flags and predicates are defined,
 * features can be switched on/off (t)here.
 */
:- ensure_loaded('config.pl').

/** load the language interpreter.
 * the semantics definitions for Readylog by final and trans.
 */
:- ensure_loaded('final_trans.pl').

/** load state abstraction methods.
 * not used right now.
 */
%:- ensure_loaded('stateabstraction.pl').

/** load decision-theoretic extension.
 * the semantics definitions for dt-programs by bestDoM
 */
:- ensure_loaded('decisionTheoretic').

/** load options extension.
 *  this is currently not in use 
 *  because it needs full enumeration of the state space
 */
:- ensure_loaded('options').

/** load utilities.
 *  utils.pl offers readylog shell functionality 
 *  with auto completion and history
 */
:- ensure_loaded('../../utils/utils.pl').

/** load icp_shell.
 *  icp_shell offers shell functionality 
 *  with auto completion and history
 */
%:- ensure_loaded("../../utils/icp_shell/icp_shell.pl").

/** some local variables.
 * min is needed to calculate the least time point of continuous fluents
 * zaehler is needed for caching probabilistic traces
 * fd_var is an integer number needed for progressing the database
 */
:- local variable(min).
:- local variable(zaehler).
:- local variable(fd_var).


/* ================================================================== */
/*  PROGRAM EXECUTION                                                 */
/* ================================================================== */
/** calling of programs.
 *   calling of programs is done with icp(E).
 */
icp(E) :- icpgolog(E), !.

/** initializing knowledge base
 *  and starting program execution by icpgo(E) 
 */
icpgolog(E) :- 
	initialize,
	(simple_projection -> Text="PROJECTION WITHOUT PROGRESSION."
                           ; Text="PROJECTION WITH PROGRESSION."),
	(progression_enabled ->
	    writeln("PROGRESSION ENABLED."),
	    writeln(Text), nl
	;
	    writeln("PROGRESSION DISABLED."),
	    nl
	),
	(pe ->
	    writeln("ADVANCED PROGRESSION ENABLED."),
	    writeln(Text), nl
	;
	    writeln("ADVANCED PROGRESSION DISABLED."),
	    nl
	),
        icpgo(E, [s0]). 


/* (1)- exogenous action occured */ 
icpgo(E,H) :-
	printf("(1) icpgo( .., %w)\n", [H]), flush(output),
	%printf("(1) icpgo( %w, %w)\n", [E,H]), flush(output),
	exog_occurs(Act,H), exog_action(Act),
	(
				%	  progression_enabled -> update_current_val([Act|H],H1)
	  pe, length(H, HL), getval(prgrat, PL), HL >= PL ->
	  incZaehler([Act|H]),
	  update_current_val([Act|H],H1)
	;
	  H1=[Act|H],
	  printf("(1) H1 %w)\n", [H1]), flush(output)
	), !, icpgo(E,H1). 

/* (2) - performing a step in program execution */ 
icpgo(E,H) :-
				%	printf("(2) icpgo( .., %w)\n", [H]), flush(output),
	doTrace(E,H), /* print debug info if gologtrace is on */
	trans(E,H,E1,H1),
	icpxeq(H,H1,H2),
	!, icpgo(E1,H2). 

/* (3) - program is final -> execution finished */ 
icpgo(E,H) :-
				%	printf("(3) icpgo( .., %w)\n", [H]), flush(output),
	final(E,H),
	(
	  progression_enabled -> getval(zaehler,Zaehler) 
	;
	  length(H,L), Zaehler is L-1
	), nl, write(Zaehler), writeln(" action(s)."), !.

/* (4) - waiting for an exogenous action to happen */
icpgo(E,H) :-
	printf("icpgo(4): WAITING FOR EXOGENOUS ACTIONS...\n", []),
	flush(output),
	wait_for_exog_occurs, !,
	/* now that we there is an exo action,
	we process it in icpgo(1) */
	icpgo(E,H).



/* ----------------------------------------------------------
icpxeq(H1,H2,H3) 
H1 ist die urspruengliche Historie. H2 ist H1 mit neuer
auszufuehrender Aktion.
H3 ist die resultierende Historie nach Ausfuehrung der Aktion.
Bei eingeschalteter Progression der Wissensbasis wird mittels 
"update_current_val(H1,H3) die Historie verkuerzt." 
---------------------------------------------------------- */

/* (1) - No action was performed so we don't execute anything */ 
icpxeq(H,H,H).

/* (2) - The action is not a sensing one: execute and ignore
its sensing */
/* we are doing advanced progression after prgrat
number of actions */
icpxeq(H,[Act|H],H1) :- not senses(Act,_),
	execute(Act,_,H),
	length(H, HL), getval(prgrat, PL),
	(pe, HL >= PL ->
	    incZaehler([Act|H]),
	    update_current_val([Act|H],H1)
	    ;
	    H1=[Act|H]).	

/* (3) - The action is a sensing one for fluent F:
execute sensing action*/ 
icpxeq(H,[Act|H],H1) :-
	senses(Act,F), execute(Act,Sr,H), 
	(
	  progression_enabled ->
	  incZaehler([e(F,Sr),Act|H]), 
	  update_current_val([e(F,Sr),Act|H],H1) 
	;
	  H1=[e(F,Sr),Act|H]
	). 

% Erhoehung des Zaehlers um die Anzahl der Aktionen in der Historie
incZaehler(H) :- length(H,L), L1 is L-1, getval(zaehler,Zaehler), 
	         Zaehler1 is Zaehler+L1, setval(zaehler,Zaehler1).



/* ================================================================== */
/*   INIT                                                             */
/* ================================================================== */

/** initialize.
 * Initialisierung der Praedikate
 */

% Initialisiere die Praedikate, indem fehlende Definitionen 
% durch Standardimplementierungen ersetzt werden
initialize :- 
  % Cache loeschen
  retract_all(cache(_,_)),

  % Zaehler auf 0 setzen
  setval(zaehler,0), !,

  % Falls nur eine Anfangssituation, hat diese W'keit 1.
  (initial_P(_S1,_P1) ; assert(initial_P(s0,1))), !, 

  % Situationen aus initial_P als initial eintragen
  (initial_P(S,_P), not(initial_Sit([S])),
      assert(initial_Sit([S])), fail ; true), !, 

  % allg. Aussagen ueber Low-Level-Programm fuer alle Anfangssit.
  % eintragen
  (initial_LL(Progr), initial_Sit([S]), not(initial_LL(S,Progr)), 
        assert(initial_LL(S,Progr)), fail; true), !,

  % falls einer Situation kein LL-Progr. zugeordnet, Standardprogr.
  % zuordnen
  (initial_Sit([S]), not(initial_LL(S,_Progr)),
      assert(initial_LL(S,?(fail))), fail; true), !,

  % allg. Aussagen ueber Anfangswert eines Fluenten fuer alle
  % Anfangssit. eintragen
  (initial_val(F,V), initial_Sit([S]), not(initial_val(F,V,S)), 
        assert(initial_val(F,V,S)), fail; true), !,

  % In current_val wird der aktuellen Zustand der Fluenten
  % gespeichert.
  retract_all(current_val(_,_,_)), !, 

  % alle Werte von initial_val zu current_val uebertragen.
  (initial_val(F,V,H), assert(current_val(F,V,H)), fail ; true),

  % clean up exoQueue
  retract_all(exoQueue(_)), !,
  
  % if user provided an own init predicate, call it:
  (user_init ; true).



/* ================================================================== */
/*   PROGRESSION OF KNOWLEDGE BASE                                    */
/* ================================================================== */

% Die Wissensbasis wird gemaess der Aktionen in H1 aktualisiert, 
% HH ist die verkuerzte History mit Laenge 1

% CF&AF: the worlds worst ever hack: ignore exogf_update progression
update_current_val(H1,HH) :-
	update_current_val1(H1, H2, 0, NEFU),
	(
	  NEFU > 0 ->
	  Act = exogf_Update, 
	  HH = [Act|H2]
	;
	  HH = H2
	).


/* AF: here, we extended the old claueses to counting
the number of exogf_actions */
update_current_val1([S],[S], N, N). 
update_current_val1(H1,HH, NEFU, NEFU2) :- 
	H1=[Act|H],
	(
	  Act = exogf_Update ->
	  NEFU1 is NEFU + 1
	;
	  NEFU1 = NEFU
	),
	(
	  H=[_] ->
	  update_action1(Act,H, NEFU1),
	  NEFU2 = NEFU1,
	  HH= H
         ;
	  update_current_val1(H,HH, NEFU1, NEFU2),
	  update_action1(Act,HH, NEFU1)
	).

/* AF : old update_action clause. Replaced with update_action1() */
% Die Wissensbasis wird gemaess der Aktion "Act" aktualisiert.
update_action(Act,H) :- 
	current_to_temp, !,	% current_val in temp_val sichern
	(sets_val(Act,F,V,H), update_fluent_val(F,V,H), fail ; true),
	!,
	(has_val(pll(A,B,C),true,[Act|H]), lastElement(A,A1), H=[S],
	    asserta(temp_val(pll([A1],B,C),true,S)), fail ; true), !,
	temp_to_current.      % temp_val in current_val speichern


/* AF: We do not look further than the first exog_update (case 1)
 * alternatively, we can ignore only the exog_update actions */
update_action1(Act,H, NEFU) :-
	(
	  NEFU>1,
	  Act = exogf_Update ->
	  true
	;
	  current_to_temp, !,	% current_val in temp_val sichern
	  (sets_val(Act,F,V,H), update_fluent_val(F,V,H), fail ; true),
	  !,
	  (has_val(pll(A,B,C),true,[Act|H]), lastElement(A,A1), H=[S], 	
	      asserta(temp_val(pll([A1],B,C),true,S)), fail ; true), !,
	  temp_to_current
	).

% Wissensbasis (current_val) in temp_val sichern.
current_to_temp :-  
	retract_all(temp_val(_,_,_)), !,  
	(clause(current_val(F,V,H):-B),
	    assert(temp_val(F,V,H):-B), fail ; true), !,
	retract_all(temp_val(pll(_,_,_),_,_)).

% Wissensbasis (current_val) von temp_val wiederherstellen.
temp_to_current :-  
	retract_all(current_val(_,_,_)), !,  
	(clause(temp_val(F,V,H):-B),
	    assert(current_val(F,V,H):-B), fail ; true).

% Benutzt endliche Mengen als Domaenen fuer Parameter des Fluenten.
update_fluent_val(F,V,[S]) :-
	F=..[Name|_], domains_parameter(F,Dom),
	wb_update(Name,Dom),
	asserta(temp_val(F,V,S)), !.

wb_update(Fluent_name, Dom) :-
	(temp_val(F,V,S), F=..[Fluent_name|_] ->
       	   retract(temp_val(F,V,S)),
	   wb_update(Fluent_name, Dom),
           domains_parameter(F,D), 
	   (restrict_domains(D,Dom,D2), assert_domains(F,D2),
	       asserta(temp_val(F,V,S)), fail ; true) ;
	   true).

% Gibt an, ob zwei Domaenenlisten disjunkt sind.
% Dies ist bereits der Fall, wenn ein Domaenenpaar disjunkt ist.
% unterschiedl. viele Elemente -> disjunkt:
disjunct_domains([], [_|_]).  
disjunct_domains([_|_], []).
disjunct_domains([], []) :- !, fail.
disjunct_domains(DomList1, DomList2) :- 
   DomList1=[D1|R1], DomList2=[D2|R2],
   (not dom_intersection(D1,D2,_,_) 
     -> true /* wenn ein Domaenenpaar disjunkt ist, ist die
                gesamte Liste disjunkt */
    ;  disjunct_domains(R1, R2)).

restrict_domains(DomList1, DomList2, DomList3) :-
   (disjunct_domains(DomList1, DomList2) 
     -> /* wenn Domaenen disjunkt, braucht DomList1 nicht
           eingeschraenkt zu werden */
	DomList3 = DomList1  
     ;  restrict_domains2(DomList1,DomList2,DomList3, [])).

/* Beachte: dom_difference und dom_intersection "failen", wenn
das Ergebnis die leere Menge waere. Somit entstehen als Ausgabe
dieses Praedikats keine Mengenvektoren, die eine leere Menge
enthalten. */
restrict_domains2([],[],_,_) :- fail.
restrict_domains2(DomList1,DomList2,DomList3,Bisher) :-
    DomList1=[D1|R1], DomList2=[D2|R2],
    (% entweder erste Domaene einschraenken
     dom_difference(D1,D2,D3,_), append(Bisher,[D3|R1],DomList3)  
   ; % oder eine der anderen Domaenen.
     dom_intersection(D1,D2,D3,_), append(Bisher,[D3],Bisher2),   
     restrict_domains2(R1,R2,DomList3,Bisher2)).  


domains_parameter(F,Dom) :-
	F=..[_|Param], integers(Param),
	domains_parameter2(Param,Dom).
domains_parameter2([],[]). 
domains_parameter2(Param,Dom) :-
	Param=[A|R], dvar_domain(A,D), 
	domains_parameter2(R,Dom2), Dom=[D|Dom2].

assert_domains(F,DomList) :-
	F=..[_|Param],
	assert_domains2(Param,DomList).
assert_domains2([],[]).
assert_domains2(Param,DomList) :-
	Param=[A|R],
	DomList=[D|R2],
	dvar_update(A,D),
	assert_domains2(R,R2).
	
lastElement(A,A1) :- A=[A1].
lastElement(A,A1) :- A=[_|A3], lastElement(A3,A1).

% 


/* ==========================================================
   Transition Semantics
========================================================== */


transStar(E,H,E,H)   :- final(E,H), !.
transStar(E,H,EE,HH) :-
	transPr(E,H,E1,H1,1), !,
	transStar(E1,H1,EE,HH).


/* ==========================================================
   Continuous Change
========================================================== */
%  Continuous Change
% >>>>

% ltp(P,T,H) (least time point). 
% T ist der früheste Zeitpunkt, in dem TForm P erfuellt ist. 
ltp(P,T,H) :- setval(min, unendlich), !, ltp_min(P,T,H).

ltp_min(P,T,H) :-
	holdsTForm(P,T,H), has_val(start,T0,H),
	{T >= T0}, minimize(T), 
	getval(min,Akt_Min), 
	(
	  Akt_Min=unendlich -> Neues_Min=T
	; min(T,Akt_Min,Neues_Min)),
	setval(min,Neues_Min), fail.
ltp_min(_,T,_) :-
	getval(min,T1),
	( T1=unendlich -> fail
	; T=T1).


% Definition der kontinuierlichen Funktionen durch t_function(name)
t_function(constant(_CONST)).
t_function(toward(_FROM,_TO,_SPEED,_T0)).
t_function(linear(_FROM,_TO,_VEL)).
t_function(constant2d([_X,_Y])).
t_function(toward2d([_X0,_Y0],_T0,[_Xd,_Yd],_V)).
t_function(linear2d([_X0,_Y0],_T0,[_VX,_VY])).

% Mittels t_function(name,val,t) wird der Wert der 
% kontinuierlichen Funktionen zur Zeit t definiert. 
t_function(constant(CONST),V,T) :- nonvar(T) -> V = CONST;
	{V = CONST, T >= 0}.

t_function(toward(FROM,TO,SPEED,T0),V,T) :-
	{V = FROM+(TO-FROM)*SPEED*(T-T0)}.

t_function(linear(FROM,T0,VEL),V,T) :-
	{V = FROM+VEL*(T-T0)}.

% 2 dimensional functions
t_function(constant2d([X,Y]),[X1,Y1],T) :- nonvar(T) -> X1=X, Y1=Y;
        {X1 = X, Y1 = Y, T >= 0}.

t_function(toward2d([X1,Y1],T0,[Xd1,Yd1],V),[X,Y],T) :-
        % in Realwerte umwandeln
        X0 is X1*1.0, Y0 is Y1*1.0, Xd is Xd1*1.0, Yd is Yd1*1.0,  
	(X0=Xd, Y0=Yd, !, X=X0, Y=Y0
        ; 
       	Nu2 is (X0-Xd)*(X0-Xd)+(Y0-Yd)*(Y0-Yd), sqrt(Nu2,Nu), 
	{X = X0 + (Xd-X0)*(T-T0)*V/Nu,
	 Y = Y0 + (Yd-Y0)*(T-T0)*V/Nu}).

t_function(linear2d([X0,Y0],T0,[VX,VY]), [X, Y], T) :-
        % convert to real values
        X0f is X0*1.0, Y0f is Y0*1.0, VXf is VX*1.0, VYf is VY*1.0,  
	{ Diff = T-T0,
	  X = X0f + (VXf * Diff),
	  Y = Y0f + (VYf * Diff) }.
	  


% Auswertung von t-Forms
% Dabei wird die Bibliothek CLP(r/q) fuer Beschraenkungen benutzt.
holdsTForm(and(P1,P2),T,H) :-
	!, holdsTForm(P1,T,H), holdsTForm(P2,T,H).
holdsTForm(and([]),_,_) :- !.
holdsTForm(and([P|R]),T,H) :-
	!, holdsTForm(P,T,H), holdsTForm(and(R),T,H).
holdsTForm(or(P1,P2),T,H) :-
	!, (holdsTForm(P1,T,H) ; holdsTForm(P2,T,H)).
holdsTForm(or([P]),T,H) :- !, holdsTForm(P,T,H).
holdsTForm(or([P|R]),T,H) :-
	!, (holdsTForm(P,T,H) ; holdsTForm(or(R),T,H)).
holdsTForm(not(P),T,H) :- !, holdsTForm_not(P,T,H).
holdsTForm(neg(P),T,H) :- !, holdsTForm_not(P,T,H). % synonym for not
holdsTForm(P,T,H) :-
	tForm(P,_), subf(P,P1,H), tForm(P1,P2),
	!, holdsTForm(P2,T,H).
holdsTForm(P,T,H) :- % Auswertung von Vergleichen (=, <, >, =<, >=) 
     P=..[Op,A,B], (Op=(=); Op=(<); (Op=(>); (Op=(=<); Op=(>=)))), !,
     eval_TForm_comparison(false,Op,A,B,Expr,T,H),
     (Expr=and(_) -> holdsTForm(Expr,T,H) ; holdsTForm(Expr)). 

/* holdsTForm_not wertet negierte t-Forms aus. Dabei wird die
Negation nach innen geschoben und auf die Grundterme angewendet. */
holdsTForm_not(and(P1,P2),T,H) :-
	!, holdsTForm(or(not(P1),not(P2)),T,H).     
holdsTForm_not(and(L),T,H)     :-
	neg_list(L,L2), !, holdsTForm(or(L2),T,H).  
holdsTForm_not(or(P1,P2),T,H)  :-
	!, holdsTForm(and(not(P1),not(P2)),T,H).    
holdsTForm_not(or(L),T,H)      :-
	neg_list(L,L2), !, holdsTForm(and(L2),T,H). 
holdsTForm_not(not(P1),T,H) :- !, holdsTForm(P1,T,H).
holdsTForm_not(neg(P1),T,H) :- !, holdsTForm(P1,T,H).
holdsTForm_not(P,T,H) :-
	tForm(P,_), subf(P,P1,H), 
	tForm(P1,P2), !, holdsTForm_not(P2,T,H).
holdsTForm_not(P,T,H) :- 
     P=..[Op,A,B], (Op=(=); Op=(<); (Op=(>); (Op=(=<); Op=(>=)))), !,
     eval_TForm_comparison(not,Op,A,B,Expr,T,H),  
     (Expr=not(and(_)) -> holdsTForm(Expr,T,H) ; holdsTForm(Expr)). 

% Auswertung von atomaren Vergleichen
holdsTForm(A =< B) :- !, {A =< B}.
holdsTForm(A >= B) :- !, {A >= B}.
holdsTForm(A <  B) :- !, delta(D), {A =< B-D}.
holdsTForm(A >  B) :- !, delta(D), {A >= B+D}.
holdsTForm(A =  B) :- !, delta(D), {A >= B-D, A =< B+D}.
holdsTForm(A \= B) :- delta(D), !, {A =< B-D} ; {A >= B+D}.

% Auswertung von Vergleichen (=, <, >, =<, >=) 
eval_TForm_comparison(Not,Op,A,B,Expr,T,H) :-  
	subf(A,A1,H), subf(B,B1,H),
        (\+var(A1), t_function(A1) -> t_function(A1,A2,T) ; A2 = A1),
        (\+var(B1), t_function(B1) -> t_function(B1,B2,T) ; B2 = B1),
        % Erstellung eines fuer den komponentenweisen Vergleich 
        % von Listen aequivalenten "and-Ausdrucks".
        (liste(A2), liste(B2) ->      
	    merge_lists(Op,A2,B2,C), 
	    Expr2=and(C), 
	    (Not=not -> Expr=not(Expr2) ; Expr=Expr2) 
	  ; neg_op(Not,Op,Op2), Expr=..[Op2,A2,B2]).

% Negation von Operatoren
neg_op(Not,=,P) :-  (Not=not -> P=(\=) ; P=(=)).
neg_op(Not,<,P) :-  (Not=not -> P=(>=) ; P=(<)).
neg_op(Not,>,P) :-  (Not=not -> P=(=<) ; P=(>)).
neg_op(Not,=<,P) :- (Not=not -> P=(>)  ; P=(=<)).
neg_op(Not,>=,P) :- (Not=not -> P=(<)  ; P=(>=)).
       

liste(L) :- not var(L), (L=[]; L=[_|_]),!.

% 


/* ==========================================================
   defined fluents and actions of icpGolog
========================================================== */
%  defined fluents and actions of icpGolog
% >>>>

const(true).
const(false).
const(nil).

initial_Sit([s0]).

/*  Fluenten */

prim_fluent(R) :- register(R).
prim_fluent(online).
prim_fluent(eval_registers).
prim_fluent(start).

prim_fluent(pll(_,_,_,_)).
prim_fluent(pproj(_,_)).
prim_fluent(lookahead(_,_,_,_)).
prim_fluent(bel(_)).
prim_fluent(ltp(_)).


prim_fluent(useAbstraction).

% Spezial-Fluenten muessen gesondert behandelt werden.
special_fluent(pll(_,_,_,_)).  
special_fluent(pproj(_,_)).
special_fluent(lookahead(_,_,_,_)).
special_fluent(bel(_)).
special_fluent(ltp(_)).

%special_fluent(online).

initial_val(R,nil) :- register(R).
initial_val(online,true).
initial_val(eval_registers,true).
initial_val(start,0).
initial_val(pll([H1],LL1,P), true)  :-
	initial_P(H1,Q), P is Q, initial_LL(H1,LL1).

initival_val(useAbstraction, V) :- use_state_abstraction -> V = true; V = false.

/*  Aktionen */
prim_action(clipAbstraction).
prim_action(setAbstraction).

prim_action(send(_,_)).
prim_action(reply(_,_)).
prim_action(setOnline).
prim_action(clipOnline).
prim_action(setTime(_)).

exog_action(ccUpdate(_,_)).
exog_action(reply(_,_)).

/* effect axioms */
causes_val(send(R,V),R,V,true) :- register(R).
causes_val(reply(R,V),R,V,true) :- register(R).
causes_val(setOnline,online,true,true).
causes_val(clipOnline,online,false,true).
causes_val(setTime(T),    start, T, true).
causes_val(ccUpdate(_,T), start, T, true).

causes_val(clipAbstraction, useAbstraction, false).
causes_val(setAbstraction, useAbstraction, true).

/* Vorbedingungen */
poss(send(_R,_),true). % :- register(R).
poss(reply(_R,_),true). % :- register(R).
poss(setOnline,true).
poss(clipOnline,true).
poss(setTime(_),true).

poss(clipAbstraction, true).
poss(setAbstraction, true).

/** set/2 is a general set action for all fluents (cf) */
prim_action(set(_Fluent,_Value)).
causes_val(set(Fluent, Value), Fluent, Value, true).
poss(set(_Fluent,_Value), true).

/* exogenous fluents are updated with the exogf_Update action.
While progression, the history past this action is not regarded.
*/
prim_action(exogf_Update).
poss(exogf_Update, true).
exog_action(exogf_Update).

/** to indicate that exogenous functions should not
be evaluated by now */
prim_fluent(eval_exog_functions).
initial_val(eval_exog_functions, true).

% 

/* ==========================================================
   Cache
========================================================== */
%  Cache
% >>>>

/* Aufrufe der Projektionen und des "Beliefs" werden gecached, da
sie bei der Ausfuehrung eines Programmes mehrfach durchgefuehrt
werden muessen. */
cache(Praed) :- 
	getval(zaehler,Zaehler),
	(clause(cache(Zaehler1,_),_) -> true ; Zaehler1=Zaehler), !,
        % Cache ist alt => Cache loeschen
        (Zaehler1 < Zaehler -> retract_all(cache(_,_)) ; true),  
	(cache(Zaehler,Praed) -> true ;
				% Ergebnis bereits im Cache ?
	    Praed,              % sonst: neu berechnen:
            Praed=..[Func|_],
	    (Func=proj            ->
		Text="Prob. Proj. Test (cached result)." ;
            (Func=bel             ->
		Text="Belief (cached result)." ;
            (Func=limitedProjTest ->
		Text="Projection Test (cached result)." ;
		Text="(cached result)."))),
            % neues Datum im Cache eintragen 
	    asserta(cache(Zaehler,Praed):- writeln(Text))).  

% 

/* ==========================================================
   Believe
========================================================== */
%  Believe
% >>>>

/* bel(Phi,Res,H) sagt aus, dass die Formel Phi mit
Wahrscheinlichkeit Res in der Historie H fuer moeglich gehalten
wird. */
bel(Phi,Res,H) :- % Belief 
     % Ueber die Register besteht keine Unsicherheit
     (
       Phi=(R=Val), register(R) ->
       (holds(R=Val,H) -> Res=1; Res=0)
     ;
       write("Belief "), 
       findall([H1,P],has_val(pll(H1,_LL1,P),true,H),L),
       sort(L,Luniq),!,   
       length(L,N1), length(Luniq,N2),
       printf("(unsorted/sorted # of configs: %p / %p)",[N1,N2]),nl,
       belBindUniq(Phi,Luniq),
       sumBelProb(Phi,Luniq,Punorm),
       normBel(Luniq,Norm),
       (
	 Norm = 0 ->
	 printf("Division by Zero in Bel %p %nin Sit %p %n",[Phi,H]),
	 Res = undefined
       ; Res2 is Punorm/Norm, Res=Res2)
     ).

/* If there are free variables in Phi, ensure that any binding is
possible */
belBindUniq(Phi,Luniq) :-
     nonground(Phi), 
     findall(Phi,belBind(Phi,Luniq),L),
     sort(L,L2), !,
     member(Phi,L2);
     true.

belBind(Phi,Luniq) :-
     member([H,_P],Luniq),
     holds(Phi,H).

sumBelProb(_Phi,[],0).
sumBelProb(Phi,[[H1,P1]|T],P) :-
     holds(Phi,H1) -> sumBelProb(Phi,T,P2), P is P1+P2;
     sumBelProb(Phi,T,P2), P is P2.

normBel([],0).
normBel([[_,P1]|L],S) :- normBel(L,S1), S is S1+P1.


        
schreibe([]) :- nl.
schreibe(L):- L=[L1|L2], print(L1), nl, schreibe(L2). 

% 

/* ==========================================================
   Knowledge Update
========================================================== */
%  Knowledge Update
% >>>>

transPrTo(L,H,LL,HH,T,P) :-
	findall([L1,H1,P1],
		( transPr(L,H,L1,H1,P1), not (H1 = [reply(_,_)|H]),
		    has_val(start,TH1,H1), TH1 =< T),
		Lsucc ),
	(
	  Lsucc=[] -> LL=L,HH=H,P=1
	;
	  member([L1,H1,P1],Lsucc),
	  transPrTo(L1,H1,LL,HH,T,P2),
	  P is P1*P2
	).

% 

/* ==========================================================
   Probabilistic Projection
========================================================== */
%  Probabilistic Projection
% >>>>

/* Probabilistische Projektion (mit oder ohne Progression der
Wissensbasis) */
proj(Phi,E,Prob,H) :- 
     write("Prob. Proj. Test "),
     findall([H1,LL1,P1],has_val(pll(H1,LL1,P1),true,H),L),
     length(L,N1), write("(# of initial configs: "),
     write(N1), write(",  "),
     (
       simple_projection ->  % 1. keine Progression der Wissensbasis:
       (
	 getAllTraces(L,E,Traces), 
	 sort(Traces,TracesS),
	 length(Traces,TN1), length(TracesS,TSN), 
	 printf("unsorted/sorted # of traces: %p / %p",[TN1,TSN]),
	 writeln(")."), 
	 (projection_debug -> schreibe(TracesS) ; true),
	 belBindUniq(Phi,TracesS),
	 calcPhiProb(Phi,TracesS,Punorm),
	 normProj(L,Norm)
       )
     ; 
	% 2. Projektion mit Progression der Wissensbasis:
       (
	 save_current_val,         
	 getAllTraces2(L,E,Phi,Traces),
	 length(Traces,TN1), write("# of traces: "),
	 write(TN1), writeln(")."),
	 (projection_debug -> schreibe(Traces); true),
	 calcPhiProb2(Traces,Punorm),
	 normProj(L,Norm)
       )
     ),
     (Norm = 0 -> Prob = undefined; Prob is Punorm/Norm).


transPrStar(E,H,E,H,1) :- final(E,H), !.
transPrStar(E,H,EE,HH,P) :- 
     transPr(E,H,E1,H1,P1), transPrStar(E1,H1,EE,HH,P2), P is P1*P2.

/* transPrStar2 simuliert eine Programmausfuehrung mit
Progression der Wissensbasis */
transPrStar2(E,H,E,H,_Zaehler,Phi,Holds_Phi,1) :- 
     final(E,H), (holds(Phi,H) -> Holds_Phi=1; Holds_Phi=0), !.
transPrStar2(E,H,EE,HH,Zaehler,Phi,Holds_Phi,P) :- 
     Zaehler2 is Zaehler+1,
     transPr(E,H,E1,H1,P1), !,
     (
       H1=[tossHead|Rest], H2=[tossTail|Rest], transPr(E,H,E2,H2,P2) ->
       /* probabilistische Verzweigung ? */
       (
	 save_current_val(Zaehler), E3=E1, H3=H1, P3=P1 
       ;		% hier findet die prob. Verzweigung statt
	 load_current_val(Zaehler), E3=E2, H3=H2, P3=P2) 
     ;
       E3=E1, H3=H1, P3=P1),
     update_current_val(H3,H4),	% Progression der Wissensbasis
     transPrStar2(E3,H4,EE,HH,Zaehler2,Phi,Holds_Phi,P4), P is P3*P4.

% Sichern der Wissensbasis an Position Z
save_current_val(Z) :-  
     retract_all(temp_val(Z,_,_,_)), !,
     (clause(current_val(F,V,H):-B),
	 assert(temp_val(Z,F,V,H):-B), fail ; true), !,
     retract_all(temp_val(Z,pll(_,_,_),_,_)).        

% Wiederherstellen der Wissensbasis von Position Z
load_current_val(Z) :-  
     retract_all(current_val(_,_,_)), !,
     (clause(temp_val(Z,F,V,H):-B),
	 assert(current_val(F,V,H):-B), fail ; true), !,
     retract_all(temp_val(Z,_,_,_)).                        

% Sichern der Wissensbasis
save_current_val :-  
     retract_all(temp_val(0,_,_,_)), !,
     (clause(current_val(F,V,H):-B),
	 assert(temp_val(0,F,V,H):-B), fail ; true).

% Wiederherstellen der Wissensbasis
load_current_val :-  
     retract_all(current_val(_,_,_)), !,
     (clause(temp_val(0,F,V,H):-B),
	 assert(current_val(F,V,H):-B), fail ; true).

       	
calcPhiProb2([],0).
calcPhiProb2([[P1,Holds_Phi]|T],Punorm) :-
     calcPhiProb2(T,P2), Punorm is Holds_Phi*P1+P2.

getAllTraces2([],_E,_Phi,[]).
getAllTraces2([[H1,LL1,P1]|T],E,Phi,Traces) :-
     findall([Pw,Holds_Phi],
	     weightedDoPr2(pconc(LL1,E),H1,P1,Phi,Pw,Holds_Phi),
	     Traces1),
     load_current_val,  % Wissensbasis wiederherstellen
     getAllTraces2(T,E,Phi,TracesT),
     append(Traces1,TracesT,Traces).

weightedDoPr2(pconc(LL1,E),H,P1,Phi,Pweighted,Holds_Phi) :-
     transPrStar2(pconc(LL1,E),H,_EE,_HH,1,Phi,Holds_Phi,Ptrace),
     Pweighted is Ptrace*P1.

weightedDoPr(pconc(LL1,E),H,P1,HH,Pweighted) :-
     transPrStar(pconc(LL1,E),H,_EE,HH,Ptrace),
     Pweighted is Ptrace*P1.

normProj([],0).
normProj([[_,_,P1]|L],S) :- normProj(L,S1), S is S1+P1.

getAllTraces([],_E,[]).
getAllTraces([[H1,LL1,P1]|T],E,Traces) :-
     findall([H,Pw],weightedDoPr(pconc(LL1,E),H1,P1,H,Pw),Traces1),
     getAllTraces(T,E,TracesT),
     append(Traces1,TracesT,Traces).
	
calcPhiProb(_Phi,[],0).
calcPhiProb(Phi,[[H1,P1]|T],P) :-
     holds(Phi,H1) -> calcPhiProb(Phi,T,P2), P is P1+P2;
     calcPhiProb(Phi,T,P2), P is P2.


% 


/* ==========================================================
   Limited Projection Tests
========================================================== */
%  Limited Projection Tests
% >>>>

/* zeitlich begrenzte Projektion eines Programmes ohne
probabilistische Anweisungen (prob). */
limitedDo(E,H,_T,H) :- final(E,H).

limitedDo(E,H,T,HH) :- 
     trans(E,H,E1,H1), has_val(start,T1,H1),
     (T1 > T, H1 = H;
     T1 =< T, limitedDo(E1,H1,T,HH)).

limitedDo(E,H,T,LL,HH) :- limitedDo(pconc(LL,E),H,T,HH).

limitedProjTest(Phi,T,E,LL,Erg,H) :- % Projektions-Test
     writeln("Projection Test"),
     (
       limitedDo([clipOnline|E],H,T,LL,HH),
       holds(Phi,HH) ->
       Erg=true
     ; Erg=false).
	    
% 


/* ==========================================================
   Evaluating Condition
========================================================== */
%  Evaluating Condition
% >>>>

% Auswertung von Bedingungen bezueglich der Historie H
holds(and(P1,P2),H)  :- !, holds(P1,H), holds(P2,H). 
holds(and([]),_)     :- !.
holds(and([P|R]),H)  :- !, holds(P,H), holds(and(R),H).
holds(or(P1,P2),H)   :- !, (holds(P1,H) ; holds(P2,H)). 
holds(or([P]),H)     :- !, holds(P,H).
holds(or([P|R]),H)   :- !, (holds(P,H) ; holds(or(R),H)).
holds(some(V,P),H)   :- !, subv(V,_,P,P1), holds(P1,H).
holds(exists(V,P),H) :- !, subv(V,_,P,P1), holds(P1,H).
holds(false,_H)      :- !, fail.
holds(0,_H)          :- !, fail.
holds(true,_H)       :- !. 
holds(1,_H)          :- !. 
holds(know(Phi),H)   :- !, holds(bel(Phi)=1,H), !.
holds(lookahead(P,T,E,LL),H) :-
	!, cache(limitedProjTest(P,T,E,LL,Erg,H)), Erg=true.
holds(neg(P),H)      :- !, holds(not(P),H).
holds(not(P),H)      :- !, (
			     nonground(P) ->
			     holds_not(P,H)
			   ; not holds(P,H) ).

/* is action/stoch_proc A possible ? */
holds(is_possible(A), S) :- !,
	subf(A,A_sub,S), !,
	(
	  stoch_proc(A_sub) ->
	  /* A is stoch_proc */
	  stoch_proc_poss(A_sub, S)
	;
	  prolog_poss( A_sub ) ->
	  /* a prolog poss has been defined */
	  prolog_poss( A_sub, S)
	;
	  /* nothing special, look for a poss condition */
	  poss(A_sub, Cond) ->
	  holds(Cond, S)
	).

/** (cf) adding new logical construct:
lif(Cond, Cond1, Cond2)
equivalent to or(and(Cond, Cond1), and(not(Cond), Cond2)) */
holds(lif(Cond, Cond1, Cond2), S) :-
	!,
	(
	  holds(Cond, S) ->
	  holds(Cond1, S)
	;
	  holds(Cond2, S)
	).

holds(writeln(X), S) :-
	!, subf(X, Xsub, S),
	writeln(Xsub),
	flush(output).

holds(P,H) :-	
	P=..[Op,A,B], (Op=(=); Op=(<); (Op=(>); (Op=(=<); Op=(>=)))),
	!, eval_comparison(false,Op,A,B,Expr,H),
	(Expr=and(_) -> holds(Expr,H) ; call(Expr)). 

holds(P,H) :- proc(P,_), !, subf(P,P1,H), % call-by-value 
              proc(P1,P2), holds(P2,H).

/* allow conditions returned by functions */
holds(P,H) :- function(P,_,_), !, subf(P,P1,H),
              holds(P1,H).  

holds(P,H) :- exog_function(P), !, subf(P,P1,H),
              holds(P1,H).  

holds(P,H) :- subf(P,P1,H), call(P1).


/* Auswertung von negierten Bedingungen. "holds_not" schiebt
Negation nach innen und wendet sie auf Grundterme an. Dies ist
notwendig, da eine "negation by failure" nichtlogische Ergebnisse
nach sich ziehen kann, wenn in der Formel freie Variablen
vorkommen. */
holds_not(and(P1,P2),H) :- !, holds(or(not(P1),not(P2)),H).     
holds_not(and(L),H)     :- neg_list(L,L2), !, holds(or(L2),H).  
holds_not(or(P1,P2),H)  :- !, holds(and(not(P1),not(P2)),H).    
holds_not(or(L),H)      :- neg_list(L,L2), !, holds(and(L2),H). 
holds_not(not(P1),H) :- !, holds(P1,H).  % doppelte Negation
holds_not(neg(P1),H) :- !, holds(P1,H).  %     "    "

holds_not(is_possible(A), S) :- !,
	subf(A,A_sub,S), !,
	(
	  stoch_proc(A_sub) ->
	  /* A is stoch_proc */
	  not(stoch_proc_poss(A_sub, S))
	;
	  option(A_sub, _) ->
	  /* A is an option */
	  not(prolog_option_poss(A_sub, S))
	;
	  prolog_poss( A_sub ) ->
	  /* a prolog poss has been defined */
	  not(prolog_poss( A_sub, S))
	;
	  /* nothing special, look for a poss condition */
	  poss(A_sub, Cond) ->
	  holds(not(Cond), S)
	).

holds_not(lif(Cond, Cond1, Cond2), S) :-
	!,
	(
	  holds(Cond, S) ->
	  holds(not(Cond1), S)
	;
	  holds(not(Cond2), S)
	).

holds_not(P,H) :- 
  	P=..[Op,A,B], (Op=(=); Op=(<); (Op=(>); (Op=(=<); Op=(>=)))),
	!, eval_comparison(not,Op,A,B,Expr,H),
  	(Expr=not(and(_)) -> holds(Expr,H) ; call(Expr)). 

holds_not(P,H) :- fluent(P), !, subf(P,P1,H), call(not(P1)).
holds_not(P,H) :- proc(P,_), !, subf(P,P1,H),  % call-by-value
	          proc(P1,P2), holds(not(P2),H). 
holds_not(P,H) :- !, not holds(P,H).  % Negation by failure


/* Evaluating Comparisons (=, <, >, =<, >=) */
eval_comparison(Not,Op,A,B,Expr,H) :-  
     subf(A,A1,H), subf(B,B1,H),
     (
       \+var(A1), \+var(B1),
       t_function(A1), t_function(B1) ->
       A2 = A1, B2 = B1
     ;       
       (
	 (
	   \+var(A1),t_function(A1) ->
	   (has_val(start,T1,H), t_function(A1,A2,T1))
	 ;
	   A2 = A1
	 ),  
	 (
	   \+var(B1),t_function(B1) ->
	   (has_val(start,T2,H), t_function(B1,B2,T2))
	 ;
	   B2 = B1
	 )
       )
     ),
     (
       /* comment out this first case to reduce expressiveness
       (drop comparing lists component wise), but gain speed */
        liste(A2), liste(B2) ->      
        merge_lists(Op,A2,B2,C), 
        Expr2=and(C), 
        (
 	 Not=not ->
 	 Expr=not(Expr2)
        ;
 	 Expr=Expr2
        )
      ;
       (is_domain(A2) ; is_domain(B2)) ->
       fd_op(Not,Op,Op2), Expr=..[Op2,A2,B2] 
     ;
       Expr2=..[Op,A2,B2], 
       (Not=not -> Expr=not(Expr2) ; Expr=Expr2)
     ).


/* merge_lists verknuepft zwei Listen mittels der Operation "Op"
komponentenweise. */
merge_lists(_,[],[],[]).
merge_lists(Op,L1,L2,L3) :- 
     L1=[A1|R1], L2=[A2|R2],
     merge_lists(Op,R1,R2,R3),
     A3=..[Op,A1,A2],
     L3=[A3|R3].


% Jedes Element der Liste wird durch ein "not()" umklammert
neg_list([],[]).
neg_list(L,L2) :-  
     L=[A|B], 
     A2=not(A), neg_list(B,B2),
     L2=[A2|B2].

% Ersetzung eines Vergleichoperators durch einen Operator der
% finite domain library. 
% Notwendig fuer die Progression der Wissensbasis.
fd_op(Not,=,P) :-  (Not=not -> P=(##)  ; P=(#=)).
fd_op(Not,<,P) :-  (Not=not -> P=(#>=) ; P=(#<)).
fd_op(Not,>,P) :-  (Not=not -> P=(#<=) ; P=(#>)).
fd_op(Not,=<,P) :- (Not=not -> P=(#>)  ; P=(#<=)).
fd_op(Not,>=,P) :- (Not=not -> P=(#<)  ; P=(#>=)).



/* ----------------------------------------------------------
   subf
---------------------------------------------------------- */


% Ersetzung von Fluenten durch ihre aktuellen Werte und 
% Auswertung von arithmetischen Ausdruecken.
% P2 ist das ausgewertete P1.
subf(P,P,_H) :- var(P), !.
subf(P,P,_H) :- number(P), !.
subf(P,P,_H) :- const(P), !.

subf(A+B,C,H)  :- !, subf_arithm(A+B,C,H).
subf(A-B,C,H)  :- !, subf_arithm(A-B,C,H).
subf(A*B,C,H)  :- !, subf_arithm(A*B,C,H).
subf(A/B,C,H)  :- !, subf_arithm(A/B,C,H).

/* cf: added rule for lists.
was missing in icpGolog: lists were somehow
treated by last clause of subf */
%subf([H|L],Res,S) :- subfl([H|L],Res,S).
/* somehow this doesn't work, don't know why!?? */


subf(P1,P2,H)  :- fluent(P1), !, 
                  (% Parameter auswerten
		    special_fluent(P1) ->
		    P3=P1 
		  ; P1=..[F|L1],
		    subfl(L1,L2,H),
		    P3=..[F|L2]
		  ),
		  (
		    register(P1) ->
		    /* special treatment for registers */		    
		    has_val( eval_registers, Eval, H),
		    (
		      Eval ->  has_val(P3,P2,H)
		    ;
		      not(Eval) -> P2 = P3
		    )
		  ;
		    
% 		    /* if not a register */
% 				%		    has_val(P3,P2,H),
% 		    /**<state_abstraction> */		    
% 		    bb_dbg(5, green, "Before Fluent QQ \n\n", []),
% 		    printf("P1: %w, P2: %w, H: %w\n", [P1, P2, H]),
% 		    read(Y),
% 		    (
% 		      (use_state_abstraction,
% 			  has_val(online, V, H), V = false) ->
% 		      bb_dbg(5, green, "\n\n\nUsing QQ\n\n", []),
% 		      read(X1),
% 		      P3 =.. [Function|Args],
% 		      term_string(Function, FunctionString),
% % 		      (
% % 			substring(FunctionString, "qq_", 1)->
% % 			PNew =.. [Function|Args]
% % 		      ;
% % 			append_strings("qq_", FunctionString, FunctionStringNew),
% % 			term_string(FunctionNew, FunctionStringNew),
% % 			PNew =..[FunctionNew|Args]
% % 			),
% 		      (
% 			fluent(PNew) ->
% 			  P4 = PNew
% 		      ;
% 			bb_dbg(5,red, "QQ-Fluent %w does not exist\n", [PNew]),
% 			P4 = P3
% 		      )
% 		    ;
% 		      P4 = P3,
% 		      bb_dbg(5, red, "Calling hasval %w" ,[]),
% 		      ),
% 		    bb_dbg(5, red, "Calling hasval" ,[]),
% 		    has_val(P4, P2, H)
% 		    /**</state_abstraction> */
		    has_val(P3, P2, H)
		    ).

/** cf: here comes restriction for only simple args (nothing like
[X,Y]) */
subf(P,P2,H)  :-
	function(P,_,_),!,	
 	/**<state_abstraction> */
 	(
 	  %printColor(green, "Before QQ \n", []),
 	  use_state_abstraction, holds(not(online), H) ->
 	  bb_dbg(5, green, "Using QQ \n", []),
 	  P =.. [Function|Args],
 	  term_string(Function, FunctionString),
 	  append_strings("qq_", FunctionString, FunctionStringNew),
 	  term_string(FunctionNew, FunctionStringNew),
 	  PNew =..[FunctionNew|Args],
 	  (
 	    function(PNew, _, _) ->
 	    P1 = PNew
 	  ;
 	    bb_dbg(5,red, "QQ-Function %w does not exist\n", [PNew]),
 	    P1 = P
 	  )
 	;
	  P1 = P
	  ), /**</state_abstraction> */
 	  bb_dbg(5, green, "After QQ\n\n", []),	
	/* evaluating parameters */
	P1=..[F|L1],
	/* special function id returns the argument as a string
	without evaluating it first ! */
	(
	  F = idstring ->
	  L1 = [Arg],
	  term_string( Arg, P2)
	;
	  not(F = idstring ),
	  subfl(L1,L2,H),
	  P3=..[F|L2],
	  (
	    /* if preprocessed version of this function exists:
	    use it! */
	    prolog_function(P1) ->
	    prolog_function(P3, P2, H)
	  ;
	    function(P3,P4,Praed) ->
	    holds(Praed,H),
	    subf(P4,P2,H)
	  )
	).

/** (26.5.04) for exogenous functions: evaluated externally (kl
in our case) */
subf( F, F_eval, S) :-
	exog_function( F ), !,
	F =.. [Name|Args],
	subfl(Args, Args_sub, S),
	F_sub =.. [Name|Args_sub],
	/* see if evaluation of exogenous function requested
	or turned off */
	has_val( eval_exog_functions, Eval, S),
	(
	  Eval ->
	  exog_function_getValue( F_sub, F_eval), !
	;
	  not(Eval) ->
	  F_eval = F_sub
	), !.


/* cf: special rule for set(fluentname, Value)-action:
the fluentname must not be subf-ed */
subf(P,P_res,H) :-
	P =..[set,Fluentname|L], !,
	subfl(L,L_sub,H),
	P_res =.. [set, Fluentname|L_sub].

/** cf: this is analogous to preprocessor: e.g. allow X=sqrt(4.0) */
subf(P1,P_res,H)  :-
	P1=..[F|L1],
	subfl(L1,L2,H),
	length(L1, ArgC), ArgC_plus is ArgC+1,
	(
	  string(F) ->
	  P_res=..[F|L2]
	;
	  is_predicate(F/ArgC_plus),
	  is_built_in(F/ArgC_plus) ->
	  /* arithmetic predicate call */
  	  P_tmp=..[F|L2],
	  P_res is P_tmp
	;
	  P_res=..[F|L2]
	).

subfl([],[],_H).
subfl([T1|L1],[T2|L2],H) :- subf(T1,T2,H), subfl(L1,L2,H).

% Auswertung von arithm. Ausdruecken
subf_arithm(P,C,H) :- 
	P=..[Op,A,B],                 
	subf(A,A1,H), subf(B,B1,H),
	/* evaluate t-function at current time */ 
	(
	  \+var(A1),t_function(A1) ->
	  (has_val(start,T1,H), t_function(A1,A2,T1))
	;
	  A2 = A1
	),  
	(
	  \+var(B1),t_function(B1) ->
	  (has_val(start,T2,H), t_function(B1,B2,T2))
	;
	  B2 = B1
	),
	(
	  liste(A2), liste(B2) ->
	  /* arithm. Verknuepfung von Listen (komponentenweise) */
	  (
	    A2=[A3], B2=[B3], C1=..[Op,A3,B3] ->
	    subf(C1,C2,H), C=[C2] 
	  ;
	    A2=[A3|A4], B2=[B3|B4], C1=..[Op,A3,B3], C2=..[Op,A4,B4],
	    subf(C1,C3,H), subf(C2,C4,H), C=[C3|C4]
	  ) 
	;
	  /* arithm. Verknuepfung von einfachen Elementen */
	  (
	    C1=..[Op,A2,B2],  
	    (isNumber(A2), isNumber(B2) -> C is C1; C = C1)
	  )
	).

isNumber(X) :- number(X), !.
isNumber(pi).


% 


/* ==========================================================
   Regression
========================================================== */
%  Regression
% >>>>

/* ----------------------------------------------------------
   has_val(F,V,H): berechnet den aktuellen Wert des Fluenten F in
   der Historie H
---------------------------------------------------------- */

/** fluent is a so-called exogenous fluent and an 'update
exogenous fluents action' had happened: look up the value in the
database for (direct access) exogenous fluent */
has_val(F,V,[exogf_Update]) :-
	exog_fluent(F), !,
	exog_fluent_getValue(F, V, []).

/** situation term of length one: look up the fluent value in the
knowledge base created by progression or if the situation is [s0]
use the initial value (if defined) */
has_val(F,V,[S]) :- current_val(F,V,S), !.  
has_val(F,V,[s0]) :- initial_val(F,V).  


/* Sonder-Fluenten, deren Werte nicht durch Aktionen veraendert
werden: */
has_val(pproj(Phi,E),V,H) :- !, cache(proj(Phi,[clipOnline|E],V,H)).
has_val(bel(Phi),V,H) :- !,
	cache(bel(Phi,Prob,H)), (free(V) -> V = Prob; V =:= Prob).
has_val(ltp(P),V,H) :- !, ltp(P,V,H). 
has_val(pll(H1,LL1,P), true, [E|H]) :- !,
	has_val(pll(H11,LL11,P1),true,H), has_val(start,T,[E|H]),
        transPrTo(LL11,H11,L1,HH1,T,P11), H1=[E|HH1],
	(E \= reply(_,_), LL1=L1, P is P1 * P11;
	 E = reply(_,_), transPr(L1,HH1,LL1,H1,1), P is P1 * P11).


/* if there is an SSA for this fluent, use it */
has_val(F,V,S) :- ssa(F,V,S), !.

/* usual fluents change their value only by actions */
has_val(F,V,[Act|H]) :-
	(nonground(F) -> Cut=fail; Cut=true), 
	sets_val(Act,F,V,H), (Cut -> !; true) ; 
	has_val(F,V,H), not sets_val(Act,F,_V1,H). 
 

/* ----------------------------------------------------------
sets_val(Act,F,V,H):
Ausfuehrung der Aktion Act fuehrt dazu, dass der Fluent F in der
aktuellen Historie H den Wert V erhaelt. Act kann eine normale
Aktion mit Nachfolgeraxiom oder eine exogene Aktion e(F,V) sein.
---------------------------------------------------------- */
/* sensor action setting the value */
sets_val(e(F,V1),F,V,H) :- !, subf(V1,V,H).

%cf: action did the update of all exogenous fluents
sets_val(exogf_Update, F, V, S) :- !,
	exog_fluent(F),
	exog_fluent_getValue(F, V, S).

/** general set(fluent, NewValue) action (cf) */
set_val(set(Fluent, NewValue), Fluent, NewValue, _) :- !.

/* usual action setting the value */
sets_val(Act,F,V,H) :-                        
	causes_val(Act,F,V1,P),
	(
	  (progression_enabled ; pe) ->
	  /* Fluentenparameter an endliche Integer-Domaene binden */
	  F=..[_|Param], integers(Param) 
	; true),  
	holds(P,H), subf(V1,V,H).  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- writeln(" <----- Loading readylog.pl \t[DONE]\n").
