/*******************************
* file generated from ReadyLog *
*******************************/

:- pragma(nodebug).
prolog_function( f_is_pos(PreVar4, PreVar12) ).
prolog_function( f_is_pos(PreVar4, PreVar12), PreVar20, S ) :- world(PreVar0, PreVar1, PreVar2, PreVar3), PreVar4 >= PreVar0, PreVar4 =< PreVar2, PreVar12 >= PreVar1, PreVar12 =< PreVar3 -> PreVar20=true ; PreVar20=false.
prolog_function( is_occupied(PreVar28, PreVar29) ).
prolog_function( is_occupied(PreVar28, PreVar29), PreVar30, S ) :- occupied(PreVar28, PreVar29) -> PreVar30=true ; PreVar30=false.
prolog_function( f_is_reachable(PreVar38, PreVar39, PreVar40, PreVar41) ).
prolog_function( f_is_reachable(PreVar38, PreVar39, PreVar40, PreVar41), PreVar42, S ) :- reachable_init(PreVar38, PreVar39, PreVar40, PreVar41) -> PreVar42=true ; PreVar42=false.
stoch_proc(goto(Xrelto, Yrelto)).
stoch_proc_outcomes(goto(PreVar62, PreVar66), S, Outcomes, SenseEffect) :- (has_val(epf_agent_pos, PreVar52, S), PreVar52=[PreVar57, PreVar58]), (PreVar60 is PreVar57 + PreVar62, PreVar59 is PreVar60), (PreVar64 is PreVar58 + PreVar66, PreVar63 is PreVar64), Outcomes=[(noop, 0.1, epf_agent_pos=[PreVar57, PreVar58]), (goto_prim(PreVar59, PreVar63), 0.9, epf_agent_pos=[PreVar59, PreVar63])], SenseEffect=[exogf_Update], !.
stoch_proc_poss(goto(PreVar79, PreVar83), S) :- (has_val(epf_agent_pos, PreVar69, S), PreVar69=[PreVar74, PreVar75]), (PreVar77 is PreVar74 + PreVar79, PreVar76 is PreVar77), (PreVar81 is PreVar75 + PreVar83, PreVar80 is PreVar81), prolog_function(f_is_reachable(PreVar74, PreVar75, PreVar76, PreVar80), P_res, S), holds(P_res, [s0]), !.
stoch_proc_costs(goto(PreVar100, PreVar104), PreVar109, S) :- (has_val(epf_agent_pos, PreVar90, S), PreVar90=[PreVar95, PreVar96]), (PreVar98 is PreVar95 + PreVar100, PreVar97 is PreVar98), (PreVar102 is PreVar96 + PreVar104, PreVar101 is PreVar102), manhattan_dist(PreVar95, PreVar96, PreVar97, PreVar101, PreVar109), !.
ssa( online, false, [clipOnline|Srest] ) :- !, true.
ssa( epf_human_pos, PreVar123, [human_down|Srest] ) :- !, (has_val(epf_human_pos, PreVar112, Srest), PreVar112=[PreVar117, PreVar118]), (PreVar120 is PreVar118 - (1), PreVar119 is PreVar120), PreVar123=[PreVar117, PreVar119].
ssa( epf_human_pos, PreVar142, [human_left|Srest] ) :- !, (has_val(epf_human_pos, PreVar131, Srest), PreVar131=[PreVar136, PreVar137]), (PreVar139 is PreVar136 - (1), PreVar138 is PreVar139), PreVar142=[PreVar138, PreVar137].
ssa( epf_human_pos, PreVar161, [human_right|Srest] ) :- !, (has_val(epf_human_pos, PreVar150, Srest), PreVar150=[PreVar155, PreVar156]), (PreVar158 is PreVar155 + (1), PreVar157 is PreVar158), PreVar161=[PreVar157, PreVar156].
ssa( epf_human_pos, PreVar180, [human_up|Srest] ) :- !, (has_val(epf_human_pos, PreVar169, Srest), PreVar169=[PreVar174, PreVar175]), (PreVar177 is PreVar175 + (1), PreVar176 is PreVar177), PreVar180=[PreVar174, PreVar176].
ssa( online, true, [setOnline|Srest] ) :- !, true.
ssa( start, NewValue, [setTime(NewValue)|Srest] ) :- !, true.
ssa( start, NewValue, [ccUpdate(_9130, NewValue)|Srest] ) :- !, true.
ssa( epf_agent_pos, PreVar203, [goto_prim(PreVar198, PreVar202)|Srest] ) :- !, (has_val(epf_agent_pos, PreVar188, Srest), PreVar188=[PreVar193, PreVar194]), (PreVar196 is PreVar193 + PreVar198, PreVar195 is PreVar196), (PreVar200 is PreVar194 + PreVar202, PreVar199 is PreVar200), PreVar203=[PreVar195, PreVar199].
ssa( action, NewValue, [reply(action, NewValue)|Srest] ) :- !, true.
ssa( action, NewValue, [send(action, NewValue)|Srest] ) :- !, true.
ssa( Fluent, NewValue, [set(Fluent, NewValue)|Srest] ) :- !, true.

/* 	___ dummy SSAs ___ */
ssa( action, NewValue, [set(action, NewValue)|_] ) :- !.
ssa( eval_exog_functions, NewValue, [set(eval_exog_functions, NewValue)|_] ) :- !.
ssa( eval_registers, NewValue, [set(eval_registers, NewValue)|_] ) :- !.
ssa( online, NewValue, [set(online, NewValue)|_] ) :- !.
ssa( start, NewValue, [set(start, NewValue)|_] ) :- !.
ssa( useAbstraction, NewValue, [set(useAbstraction, NewValue)|_] ) :- !.
ssa( bel(_14592), NewValue, [set(bel(_14592), NewValue)|_] ) :- !.
ssa( ltp(_14596), NewValue, [set(ltp(_14596), NewValue)|_] ) :- !.
ssa( pproj(_14580, _14581), NewValue, [set(pproj(_14580, _14581), NewValue)|_] ) :- !.
ssa( lookahead(_14585, _14586, _14587, _14588), NewValue, [set(lookahead(_14585, _14586, _14587, _14588), NewValue)|_] ) :- !.
ssa( pll(_14573, _14574, _14575, _14576), NewValue, [set(pll(_14573, _14574, _14575, _14576), NewValue)|_] ) :- !.
ssa( epf_agent_pos, NewValue, [set(epf_agent_pos, NewValue)|_] ) :- !.
ssa( epf_human_pos, NewValue, [set(epf_human_pos, NewValue)|_] ) :- !.
ssa( epf_sense_human, NewValue, [set(epf_sense_human, NewValue)|_] ) :- !.
ssa( epf_agent_pos, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_agent_pos, NewValue, Srest).
ssa( epf_human_pos, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_human_pos, NewValue, Srest).
ssa( epf_sense_human, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_sense_human, NewValue, Srest).
ssa( action, NewValue, [_UnknownAction|Srest] ) :- !, has_val(action, NewValue, Srest).
ssa( eval_exog_functions, NewValue, [_UnknownAction|Srest] ) :- !, has_val(eval_exog_functions, NewValue, Srest).
ssa( eval_registers, NewValue, [_UnknownAction|Srest] ) :- !, has_val(eval_registers, NewValue, Srest).
ssa( online, NewValue, [_UnknownAction|Srest] ) :- !, has_val(online, NewValue, Srest).
ssa( start, NewValue, [_UnknownAction|Srest] ) :- !, has_val(start, NewValue, Srest).
ssa( useAbstraction, NewValue, [_UnknownAction|Srest] ) :- !, has_val(useAbstraction, NewValue, Srest).
ssa( bel(_14592), NewValue, [_UnknownAction|Srest] ) :- !, has_val(bel(_14592), NewValue, Srest).
ssa( ltp(_14596), NewValue, [_UnknownAction|Srest] ) :- !, has_val(ltp(_14596), NewValue, Srest).
ssa( pproj(_14580, _14581), NewValue, [_UnknownAction|Srest] ) :- !, has_val(pproj(_14580, _14581), NewValue, Srest).
ssa( lookahead(_14585, _14586, _14587, _14588), NewValue, [_UnknownAction|Srest] ) :- !, has_val(lookahead(_14585, _14586, _14587, _14588), NewValue, Srest).
ssa( pll(_14573, _14574, _14575, _14576), NewValue, [_UnknownAction|Srest] ) :- !, has_val(pll(_14573, _14574, _14575, _14576), NewValue, Srest).
ssa( epf_agent_pos, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_agent_pos, NewValue, Srest).
ssa( epf_human_pos, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_human_pos, NewValue, Srest).
ssa( epf_sense_human, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_sense_human, NewValue, Srest).
prolog_poss( send(_R, _15137) ).
prolog_poss( send(_R, _15137), S ) :- true.
prolog_poss( reply(_R, _15146) ).
prolog_poss( reply(_R, _15146), S ) :- true.
prolog_poss( setOnline ).
prolog_poss( setOnline, S ) :- true.
prolog_poss( clipOnline ).
prolog_poss( clipOnline, S ) :- true.
prolog_poss( setTime(_15164) ).
prolog_poss( setTime(_15164), S ) :- true.
prolog_poss( clipAbstraction ).
prolog_poss( clipAbstraction, S ) :- true.
prolog_poss( setAbstraction ).
prolog_poss( setAbstraction, S ) :- true.
prolog_poss( set(_Fluent, _Value) ).
prolog_poss( set(_Fluent, _Value), S ) :- true.
prolog_poss( exogf_Update ).
prolog_poss( exogf_Update, S ) :- true.
prolog_poss( noop ).
prolog_poss( noop, S ) :- true.
prolog_poss( goto_prim(PreVar221, PreVar225) ).
prolog_poss( goto_prim(PreVar221, PreVar225), S ) :- (has_val(epf_agent_pos, PreVar211, S), PreVar211=[PreVar216, PreVar217]), (PreVar219 is PreVar216 + PreVar221, PreVar218 is PreVar219), (PreVar223 is PreVar217 + PreVar225, PreVar222 is PreVar223), prolog_function(f_is_reachable(PreVar216, PreVar217, PreVar218, PreVar222), P_res, S), holds(P_res, [s0]).
prolog_poss( human_up ).
prolog_poss( human_up, S ) :- true.
prolog_poss( human_down ).
prolog_poss( human_down, S ) :- true.
prolog_poss( human_right ).
prolog_poss( human_right, S ) :- true.
prolog_poss( human_left ).
prolog_poss( human_left, S ) :- true.
prolog_poss( cout(_Text) ).
prolog_poss( cout(_Text), S ) :- true.
prolog_poss( cout(_Text, _Params) ).
prolog_poss( cout(_Text, _Params), S ) :- true.
prolog_poss( cout(_Color, _Text, _Params) ).
prolog_poss( cout(_Color, _Text, _Params), S ) :- true.
prolog_poss( printf(_String, _Params) ).
prolog_poss( printf(_String, _Params), S ) :- true.
prolog_poss( printf(_Stream, _String, _Params) ).
prolog_poss( printf(_Stream, _String, _Params), S ) :- true.
events_list([]).
