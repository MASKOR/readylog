/*******************************
* file generated from ReadyLog *
*******************************/

:- pragma(nodebug).
prolog_function( f_is_pos(PreVar4, PreVar12) ).
prolog_function( f_is_pos(PreVar4, PreVar12), PreVar20, S ) :- world(PreVar0, PreVar1, PreVar2, PreVar3), PreVar4 >= PreVar0, PreVar4 =< PreVar2, PreVar12 >= PreVar1, PreVar12 =< PreVar3 -> PreVar20=true ; PreVar20=false.
prolog_function( f_is_wall(PreVar28, PreVar29, PreVar30, PreVar31) ).
prolog_function( f_is_wall(PreVar28, PreVar29, PreVar30, PreVar31), PreVar36, S ) :- (wall(PreVar28, PreVar29, PreVar30, PreVar31) ; wall(PreVar30, PreVar31, PreVar28, PreVar29)) -> PreVar36=true ; PreVar36=false.
prolog_function( reward ).
prolog_function( reward, PreVar51, S ) :- has_val(requests, PreVar46, S), PreVar46 = [] -> PreVar51=100 ; PreVar51 = -1.
ssa( online, false, [clipOnline|Srest] ) :- !, true.
ssa( coffee_prepared, PreVar59, [prepare_coffee|Srest] ) :- !, PreVar59=true.
ssa( online, true, [setOnline|Srest] ) :- !, true.
ssa( interacting, true, [start_interact|Srest] ) :- !, true.
ssa( moving_arm, true, [start_move_arm|Srest] ) :- !, true.
ssa( interacting, false, [stop_interact|Srest] ) :- !, true.
ssa( moving_arm, false, [stop_move_arm|Srest] ) :- !, true.
ssa( pos, PreVar63, [goto(PreVar64)|Srest] ) :- !, PreVar63 = PreVar64.
ssa( located, PreVar67, [goto(_6605)|Srest] ) :- !, PreVar67=nil.
ssa( holding, PreVar71, [pickup(PreVar72)|Srest] ) :- !, PreVar71 = PreVar72.
ssa( coffee_prepared, PreVar79, [pickup(PreVar75)|Srest] ) :- !, PreVar75=coffee, PreVar79=false.
ssa( start, NewValue, [setTime(NewValue)|Srest] ) :- !, true.
ssa( holding, PreVar83, [set_holding(PreVar84)|Srest] ) :- !, PreVar83 = PreVar84.
ssa( task, PreVar87, [set_task(PreVar88)|Srest] ) :- !, PreVar87 = PreVar88.
ssa( going_to, true, [start_goto(R)|Srest] ) :- !, true.
ssa( picking_up, true, [start_pickup(X)|Srest] ) :- !, true.
ssa( placing_order, true, [start_place_order(P)|Srest] ) :- !, true.
ssa( taking_order, true, [start_take_order(P)|Srest] ) :- !, true.
ssa( going_to, false, [stop_goto(R)|Srest] ) :- !, true.
ssa( pos, PreVar91, [stop_goto(PreVar92)|Srest] ) :- !, PreVar91 = PreVar92.
ssa( located, PreVar95, [stop_goto(R)|Srest] ) :- !, PreVar95=nil.
ssa( picking_up, false, [stop_pickup(X)|Srest] ) :- !, true.
ssa( holding, PreVar99, [stop_pickup(PreVar100)|Srest] ) :- !, PreVar99 = PreVar100.
ssa( coffee_prepared, PreVar107, [stop_pickup(PreVar103)|Srest] ) :- !, PreVar103=coffee, PreVar107=false.
ssa( placing_order, false, [stop_place_order(P)|Srest] ) :- !, true.
ssa( taking_order, false, [stop_take_order(P)|Srest] ) :- !, true.
ssa( task, PreVar118, [stop_take_order(P)|Srest] ) :- !, has_val(requests, PreVar113, Srest), PreVar113=[PreVar118|_T].
ssa( requests, NewValue, [stop_take_order(P)|Srest] ) :- !, has_val(requests, PreVar121, Srest), PreVar121=[PreVar126|NewValue].
ssa( task, PreVar134, [take_order(_6698)|Srest] ) :- !, has_val(requests, PreVar129, Srest), PreVar129=[PreVar134|_T].
ssa( requests, NewValue, [take_order(_6722)|Srest] ) :- !, has_val(requests, PreVar137, Srest), PreVar137=[PreVar142|NewValue].
ssa( pos, NewValue, [teleport(NewValue)|Srest] ) :- !, true.
ssa( start, NewValue, [ccUpdate(_6409, NewValue)|Srest] ) :- !, true.
ssa( holding, PreVar143, [drop(_X, _R)|Srest] ) :- !, PreVar143=nil.
ssa( task, PreVar158, [drop(PreVar154, PreVar148)|Srest] ) :- !, (has_val(task, PreVar149, Srest), PreVar149 = PreVar148), PreVar154=coffee, PreVar158=nil.
ssa( located, PreVar162, [locate(PreVar163, _6627)|Srest] ) :- !, PreVar162 = PreVar163.
ssa( action, NewValue, [reply(action, NewValue)|Srest] ) :- !, true.
ssa( action, NewValue, [send(action, NewValue)|Srest] ) :- !, true.
ssa( Fluent, NewValue, [set(Fluent, NewValue)|Srest] ) :- !, true.
ssa( Fluent, NewValue, [set_component(Fluent, NewValue)|Srest] ) :- !, true.
ssa( dropping, true, [start_drop(X, R)|Srest] ) :- !, true.
ssa( locating, true, [start_locate(P, R)|Srest] ) :- !, true.
ssa( dropping, false, [stop_drop(X, R)|Srest] ) :- !, true.
ssa( holding, PreVar166, [stop_drop(X, R)|Srest] ) :- !, PreVar166=nil.
ssa( task, PreVar181, [stop_drop(PreVar177, PreVar171)|Srest] ) :- !, (has_val(task, PreVar172, Srest), PreVar172 = PreVar171), PreVar177=coffee, PreVar181=nil.
ssa( locating, false, [stop_locate(P, R)|Srest] ) :- !, true.
ssa( located, PreVar185, [stop_locate(PreVar186, R)|Srest] ) :- !, PreVar185 = PreVar186.

/* 	___ dummy SSAs ___ */
ssa( action, NewValue, [set(action, NewValue)|_] ) :- !.
ssa( armState, NewValue, [set(armState, NewValue)|_] ) :- !.
ssa( dropping, NewValue, [set(dropping, NewValue)|_] ) :- !.
ssa( eval_exog_functions, NewValue, [set(eval_exog_functions, NewValue)|_] ) :- !.
ssa( eval_registers, NewValue, [set(eval_registers, NewValue)|_] ) :- !.
ssa( holding, NewValue, [set(holding, NewValue)|_] ) :- !.
ssa( interacting, NewValue, [set(interacting, NewValue)|_] ) :- !.
ssa( located, NewValue, [set(located, NewValue)|_] ) :- !.
ssa( locating, NewValue, [set(locating, NewValue)|_] ) :- !.
ssa( moving_arm, NewValue, [set(moving_arm, NewValue)|_] ) :- !.
ssa( online, NewValue, [set(online, NewValue)|_] ) :- !.
ssa( picking_up, NewValue, [set(picking_up, NewValue)|_] ) :- !.
ssa( placing_order, NewValue, [set(placing_order, NewValue)|_] ) :- !.
ssa( start, NewValue, [set(start, NewValue)|_] ) :- !.
ssa( taking_order, NewValue, [set(taking_order, NewValue)|_] ) :- !.
ssa( task, NewValue, [set(task, NewValue)|_] ) :- !.
ssa( useAbstraction, NewValue, [set(useAbstraction, NewValue)|_] ) :- !.
ssa( bel(_13404), NewValue, [set(bel(_13404), NewValue)|_] ) :- !.
ssa( ltp(_13408), NewValue, [set(ltp(_13408), NewValue)|_] ) :- !.
ssa( pproj(_13392, _13393), NewValue, [set(pproj(_13392, _13393), NewValue)|_] ) :- !.
ssa( lookahead(_13397, _13398, _13399, _13400), NewValue, [set(lookahead(_13397, _13398, _13399, _13400), NewValue)|_] ) :- !.
ssa( pll(_13385, _13386, _13387, _13388), NewValue, [set(pll(_13385, _13386, _13387, _13388), NewValue)|_] ) :- !.
ssa( coffee_prepared, NewValue, [set(coffee_prepared, NewValue)|_] ) :- !.
ssa( going_to, NewValue, [set(going_to, NewValue)|_] ) :- !.
ssa( pos, NewValue, [set(pos, NewValue)|_] ) :- !.
ssa( requests, NewValue, [set(requests, NewValue)|_] ) :- !.
ssa( coffee_prepared, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(coffee_prepared, NewValue, Srest).
ssa( going_to, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(going_to, NewValue, Srest).
ssa( pos, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(pos, NewValue, Srest).
ssa( requests, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(requests, NewValue, Srest).
ssa( action, NewValue, [_UnknownAction|Srest] ) :- !, has_val(action, NewValue, Srest).
ssa( armState, NewValue, [_UnknownAction|Srest] ) :- !, has_val(armState, NewValue, Srest).
ssa( dropping, NewValue, [_UnknownAction|Srest] ) :- !, has_val(dropping, NewValue, Srest).
ssa( eval_exog_functions, NewValue, [_UnknownAction|Srest] ) :- !, has_val(eval_exog_functions, NewValue, Srest).
ssa( eval_registers, NewValue, [_UnknownAction|Srest] ) :- !, has_val(eval_registers, NewValue, Srest).
ssa( holding, NewValue, [_UnknownAction|Srest] ) :- !, has_val(holding, NewValue, Srest).
ssa( interacting, NewValue, [_UnknownAction|Srest] ) :- !, has_val(interacting, NewValue, Srest).
ssa( located, NewValue, [_UnknownAction|Srest] ) :- !, has_val(located, NewValue, Srest).
ssa( locating, NewValue, [_UnknownAction|Srest] ) :- !, has_val(locating, NewValue, Srest).
ssa( moving_arm, NewValue, [_UnknownAction|Srest] ) :- !, has_val(moving_arm, NewValue, Srest).
ssa( online, NewValue, [_UnknownAction|Srest] ) :- !, has_val(online, NewValue, Srest).
ssa( picking_up, NewValue, [_UnknownAction|Srest] ) :- !, has_val(picking_up, NewValue, Srest).
ssa( placing_order, NewValue, [_UnknownAction|Srest] ) :- !, has_val(placing_order, NewValue, Srest).
ssa( start, NewValue, [_UnknownAction|Srest] ) :- !, has_val(start, NewValue, Srest).
ssa( taking_order, NewValue, [_UnknownAction|Srest] ) :- !, has_val(taking_order, NewValue, Srest).
ssa( task, NewValue, [_UnknownAction|Srest] ) :- !, has_val(task, NewValue, Srest).
ssa( useAbstraction, NewValue, [_UnknownAction|Srest] ) :- !, has_val(useAbstraction, NewValue, Srest).
ssa( bel(_13404), NewValue, [_UnknownAction|Srest] ) :- !, has_val(bel(_13404), NewValue, Srest).
ssa( ltp(_13408), NewValue, [_UnknownAction|Srest] ) :- !, has_val(ltp(_13408), NewValue, Srest).
ssa( pproj(_13392, _13393), NewValue, [_UnknownAction|Srest] ) :- !, has_val(pproj(_13392, _13393), NewValue, Srest).
ssa( lookahead(_13397, _13398, _13399, _13400), NewValue, [_UnknownAction|Srest] ) :- !, has_val(lookahead(_13397, _13398, _13399, _13400), NewValue, Srest).
ssa( pll(_13385, _13386, _13387, _13388), NewValue, [_UnknownAction|Srest] ) :- !, has_val(pll(_13385, _13386, _13387, _13388), NewValue, Srest).
ssa( coffee_prepared, NewValue, [_UnknownAction|Srest] ) :- !, has_val(coffee_prepared, NewValue, Srest).
ssa( going_to, NewValue, [_UnknownAction|Srest] ) :- !, has_val(going_to, NewValue, Srest).
ssa( pos, NewValue, [_UnknownAction|Srest] ) :- !, has_val(pos, NewValue, Srest).
ssa( requests, NewValue, [_UnknownAction|Srest] ) :- !, has_val(requests, NewValue, Srest).
prolog_poss( start_drop(PreVar190, PreVar197) ).
prolog_poss( start_drop(PreVar190, PreVar197), S ) :- ((has_val(holding, PreVar191, S), PreVar191 = PreVar190), has_val(pos, PreVar198, S), PreVar198 = PreVar197), not (has_val(dropping, PreVar205, S), PreVar205=true).
prolog_poss( start_goto(PreVar210) ).
prolog_poss( start_goto(PreVar210), S ) :- (room(PreVar210), has_val(pos, PreVar213, S), not PreVar213 = PreVar210), not (has_val(going_to, PreVar220, S), PreVar220=true).
prolog_poss( start_interact ).
prolog_poss( start_interact, S ) :- not (has_val(interacting, PreVar227, S), PreVar227=true).
prolog_poss( start_locate(PreVar240, PreVar233) ).
prolog_poss( start_locate(PreVar240, PreVar233), S ) :- ((has_val(pos, PreVar234, S), PreVar234 = PreVar233), in(PreVar233, PreVar240), person(PreVar240)), not (has_val(locating, PreVar244, S), PreVar244=true).
prolog_poss( start_move_arm ).
prolog_poss( start_move_arm, S ) :- not (has_val(moving_arm, PreVar251, S), PreVar251=true).
prolog_poss( start_pickup(X) ).
prolog_poss( start_pickup(X), S ) :- (has_val(holding, PreVar258, S), PreVar258=nil), not (has_val(picking_up, PreVar265, S), PreVar265=true).
prolog_poss( start_place_order(P) ).
prolog_poss( start_place_order(P), S ) :- (not (has_val(task, PreVar272, S), PreVar272=nil), (has_val(pos, PreVar279, S), PreVar279=kitchen), has_val(located, PreVar286, S), PreVar286=chef), not (has_val(placing_order, PreVar293, S), PreVar293=true).
prolog_poss( start_take_order(PreVar314) ).
prolog_poss( start_take_order(PreVar314), S ) :- ((has_val(requests, PreVar300, S), PreVar300=[PreVar305|_T]), (has_val(pos, PreVar308, S), PreVar308 = PreVar305), in(PreVar305, PreVar314), has_val(located, PreVar317, S), PreVar317 = PreVar314), not (has_val(taking_order, PreVar324, S), PreVar324=true).
prolog_poss( stop_goto(R) ).
prolog_poss( stop_goto(R), S ) :- true.
events_list([]).
