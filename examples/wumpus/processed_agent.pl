/*******************************
* file generated from ReadyLog *
*******************************/

:- pragma(nodebug).
prolog_function( f_is_pos(PreVar4, PreVar12) ).
prolog_function( f_is_pos(PreVar4, PreVar12), PreVar20, S ) :- world(PreVar0, PreVar1, PreVar2, PreVar3), PreVar4 >= PreVar0, PreVar4 =< PreVar2, PreVar12 >= PreVar1, PreVar12 =< PreVar3 -> PreVar20=true ; PreVar20=false.
prolog_function( f_is_wall(PreVar28, PreVar29, PreVar30, PreVar31) ).
prolog_function( f_is_wall(PreVar28, PreVar29, PreVar30, PreVar31), PreVar36, S ) :- (wall(PreVar28, PreVar29, PreVar30, PreVar31) ; wall(PreVar30, PreVar31, PreVar28, PreVar29)) -> PreVar36=true ; PreVar36=false.
prolog_function( f_is_pit(PreVar44, PreVar45) ).
prolog_function( f_is_pit(PreVar44, PreVar45), PreVar46, S ) :- is_pit(PreVar44, PreVar45) -> PreVar46=true ; PreVar46=false.
prolog_function( f_is_wumpus(PreVar54, PreVar55) ).
prolog_function( f_is_wumpus(PreVar54, PreVar55), PreVar56, S ) :- is_wumpus(PreVar54, PreVar55) -> PreVar56=true ; PreVar56=false.
prolog_function( f_is_stench(PreVar64, PreVar65) ).
prolog_function( f_is_stench(PreVar64, PreVar65), PreVar66, S ) :- is_stench(PreVar64, PreVar65) -> PreVar66=true ; PreVar66=false.
prolog_function( f_is_direction(PreVar74) ).
prolog_function( f_is_direction(PreVar74), PreVar75, S ) :- is_direction(PreVar74) -> PreVar75=true ; PreVar75=false.
prolog_function( reward ).
prolog_function( reward, PreVar92, S ) :- has_val(epf_agent_pos, PreVar85, S), PreVar85=[3, 3] -> PreVar92=100 ; PreVar92 = -1.
prolog_function( reward(PreVar107, PreVar108) ).
prolog_function( reward(PreVar107, PreVar108), PreVar109, S ) :- has_val(epf_agent_pos, PreVar102, S), PreVar102=[PreVar107, PreVar108] -> PreVar109=100 ; PreVar109 = -1.
prolog_function( reward_heuristic(PreVar124, PreVar125) ).
prolog_function( reward_heuristic(PreVar124, PreVar125), PreVar126, S ) :- has_val(epf_agent_pos, PreVar119, S), PreVar119=[PreVar124, PreVar125] -> PreVar126=100 ; (has_val(epf_agent_pos, PreVar132, S), PreVar132=[PreVar137, PreVar138]), (PreVar142 is PreVar124 - PreVar137, PreVar139 = PreVar142), (PreVar139>0 -> PreVar152 is -(PreVar139), PreVar149 = PreVar152 ; PreVar149 = PreVar139), (PreVar161 is PreVar125 - PreVar138, PreVar158 = PreVar161), (PreVar158>0 -> PreVar171 is -(PreVar158), PreVar168 = PreVar171 ; PreVar168 = PreVar158), PreVar180 is PreVar149 + PreVar168, PreVar126 = PreVar180.
prolog_function( reward_search_gold ).
prolog_function( reward_search_gold, PreVar249, S ) :- (has_val(epf_wumpus_alive, PreVar185, S), PreVar185=true -> PreVar190=0 ; PreVar190=50), (has_val(epf_sense_gold, PreVar200, S), PreVar200=true -> PreVar205=100 ; PreVar205=0), (has_val(epf_agent_arrow, PreVar215, S), PreVar215=true -> PreVar220=20 ; PreVar220 = -20), ((((((has_val(epf_distance_to_closest_safe_cell, PreVar236, S), PreVar234 is PreVar236 * (-1)), has_val(epf_num_visited, PreVar241, S), PreVar235 is PreVar241 * (20)), PreVar232 is PreVar234 + PreVar235), has_val(epf_num_facedShades, PreVar233, S)), PreVar231 is PreVar232 + PreVar233), PreVar228 = PreVar231), ((PreVar255 is PreVar190 + PreVar205, PreVar253 is PreVar255 + PreVar220), PreVar252 is PreVar253 + PreVar228), PreVar249 = PreVar252.
prolog_function( f_gold_pos_by_id(PreVar261) ).
prolog_function( f_gold_pos_by_id(PreVar261), PreVar305, S ) :- (PreVar260 is PreVar261 - (5), PreVar259 is PreVar260), (PreVar264 is PreVar261 - (4), PreVar263 is PreVar264), (PreVar268 is PreVar259 mod 3, PreVar267 is PreVar268), (PreVar272 is PreVar263 div 3, PreVar271 is PreVar272), (PreVar267=2 -> PreVar279 = -1 ; PreVar279 = PreVar267), PreVar287 = PreVar271, pos=[PreVar295, PreVar296], (PreVar298 is PreVar295 + PreVar279, PreVar297 is PreVar298), (PreVar302 is PreVar296 + PreVar287, PreVar301 is PreVar302), PreVar305=[PreVar297, PreVar301].
prolog_function( adl(PreVar311, PreVar312) ).
prolog_function( adl(PreVar311, PreVar312), PreVar313, S ) :- append(PreVar311, PreVar312, PreVar313).
stoch_proc(forward_det).
stoch_proc(turn_right_det).
stoch_proc(turn_left_det).
stoch_proc(shoot_det).
stoch_proc_outcomes(forward_det, S, Outcomes, SenseEffect) :- Outcomes=[([go_forward], 1.0, true)], SenseEffect = [], !.
stoch_proc_outcomes(turn_right_det, S, Outcomes, SenseEffect) :- Outcomes=[([turn_right], 1.0, true)], SenseEffect = [], !.
stoch_proc_outcomes(turn_left_det, S, Outcomes, SenseEffect) :- Outcomes=[([turn_left], 1.0, true)], SenseEffect = [], !.
stoch_proc_outcomes(shoot_det, S, Outcomes, SenseEffect) :- Outcomes=[([shoot], 1.0, true)], SenseEffect = [], !.
stoch_proc_poss(forward_det, S) :- true, !.
stoch_proc_poss(turn_right_det, S) :- true, !.
stoch_proc_poss(turn_left_det, S) :- true, !.
stoch_proc_poss(shoot_det, S) :- true, !.
stoch_proc_costs(forward_det, 1, S) :- true, !.
stoch_proc_costs(turn_right_det, 1, S) :- true, !.
stoch_proc_costs(turn_left_det, 1, S) :- true, !.
stoch_proc_costs(shoot_det, 1, S) :- true, !.
ssa( online, false, [clipOnline|Srest] ) :- !, true.
ssa( epf_agent_pos, PreVar341, [go_forward|Srest] ) :- !, (has_val(epf_agent_pos, PreVar316, Srest), PreVar316=[PreVar321, PreVar322]), is_pos(PreVar321, PreVar322), (has_val(epf_agent_direction, PreVar327, Srest), PreVar327 = PreVar326), is_direction(PreVar326), PreVar326=east, (PreVar338 is PreVar321 + (1), PreVar337 is PreVar338), PreVar341=[PreVar337, PreVar322] ; (has_val(epf_agent_pos, PreVar349, Srest), PreVar349=[PreVar354, PreVar355]), is_pos(PreVar354, PreVar355), (has_val(epf_agent_direction, PreVar360, Srest), PreVar360 = PreVar359), is_direction(PreVar359), PreVar359=south, (PreVar371 is PreVar355 - (1), PreVar370 is PreVar371), PreVar341=[PreVar354, PreVar370] ; (has_val(epf_agent_pos, PreVar382, Srest), PreVar382=[PreVar387, PreVar388]), is_pos(PreVar387, PreVar388), (has_val(epf_agent_direction, PreVar393, Srest), PreVar393 = PreVar392), is_direction(PreVar392), PreVar392=west, (PreVar404 is PreVar387 - (1), PreVar403 is PreVar404), PreVar341=[PreVar403, PreVar388] ; (has_val(epf_agent_pos, PreVar415, Srest), PreVar415=[PreVar420, PreVar421]), is_pos(PreVar420, PreVar421), (has_val(epf_agent_direction, PreVar426, Srest), PreVar426 = PreVar425), is_direction(PreVar425), PreVar425=north, (PreVar437 is PreVar421 + (1), PreVar436 is PreVar437), PreVar341=[PreVar420, PreVar436].
ssa( epf_cells_visited, PreVar493, [go_forward|Srest] ) :- !, (has_val(epf_agent_pos, PreVar448, Srest), PreVar448=[PreVar453, PreVar454]), is_pos(PreVar453, PreVar454), (has_val(epf_agent_direction, PreVar459, Srest), PreVar459 = PreVar458), is_direction(PreVar458), (has_val(epf_cells_visited, PreVar467, Srest), PreVar467 = PreVar466), PreVar458=east, (PreVar477 is PreVar453 + (1), PreVar476 is PreVar477), PreVar480 = PreVar454, not member([PreVar476, PreVar480], PreVar466), append(PreVar466, [[PreVar476, PreVar480]], PreVar493) ; (has_val(epf_agent_pos, PreVar496, Srest), PreVar496=[PreVar501, PreVar502]), is_pos(PreVar501, PreVar502), (has_val(epf_agent_direction, PreVar507, Srest), PreVar507 = PreVar506), is_direction(PreVar506), (has_val(epf_cells_visited, PreVar515, Srest), PreVar515 = PreVar514), PreVar506=south, PreVar476 = PreVar501, (PreVar529 is PreVar502 - (1), PreVar480 is PreVar529), not member([PreVar476, PreVar480], PreVar514), append(PreVar514, [[PreVar476, PreVar480]], PreVar493) ; (has_val(epf_agent_pos, PreVar544, Srest), PreVar544=[PreVar549, PreVar550]), is_pos(PreVar549, PreVar550), (has_val(epf_agent_direction, PreVar555, Srest), PreVar555 = PreVar554), is_direction(PreVar554), (has_val(epf_cells_visited, PreVar563, Srest), PreVar563 = PreVar562), PreVar554=west, (PreVar573 is PreVar549 - (1), PreVar476 is PreVar573), PreVar480 = PreVar550, not member([PreVar476, PreVar480], PreVar562), append(PreVar562, [[PreVar476, PreVar480]], PreVar493) ; (has_val(epf_agent_pos, PreVar592, Srest), PreVar592=[PreVar597, PreVar598]), is_pos(PreVar597, PreVar598), (has_val(epf_agent_direction, PreVar603, Srest), PreVar603 = PreVar602), is_direction(PreVar602), (has_val(epf_cells_visited, PreVar611, Srest), PreVar611 = PreVar610), PreVar602=north, PreVar476 = PreVar597, (PreVar625 is PreVar598 + (1), PreVar480 is PreVar625), not member([PreVar476, PreVar480], PreVar610), append(PreVar610, [[PreVar476, PreVar480]], PreVar493).
ssa( epf_num_facedShades, PreVar698, [go_forward|Srest] ) :- !, (has_val(epf_agent_direction, PreVar640, Srest), PreVar640 = PreVar639), is_direction(PreVar639), PreVar639=east, (has_val(epf_agent_pos, PreVar652, Srest), PreVar652=[PreVar657, PreVar658]), is_pos(PreVar657, PreVar658), (has_val(epf_cells_shaded, PreVar663, Srest), PreVar663 = PreVar662), world(PreVar668, PreVar669, PreVar670, PreVar671), (PreVar678 is PreVar657 + (1), findall([PreVar673, PreVar658], (PreVar677 is PreVar678, between(PreVar677, PreVar670, 1, PreVar673), is_pos(PreVar673, PreVar658), member([PreVar673, PreVar658], PreVar662)), PreVar696)), length(PreVar696, PreVar698) ; (has_val(epf_agent_direction, PreVar701, Srest), PreVar701 = PreVar700), is_direction(PreVar700), PreVar700=south, (has_val(epf_agent_pos, PreVar713, Srest), PreVar713=[PreVar718, PreVar719]), is_pos(PreVar718, PreVar719), (has_val(epf_cells_shaded, PreVar724, Srest), PreVar724 = PreVar723), world(PreVar668, PreVar669, PreVar670, PreVar671), (PreVar739 is PreVar719 - (1), findall([PreVar718, PreVar735], (PreVar738 is PreVar739, between(PreVar669, PreVar738, 1, PreVar735), is_pos(PreVar718, PreVar735), member([PreVar718, PreVar735], PreVar723)), PreVar757)), length(PreVar757, PreVar698) ; (has_val(epf_agent_direction, PreVar762, Srest), PreVar762 = PreVar761), is_direction(PreVar761), PreVar761=west, (has_val(epf_agent_pos, PreVar774, Srest), PreVar774=[PreVar779, PreVar780]), is_pos(PreVar779, PreVar780), (has_val(epf_cells_shaded, PreVar785, Srest), PreVar785 = PreVar784), world(PreVar668, PreVar669, PreVar670, PreVar671), (PreVar800 is PreVar779 - (1), findall([PreVar673, PreVar780], (PreVar799 is PreVar800, between(PreVar668, PreVar799, 1, PreVar673), is_pos(PreVar673, PreVar780), member([PreVar673, PreVar780], PreVar784)), PreVar818)), length(PreVar818, PreVar698) ; (has_val(epf_agent_direction, PreVar823, Srest), PreVar823 = PreVar822), is_direction(PreVar822), PreVar822=north, (has_val(epf_agent_pos, PreVar835, Srest), PreVar835=[PreVar840, PreVar841]), is_pos(PreVar840, PreVar841), (has_val(epf_cells_shaded, PreVar846, Srest), PreVar846 = PreVar845), world(PreVar668, PreVar669, PreVar670, PreVar671), (PreVar861 is PreVar841 + (1), findall([PreVar840, PreVar735], (PreVar860 is PreVar861, between(PreVar860, PreVar671, 1, PreVar735), is_pos(PreVar840, PreVar735), member([PreVar840, PreVar735], PreVar845)), PreVar879)), length(PreVar879, PreVar698).
ssa( epf_carry_gold, PreVar882, [pickup_gold|Srest] ) :- !, PreVar882=true.
ssa( visited(_21081, _21082), PreVar886, [reset_visited|Srest] ) :- !, PreVar886=false.
ssa( online, true, [setOnline|Srest] ) :- !, true.
ssa( epf_agent_arrow, PreVar890, [shoot|Srest] ) :- !, PreVar890=false.
ssa( epf_wumpus_alive, PreVar934, [shoot|Srest] ) :- !, (has_val(epf_agent_direction, PreVar896, Srest), PreVar896 = PreVar895), is_direction(PreVar895), PreVar895=east, (has_val(epf_agent_pos, PreVar908, Srest), PreVar908=[PreVar913, PreVar914]), is_pos(PreVar913, PreVar914), (has_val(epf_wumpus_pos, PreVar919, Srest), PreVar919=[PreVar924, PreVar925]), PreVar925 = PreVar914, PreVar924 > PreVar913, PreVar934=false ; (has_val(epf_agent_direction, PreVar940, Srest), PreVar940 = PreVar939), is_direction(PreVar939), PreVar939=south, (has_val(epf_agent_pos, PreVar952, Srest), PreVar952=[PreVar957, PreVar958]), is_pos(PreVar957, PreVar958), (has_val(epf_wumpus_pos, PreVar963, Srest), PreVar963=[PreVar968, PreVar969]), PreVar968 = PreVar957, PreVar969 < PreVar958, PreVar934=false ; (has_val(epf_agent_direction, PreVar984, Srest), PreVar984 = PreVar983), is_direction(PreVar983), PreVar983=west, (has_val(epf_agent_pos, PreVar996, Srest), PreVar996=[PreVar1001, PreVar1002]), is_pos(PreVar1001, PreVar1002), (has_val(epf_wumpus_pos, PreVar1007, Srest), PreVar1007=[PreVar1012, PreVar1013]), PreVar1013 = PreVar1002, PreVar1012 < PreVar1001, PreVar934=false ; (has_val(epf_agent_direction, PreVar1028, Srest), PreVar1028 = PreVar1027), is_direction(PreVar1027), PreVar1027=north, (has_val(epf_agent_pos, PreVar1040, Srest), PreVar1040=[PreVar1045, PreVar1046]), is_pos(PreVar1045, PreVar1046), (has_val(epf_wumpus_pos, PreVar1051, Srest), PreVar1051=[PreVar1056, PreVar1057]), PreVar1056 = PreVar1045, PreVar1057 > PreVar1046, PreVar934=false ; PreVar934=true.
ssa( epf_agent_direction, PreVar1086, [turn_left|Srest] ) :- !, (has_val(epf_agent_direction, PreVar1076, Srest), PreVar1076 = PreVar1075), is_direction(PreVar1075), PreVar1075=east, PreVar1086=north ; (has_val(epf_agent_direction, PreVar1092, Srest), PreVar1092 = PreVar1091), is_direction(PreVar1091), PreVar1091=south, PreVar1086=east ; (has_val(epf_agent_direction, PreVar1108, Srest), PreVar1108 = PreVar1107), is_direction(PreVar1107), PreVar1107=west, PreVar1086=south ; (has_val(epf_agent_direction, PreVar1124, Srest), PreVar1124 = PreVar1123), is_direction(PreVar1123), PreVar1123=north, PreVar1086=west.
ssa( epf_num_facedShades, PreVar1198, [turn_left|Srest] ) :- !, (has_val(epf_agent_direction, PreVar1140, Srest), PreVar1140 = PreVar1139), is_direction(PreVar1139), PreVar1139=east, (has_val(epf_agent_pos, PreVar1152, Srest), PreVar1152=[PreVar1157, PreVar1158]), is_pos(PreVar1157, PreVar1158), (has_val(epf_cells_shaded, PreVar1163, Srest), PreVar1163 = PreVar1162), world(PreVar1168, PreVar1169, PreVar1170, PreVar1171), (PreVar1178 is PreVar1158 + (1), findall([PreVar1157, PreVar1174], (PreVar1177 is PreVar1178, between(PreVar1177, PreVar1171, 1, PreVar1174), is_pos(PreVar1157, PreVar1174), member([PreVar1157, PreVar1174], PreVar1162)), PreVar1196)), length(PreVar1196, PreVar1198) ; (has_val(epf_agent_direction, PreVar1201, Srest), PreVar1201 = PreVar1200), is_direction(PreVar1200), PreVar1200=south, (has_val(epf_agent_pos, PreVar1213, Srest), PreVar1213=[PreVar1218, PreVar1219]), is_pos(PreVar1218, PreVar1219), (has_val(epf_cells_shaded, PreVar1224, Srest), PreVar1224 = PreVar1223), world(PreVar1168, PreVar1169, PreVar1170, PreVar1171), (PreVar1239 is PreVar1218 + (1), findall([PreVar1234, PreVar1219], (PreVar1238 is PreVar1239, between(PreVar1238, PreVar1170, 1, PreVar1234), is_pos(PreVar1234, PreVar1219), member([PreVar1234, PreVar1219], PreVar1223)), PreVar1257)), length(PreVar1257, PreVar1198) ; (has_val(epf_agent_direction, PreVar1262, Srest), PreVar1262 = PreVar1261), is_direction(PreVar1261), PreVar1261=west, (has_val(epf_agent_pos, PreVar1274, Srest), PreVar1274=[PreVar1279, PreVar1280]), is_pos(PreVar1279, PreVar1280), (has_val(epf_cells_shaded, PreVar1285, Srest), PreVar1285 = PreVar1284), world(PreVar1168, PreVar1169, PreVar1170, PreVar1171), (PreVar1300 is PreVar1280 - (1), findall([PreVar1279, PreVar1174], (PreVar1299 is PreVar1300, between(PreVar1169, PreVar1299, 1, PreVar1174), is_pos(PreVar1279, PreVar1174), member([PreVar1279, PreVar1174], PreVar1284)), PreVar1318)), length(PreVar1318, PreVar1198) ; (has_val(epf_agent_direction, PreVar1323, Srest), PreVar1323 = PreVar1322), is_direction(PreVar1322), PreVar1322=north, (has_val(epf_agent_pos, PreVar1335, Srest), PreVar1335=[PreVar1340, PreVar1341]), is_pos(PreVar1340, PreVar1341), (has_val(epf_cells_shaded, PreVar1346, Srest), PreVar1346 = PreVar1345), world(PreVar1168, PreVar1169, PreVar1170, PreVar1171), (PreVar1361 is PreVar1340 - (1), findall([PreVar1234, PreVar1341], (PreVar1360 is PreVar1361, between(PreVar1168, PreVar1360, 1, PreVar1234), is_pos(PreVar1234, PreVar1341), member([PreVar1234, PreVar1341], PreVar1345)), PreVar1379)), length(PreVar1379, PreVar1198).
ssa( epf_agent_direction, PreVar1394, [turn_right|Srest] ) :- !, (has_val(epf_agent_direction, PreVar1384, Srest), PreVar1384 = PreVar1383), is_direction(PreVar1383), PreVar1383=east, PreVar1394=south ; (has_val(epf_agent_direction, PreVar1400, Srest), PreVar1400 = PreVar1399), is_direction(PreVar1399), PreVar1399=south, PreVar1394=west ; (has_val(epf_agent_direction, PreVar1416, Srest), PreVar1416 = PreVar1415), is_direction(PreVar1415), PreVar1415=west, PreVar1394=north ; (has_val(epf_agent_direction, PreVar1432, Srest), PreVar1432 = PreVar1431), is_direction(PreVar1431), PreVar1431=north, PreVar1394=east.
ssa( epf_num_facedShades, PreVar1506, [turn_right|Srest] ) :- !, (has_val(epf_agent_direction, PreVar1448, Srest), PreVar1448 = PreVar1447), is_direction(PreVar1447), PreVar1447=east, (has_val(epf_agent_pos, PreVar1460, Srest), PreVar1460=[PreVar1465, PreVar1466]), is_pos(PreVar1465, PreVar1466), (has_val(epf_cells_shaded, PreVar1471, Srest), PreVar1471 = PreVar1470), world(PreVar1476, PreVar1477, PreVar1478, PreVar1479), (PreVar1486 is PreVar1466 - (1), findall([PreVar1465, PreVar1482], (PreVar1485 is PreVar1486, between(PreVar1477, PreVar1485, 1, PreVar1482), is_pos(PreVar1465, PreVar1482), member([PreVar1465, PreVar1482], PreVar1470)), PreVar1504)), length(PreVar1504, PreVar1506) ; (has_val(epf_agent_direction, PreVar1509, Srest), PreVar1509 = PreVar1508), is_direction(PreVar1508), PreVar1508=south, (has_val(epf_agent_pos, PreVar1521, Srest), PreVar1521=[PreVar1526, PreVar1527]), is_pos(PreVar1526, PreVar1527), (has_val(epf_cells_shaded, PreVar1532, Srest), PreVar1532 = PreVar1531), world(PreVar1476, PreVar1477, PreVar1478, PreVar1479), (PreVar1547 is PreVar1526 - (1), findall([PreVar1542, PreVar1527], (PreVar1546 is PreVar1547, between(PreVar1476, PreVar1546, 1, PreVar1542), is_pos(PreVar1542, PreVar1527), member([PreVar1542, PreVar1527], PreVar1531)), PreVar1565)), length(PreVar1565, PreVar1506) ; (has_val(epf_agent_direction, PreVar1570, Srest), PreVar1570 = PreVar1569), is_direction(PreVar1569), PreVar1569=west, (has_val(epf_agent_pos, PreVar1582, Srest), PreVar1582=[PreVar1587, PreVar1588]), is_pos(PreVar1587, PreVar1588), (has_val(epf_cells_shaded, PreVar1593, Srest), PreVar1593 = PreVar1592), world(PreVar1476, PreVar1477, PreVar1478, PreVar1479), (PreVar1608 is PreVar1588 + (1), findall([PreVar1587, PreVar1482], (PreVar1607 is PreVar1608, between(PreVar1607, PreVar1479, 1, PreVar1482), is_pos(PreVar1587, PreVar1482), member([PreVar1587, PreVar1482], PreVar1592)), PreVar1626)), length(PreVar1626, PreVar1506) ; (has_val(epf_agent_direction, PreVar1631, Srest), PreVar1631 = PreVar1630), is_direction(PreVar1630), PreVar1630=north, (has_val(epf_agent_pos, PreVar1643, Srest), PreVar1643=[PreVar1648, PreVar1649]), is_pos(PreVar1648, PreVar1649), (has_val(epf_cells_shaded, PreVar1654, Srest), PreVar1654 = PreVar1653), world(PreVar1476, PreVar1477, PreVar1478, PreVar1479), (PreVar1669 is PreVar1648 + (1), findall([PreVar1542, PreVar1649], (PreVar1668 is PreVar1669, between(PreVar1668, PreVar1478, 1, PreVar1542), is_pos(PreVar1542, PreVar1649), member([PreVar1542, PreVar1649], PreVar1653)), PreVar1687)), length(PreVar1687, PreVar1506).
ssa( start, NewValue, [setTime(NewValue)|Srest] ) :- !, true.
ssa( start, NewValue, [ccUpdate(_18784, NewValue)|Srest] ) :- !, true.
ssa( action, NewValue, [reply(action, NewValue)|Srest] ) :- !, true.
ssa( action, NewValue, [send(action, NewValue)|Srest] ) :- !, true.
ssa( Fluent, NewValue, [set(Fluent, NewValue)|Srest] ) :- !, true.
ssa( pos, PreVar1690, [teleport(PreVar1694, PreVar1695)|Srest] ) :- !, PreVar1690=[PreVar1694, PreVar1695].

/* 	___ dummy SSAs ___ */
ssa( action, NewValue, [set(action, NewValue)|_] ) :- !.
ssa( eval_exog_functions, NewValue, [set(eval_exog_functions, NewValue)|_] ) :- !.
ssa( eval_registers, NewValue, [set(eval_registers, NewValue)|_] ) :- !.
ssa( online, NewValue, [set(online, NewValue)|_] ) :- !.
ssa( start, NewValue, [set(start, NewValue)|_] ) :- !.
ssa( useAbstraction, NewValue, [set(useAbstraction, NewValue)|_] ) :- !.
ssa( bel(_92727), NewValue, [set(bel(_92727), NewValue)|_] ) :- !.
ssa( ltp(_92731), NewValue, [set(ltp(_92731), NewValue)|_] ) :- !.
ssa( pproj(_92715, _92716), NewValue, [set(pproj(_92715, _92716), NewValue)|_] ) :- !.
ssa( lookahead(_92720, _92721, _92722, _92723), NewValue, [set(lookahead(_92720, _92721, _92722, _92723), NewValue)|_] ) :- !.
ssa( pll(_92708, _92709, _92710, _92711), NewValue, [set(pll(_92708, _92709, _92710, _92711), NewValue)|_] ) :- !.
ssa( epf_agent_arrow, NewValue, [set(epf_agent_arrow, NewValue)|_] ) :- !.
ssa( epf_agent_direction, NewValue, [set(epf_agent_direction, NewValue)|_] ) :- !.
ssa( epf_agent_pos, NewValue, [set(epf_agent_pos, NewValue)|_] ) :- !.
ssa( epf_carry_gold, NewValue, [set(epf_carry_gold, NewValue)|_] ) :- !.
ssa( epf_cells_know_no_pit, NewValue, [set(epf_cells_know_no_pit, NewValue)|_] ) :- !.
ssa( epf_cells_know_no_wumpus, NewValue, [set(epf_cells_know_no_wumpus, NewValue)|_] ) :- !.
ssa( epf_cells_shaded, NewValue, [set(epf_cells_shaded, NewValue)|_] ) :- !.
ssa( epf_cells_visited, NewValue, [set(epf_cells_visited, NewValue)|_] ) :- !.
ssa( epf_distance_to_closest_safe_cell, NewValue, [set(epf_distance_to_closest_safe_cell, NewValue)|_] ) :- !.
ssa( epf_gold_pos, NewValue, [set(epf_gold_pos, NewValue)|_] ) :- !.
ssa( epf_gold_reachable, NewValue, [set(epf_gold_reachable, NewValue)|_] ) :- !.
ssa( epf_num_facedShades, NewValue, [set(epf_num_facedShades, NewValue)|_] ) :- !.
ssa( epf_num_visited, NewValue, [set(epf_num_visited, NewValue)|_] ) :- !.
ssa( epf_sense_breeze, NewValue, [set(epf_sense_breeze, NewValue)|_] ) :- !.
ssa( epf_sense_gold, NewValue, [set(epf_sense_gold, NewValue)|_] ) :- !.
ssa( epf_sense_stench, NewValue, [set(epf_sense_stench, NewValue)|_] ) :- !.
ssa( epf_wumpus_alive, NewValue, [set(epf_wumpus_alive, NewValue)|_] ) :- !.
ssa( epf_wumpus_pos, NewValue, [set(epf_wumpus_pos, NewValue)|_] ) :- !.
ssa( epf_agent_arrow, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_agent_arrow, NewValue, Srest).
ssa( epf_agent_direction, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_agent_direction, NewValue, Srest).
ssa( epf_agent_pos, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_agent_pos, NewValue, Srest).
ssa( epf_carry_gold, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_carry_gold, NewValue, Srest).
ssa( epf_cells_know_no_pit, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_cells_know_no_pit, NewValue, Srest).
ssa( epf_cells_know_no_wumpus, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_cells_know_no_wumpus, NewValue, Srest).
ssa( epf_cells_shaded, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_cells_shaded, NewValue, Srest).
ssa( epf_cells_visited, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_cells_visited, NewValue, Srest).
ssa( epf_distance_to_closest_safe_cell, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_distance_to_closest_safe_cell, NewValue, Srest).
ssa( epf_gold_pos, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_gold_pos, NewValue, Srest).
ssa( epf_gold_reachable, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_gold_reachable, NewValue, Srest).
ssa( epf_num_facedShades, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_num_facedShades, NewValue, Srest).
ssa( epf_num_visited, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_num_visited, NewValue, Srest).
ssa( epf_sense_breeze, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_sense_breeze, NewValue, Srest).
ssa( epf_sense_gold, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_sense_gold, NewValue, Srest).
ssa( epf_sense_stench, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_sense_stench, NewValue, Srest).
ssa( epf_wumpus_alive, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_wumpus_alive, NewValue, Srest).
ssa( epf_wumpus_pos, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_wumpus_pos, NewValue, Srest).
ssa( action, NewValue, [_UnknownAction|Srest] ) :- !, has_val(action, NewValue, Srest).
ssa( eval_exog_functions, NewValue, [_UnknownAction|Srest] ) :- !, has_val(eval_exog_functions, NewValue, Srest).
ssa( eval_registers, NewValue, [_UnknownAction|Srest] ) :- !, has_val(eval_registers, NewValue, Srest).
ssa( online, NewValue, [_UnknownAction|Srest] ) :- !, has_val(online, NewValue, Srest).
ssa( start, NewValue, [_UnknownAction|Srest] ) :- !, has_val(start, NewValue, Srest).
ssa( useAbstraction, NewValue, [_UnknownAction|Srest] ) :- !, has_val(useAbstraction, NewValue, Srest).
ssa( bel(_92727), NewValue, [_UnknownAction|Srest] ) :- !, has_val(bel(_92727), NewValue, Srest).
ssa( ltp(_92731), NewValue, [_UnknownAction|Srest] ) :- !, has_val(ltp(_92731), NewValue, Srest).
ssa( pproj(_92715, _92716), NewValue, [_UnknownAction|Srest] ) :- !, has_val(pproj(_92715, _92716), NewValue, Srest).
ssa( lookahead(_92720, _92721, _92722, _92723), NewValue, [_UnknownAction|Srest] ) :- !, has_val(lookahead(_92720, _92721, _92722, _92723), NewValue, Srest).
ssa( pll(_92708, _92709, _92710, _92711), NewValue, [_UnknownAction|Srest] ) :- !, has_val(pll(_92708, _92709, _92710, _92711), NewValue, Srest).
ssa( epf_agent_arrow, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_agent_arrow, NewValue, Srest).
ssa( epf_agent_direction, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_agent_direction, NewValue, Srest).
ssa( epf_agent_pos, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_agent_pos, NewValue, Srest).
ssa( epf_carry_gold, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_carry_gold, NewValue, Srest).
ssa( epf_cells_know_no_pit, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_cells_know_no_pit, NewValue, Srest).
ssa( epf_cells_know_no_wumpus, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_cells_know_no_wumpus, NewValue, Srest).
ssa( epf_cells_shaded, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_cells_shaded, NewValue, Srest).
ssa( epf_cells_visited, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_cells_visited, NewValue, Srest).
ssa( epf_distance_to_closest_safe_cell, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_distance_to_closest_safe_cell, NewValue, Srest).
ssa( epf_gold_pos, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_gold_pos, NewValue, Srest).
ssa( epf_gold_reachable, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_gold_reachable, NewValue, Srest).
ssa( epf_num_facedShades, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_num_facedShades, NewValue, Srest).
ssa( epf_num_visited, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_num_visited, NewValue, Srest).
ssa( epf_sense_breeze, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_sense_breeze, NewValue, Srest).
ssa( epf_sense_gold, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_sense_gold, NewValue, Srest).
ssa( epf_sense_stench, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_sense_stench, NewValue, Srest).
ssa( epf_wumpus_alive, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_wumpus_alive, NewValue, Srest).
ssa( epf_wumpus_pos, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_wumpus_pos, NewValue, Srest).
prolog_poss( send(_R, _94052) ).
prolog_poss( send(_R, _94052), S ) :- true.
prolog_poss( reply(_R, _94061) ).
prolog_poss( reply(_R, _94061), S ) :- true.
prolog_poss( setOnline ).
prolog_poss( setOnline, S ) :- true.
prolog_poss( clipOnline ).
prolog_poss( clipOnline, S ) :- true.
prolog_poss( setTime(_94079) ).
prolog_poss( setTime(_94079), S ) :- true.
prolog_poss( clipAbstraction ).
prolog_poss( clipAbstraction, S ) :- true.
prolog_poss( setAbstraction ).
prolog_poss( setAbstraction, S ) :- true.
prolog_poss( set(_Fluent, _Value) ).
prolog_poss( set(_Fluent, _Value), S ) :- true.
prolog_poss( exogf_Update ).
prolog_poss( exogf_Update, S ) :- true.
prolog_poss( go_forward ).
prolog_poss( go_forward, S ) :- (has_val(epf_agent_pos, PreVar1698, S), PreVar1698=[PreVar1703, PreVar1704]), is_pos(PreVar1703, PreVar1704), (has_val(epf_wumpus_alive, PreVar1709, S), PreVar1709 = PreVar1708), (has_val(epf_cells_know_no_pit, PreVar1716, S), PreVar1716 = PreVar1715), (has_val(epf_cells_know_no_wumpus, PreVar1723, S), PreVar1723 = PreVar1722), (has_val(epf_agent_direction, PreVar1730, S), PreVar1730 = PreVar1729), PreVar1729=east, (PreVar1740 is PreVar1703 + (1), PreVar1739 is PreVar1740), is_pos(PreVar1739, PreVar1704), not is_wall(PreVar1703, PreVar1704, PreVar1739, PreVar1704), member([PreVar1739, PreVar1704], PreVar1715), (member([PreVar1739, PreVar1704], PreVar1722) ; PreVar1708=false) ; (has_val(epf_agent_pos, PreVar1763, S), PreVar1763=[PreVar1768, PreVar1769]), is_pos(PreVar1768, PreVar1769), (has_val(epf_wumpus_alive, PreVar1774, S), PreVar1774 = PreVar1773), (has_val(epf_cells_know_no_pit, PreVar1781, S), PreVar1781 = PreVar1780), (has_val(epf_cells_know_no_wumpus, PreVar1788, S), PreVar1788 = PreVar1787), (has_val(epf_agent_direction, PreVar1795, S), PreVar1795 = PreVar1794), PreVar1794=south, (PreVar1805 is PreVar1769 - (1), PreVar1804 is PreVar1805), is_pos(PreVar1768, PreVar1804), not is_wall(PreVar1768, PreVar1769, PreVar1768, PreVar1804), member([PreVar1768, PreVar1804], PreVar1780), (member([PreVar1768, PreVar1804], PreVar1787) ; PreVar1773=false) ; (has_val(epf_agent_pos, PreVar1828, S), PreVar1828=[PreVar1833, PreVar1834]), is_pos(PreVar1833, PreVar1834), (has_val(epf_wumpus_alive, PreVar1839, S), PreVar1839 = PreVar1838), (has_val(epf_cells_know_no_pit, PreVar1846, S), PreVar1846 = PreVar1845), (has_val(epf_cells_know_no_wumpus, PreVar1853, S), PreVar1853 = PreVar1852), (has_val(epf_agent_direction, PreVar1860, S), PreVar1860 = PreVar1859), PreVar1859=west, (PreVar1870 is PreVar1833 - (1), PreVar1869 is PreVar1870), is_pos(PreVar1869, PreVar1834), not is_wall(PreVar1833, PreVar1834, PreVar1869, PreVar1834), member([PreVar1869, PreVar1834], PreVar1845), (member([PreVar1869, PreVar1834], PreVar1852) ; PreVar1838=false) ; (has_val(epf_agent_pos, PreVar1893, S), PreVar1893=[PreVar1898, PreVar1899]), is_pos(PreVar1898, PreVar1899), (has_val(epf_wumpus_alive, PreVar1904, S), PreVar1904 = PreVar1903), (has_val(epf_cells_know_no_pit, PreVar1911, S), PreVar1911 = PreVar1910), (has_val(epf_cells_know_no_wumpus, PreVar1918, S), PreVar1918 = PreVar1917), (has_val(epf_agent_direction, PreVar1925, S), PreVar1925 = PreVar1924), PreVar1924=north, (PreVar1935 is PreVar1899 + (1), PreVar1934 is PreVar1935), is_pos(PreVar1898, PreVar1934), not is_wall(PreVar1898, PreVar1899, PreVar1898, PreVar1934), member([PreVar1898, PreVar1934], PreVar1910), (member([PreVar1898, PreVar1934], PreVar1917) ; PreVar1903=false).
prolog_poss( turn_right ).
prolog_poss( turn_right, S ) :- true.
prolog_poss( turn_left ).
prolog_poss( turn_left, S ) :- true.
prolog_poss( shoot ).
prolog_poss( shoot, S ) :- has_val(epf_agent_arrow, PreVar1958, S), PreVar1958=true.
prolog_poss( rest ).
prolog_poss( rest, S ) :- (has_val(epf_agent_pos, PreVar1965, S), PreVar1965=[PreVar1970, PreVar1971]), is_pos(PreVar1970, PreVar1971).
prolog_poss( pickup_gold ).
prolog_poss( pickup_gold, S ) :- (has_val(epf_agent_pos, PreVar1976, S), PreVar1976=[PreVar1981, PreVar1982]), is_pos(PreVar1981, PreVar1982), getval(real_gold_pos, [PreVar1987, PreVar1988]), PreVar1981 =:= PreVar1987, PreVar1982 =:= PreVar1988, not has_val(epf_carry_gold, true, S).
prolog_poss( noop ).
prolog_poss( noop, S ) :- true.
prolog_poss( distribute_pits(_Seed) ).
prolog_poss( distribute_pits(_Seed), S ) :- true.
prolog_poss( distribute_breezes ).
prolog_poss( distribute_breezes, S ) :- true.
prolog_poss( distribute_stenches ).
prolog_poss( distribute_stenches, S ) :- true.
prolog_poss( place_wumpus(_Seed) ).
prolog_poss( place_wumpus(_Seed), S ) :- true.
prolog_poss( place_agent(_Seed) ).
prolog_poss( place_agent(_Seed), S ) :- true.
prolog_poss( place_gold(_Seed) ).
prolog_poss( place_gold(_Seed), S ) :- true.
prolog_poss( reset_visited ).
prolog_poss( reset_visited, S ) :- true.
prolog_poss( teleport(_X, _Y) ).
prolog_poss( teleport(_X, _Y), S ) :- true.
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
