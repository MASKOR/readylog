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
ssa( epf_visited(_21099, _21100), PreVar886, [reset_visited|Srest] ) :- !, PreVar886=false.
ssa( visited(_21081, _21082), PreVar890, [reset_visited|Srest] ) :- !, PreVar890=false.
ssa( online, true, [setOnline|Srest] ) :- !, true.
ssa( epf_agent_arrow, PreVar894, [shoot|Srest] ) :- !, PreVar894=false.
ssa( epf_wumpus_alive, PreVar938, [shoot|Srest] ) :- !, (has_val(epf_agent_direction, PreVar900, Srest), PreVar900 = PreVar899), is_direction(PreVar899), PreVar899=east, (has_val(epf_agent_pos, PreVar912, Srest), PreVar912=[PreVar917, PreVar918]), is_pos(PreVar917, PreVar918), (has_val(epf_wumpus_pos, PreVar923, Srest), PreVar923=[PreVar928, PreVar929]), PreVar929 = PreVar918, PreVar928 > PreVar917, PreVar938=false ; (has_val(epf_agent_direction, PreVar944, Srest), PreVar944 = PreVar943), is_direction(PreVar943), PreVar943=south, (has_val(epf_agent_pos, PreVar956, Srest), PreVar956=[PreVar961, PreVar962]), is_pos(PreVar961, PreVar962), (has_val(epf_wumpus_pos, PreVar967, Srest), PreVar967=[PreVar972, PreVar973]), PreVar972 = PreVar961, PreVar973 < PreVar962, PreVar938=false ; (has_val(epf_agent_direction, PreVar988, Srest), PreVar988 = PreVar987), is_direction(PreVar987), PreVar987=west, (has_val(epf_agent_pos, PreVar1000, Srest), PreVar1000=[PreVar1005, PreVar1006]), is_pos(PreVar1005, PreVar1006), (has_val(epf_wumpus_pos, PreVar1011, Srest), PreVar1011=[PreVar1016, PreVar1017]), PreVar1017 = PreVar1006, PreVar1016 < PreVar1005, PreVar938=false ; (has_val(epf_agent_direction, PreVar1032, Srest), PreVar1032 = PreVar1031), is_direction(PreVar1031), PreVar1031=north, (has_val(epf_agent_pos, PreVar1044, Srest), PreVar1044=[PreVar1049, PreVar1050]), is_pos(PreVar1049, PreVar1050), (has_val(epf_wumpus_pos, PreVar1055, Srest), PreVar1055=[PreVar1060, PreVar1061]), PreVar1060 = PreVar1049, PreVar1061 > PreVar1050, PreVar938=false ; PreVar938=true.
ssa( epf_agent_direction, PreVar1090, [turn_left|Srest] ) :- !, (has_val(epf_agent_direction, PreVar1080, Srest), PreVar1080 = PreVar1079), is_direction(PreVar1079), PreVar1079=east, PreVar1090=north ; (has_val(epf_agent_direction, PreVar1096, Srest), PreVar1096 = PreVar1095), is_direction(PreVar1095), PreVar1095=south, PreVar1090=east ; (has_val(epf_agent_direction, PreVar1112, Srest), PreVar1112 = PreVar1111), is_direction(PreVar1111), PreVar1111=west, PreVar1090=south ; (has_val(epf_agent_direction, PreVar1128, Srest), PreVar1128 = PreVar1127), is_direction(PreVar1127), PreVar1127=north, PreVar1090=west.
ssa( epf_num_facedShades, PreVar1202, [turn_left|Srest] ) :- !, (has_val(epf_agent_direction, PreVar1144, Srest), PreVar1144 = PreVar1143), is_direction(PreVar1143), PreVar1143=east, (has_val(epf_agent_pos, PreVar1156, Srest), PreVar1156=[PreVar1161, PreVar1162]), is_pos(PreVar1161, PreVar1162), (has_val(epf_cells_shaded, PreVar1167, Srest), PreVar1167 = PreVar1166), world(PreVar1172, PreVar1173, PreVar1174, PreVar1175), (PreVar1182 is PreVar1162 + (1), findall([PreVar1161, PreVar1178], (PreVar1181 is PreVar1182, between(PreVar1181, PreVar1175, 1, PreVar1178), is_pos(PreVar1161, PreVar1178), member([PreVar1161, PreVar1178], PreVar1166)), PreVar1200)), length(PreVar1200, PreVar1202) ; (has_val(epf_agent_direction, PreVar1205, Srest), PreVar1205 = PreVar1204), is_direction(PreVar1204), PreVar1204=south, (has_val(epf_agent_pos, PreVar1217, Srest), PreVar1217=[PreVar1222, PreVar1223]), is_pos(PreVar1222, PreVar1223), (has_val(epf_cells_shaded, PreVar1228, Srest), PreVar1228 = PreVar1227), world(PreVar1172, PreVar1173, PreVar1174, PreVar1175), (PreVar1243 is PreVar1222 + (1), findall([PreVar1238, PreVar1223], (PreVar1242 is PreVar1243, between(PreVar1242, PreVar1174, 1, PreVar1238), is_pos(PreVar1238, PreVar1223), member([PreVar1238, PreVar1223], PreVar1227)), PreVar1261)), length(PreVar1261, PreVar1202) ; (has_val(epf_agent_direction, PreVar1266, Srest), PreVar1266 = PreVar1265), is_direction(PreVar1265), PreVar1265=west, (has_val(epf_agent_pos, PreVar1278, Srest), PreVar1278=[PreVar1283, PreVar1284]), is_pos(PreVar1283, PreVar1284), (has_val(epf_cells_shaded, PreVar1289, Srest), PreVar1289 = PreVar1288), world(PreVar1172, PreVar1173, PreVar1174, PreVar1175), (PreVar1304 is PreVar1284 - (1), findall([PreVar1283, PreVar1178], (PreVar1303 is PreVar1304, between(PreVar1173, PreVar1303, 1, PreVar1178), is_pos(PreVar1283, PreVar1178), member([PreVar1283, PreVar1178], PreVar1288)), PreVar1322)), length(PreVar1322, PreVar1202) ; (has_val(epf_agent_direction, PreVar1327, Srest), PreVar1327 = PreVar1326), is_direction(PreVar1326), PreVar1326=north, (has_val(epf_agent_pos, PreVar1339, Srest), PreVar1339=[PreVar1344, PreVar1345]), is_pos(PreVar1344, PreVar1345), (has_val(epf_cells_shaded, PreVar1350, Srest), PreVar1350 = PreVar1349), world(PreVar1172, PreVar1173, PreVar1174, PreVar1175), (PreVar1365 is PreVar1344 - (1), findall([PreVar1238, PreVar1345], (PreVar1364 is PreVar1365, between(PreVar1172, PreVar1364, 1, PreVar1238), is_pos(PreVar1238, PreVar1345), member([PreVar1238, PreVar1345], PreVar1349)), PreVar1383)), length(PreVar1383, PreVar1202).
ssa( epf_agent_direction, PreVar1398, [turn_right|Srest] ) :- !, (has_val(epf_agent_direction, PreVar1388, Srest), PreVar1388 = PreVar1387), is_direction(PreVar1387), PreVar1387=east, PreVar1398=south ; (has_val(epf_agent_direction, PreVar1404, Srest), PreVar1404 = PreVar1403), is_direction(PreVar1403), PreVar1403=south, PreVar1398=west ; (has_val(epf_agent_direction, PreVar1420, Srest), PreVar1420 = PreVar1419), is_direction(PreVar1419), PreVar1419=west, PreVar1398=north ; (has_val(epf_agent_direction, PreVar1436, Srest), PreVar1436 = PreVar1435), is_direction(PreVar1435), PreVar1435=north, PreVar1398=east.
ssa( epf_num_facedShades, PreVar1510, [turn_right|Srest] ) :- !, (has_val(epf_agent_direction, PreVar1452, Srest), PreVar1452 = PreVar1451), is_direction(PreVar1451), PreVar1451=east, (has_val(epf_agent_pos, PreVar1464, Srest), PreVar1464=[PreVar1469, PreVar1470]), is_pos(PreVar1469, PreVar1470), (has_val(epf_cells_shaded, PreVar1475, Srest), PreVar1475 = PreVar1474), world(PreVar1480, PreVar1481, PreVar1482, PreVar1483), (PreVar1490 is PreVar1470 - (1), findall([PreVar1469, PreVar1486], (PreVar1489 is PreVar1490, between(PreVar1481, PreVar1489, 1, PreVar1486), is_pos(PreVar1469, PreVar1486), member([PreVar1469, PreVar1486], PreVar1474)), PreVar1508)), length(PreVar1508, PreVar1510) ; (has_val(epf_agent_direction, PreVar1513, Srest), PreVar1513 = PreVar1512), is_direction(PreVar1512), PreVar1512=south, (has_val(epf_agent_pos, PreVar1525, Srest), PreVar1525=[PreVar1530, PreVar1531]), is_pos(PreVar1530, PreVar1531), (has_val(epf_cells_shaded, PreVar1536, Srest), PreVar1536 = PreVar1535), world(PreVar1480, PreVar1481, PreVar1482, PreVar1483), (PreVar1551 is PreVar1530 - (1), findall([PreVar1546, PreVar1531], (PreVar1550 is PreVar1551, between(PreVar1480, PreVar1550, 1, PreVar1546), is_pos(PreVar1546, PreVar1531), member([PreVar1546, PreVar1531], PreVar1535)), PreVar1569)), length(PreVar1569, PreVar1510) ; (has_val(epf_agent_direction, PreVar1574, Srest), PreVar1574 = PreVar1573), is_direction(PreVar1573), PreVar1573=west, (has_val(epf_agent_pos, PreVar1586, Srest), PreVar1586=[PreVar1591, PreVar1592]), is_pos(PreVar1591, PreVar1592), (has_val(epf_cells_shaded, PreVar1597, Srest), PreVar1597 = PreVar1596), world(PreVar1480, PreVar1481, PreVar1482, PreVar1483), (PreVar1612 is PreVar1592 + (1), findall([PreVar1591, PreVar1486], (PreVar1611 is PreVar1612, between(PreVar1611, PreVar1483, 1, PreVar1486), is_pos(PreVar1591, PreVar1486), member([PreVar1591, PreVar1486], PreVar1596)), PreVar1630)), length(PreVar1630, PreVar1510) ; (has_val(epf_agent_direction, PreVar1635, Srest), PreVar1635 = PreVar1634), is_direction(PreVar1634), PreVar1634=north, (has_val(epf_agent_pos, PreVar1647, Srest), PreVar1647=[PreVar1652, PreVar1653]), is_pos(PreVar1652, PreVar1653), (has_val(epf_cells_shaded, PreVar1658, Srest), PreVar1658 = PreVar1657), world(PreVar1480, PreVar1481, PreVar1482, PreVar1483), (PreVar1673 is PreVar1652 + (1), findall([PreVar1546, PreVar1653], (PreVar1672 is PreVar1673, between(PreVar1672, PreVar1482, 1, PreVar1546), is_pos(PreVar1546, PreVar1653), member([PreVar1546, PreVar1653], PreVar1657)), PreVar1691)), length(PreVar1691, PreVar1510).
ssa( start, NewValue, [setTime(NewValue)|Srest] ) :- !, true.
ssa( start, NewValue, [ccUpdate(_18784, NewValue)|Srest] ) :- !, true.
ssa( action, NewValue, [reply(action, NewValue)|Srest] ) :- !, true.
ssa( action, NewValue, [send(action, NewValue)|Srest] ) :- !, true.
ssa( Fluent, NewValue, [set(Fluent, NewValue)|Srest] ) :- !, true.
ssa( pos, PreVar1694, [teleport(PreVar1698, PreVar1699)|Srest] ) :- !, PreVar1694=[PreVar1698, PreVar1699].

/* 	___ dummy SSAs ___ */
ssa( action, NewValue, [set(action, NewValue)|_] ) :- !.
ssa( eval_exog_functions, NewValue, [set(eval_exog_functions, NewValue)|_] ) :- !.
ssa( eval_registers, NewValue, [set(eval_registers, NewValue)|_] ) :- !.
ssa( online, NewValue, [set(online, NewValue)|_] ) :- !.
ssa( start, NewValue, [set(start, NewValue)|_] ) :- !.
ssa( useAbstraction, NewValue, [set(useAbstraction, NewValue)|_] ) :- !.
ssa( bel(_92935), NewValue, [set(bel(_92935), NewValue)|_] ) :- !.
ssa( ltp(_92939), NewValue, [set(ltp(_92939), NewValue)|_] ) :- !.
ssa( pproj(_92923, _92924), NewValue, [set(pproj(_92923, _92924), NewValue)|_] ) :- !.
ssa( lookahead(_92928, _92929, _92930, _92931), NewValue, [set(lookahead(_92928, _92929, _92930, _92931), NewValue)|_] ) :- !.
ssa( pll(_92916, _92917, _92918, _92919), NewValue, [set(pll(_92916, _92917, _92918, _92919), NewValue)|_] ) :- !.
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
ssa( epf_visited(_X, _Y), NewValue, [set(epf_visited(_X, _Y), NewValue)|_] ) :- !.
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
ssa( epf_visited(_X, _Y), NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_visited(_X, _Y), NewValue, Srest).
ssa( action, NewValue, [_UnknownAction|Srest] ) :- !, has_val(action, NewValue, Srest).
ssa( eval_exog_functions, NewValue, [_UnknownAction|Srest] ) :- !, has_val(eval_exog_functions, NewValue, Srest).
ssa( eval_registers, NewValue, [_UnknownAction|Srest] ) :- !, has_val(eval_registers, NewValue, Srest).
ssa( online, NewValue, [_UnknownAction|Srest] ) :- !, has_val(online, NewValue, Srest).
ssa( start, NewValue, [_UnknownAction|Srest] ) :- !, has_val(start, NewValue, Srest).
ssa( useAbstraction, NewValue, [_UnknownAction|Srest] ) :- !, has_val(useAbstraction, NewValue, Srest).
ssa( bel(_92935), NewValue, [_UnknownAction|Srest] ) :- !, has_val(bel(_92935), NewValue, Srest).
ssa( ltp(_92939), NewValue, [_UnknownAction|Srest] ) :- !, has_val(ltp(_92939), NewValue, Srest).
ssa( pproj(_92923, _92924), NewValue, [_UnknownAction|Srest] ) :- !, has_val(pproj(_92923, _92924), NewValue, Srest).
ssa( lookahead(_92928, _92929, _92930, _92931), NewValue, [_UnknownAction|Srest] ) :- !, has_val(lookahead(_92928, _92929, _92930, _92931), NewValue, Srest).
ssa( pll(_92916, _92917, _92918, _92919), NewValue, [_UnknownAction|Srest] ) :- !, has_val(pll(_92916, _92917, _92918, _92919), NewValue, Srest).
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
ssa( epf_visited(_X, _Y), NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_visited(_X, _Y), NewValue, Srest).
prolog_poss( send(_R, _94317) ).
prolog_poss( send(_R, _94317), S ) :- true.
prolog_poss( reply(_R, _94326) ).
prolog_poss( reply(_R, _94326), S ) :- true.
prolog_poss( setOnline ).
prolog_poss( setOnline, S ) :- true.
prolog_poss( clipOnline ).
prolog_poss( clipOnline, S ) :- true.
prolog_poss( setTime(_94344) ).
prolog_poss( setTime(_94344), S ) :- true.
prolog_poss( clipAbstraction ).
prolog_poss( clipAbstraction, S ) :- true.
prolog_poss( setAbstraction ).
prolog_poss( setAbstraction, S ) :- true.
prolog_poss( set(_Fluent, _Value) ).
prolog_poss( set(_Fluent, _Value), S ) :- true.
prolog_poss( exogf_Update ).
prolog_poss( exogf_Update, S ) :- true.
prolog_poss( go_forward ).
prolog_poss( go_forward, S ) :- (has_val(epf_agent_pos, PreVar1702, S), PreVar1702=[PreVar1707, PreVar1708]), is_pos(PreVar1707, PreVar1708), (has_val(epf_wumpus_alive, PreVar1713, S), PreVar1713 = PreVar1712), (has_val(epf_cells_know_no_pit, PreVar1720, S), PreVar1720 = PreVar1719), (has_val(epf_cells_know_no_wumpus, PreVar1727, S), PreVar1727 = PreVar1726), (has_val(epf_agent_direction, PreVar1734, S), PreVar1734 = PreVar1733), PreVar1733=east, (PreVar1744 is PreVar1707 + (1), PreVar1743 is PreVar1744), is_pos(PreVar1743, PreVar1708), not is_wall(PreVar1707, PreVar1708, PreVar1743, PreVar1708), member([PreVar1743, PreVar1708], PreVar1719), (member([PreVar1743, PreVar1708], PreVar1726) ; PreVar1712=false) ; (has_val(epf_agent_pos, PreVar1767, S), PreVar1767=[PreVar1772, PreVar1773]), is_pos(PreVar1772, PreVar1773), (has_val(epf_wumpus_alive, PreVar1778, S), PreVar1778 = PreVar1777), (has_val(epf_cells_know_no_pit, PreVar1785, S), PreVar1785 = PreVar1784), (has_val(epf_cells_know_no_wumpus, PreVar1792, S), PreVar1792 = PreVar1791), (has_val(epf_agent_direction, PreVar1799, S), PreVar1799 = PreVar1798), PreVar1798=south, (PreVar1809 is PreVar1773 - (1), PreVar1808 is PreVar1809), is_pos(PreVar1772, PreVar1808), not is_wall(PreVar1772, PreVar1773, PreVar1772, PreVar1808), member([PreVar1772, PreVar1808], PreVar1784), (member([PreVar1772, PreVar1808], PreVar1791) ; PreVar1777=false) ; (has_val(epf_agent_pos, PreVar1832, S), PreVar1832=[PreVar1837, PreVar1838]), is_pos(PreVar1837, PreVar1838), (has_val(epf_wumpus_alive, PreVar1843, S), PreVar1843 = PreVar1842), (has_val(epf_cells_know_no_pit, PreVar1850, S), PreVar1850 = PreVar1849), (has_val(epf_cells_know_no_wumpus, PreVar1857, S), PreVar1857 = PreVar1856), (has_val(epf_agent_direction, PreVar1864, S), PreVar1864 = PreVar1863), PreVar1863=west, (PreVar1874 is PreVar1837 - (1), PreVar1873 is PreVar1874), is_pos(PreVar1873, PreVar1838), not is_wall(PreVar1837, PreVar1838, PreVar1873, PreVar1838), member([PreVar1873, PreVar1838], PreVar1849), (member([PreVar1873, PreVar1838], PreVar1856) ; PreVar1842=false) ; (has_val(epf_agent_pos, PreVar1897, S), PreVar1897=[PreVar1902, PreVar1903]), is_pos(PreVar1902, PreVar1903), (has_val(epf_wumpus_alive, PreVar1908, S), PreVar1908 = PreVar1907), (has_val(epf_cells_know_no_pit, PreVar1915, S), PreVar1915 = PreVar1914), (has_val(epf_cells_know_no_wumpus, PreVar1922, S), PreVar1922 = PreVar1921), (has_val(epf_agent_direction, PreVar1929, S), PreVar1929 = PreVar1928), PreVar1928=north, (PreVar1939 is PreVar1903 + (1), PreVar1938 is PreVar1939), is_pos(PreVar1902, PreVar1938), not is_wall(PreVar1902, PreVar1903, PreVar1902, PreVar1938), member([PreVar1902, PreVar1938], PreVar1914), (member([PreVar1902, PreVar1938], PreVar1921) ; PreVar1907=false).
prolog_poss( turn_right ).
prolog_poss( turn_right, S ) :- true.
prolog_poss( turn_left ).
prolog_poss( turn_left, S ) :- true.
prolog_poss( shoot ).
prolog_poss( shoot, S ) :- has_val(epf_agent_arrow, PreVar1962, S), PreVar1962=true.
prolog_poss( rest ).
prolog_poss( rest, S ) :- (has_val(epf_agent_pos, PreVar1969, S), PreVar1969=[PreVar1974, PreVar1975]), is_pos(PreVar1974, PreVar1975).
prolog_poss( pickup_gold ).
prolog_poss( pickup_gold, S ) :- (has_val(epf_agent_pos, PreVar1980, S), PreVar1980=[PreVar1985, PreVar1986]), is_pos(PreVar1985, PreVar1986), getval(real_gold_pos, [PreVar1991, PreVar1992]), PreVar1985 =:= PreVar1991, PreVar1986 =:= PreVar1992, not has_val(epf_carry_gold, true, S).
prolog_poss( noop ).
prolog_poss( noop, S ) :- true.
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
