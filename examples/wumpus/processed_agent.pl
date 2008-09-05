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
ssa( epf_visited(_21092, _21093), PreVar886, [reset_visited|Srest] ) :- !, PreVar886=false.
ssa( visited(_21074, _21075), PreVar890, [reset_visited|Srest] ) :- !, PreVar890=false.
ssa( online, true, [setOnline|Srest] ) :- !, true.
ssa( epf_agent_arrow, PreVar894, [shoot|Srest] ) :- !, PreVar894=false.
ssa( epf_wumpus_alive, PreVar938, [shoot|Srest] ) :- !, (has_val(epf_agent_direction, PreVar900, Srest), PreVar900 = PreVar899), is_direction(PreVar899), PreVar899=east, (has_val(epf_agent_pos, PreVar912, Srest), PreVar912=[PreVar917, PreVar918]), is_pos(PreVar917, PreVar918), (has_val(epf_wumpus_pos, PreVar923, Srest), PreVar923=[PreVar928, PreVar929]), PreVar929 = PreVar918, PreVar928 > PreVar917, PreVar938=false ; (has_val(epf_agent_direction, PreVar944, Srest), PreVar944 = PreVar943), is_direction(PreVar943), PreVar943=south, (has_val(epf_agent_pos, PreVar956, Srest), PreVar956=[PreVar961, PreVar962]), is_pos(PreVar961, PreVar962), (has_val(epf_wumpus_pos, PreVar967, Srest), PreVar967=[PreVar972, PreVar973]), PreVar973 < PreVar962, PreVar938=false ; (has_val(epf_agent_direction, PreVar984, Srest), PreVar984 = PreVar983), is_direction(PreVar983), PreVar983=west, (has_val(epf_agent_pos, PreVar996, Srest), PreVar996=[PreVar1001, PreVar1002]), is_pos(PreVar1001, PreVar1002), (has_val(epf_wumpus_pos, PreVar1007, Srest), PreVar1007=[PreVar1012, PreVar1013]), PreVar1012 < PreVar1001, PreVar938=false ; (has_val(epf_agent_direction, PreVar1024, Srest), PreVar1024 = PreVar1023), is_direction(PreVar1023), PreVar1023=north, (has_val(epf_agent_pos, PreVar1036, Srest), PreVar1036=[PreVar1041, PreVar1042]), is_pos(PreVar1041, PreVar1042), (has_val(epf_wumpus_pos, PreVar1047, Srest), PreVar1047=[PreVar1052, PreVar1053]), PreVar1052 = PreVar1041, PreVar1053 > PreVar1042, PreVar938=false ; PreVar938=true.
ssa( epf_agent_direction, PreVar1082, [turn_left|Srest] ) :- !, (has_val(epf_agent_direction, PreVar1072, Srest), PreVar1072 = PreVar1071), is_direction(PreVar1071), PreVar1071=east, PreVar1082=north ; (has_val(epf_agent_direction, PreVar1088, Srest), PreVar1088 = PreVar1087), is_direction(PreVar1087), PreVar1087=south, PreVar1082=east ; (has_val(epf_agent_direction, PreVar1104, Srest), PreVar1104 = PreVar1103), is_direction(PreVar1103), PreVar1103=west, PreVar1082=south ; (has_val(epf_agent_direction, PreVar1120, Srest), PreVar1120 = PreVar1119), is_direction(PreVar1119), PreVar1119=north, PreVar1082=west.
ssa( epf_num_facedShades, PreVar1194, [turn_left|Srest] ) :- !, (has_val(epf_agent_direction, PreVar1136, Srest), PreVar1136 = PreVar1135), is_direction(PreVar1135), PreVar1135=east, (has_val(epf_agent_pos, PreVar1148, Srest), PreVar1148=[PreVar1153, PreVar1154]), is_pos(PreVar1153, PreVar1154), (has_val(epf_cells_shaded, PreVar1159, Srest), PreVar1159 = PreVar1158), world(PreVar1164, PreVar1165, PreVar1166, PreVar1167), (PreVar1174 is PreVar1154 + (1), findall([PreVar1153, PreVar1170], (PreVar1173 is PreVar1174, between(PreVar1173, PreVar1167, 1, PreVar1170), is_pos(PreVar1153, PreVar1170), member([PreVar1153, PreVar1170], PreVar1158)), PreVar1192)), length(PreVar1192, PreVar1194) ; (has_val(epf_agent_direction, PreVar1197, Srest), PreVar1197 = PreVar1196), is_direction(PreVar1196), PreVar1196=south, (has_val(epf_agent_pos, PreVar1209, Srest), PreVar1209=[PreVar1214, PreVar1215]), is_pos(PreVar1214, PreVar1215), (has_val(epf_cells_shaded, PreVar1220, Srest), PreVar1220 = PreVar1219), world(PreVar1164, PreVar1165, PreVar1166, PreVar1167), (PreVar1235 is PreVar1214 + (1), findall([PreVar1230, PreVar1215], (PreVar1234 is PreVar1235, between(PreVar1234, PreVar1166, 1, PreVar1230), is_pos(PreVar1230, PreVar1215), member([PreVar1230, PreVar1215], PreVar1219)), PreVar1253)), length(PreVar1253, PreVar1194) ; (has_val(epf_agent_direction, PreVar1258, Srest), PreVar1258 = PreVar1257), is_direction(PreVar1257), PreVar1257=west, (has_val(epf_agent_pos, PreVar1270, Srest), PreVar1270=[PreVar1275, PreVar1276]), is_pos(PreVar1275, PreVar1276), (has_val(epf_cells_shaded, PreVar1281, Srest), PreVar1281 = PreVar1280), world(PreVar1164, PreVar1165, PreVar1166, PreVar1167), (PreVar1296 is PreVar1276 - (1), findall([PreVar1275, PreVar1170], (PreVar1295 is PreVar1296, between(PreVar1165, PreVar1295, 1, PreVar1170), is_pos(PreVar1275, PreVar1170), member([PreVar1275, PreVar1170], PreVar1280)), PreVar1314)), length(PreVar1314, PreVar1194) ; (has_val(epf_agent_direction, PreVar1319, Srest), PreVar1319 = PreVar1318), is_direction(PreVar1318), PreVar1318=north, (has_val(epf_agent_pos, PreVar1331, Srest), PreVar1331=[PreVar1336, PreVar1337]), is_pos(PreVar1336, PreVar1337), (has_val(epf_cells_shaded, PreVar1342, Srest), PreVar1342 = PreVar1341), world(PreVar1164, PreVar1165, PreVar1166, PreVar1167), (PreVar1357 is PreVar1336 - (1), findall([PreVar1230, PreVar1337], (PreVar1356 is PreVar1357, between(PreVar1164, PreVar1356, 1, PreVar1230), is_pos(PreVar1230, PreVar1337), member([PreVar1230, PreVar1337], PreVar1341)), PreVar1375)), length(PreVar1375, PreVar1194).
ssa( epf_agent_direction, PreVar1390, [turn_right|Srest] ) :- !, (has_val(epf_agent_direction, PreVar1380, Srest), PreVar1380 = PreVar1379), is_direction(PreVar1379), PreVar1379=east, PreVar1390=south ; (has_val(epf_agent_direction, PreVar1396, Srest), PreVar1396 = PreVar1395), is_direction(PreVar1395), PreVar1395=south, PreVar1390=west ; (has_val(epf_agent_direction, PreVar1412, Srest), PreVar1412 = PreVar1411), is_direction(PreVar1411), PreVar1411=west, PreVar1390=north ; (has_val(epf_agent_direction, PreVar1428, Srest), PreVar1428 = PreVar1427), is_direction(PreVar1427), PreVar1427=north, PreVar1390=east.
ssa( epf_num_facedShades, PreVar1502, [turn_right|Srest] ) :- !, (has_val(epf_agent_direction, PreVar1444, Srest), PreVar1444 = PreVar1443), is_direction(PreVar1443), PreVar1443=east, (has_val(epf_agent_pos, PreVar1456, Srest), PreVar1456=[PreVar1461, PreVar1462]), is_pos(PreVar1461, PreVar1462), (has_val(epf_cells_shaded, PreVar1467, Srest), PreVar1467 = PreVar1466), world(PreVar1472, PreVar1473, PreVar1474, PreVar1475), (PreVar1482 is PreVar1462 - (1), findall([PreVar1461, PreVar1478], (PreVar1481 is PreVar1482, between(PreVar1473, PreVar1481, 1, PreVar1478), is_pos(PreVar1461, PreVar1478), member([PreVar1461, PreVar1478], PreVar1466)), PreVar1500)), length(PreVar1500, PreVar1502) ; (has_val(epf_agent_direction, PreVar1505, Srest), PreVar1505 = PreVar1504), is_direction(PreVar1504), PreVar1504=south, (has_val(epf_agent_pos, PreVar1517, Srest), PreVar1517=[PreVar1522, PreVar1523]), is_pos(PreVar1522, PreVar1523), (has_val(epf_cells_shaded, PreVar1528, Srest), PreVar1528 = PreVar1527), world(PreVar1472, PreVar1473, PreVar1474, PreVar1475), (PreVar1543 is PreVar1522 - (1), findall([PreVar1538, PreVar1523], (PreVar1542 is PreVar1543, between(PreVar1472, PreVar1542, 1, PreVar1538), is_pos(PreVar1538, PreVar1523), member([PreVar1538, PreVar1523], PreVar1527)), PreVar1561)), length(PreVar1561, PreVar1502) ; (has_val(epf_agent_direction, PreVar1566, Srest), PreVar1566 = PreVar1565), is_direction(PreVar1565), PreVar1565=west, (has_val(epf_agent_pos, PreVar1578, Srest), PreVar1578=[PreVar1583, PreVar1584]), is_pos(PreVar1583, PreVar1584), (has_val(epf_cells_shaded, PreVar1589, Srest), PreVar1589 = PreVar1588), world(PreVar1472, PreVar1473, PreVar1474, PreVar1475), (PreVar1604 is PreVar1584 + (1), findall([PreVar1583, PreVar1478], (PreVar1603 is PreVar1604, between(PreVar1603, PreVar1475, 1, PreVar1478), is_pos(PreVar1583, PreVar1478), member([PreVar1583, PreVar1478], PreVar1588)), PreVar1622)), length(PreVar1622, PreVar1502) ; (has_val(epf_agent_direction, PreVar1627, Srest), PreVar1627 = PreVar1626), is_direction(PreVar1626), PreVar1626=north, (has_val(epf_agent_pos, PreVar1639, Srest), PreVar1639=[PreVar1644, PreVar1645]), is_pos(PreVar1644, PreVar1645), (has_val(epf_cells_shaded, PreVar1650, Srest), PreVar1650 = PreVar1649), world(PreVar1472, PreVar1473, PreVar1474, PreVar1475), (PreVar1665 is PreVar1644 + (1), findall([PreVar1538, PreVar1645], (PreVar1664 is PreVar1665, between(PreVar1664, PreVar1474, 1, PreVar1538), is_pos(PreVar1538, PreVar1645), member([PreVar1538, PreVar1645], PreVar1649)), PreVar1683)), length(PreVar1683, PreVar1502).
ssa( start, NewValue, [setTime(NewValue)|Srest] ) :- !, true.
ssa( start, NewValue, [ccUpdate(_18787, NewValue)|Srest] ) :- !, true.
ssa( action, NewValue, [reply(action, NewValue)|Srest] ) :- !, true.
ssa( action, NewValue, [send(action, NewValue)|Srest] ) :- !, true.
ssa( Fluent, NewValue, [set(Fluent, NewValue)|Srest] ) :- !, true.
ssa( pos, PreVar1686, [teleport(PreVar1690, PreVar1691)|Srest] ) :- !, PreVar1686=[PreVar1690, PreVar1691].

/* 	___ dummy SSAs ___ */
ssa( action, NewValue, [set(action, NewValue)|_] ) :- !.
ssa( eval_exog_functions, NewValue, [set(eval_exog_functions, NewValue)|_] ) :- !.
ssa( eval_registers, NewValue, [set(eval_registers, NewValue)|_] ) :- !.
ssa( online, NewValue, [set(online, NewValue)|_] ) :- !.
ssa( start, NewValue, [set(start, NewValue)|_] ) :- !.
ssa( useAbstraction, NewValue, [set(useAbstraction, NewValue)|_] ) :- !.
ssa( bel(_92570), NewValue, [set(bel(_92570), NewValue)|_] ) :- !.
ssa( ltp(_92574), NewValue, [set(ltp(_92574), NewValue)|_] ) :- !.
ssa( pproj(_92558, _92559), NewValue, [set(pproj(_92558, _92559), NewValue)|_] ) :- !.
ssa( lookahead(_92563, _92564, _92565, _92566), NewValue, [set(lookahead(_92563, _92564, _92565, _92566), NewValue)|_] ) :- !.
ssa( pll(_92551, _92552, _92553, _92554), NewValue, [set(pll(_92551, _92552, _92553, _92554), NewValue)|_] ) :- !.
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
ssa( bel(_92570), NewValue, [_UnknownAction|Srest] ) :- !, has_val(bel(_92570), NewValue, Srest).
ssa( ltp(_92574), NewValue, [_UnknownAction|Srest] ) :- !, has_val(ltp(_92574), NewValue, Srest).
ssa( pproj(_92558, _92559), NewValue, [_UnknownAction|Srest] ) :- !, has_val(pproj(_92558, _92559), NewValue, Srest).
ssa( lookahead(_92563, _92564, _92565, _92566), NewValue, [_UnknownAction|Srest] ) :- !, has_val(lookahead(_92563, _92564, _92565, _92566), NewValue, Srest).
ssa( pll(_92551, _92552, _92553, _92554), NewValue, [_UnknownAction|Srest] ) :- !, has_val(pll(_92551, _92552, _92553, _92554), NewValue, Srest).
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
prolog_poss( send(_R, _93952) ).
prolog_poss( send(_R, _93952), S ) :- true.
prolog_poss( reply(_R, _93961) ).
prolog_poss( reply(_R, _93961), S ) :- true.
prolog_poss( setOnline ).
prolog_poss( setOnline, S ) :- true.
prolog_poss( clipOnline ).
prolog_poss( clipOnline, S ) :- true.
prolog_poss( setTime(_93979) ).
prolog_poss( setTime(_93979), S ) :- true.
prolog_poss( clipAbstraction ).
prolog_poss( clipAbstraction, S ) :- true.
prolog_poss( setAbstraction ).
prolog_poss( setAbstraction, S ) :- true.
prolog_poss( set(_Fluent, _Value) ).
prolog_poss( set(_Fluent, _Value), S ) :- true.
prolog_poss( exogf_Update ).
prolog_poss( exogf_Update, S ) :- true.
prolog_poss( go_forward ).
prolog_poss( go_forward, S ) :- (has_val(epf_agent_pos, PreVar1694, S), PreVar1694=[PreVar1699, PreVar1700]), is_pos(PreVar1699, PreVar1700), (has_val(epf_wumpus_alive, PreVar1705, S), PreVar1705 = PreVar1704), (has_val(epf_cells_know_no_pit, PreVar1712, S), PreVar1712 = PreVar1711), (has_val(epf_cells_know_no_wumpus, PreVar1719, S), PreVar1719 = PreVar1718), (has_val(epf_agent_direction, PreVar1726, S), PreVar1726 = PreVar1725), PreVar1725=east, (PreVar1736 is PreVar1699 + (1), PreVar1735 is PreVar1736), is_pos(PreVar1735, PreVar1700), not is_wall(PreVar1699, PreVar1700, PreVar1735, PreVar1700), member([PreVar1735, PreVar1700], PreVar1711), (member([PreVar1735, PreVar1700], PreVar1718) ; PreVar1704=false) ; (has_val(epf_agent_pos, PreVar1759, S), PreVar1759=[PreVar1764, PreVar1765]), is_pos(PreVar1764, PreVar1765), (has_val(epf_wumpus_alive, PreVar1770, S), PreVar1770 = PreVar1769), (has_val(epf_cells_know_no_pit, PreVar1777, S), PreVar1777 = PreVar1776), (has_val(epf_cells_know_no_wumpus, PreVar1784, S), PreVar1784 = PreVar1783), (has_val(epf_agent_direction, PreVar1791, S), PreVar1791 = PreVar1790), PreVar1790=south, (PreVar1801 is PreVar1765 - (1), PreVar1800 is PreVar1801), is_pos(PreVar1764, PreVar1800), not is_wall(PreVar1764, PreVar1765, PreVar1764, PreVar1800), member([PreVar1764, PreVar1800], PreVar1776), (member([PreVar1764, PreVar1800], PreVar1783) ; PreVar1769=false) ; (has_val(epf_agent_pos, PreVar1824, S), PreVar1824=[PreVar1829, PreVar1830]), is_pos(PreVar1829, PreVar1830), (has_val(epf_wumpus_alive, PreVar1835, S), PreVar1835 = PreVar1834), (has_val(epf_cells_know_no_pit, PreVar1842, S), PreVar1842 = PreVar1841), (has_val(epf_cells_know_no_wumpus, PreVar1849, S), PreVar1849 = PreVar1848), (has_val(epf_agent_direction, PreVar1856, S), PreVar1856 = PreVar1855), PreVar1855=west, (PreVar1866 is PreVar1829 - (1), PreVar1865 is PreVar1866), is_pos(PreVar1865, PreVar1830), not is_wall(PreVar1829, PreVar1830, PreVar1865, PreVar1830), member([PreVar1865, PreVar1830], PreVar1841), (member([PreVar1865, PreVar1830], PreVar1848) ; PreVar1834=false) ; (has_val(epf_agent_pos, PreVar1889, S), PreVar1889=[PreVar1894, PreVar1895]), is_pos(PreVar1894, PreVar1895), (has_val(epf_wumpus_alive, PreVar1900, S), PreVar1900 = PreVar1899), (has_val(epf_cells_know_no_pit, PreVar1907, S), PreVar1907 = PreVar1906), (has_val(epf_cells_know_no_wumpus, PreVar1914, S), PreVar1914 = PreVar1913), (has_val(epf_agent_direction, PreVar1921, S), PreVar1921 = PreVar1920), PreVar1920=north, (PreVar1931 is PreVar1895 + (1), PreVar1930 is PreVar1931), is_pos(PreVar1894, PreVar1930), not is_wall(PreVar1894, PreVar1895, PreVar1894, PreVar1930), member([PreVar1894, PreVar1930], PreVar1906), (member([PreVar1894, PreVar1930], PreVar1913) ; PreVar1899=false).
prolog_poss( turn_right ).
prolog_poss( turn_right, S ) :- true.
prolog_poss( turn_left ).
prolog_poss( turn_left, S ) :- true.
prolog_poss( shoot ).
prolog_poss( shoot, S ) :- has_val(epf_agent_arrow, PreVar1954, S), PreVar1954=true.
prolog_poss( rest ).
prolog_poss( rest, S ) :- (has_val(epf_agent_pos, PreVar1961, S), PreVar1961=[PreVar1966, PreVar1967]), is_pos(PreVar1966, PreVar1967).
prolog_poss( pickup_gold ).
prolog_poss( pickup_gold, S ) :- (has_val(epf_agent_pos, PreVar1972, S), PreVar1972=[PreVar1977, PreVar1978]), is_pos(PreVar1977, PreVar1978), getval(real_gold_pos, [PreVar1983, PreVar1984]), PreVar1977 =:= PreVar1983, PreVar1978 =:= PreVar1984, not has_val(epf_carry_gold, true, S).
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
