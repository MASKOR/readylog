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
prolog_function( reward_search_gold, PreVar234, S ) :- (has_val(epf_wumpus_alive, PreVar185, S), PreVar185=true -> PreVar190=0 ; PreVar190=50), (has_val(epf_sense_gold, PreVar200, S), PreVar200=true -> PreVar205=100 ; PreVar205=0), ((((((has_val(epf_distance_to_closest_safe_cell, PreVar221, S), PreVar219 is PreVar221 * (-1)), has_val(epf_num_visited, PreVar226, S), PreVar220 is PreVar226 * (20)), PreVar217 is PreVar219 + PreVar220), has_val(epf_num_facedShades, PreVar218, S)), PreVar216 is PreVar217 + PreVar218), PreVar213 = PreVar216), (PreVar238 is PreVar190 + PreVar205, PreVar237 is PreVar238 + PreVar213), PreVar234 = PreVar237.
prolog_function( f_gold_pos_by_id(PreVar244) ).
prolog_function( f_gold_pos_by_id(PreVar244), PreVar288, S ) :- (PreVar243 is PreVar244 - (5), PreVar242 is PreVar243), (PreVar247 is PreVar244 - (4), PreVar246 is PreVar247), (PreVar251 is PreVar242 mod 3, PreVar250 is PreVar251), (PreVar255 is PreVar246 div 3, PreVar254 is PreVar255), (PreVar250=2 -> PreVar262 = -1 ; PreVar262 = PreVar250), PreVar270 = PreVar254, pos=[PreVar278, PreVar279], (PreVar281 is PreVar278 + PreVar262, PreVar280 is PreVar281), (PreVar285 is PreVar279 + PreVar270, PreVar284 is PreVar285), PreVar288=[PreVar280, PreVar284].
prolog_function( adl(PreVar294, PreVar295) ).
prolog_function( adl(PreVar294, PreVar295), PreVar296, S ) :- append(PreVar294, PreVar295, PreVar296).
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
ssa( epf_agent_pos, PreVar324, [go_forward|Srest] ) :- !, (has_val(epf_agent_pos, PreVar299, Srest), PreVar299=[PreVar304, PreVar305]), is_pos(PreVar304, PreVar305), (has_val(epf_agent_direction, PreVar310, Srest), PreVar310 = PreVar309), is_direction(PreVar309), PreVar309=east, (PreVar321 is PreVar304 + (1), PreVar320 is PreVar321), PreVar324=[PreVar320, PreVar305] ; (has_val(epf_agent_pos, PreVar332, Srest), PreVar332=[PreVar337, PreVar338]), is_pos(PreVar337, PreVar338), (has_val(epf_agent_direction, PreVar343, Srest), PreVar343 = PreVar342), is_direction(PreVar342), PreVar342=south, (PreVar354 is PreVar338 - (1), PreVar353 is PreVar354), PreVar324=[PreVar337, PreVar353] ; (has_val(epf_agent_pos, PreVar365, Srest), PreVar365=[PreVar370, PreVar371]), is_pos(PreVar370, PreVar371), (has_val(epf_agent_direction, PreVar376, Srest), PreVar376 = PreVar375), is_direction(PreVar375), PreVar375=west, (PreVar387 is PreVar370 - (1), PreVar386 is PreVar387), PreVar324=[PreVar386, PreVar371] ; (has_val(epf_agent_pos, PreVar398, Srest), PreVar398=[PreVar403, PreVar404]), is_pos(PreVar403, PreVar404), (has_val(epf_agent_direction, PreVar409, Srest), PreVar409 = PreVar408), is_direction(PreVar408), PreVar408=north, (PreVar420 is PreVar404 + (1), PreVar419 is PreVar420), PreVar324=[PreVar403, PreVar419].
ssa( epf_cells_visited, PreVar471, [go_forward|Srest] ) :- !, (has_val(epf_agent_pos, PreVar431, Srest), PreVar431=[PreVar436, PreVar437]), is_pos(PreVar436, PreVar437), (has_val(epf_agent_direction, PreVar442, Srest), PreVar442 = PreVar441), is_direction(PreVar441), (has_val(epf_cells_visited, PreVar450, Srest), PreVar450 = PreVar449), PreVar441=east, (PreVar460 is PreVar436 + (1), PreVar459 is PreVar460), PreVar463 = PreVar437, not member([PreVar459, PreVar463], PreVar449), PreVar471=[PreVar449, PreVar459, PreVar463] ; (has_val(epf_agent_pos, PreVar480, Srest), PreVar480=[PreVar485, PreVar486]), is_pos(PreVar485, PreVar486), (has_val(epf_agent_direction, PreVar491, Srest), PreVar491 = PreVar490), is_direction(PreVar490), (has_val(epf_cells_visited, PreVar499, Srest), PreVar499 = PreVar498), PreVar490=south, PreVar459 = PreVar485, (PreVar513 is PreVar486 - (1), PreVar463 is PreVar513), not member([PreVar459, PreVar463], PreVar498), PreVar471=[PreVar498, PreVar459, PreVar463] ; (has_val(epf_agent_pos, PreVar529, Srest), PreVar529=[PreVar534, PreVar535]), is_pos(PreVar534, PreVar535), (has_val(epf_agent_direction, PreVar540, Srest), PreVar540 = PreVar539), is_direction(PreVar539), (has_val(epf_cells_visited, PreVar548, Srest), PreVar548 = PreVar547), PreVar539=west, (PreVar558 is PreVar534 - (1), PreVar459 is PreVar558), PreVar463 = PreVar535, not member([PreVar459, PreVar463], PreVar547), PreVar471=[PreVar547, PreVar459, PreVar463] ; (has_val(epf_agent_pos, PreVar578, Srest), PreVar578=[PreVar583, PreVar584]), is_pos(PreVar583, PreVar584), (has_val(epf_agent_direction, PreVar589, Srest), PreVar589 = PreVar588), is_direction(PreVar588), (has_val(epf_cells_visited, PreVar597, Srest), PreVar597 = PreVar596), PreVar588=north, PreVar459 = PreVar583, (PreVar611 is PreVar584 + (1), PreVar463 is PreVar611), not member([PreVar459, PreVar463], PreVar596), PreVar471=[PreVar596, PreVar459, PreVar463].
ssa( epf_num_facedShades, PreVar685, [go_forward|Srest] ) :- !, (has_val(epf_agent_direction, PreVar627, Srest), PreVar627 = PreVar626), is_direction(PreVar626), PreVar626=east, (has_val(epf_agent_pos, PreVar639, Srest), PreVar639=[PreVar644, PreVar645]), is_pos(PreVar644, PreVar645), (has_val(epf_cells_shaded, PreVar650, Srest), PreVar650 = PreVar649), world(PreVar655, PreVar656, PreVar657, PreVar658), (PreVar665 is PreVar644 + (1), findall([PreVar660, PreVar645], (PreVar664 is PreVar665, between(PreVar664, PreVar657, 1, PreVar660), is_pos(PreVar660, PreVar645), member([PreVar660, PreVar645], PreVar649)), PreVar683)), length(PreVar683, PreVar685) ; (has_val(epf_agent_direction, PreVar688, Srest), PreVar688 = PreVar687), is_direction(PreVar687), PreVar687=south, (has_val(epf_agent_pos, PreVar700, Srest), PreVar700=[PreVar705, PreVar706]), is_pos(PreVar705, PreVar706), (has_val(epf_cells_shaded, PreVar711, Srest), PreVar711 = PreVar710), world(PreVar655, PreVar656, PreVar657, PreVar658), (PreVar726 is PreVar706 - (1), findall([PreVar705, PreVar722], (PreVar725 is PreVar726, between(PreVar656, PreVar725, 1, PreVar722), is_pos(PreVar705, PreVar722), member([PreVar705, PreVar722], PreVar710)), PreVar744)), length(PreVar744, PreVar685) ; (has_val(epf_agent_direction, PreVar749, Srest), PreVar749 = PreVar748), is_direction(PreVar748), PreVar748=west, (has_val(epf_agent_pos, PreVar761, Srest), PreVar761=[PreVar766, PreVar767]), is_pos(PreVar766, PreVar767), (has_val(epf_cells_shaded, PreVar772, Srest), PreVar772 = PreVar771), world(PreVar655, PreVar656, PreVar657, PreVar658), (PreVar787 is PreVar766 - (1), findall([PreVar660, PreVar767], (PreVar786 is PreVar787, between(PreVar655, PreVar786, 1, PreVar660), is_pos(PreVar660, PreVar767), member([PreVar660, PreVar767], PreVar771)), PreVar805)), length(PreVar805, PreVar685) ; (has_val(epf_agent_direction, PreVar810, Srest), PreVar810 = PreVar809), is_direction(PreVar809), PreVar809=north, (has_val(epf_agent_pos, PreVar822, Srest), PreVar822=[PreVar827, PreVar828]), is_pos(PreVar827, PreVar828), (has_val(epf_cells_shaded, PreVar833, Srest), PreVar833 = PreVar832), world(PreVar655, PreVar656, PreVar657, PreVar658), (PreVar848 is PreVar828 + (1), findall([PreVar827, PreVar722], (PreVar847 is PreVar848, between(PreVar847, PreVar658, 1, PreVar722), is_pos(PreVar827, PreVar722), member([PreVar827, PreVar722], PreVar832)), PreVar866)), length(PreVar866, PreVar685).
ssa( epf_carry_gold, PreVar869, [pickup_gold|Srest] ) :- !, PreVar869=true.
ssa( visited(_20327, _20328), PreVar873, [reset_visited|Srest] ) :- !, PreVar873=false.
ssa( online, true, [setOnline|Srest] ) :- !, true.
ssa( epf_agent_arrow, PreVar877, [shoot|Srest] ) :- !, PreVar877=false.
ssa( epf_wumpus_alive, PreVar921, [shoot|Srest] ) :- !, (has_val(epf_agent_direction, PreVar883, Srest), PreVar883 = PreVar882), is_direction(PreVar882), PreVar882=east, (has_val(epf_agent_pos, PreVar895, Srest), PreVar895=[PreVar900, PreVar901]), is_pos(PreVar900, PreVar901), (has_val(epf_wumpus_pos, PreVar906, Srest), PreVar906=[PreVar911, PreVar912]), PreVar912 = PreVar901, PreVar911 > PreVar900, PreVar921=false ; (has_val(epf_agent_direction, PreVar927, Srest), PreVar927 = PreVar926), is_direction(PreVar926), PreVar926=south, (has_val(epf_agent_pos, PreVar939, Srest), PreVar939=[PreVar944, PreVar945]), is_pos(PreVar944, PreVar945), (has_val(epf_wumpus_pos, PreVar950, Srest), PreVar950=[PreVar955, PreVar956]), PreVar955 = PreVar944, PreVar956 < PreVar945, PreVar921=false ; (has_val(epf_agent_direction, PreVar971, Srest), PreVar971 = PreVar970), is_direction(PreVar970), PreVar970=west, (has_val(epf_agent_pos, PreVar983, Srest), PreVar983=[PreVar988, PreVar989]), is_pos(PreVar988, PreVar989), (has_val(epf_wumpus_pos, PreVar994, Srest), PreVar994=[PreVar999, PreVar1000]), PreVar1000 = PreVar989, PreVar999 < PreVar988, PreVar921=false ; (has_val(epf_agent_direction, PreVar1015, Srest), PreVar1015 = PreVar1014), is_direction(PreVar1014), PreVar1014=north, (has_val(epf_agent_pos, PreVar1027, Srest), PreVar1027=[PreVar1032, PreVar1033]), is_pos(PreVar1032, PreVar1033), (has_val(epf_wumpus_pos, PreVar1038, Srest), PreVar1038=[PreVar1043, PreVar1044]), PreVar1043 = PreVar1032, PreVar1044 > PreVar1033, PreVar921=false ; PreVar921=true.
ssa( epf_agent_direction, PreVar1073, [turn_left|Srest] ) :- !, (has_val(epf_agent_direction, PreVar1063, Srest), PreVar1063 = PreVar1062), is_direction(PreVar1062), PreVar1062=east, PreVar1073=north ; (has_val(epf_agent_direction, PreVar1079, Srest), PreVar1079 = PreVar1078), is_direction(PreVar1078), PreVar1078=south, PreVar1073=east ; (has_val(epf_agent_direction, PreVar1095, Srest), PreVar1095 = PreVar1094), is_direction(PreVar1094), PreVar1094=west, PreVar1073=south ; (has_val(epf_agent_direction, PreVar1111, Srest), PreVar1111 = PreVar1110), is_direction(PreVar1110), PreVar1110=north, PreVar1073=west.
ssa( epf_num_facedShades, PreVar1185, [turn_left|Srest] ) :- !, (has_val(epf_agent_direction, PreVar1127, Srest), PreVar1127 = PreVar1126), is_direction(PreVar1126), PreVar1126=east, (has_val(epf_agent_pos, PreVar1139, Srest), PreVar1139=[PreVar1144, PreVar1145]), is_pos(PreVar1144, PreVar1145), (has_val(epf_cells_shaded, PreVar1150, Srest), PreVar1150 = PreVar1149), world(PreVar1155, PreVar1156, PreVar1157, PreVar1158), (PreVar1165 is PreVar1145 + (1), findall([PreVar1144, PreVar1161], (PreVar1164 is PreVar1165, between(PreVar1164, PreVar1158, 1, PreVar1161), is_pos(PreVar1144, PreVar1161), member([PreVar1144, PreVar1161], PreVar1149)), PreVar1183)), length(PreVar1183, PreVar1185) ; (has_val(epf_agent_direction, PreVar1188, Srest), PreVar1188 = PreVar1187), is_direction(PreVar1187), PreVar1187=south, (has_val(epf_agent_pos, PreVar1200, Srest), PreVar1200=[PreVar1205, PreVar1206]), is_pos(PreVar1205, PreVar1206), (has_val(epf_cells_shaded, PreVar1211, Srest), PreVar1211 = PreVar1210), world(PreVar1155, PreVar1156, PreVar1157, PreVar1158), (PreVar1226 is PreVar1205 + (1), findall([PreVar1221, PreVar1206], (PreVar1225 is PreVar1226, between(PreVar1225, PreVar1157, 1, PreVar1221), is_pos(PreVar1221, PreVar1206), member([PreVar1221, PreVar1206], PreVar1210)), PreVar1244)), length(PreVar1244, PreVar1185) ; (has_val(epf_agent_direction, PreVar1249, Srest), PreVar1249 = PreVar1248), is_direction(PreVar1248), PreVar1248=west, (has_val(epf_agent_pos, PreVar1261, Srest), PreVar1261=[PreVar1266, PreVar1267]), is_pos(PreVar1266, PreVar1267), (has_val(epf_cells_shaded, PreVar1272, Srest), PreVar1272 = PreVar1271), world(PreVar1155, PreVar1156, PreVar1157, PreVar1158), (PreVar1287 is PreVar1267 - (1), findall([PreVar1266, PreVar1161], (PreVar1286 is PreVar1287, between(PreVar1156, PreVar1286, 1, PreVar1161), is_pos(PreVar1266, PreVar1161), member([PreVar1266, PreVar1161], PreVar1271)), PreVar1305)), length(PreVar1305, PreVar1185) ; (has_val(epf_agent_direction, PreVar1310, Srest), PreVar1310 = PreVar1309), is_direction(PreVar1309), PreVar1309=north, (has_val(epf_agent_pos, PreVar1322, Srest), PreVar1322=[PreVar1327, PreVar1328]), is_pos(PreVar1327, PreVar1328), (has_val(epf_cells_shaded, PreVar1333, Srest), PreVar1333 = PreVar1332), world(PreVar1155, PreVar1156, PreVar1157, PreVar1158), (PreVar1348 is PreVar1327 - (1), findall([PreVar1221, PreVar1328], (PreVar1347 is PreVar1348, between(PreVar1155, PreVar1347, 1, PreVar1221), is_pos(PreVar1221, PreVar1328), member([PreVar1221, PreVar1328], PreVar1332)), PreVar1366)), length(PreVar1366, PreVar1185).
ssa( epf_agent_direction, PreVar1381, [turn_right|Srest] ) :- !, (has_val(epf_agent_direction, PreVar1371, Srest), PreVar1371 = PreVar1370), is_direction(PreVar1370), PreVar1370=east, PreVar1381=south ; (has_val(epf_agent_direction, PreVar1387, Srest), PreVar1387 = PreVar1386), is_direction(PreVar1386), PreVar1386=south, PreVar1381=west ; (has_val(epf_agent_direction, PreVar1403, Srest), PreVar1403 = PreVar1402), is_direction(PreVar1402), PreVar1402=west, PreVar1381=north ; (has_val(epf_agent_direction, PreVar1419, Srest), PreVar1419 = PreVar1418), is_direction(PreVar1418), PreVar1418=north, PreVar1381=east.
ssa( epf_num_facedShades, PreVar1493, [turn_right|Srest] ) :- !, (has_val(epf_agent_direction, PreVar1435, Srest), PreVar1435 = PreVar1434), is_direction(PreVar1434), PreVar1434=east, (has_val(epf_agent_pos, PreVar1447, Srest), PreVar1447=[PreVar1452, PreVar1453]), is_pos(PreVar1452, PreVar1453), (has_val(epf_cells_shaded, PreVar1458, Srest), PreVar1458 = PreVar1457), world(PreVar1463, PreVar1464, PreVar1465, PreVar1466), (PreVar1473 is PreVar1453 - (1), findall([PreVar1452, PreVar1469], (PreVar1472 is PreVar1473, between(PreVar1464, PreVar1472, 1, PreVar1469), is_pos(PreVar1452, PreVar1469), member([PreVar1452, PreVar1469], PreVar1457)), PreVar1491)), length(PreVar1491, PreVar1493) ; (has_val(epf_agent_direction, PreVar1496, Srest), PreVar1496 = PreVar1495), is_direction(PreVar1495), PreVar1495=south, (has_val(epf_agent_pos, PreVar1508, Srest), PreVar1508=[PreVar1513, PreVar1514]), is_pos(PreVar1513, PreVar1514), (has_val(epf_cells_shaded, PreVar1519, Srest), PreVar1519 = PreVar1518), world(PreVar1463, PreVar1464, PreVar1465, PreVar1466), (PreVar1534 is PreVar1513 - (1), findall([PreVar1529, PreVar1514], (PreVar1533 is PreVar1534, between(PreVar1463, PreVar1533, 1, PreVar1529), is_pos(PreVar1529, PreVar1514), member([PreVar1529, PreVar1514], PreVar1518)), PreVar1552)), length(PreVar1552, PreVar1493) ; (has_val(epf_agent_direction, PreVar1557, Srest), PreVar1557 = PreVar1556), is_direction(PreVar1556), PreVar1556=west, (has_val(epf_agent_pos, PreVar1569, Srest), PreVar1569=[PreVar1574, PreVar1575]), is_pos(PreVar1574, PreVar1575), (has_val(epf_cells_shaded, PreVar1580, Srest), PreVar1580 = PreVar1579), world(PreVar1463, PreVar1464, PreVar1465, PreVar1466), (PreVar1595 is PreVar1575 + (1), findall([PreVar1574, PreVar1469], (PreVar1594 is PreVar1595, between(PreVar1594, PreVar1466, 1, PreVar1469), is_pos(PreVar1574, PreVar1469), member([PreVar1574, PreVar1469], PreVar1579)), PreVar1613)), length(PreVar1613, PreVar1493) ; (has_val(epf_agent_direction, PreVar1618, Srest), PreVar1618 = PreVar1617), is_direction(PreVar1617), PreVar1617=north, (has_val(epf_agent_pos, PreVar1630, Srest), PreVar1630=[PreVar1635, PreVar1636]), is_pos(PreVar1635, PreVar1636), (has_val(epf_cells_shaded, PreVar1641, Srest), PreVar1641 = PreVar1640), world(PreVar1463, PreVar1464, PreVar1465, PreVar1466), (PreVar1656 is PreVar1635 + (1), findall([PreVar1529, PreVar1636], (PreVar1655 is PreVar1656, between(PreVar1655, PreVar1465, 1, PreVar1529), is_pos(PreVar1529, PreVar1636), member([PreVar1529, PreVar1636], PreVar1640)), PreVar1674)), length(PreVar1674, PreVar1493).
ssa( start, NewValue, [setTime(NewValue)|Srest] ) :- !, true.
ssa( start, NewValue, [ccUpdate(_18034, NewValue)|Srest] ) :- !, true.
ssa( action, NewValue, [reply(action, NewValue)|Srest] ) :- !, true.
ssa( action, NewValue, [send(action, NewValue)|Srest] ) :- !, true.
ssa( Fluent, NewValue, [set(Fluent, NewValue)|Srest] ) :- !, true.
ssa( pos, PreVar1677, [teleport(PreVar1681, PreVar1682)|Srest] ) :- !, PreVar1677=[PreVar1681, PreVar1682].

/* 	___ dummy SSAs ___ */
ssa( action, NewValue, [set(action, NewValue)|_] ) :- !.
ssa( eval_exog_functions, NewValue, [set(eval_exog_functions, NewValue)|_] ) :- !.
ssa( eval_registers, NewValue, [set(eval_registers, NewValue)|_] ) :- !.
ssa( online, NewValue, [set(online, NewValue)|_] ) :- !.
ssa( start, NewValue, [set(start, NewValue)|_] ) :- !.
ssa( useAbstraction, NewValue, [set(useAbstraction, NewValue)|_] ) :- !.
ssa( bel(_91893), NewValue, [set(bel(_91893), NewValue)|_] ) :- !.
ssa( ltp(_91897), NewValue, [set(ltp(_91897), NewValue)|_] ) :- !.
ssa( pproj(_91881, _91882), NewValue, [set(pproj(_91881, _91882), NewValue)|_] ) :- !.
ssa( lookahead(_91886, _91887, _91888, _91889), NewValue, [set(lookahead(_91886, _91887, _91888, _91889), NewValue)|_] ) :- !.
ssa( pll(_91874, _91875, _91876, _91877), NewValue, [set(pll(_91874, _91875, _91876, _91877), NewValue)|_] ) :- !.
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
ssa( epf_known_wumpus_pos, NewValue, [set(epf_known_wumpus_pos, NewValue)|_] ) :- !.
ssa( epf_num_facedShades, NewValue, [set(epf_num_facedShades, NewValue)|_] ) :- !.
ssa( epf_num_visited, NewValue, [set(epf_num_visited, NewValue)|_] ) :- !.
ssa( epf_sense_breeze, NewValue, [set(epf_sense_breeze, NewValue)|_] ) :- !.
ssa( epf_sense_gold, NewValue, [set(epf_sense_gold, NewValue)|_] ) :- !.
ssa( epf_sense_stench, NewValue, [set(epf_sense_stench, NewValue)|_] ) :- !.
ssa( epf_sure_to_hit, NewValue, [set(epf_sure_to_hit, NewValue)|_] ) :- !.
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
ssa( epf_known_wumpus_pos, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_known_wumpus_pos, NewValue, Srest).
ssa( epf_num_facedShades, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_num_facedShades, NewValue, Srest).
ssa( epf_num_visited, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_num_visited, NewValue, Srest).
ssa( epf_sense_breeze, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_sense_breeze, NewValue, Srest).
ssa( epf_sense_gold, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_sense_gold, NewValue, Srest).
ssa( epf_sense_stench, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_sense_stench, NewValue, Srest).
ssa( epf_sure_to_hit, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_sure_to_hit, NewValue, Srest).
ssa( epf_wumpus_alive, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_wumpus_alive, NewValue, Srest).
ssa( epf_wumpus_pos, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_wumpus_pos, NewValue, Srest).
ssa( action, NewValue, [_UnknownAction|Srest] ) :- !, has_val(action, NewValue, Srest).
ssa( eval_exog_functions, NewValue, [_UnknownAction|Srest] ) :- !, has_val(eval_exog_functions, NewValue, Srest).
ssa( eval_registers, NewValue, [_UnknownAction|Srest] ) :- !, has_val(eval_registers, NewValue, Srest).
ssa( online, NewValue, [_UnknownAction|Srest] ) :- !, has_val(online, NewValue, Srest).
ssa( start, NewValue, [_UnknownAction|Srest] ) :- !, has_val(start, NewValue, Srest).
ssa( useAbstraction, NewValue, [_UnknownAction|Srest] ) :- !, has_val(useAbstraction, NewValue, Srest).
ssa( bel(_91893), NewValue, [_UnknownAction|Srest] ) :- !, has_val(bel(_91893), NewValue, Srest).
ssa( ltp(_91897), NewValue, [_UnknownAction|Srest] ) :- !, has_val(ltp(_91897), NewValue, Srest).
ssa( pproj(_91881, _91882), NewValue, [_UnknownAction|Srest] ) :- !, has_val(pproj(_91881, _91882), NewValue, Srest).
ssa( lookahead(_91886, _91887, _91888, _91889), NewValue, [_UnknownAction|Srest] ) :- !, has_val(lookahead(_91886, _91887, _91888, _91889), NewValue, Srest).
ssa( pll(_91874, _91875, _91876, _91877), NewValue, [_UnknownAction|Srest] ) :- !, has_val(pll(_91874, _91875, _91876, _91877), NewValue, Srest).
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
ssa( epf_known_wumpus_pos, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_known_wumpus_pos, NewValue, Srest).
ssa( epf_num_facedShades, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_num_facedShades, NewValue, Srest).
ssa( epf_num_visited, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_num_visited, NewValue, Srest).
ssa( epf_sense_breeze, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_sense_breeze, NewValue, Srest).
ssa( epf_sense_gold, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_sense_gold, NewValue, Srest).
ssa( epf_sense_stench, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_sense_stench, NewValue, Srest).
ssa( epf_sure_to_hit, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_sure_to_hit, NewValue, Srest).
ssa( epf_wumpus_alive, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_wumpus_alive, NewValue, Srest).
ssa( epf_wumpus_pos, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_wumpus_pos, NewValue, Srest).
prolog_poss( send(_R, _93322) ).
prolog_poss( send(_R, _93322), S ) :- true.
prolog_poss( reply(_R, _93331) ).
prolog_poss( reply(_R, _93331), S ) :- true.
prolog_poss( setOnline ).
prolog_poss( setOnline, S ) :- true.
prolog_poss( clipOnline ).
prolog_poss( clipOnline, S ) :- true.
prolog_poss( setTime(_93349) ).
prolog_poss( setTime(_93349), S ) :- true.
prolog_poss( clipAbstraction ).
prolog_poss( clipAbstraction, S ) :- true.
prolog_poss( setAbstraction ).
prolog_poss( setAbstraction, S ) :- true.
prolog_poss( set(_Fluent, _Value) ).
prolog_poss( set(_Fluent, _Value), S ) :- true.
prolog_poss( exogf_Update ).
prolog_poss( exogf_Update, S ) :- true.
prolog_poss( go_forward ).
prolog_poss( go_forward, S ) :- (has_val(epf_agent_pos, PreVar1685, S), PreVar1685=[PreVar1690, PreVar1691]), is_pos(PreVar1690, PreVar1691), (has_val(epf_wumpus_alive, PreVar1696, S), PreVar1696 = PreVar1695), (has_val(epf_cells_know_no_pit, PreVar1703, S), PreVar1703 = PreVar1702), (has_val(epf_cells_know_no_wumpus, PreVar1710, S), PreVar1710 = PreVar1709), (has_val(epf_agent_direction, PreVar1717, S), PreVar1717 = PreVar1716), PreVar1716=east, (PreVar1727 is PreVar1690 + (1), PreVar1726 is PreVar1727), is_pos(PreVar1726, PreVar1691), not is_wall(PreVar1690, PreVar1691, PreVar1726, PreVar1691), member([PreVar1726, PreVar1691], PreVar1702), (member([PreVar1726, PreVar1691], PreVar1709) ; PreVar1695=false) ; (has_val(epf_agent_pos, PreVar1750, S), PreVar1750=[PreVar1755, PreVar1756]), is_pos(PreVar1755, PreVar1756), (has_val(epf_wumpus_alive, PreVar1761, S), PreVar1761 = PreVar1760), (has_val(epf_cells_know_no_pit, PreVar1768, S), PreVar1768 = PreVar1767), (has_val(epf_cells_know_no_wumpus, PreVar1775, S), PreVar1775 = PreVar1774), (has_val(epf_agent_direction, PreVar1782, S), PreVar1782 = PreVar1781), PreVar1781=south, (PreVar1792 is PreVar1756 - (1), PreVar1791 is PreVar1792), is_pos(PreVar1755, PreVar1791), not is_wall(PreVar1755, PreVar1756, PreVar1755, PreVar1791), member([PreVar1755, PreVar1791], PreVar1767), (member([PreVar1755, PreVar1791], PreVar1774) ; PreVar1760=false) ; (has_val(epf_agent_pos, PreVar1815, S), PreVar1815=[PreVar1820, PreVar1821]), is_pos(PreVar1820, PreVar1821), (has_val(epf_wumpus_alive, PreVar1826, S), PreVar1826 = PreVar1825), (has_val(epf_cells_know_no_pit, PreVar1833, S), PreVar1833 = PreVar1832), (has_val(epf_cells_know_no_wumpus, PreVar1840, S), PreVar1840 = PreVar1839), (has_val(epf_agent_direction, PreVar1847, S), PreVar1847 = PreVar1846), PreVar1846=west, (PreVar1857 is PreVar1820 - (1), PreVar1856 is PreVar1857), is_pos(PreVar1856, PreVar1821), not is_wall(PreVar1820, PreVar1821, PreVar1856, PreVar1821), member([PreVar1856, PreVar1821], PreVar1832), (member([PreVar1856, PreVar1821], PreVar1839) ; PreVar1825=false) ; (has_val(epf_agent_pos, PreVar1880, S), PreVar1880=[PreVar1885, PreVar1886]), is_pos(PreVar1885, PreVar1886), (has_val(epf_wumpus_alive, PreVar1891, S), PreVar1891 = PreVar1890), (has_val(epf_cells_know_no_pit, PreVar1898, S), PreVar1898 = PreVar1897), (has_val(epf_cells_know_no_wumpus, PreVar1905, S), PreVar1905 = PreVar1904), (has_val(epf_agent_direction, PreVar1912, S), PreVar1912 = PreVar1911), PreVar1911=north, (PreVar1922 is PreVar1886 + (1), PreVar1921 is PreVar1922), is_pos(PreVar1885, PreVar1921), not is_wall(PreVar1885, PreVar1886, PreVar1885, PreVar1921), member([PreVar1885, PreVar1921], PreVar1897), (member([PreVar1885, PreVar1921], PreVar1904) ; PreVar1890=false).
prolog_poss( turn_right ).
prolog_poss( turn_right, S ) :- true.
prolog_poss( turn_left ).
prolog_poss( turn_left, S ) :- true.
prolog_poss( shoot ).
prolog_poss( shoot, S ) :- (has_val(epf_agent_arrow, PreVar1945, S), PreVar1945=true), has_val(epf_sure_to_hit, PreVar1952, S), PreVar1952=true.
prolog_poss( rest ).
prolog_poss( rest, S ) :- (has_val(epf_agent_pos, PreVar1959, S), PreVar1959=[PreVar1964, PreVar1965]), is_pos(PreVar1964, PreVar1965).
prolog_poss( pickup_gold ).
prolog_poss( pickup_gold, S ) :- (has_val(epf_agent_pos, PreVar1970, S), PreVar1970=[PreVar1975, PreVar1976]), is_pos(PreVar1975, PreVar1976), getval(real_gold_pos, [PreVar1981, PreVar1982]), PreVar1975 =:= PreVar1981, PreVar1976 =:= PreVar1982, not has_val(epf_carry_gold, true, S).
prolog_poss( noop ).
prolog_poss( noop, S ) :- true.
prolog_poss( distribute_pits(_Seed) ).
prolog_poss( distribute_pits(_Seed), S ) :- true.
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
