/*******************************
* file generated from ReadyLog *
*******************************/

:- pragma(nodebug).
prolog_function( f_is_pos(PreVar4, PreVar12) ).
prolog_function( f_is_pos(PreVar4, PreVar12), PreVar20, S ) :- world(PreVar0, PreVar1, PreVar2, PreVar3), PreVar4 >= PreVar0, PreVar4 =< PreVar2, PreVar12 >= PreVar1, PreVar12 =< PreVar3 -> PreVar20=true ; PreVar20=false.
prolog_function( f_is_wall(PreVar28, PreVar29, PreVar30, PreVar31) ).
prolog_function( f_is_wall(PreVar28, PreVar29, PreVar30, PreVar31), PreVar36, S ) :- (wall(PreVar28, PreVar29, PreVar30, PreVar31) ; wall(PreVar30, PreVar31, PreVar28, PreVar29)) -> PreVar36=true ; PreVar36=false.
prolog_function( reward ).
prolog_function( reward, PreVar53, S ) :- has_val(pos, PreVar46, S), PreVar46=[5, 5] -> PreVar53=100 ; PreVar53 = -1.
prolog_function( reward(PreVar68, PreVar69) ).
prolog_function( reward(PreVar68, PreVar69), PreVar70, S ) :- has_val(pos, PreVar63, S), PreVar63=[PreVar68, PreVar69] -> PreVar70=100 ; PreVar70 = -1.
prolog_function( reward_heuristic(PreVar85, PreVar86) ).
prolog_function( reward_heuristic(PreVar85, PreVar86), PreVar87, S ) :- has_val(pos, PreVar80, S), PreVar80=[PreVar85, PreVar86] -> PreVar87=100 ; (has_val(pos, PreVar93, S), PreVar93=[PreVar98, PreVar99]), (PreVar103 is PreVar85 - PreVar98, PreVar100 = PreVar103), (PreVar100>0 -> PreVar113 is -(PreVar100), PreVar110 = PreVar113 ; PreVar110 = PreVar100), (PreVar122 is PreVar86 - PreVar99, PreVar119 = PreVar122), (PreVar119>0 -> PreVar132 is -(PreVar119), PreVar129 = PreVar132 ; PreVar129 = PreVar119), PreVar141 is PreVar110 + PreVar129, PreVar87 = PreVar141.
prolog_function( reward_search_item ).
prolog_function( reward_search_item, PreVar144, S ) :- has_val(num_visited, PreVar147, S), PreVar144 = PreVar147.
prolog_function( f_sense_item ).
prolog_function( f_sense_item, PreVar152, S ) :- has_val(epf_sense_item, PreVar153, S), PreVar153 = PreVar152.
prolog_function( f_item_pos_by_id(PreVar160) ).
prolog_function( f_item_pos_by_id(PreVar160), PreVar207, S ) :- (PreVar159 is PreVar160 - (5), PreVar158 is PreVar159), (PreVar163 is PreVar160 - (4), PreVar162 is PreVar163), (PreVar167 is PreVar158 mod 3, PreVar166 is PreVar167), (PreVar171 is PreVar162 div 3, PreVar170 is PreVar171), (PreVar166=2 -> PreVar178 = -1 ; PreVar178 = PreVar166), PreVar186 = PreVar170, (has_val(pos, PreVar192, S), PreVar192=[PreVar197, PreVar198]), (PreVar200 is PreVar197 + PreVar178, PreVar199 is PreVar200), (PreVar204 is PreVar198 + PreVar186, PreVar203 is PreVar204), PreVar207=[PreVar199, PreVar203].
prolog_function( reward_interpretation ).
prolog_function( reward_interpretation, PreVar249, S ) :- (has_val(interpretation_count, PreVar216, S), PreVar213 = PreVar216), writeln(PreVar213), (has_val(assumed_action, PreVar222, S), PreVar222=nil -> PreVar227=0 ; PreVar227=1), (has_val(assumed_objects, PreVar235, S), length(PreVar235, PreVar239)), (PreVar241 is PreVar239 + PreVar227, PreVar240 is PreVar241), (has_val(assumed_arguments, PreVar244, S), length(PreVar244, PreVar248)), ((PreVar252 is (2) * PreVar248, PreVar250 is PreVar240 + PreVar252), PreVar249 is PreVar250), writeln(PreVar249), writeln(----).
prolog_function( process_utterance(PreVar255) ).
prolog_function( process_utterance(PreVar255), PreVar256, S ) :- process_utterance_external(PreVar255, PreVar256).
prolog_function( split_essence(PreVar257) ).
prolog_function( split_essence(PreVar257), PreVar258, S ) :- split_essence_external(PreVar257, PreVar258).
prolog_function( form_command(PreVar259, PreVar261) ).
prolog_function( form_command(PreVar259, PreVar261), PreVar263, S ) :- skill(PreVar259, PreVar260), substitute_arguments(PreVar261, PreVar260, PreVar263).
prolog_function( adl(PreVar264, PreVar265) ).
prolog_function( adl(PreVar264, PreVar265), PreVar266, S ) :- append(PreVar264, PreVar265, PreVar266).
stoch_proc(right_det).
stoch_proc(left_det).
stoch_proc(down_det).
stoch_proc(up_det).
stoch_proc(right_st).
stoch_proc(left_st).
stoch_proc(down_st).
stoch_proc(up_st).
stoch_proc_outcomes(right_det, S, Outcomes, SenseEffect) :- Outcomes=[([go_right], 1.0, true)], SenseEffect = [], !.
stoch_proc_outcomes(left_det, S, Outcomes, SenseEffect) :- Outcomes=[([go_left], 1.0, true)], SenseEffect = [], !.
stoch_proc_outcomes(down_det, S, Outcomes, SenseEffect) :- Outcomes=[([go_down], 1.0, true)], SenseEffect = [], !.
stoch_proc_outcomes(up_det, S, Outcomes, SenseEffect) :- Outcomes=[([go_up], 1.0, true)], SenseEffect = [], !.
stoch_proc_outcomes(right_st, S, Outcomes, SenseEffect) :- (has_val(pos, PreVar269, S), PreVar269=[PreVar274, PreVar275]), (PreVar277 is PreVar274 + (1), PreVar276 is PreVar277), Outcomes=[(go_right, 0.5, pos=[PreVar276, PreVar275]), (noop, 0.5, pos=[PreVar274, PreVar275])], SenseEffect=[exogf_Update], !.
stoch_proc_outcomes(left_st, S, Outcomes, SenseEffect) :- (has_val(pos, PreVar282, S), PreVar282=[PreVar287, PreVar288]), (PreVar290 is PreVar287 - (1), PreVar289 is PreVar290), Outcomes=[(go_left, 0.5, pos=[PreVar289, PreVar288]), (noop, 0.5, pos=[PreVar287, PreVar288])], SenseEffect=[exogf_Update], !.
stoch_proc_outcomes(down_st, S, Outcomes, SenseEffect) :- (has_val(pos, PreVar295, S), PreVar295=[PreVar300, PreVar301]), (PreVar303 is PreVar301 - (1), PreVar302 is PreVar303), Outcomes=[(go_down, 0.5, pos=[PreVar300, PreVar302]), (noop, 0.5, pos=[PreVar300, PreVar301])], SenseEffect=[exogf_Update], !.
stoch_proc_outcomes(up_st, S, Outcomes, SenseEffect) :- (has_val(pos, PreVar308, S), PreVar308=[PreVar313, PreVar314]), (PreVar316 is PreVar314 + (1), PreVar315 is PreVar316), Outcomes=[(go_up, 0.5, pos=[PreVar313, PreVar315]), (noop, 0.5, pos=[PreVar313, PreVar314])], SenseEffect=[exogf_Update], !.
stoch_proc_poss(right_det, S) :- true, !.
stoch_proc_poss(left_det, S) :- (has_val(pos, PreVar321, S), PreVar321=[PreVar326, PreVar327]), is_pos(PreVar326, PreVar327), (PreVar331 is PreVar326 - (1), PreVar330 is PreVar331), is_pos(PreVar330, PreVar327), not is_wall(PreVar326, PreVar327, PreVar330, PreVar327), !.
stoch_proc_poss(down_det, S) :- (has_val(pos, PreVar342, S), PreVar342=[PreVar347, PreVar348]), is_pos(PreVar347, PreVar348), (PreVar352 is PreVar348 - (1), PreVar351 is PreVar352), is_pos(PreVar347, PreVar351), not is_wall(PreVar347, PreVar348, PreVar347, PreVar351), !.
stoch_proc_poss(up_det, S) :- (has_val(pos, PreVar363, S), PreVar363=[PreVar368, PreVar369]), is_pos(PreVar368, PreVar369), (PreVar373 is PreVar369 + (1), PreVar372 is PreVar373), is_pos(PreVar368, PreVar372), not is_wall(PreVar368, PreVar369, PreVar368, PreVar372), !.
stoch_proc_poss(right_st, S) :- (has_val(pos, PreVar384, S), PreVar384=[PreVar389, PreVar390]), is_pos(PreVar389, PreVar390), (PreVar394 is PreVar389 + (1), PreVar393 is PreVar394), is_pos(PreVar393, PreVar390), not is_wall(PreVar389, PreVar390, PreVar393, PreVar390), !.
stoch_proc_poss(left_st, S) :- (has_val(pos, PreVar405, S), PreVar405=[PreVar410, PreVar411]), is_pos(PreVar410, PreVar411), (PreVar415 is PreVar410 - (1), PreVar414 is PreVar415), is_pos(PreVar414, PreVar411), not is_wall(PreVar410, PreVar411, PreVar414, PreVar411), !.
stoch_proc_poss(down_st, S) :- (has_val(pos, PreVar426, S), PreVar426=[PreVar431, PreVar432]), is_pos(PreVar431, PreVar432), (PreVar436 is PreVar432 - (1), PreVar435 is PreVar436), is_pos(PreVar431, PreVar435), not is_wall(PreVar431, PreVar432, PreVar431, PreVar435), !.
stoch_proc_poss(up_st, S) :- (has_val(pos, PreVar447, S), PreVar447=[PreVar452, PreVar453]), is_pos(PreVar452, PreVar453), (PreVar457 is PreVar453 + (1), PreVar456 is PreVar457), is_pos(PreVar452, PreVar456), not is_wall(PreVar452, PreVar453, PreVar452, PreVar456), !.
stoch_proc_costs(right_det, 1, S) :- true, !.
stoch_proc_costs(left_det, 1, S) :- true, !.
stoch_proc_costs(down_det, 1, S) :- true, !.
stoch_proc_costs(up_det, 1, S) :- true, !.
stoch_proc_costs(right_st, 1, S) :- true, !.
stoch_proc_costs(left_st, 1, S) :- true, !.
stoch_proc_costs(down_st, 1, S) :- true, !.
stoch_proc_costs(up_st, 1, S) :- true, !.
event_aux(e_loose_item).
event_outcomes(e_loose_item, S, Outcomes, SenseEffect) :- (has_val(epf_carry_item, PreVar469, S), PreVar466 = PreVar469), Outcomes=[(set(epf_carry_item, false), 0.3, epf_carry_item=false), ([], 0.7, epf_carry_item=true)], SenseEffect=[exogf_Update], !.
prolog_event_poss(e_loose_item, S) :- getval(agent_loose_item_enabled, true), has_val(epf_carry_item, PreVar477, S), PreVar477=true, !.
ssa( online, false, [clipOnline|Srest] ) :- !, true.
ssa( epf_carry_item, PreVar482, [drop_item|Srest] ) :- !, PreVar482=false.
ssa( num_visited, PreVar508, [go_down|Srest] ) :- !, (has_val(pos, PreVar488, Srest), PreVar488=[PreVar493, PreVar494]), PreVar495 = PreVar493, (PreVar500 is PreVar494 - (1), PreVar499 is PreVar500), (has_val(visited(PreVar495, PreVar499), true, Srest) -> has_val(num_visited, PreVar511, Srest), PreVar508 = PreVar511 ; (has_val(num_visited, PreVar517, Srest), PreVar516 is PreVar517 + (1)), PreVar508 is PreVar516).
ssa( pos, PreVar535, [go_down|Srest] ) :- !, (has_val(pos, PreVar524, Srest), PreVar524=[PreVar529, PreVar530]), (PreVar532 is PreVar530 - (1), PreVar531 is PreVar532), PreVar535=[PreVar529, PreVar531].
ssa( visited(PreVar550, PreVar554), true, [go_down|Srest] ) :- !, (has_val(pos, PreVar543, Srest), PreVar543=[PreVar548, PreVar549]), PreVar550 = PreVar548, PreVar555 is PreVar549 - (1), PreVar554 is PreVar555.
ssa( num_visited, PreVar580, [go_down_st|Srest] ) :- !, (has_val(pos, PreVar560, Srest), PreVar560=[PreVar565, PreVar566]), PreVar567 = PreVar565, (PreVar572 is PreVar566 - (1), PreVar571 is PreVar572), (has_val(visited(PreVar567, PreVar571), true, Srest) -> has_val(num_visited, PreVar581, Srest), PreVar580 is PreVar581 ; (has_val(num_visited, PreVar587, Srest), PreVar586 is PreVar587 + (1)), PreVar580 is PreVar586).
ssa( pos, PreVar607, [go_down_st|Srest] ) :- !, (has_val(pos, PreVar594, Srest), PreVar594=[PreVar599, PreVar600]), (PreVar604 is PreVar600 - (1), PreVar601 = PreVar604), PreVar607=[PreVar599, PreVar601].
ssa( visited(PreVar622, PreVar626), true, [go_down_st|Srest] ) :- !, (has_val(pos, PreVar615, Srest), PreVar615=[PreVar620, PreVar621]), PreVar622 = PreVar620, PreVar627 is PreVar621 - (1), PreVar626 is PreVar627.
ssa( num_visited, PreVar652, [go_left|Srest] ) :- !, (has_val(pos, PreVar632, Srest), PreVar632=[PreVar637, PreVar638]), (PreVar640 is PreVar637 - (1), PreVar639 is PreVar640), PreVar643 = PreVar638, (has_val(visited(PreVar639, PreVar643), true, Srest) -> has_val(num_visited, PreVar655, Srest), PreVar652 = PreVar655 ; (has_val(num_visited, PreVar661, Srest), PreVar660 is PreVar661 + (1)), PreVar652 is PreVar660).
ssa( pos, PreVar679, [go_left|Srest] ) :- !, (has_val(pos, PreVar668, Srest), PreVar668=[PreVar673, PreVar674]), (PreVar676 is PreVar673 - (1), PreVar675 is PreVar676), PreVar679=[PreVar675, PreVar674].
ssa( visited(PreVar694, PreVar698), true, [go_left|Srest] ) :- !, (has_val(pos, PreVar687, Srest), PreVar687=[PreVar692, PreVar693]), (PreVar695 is PreVar692 - (1), PreVar694 is PreVar695), PreVar698 = PreVar693.
ssa( num_visited, PreVar724, [go_left_st|Srest] ) :- !, (has_val(pos, PreVar704, Srest), PreVar704=[PreVar709, PreVar710]), (PreVar712 is PreVar709 - (1), PreVar711 is PreVar712), PreVar715 = PreVar710, (has_val(visited(PreVar711, PreVar715), true, Srest) -> has_val(num_visited, PreVar725, Srest), PreVar724 is PreVar725 ; (has_val(num_visited, PreVar731, Srest), PreVar730 is PreVar731 + (1)), PreVar724 is PreVar730).
ssa( pos, PreVar751, [go_left_st|Srest] ) :- !, (has_val(pos, PreVar738, Srest), PreVar738=[PreVar743, PreVar744]), (PreVar748 is PreVar743 - (1), PreVar745 = PreVar748), PreVar751=[PreVar745, PreVar744].
ssa( visited(PreVar766, PreVar770), true, [go_left_st|Srest] ) :- !, (has_val(pos, PreVar759, Srest), PreVar759=[PreVar764, PreVar765]), (PreVar767 is PreVar764 - (1), PreVar766 is PreVar767), PreVar770 = PreVar765.
ssa( num_visited, PreVar796, [go_right|Srest] ) :- !, (has_val(pos, PreVar776, Srest), PreVar776=[PreVar781, PreVar782]), (PreVar784 is PreVar781 + (1), PreVar783 is PreVar784), PreVar787 = PreVar782, (has_val(visited(PreVar783, PreVar787), true, Srest) -> has_val(num_visited, PreVar799, Srest), PreVar796 = PreVar799 ; (has_val(num_visited, PreVar805, Srest), PreVar804 is PreVar805 + (1)), PreVar796 is PreVar804).
ssa( pos, PreVar823, [go_right|Srest] ) :- !, (has_val(pos, PreVar812, Srest), PreVar812=[PreVar817, PreVar818]), (PreVar820 is PreVar817 + (1), PreVar819 is PreVar820), PreVar823=[PreVar819, PreVar818].
ssa( visited(PreVar838, PreVar842), true, [go_right|Srest] ) :- !, (has_val(pos, PreVar831, Srest), PreVar831=[PreVar836, PreVar837]), (PreVar839 is PreVar836 + (1), PreVar838 is PreVar839), PreVar842 = PreVar837.
ssa( num_visited, PreVar868, [go_right_st|Srest] ) :- !, (has_val(pos, PreVar848, Srest), PreVar848=[PreVar853, PreVar854]), (PreVar856 is PreVar853 + (1), PreVar855 is PreVar856), PreVar859 = PreVar854, (has_val(visited(PreVar855, PreVar859), true, Srest) -> has_val(num_visited, PreVar869, Srest), PreVar868 is PreVar869 ; (has_val(num_visited, PreVar875, Srest), PreVar874 is PreVar875 + (1)), PreVar868 is PreVar874).
ssa( pos, PreVar895, [go_right_st|Srest] ) :- !, (has_val(pos, PreVar882, Srest), PreVar882=[PreVar887, PreVar888]), (PreVar892 is PreVar887 + (1), PreVar889 = PreVar892), PreVar895=[PreVar889, PreVar888].
ssa( visited(PreVar910, PreVar914), true, [go_right_st|Srest] ) :- !, (has_val(pos, PreVar903, Srest), PreVar903=[PreVar908, PreVar909]), (PreVar911 is PreVar908 + (1), PreVar910 is PreVar911), PreVar914 = PreVar909.
ssa( num_visited, PreVar940, [go_up|Srest] ) :- !, (has_val(pos, PreVar920, Srest), PreVar920=[PreVar925, PreVar926]), PreVar927 = PreVar925, (PreVar932 is PreVar926 + (1), PreVar931 is PreVar932), (has_val(visited(PreVar927, PreVar931), true, Srest) -> has_val(num_visited, PreVar943, Srest), PreVar940 = PreVar943 ; (has_val(num_visited, PreVar949, Srest), PreVar948 is PreVar949 + (1)), PreVar940 is PreVar948).
ssa( pos, PreVar967, [go_up|Srest] ) :- !, (has_val(pos, PreVar956, Srest), PreVar956=[PreVar961, PreVar962]), (PreVar964 is PreVar962 + (1), PreVar963 is PreVar964), PreVar967=[PreVar961, PreVar963].
ssa( visited(PreVar982, PreVar986), true, [go_up|Srest] ) :- !, (has_val(pos, PreVar975, Srest), PreVar975=[PreVar980, PreVar981]), PreVar982 = PreVar980, PreVar987 is PreVar981 + (1), PreVar986 is PreVar987.
ssa( num_visited, PreVar1012, [go_up_st|Srest] ) :- !, (has_val(pos, PreVar992, Srest), PreVar992=[PreVar997, PreVar998]), PreVar999 = PreVar997, (PreVar1004 is PreVar998 + (1), PreVar1003 is PreVar1004), (has_val(visited(PreVar999, PreVar1003), true, Srest) -> has_val(num_visited, PreVar1013, Srest), PreVar1012 is PreVar1013 ; (has_val(num_visited, PreVar1019, Srest), PreVar1018 is PreVar1019 + (1)), PreVar1012 is PreVar1018).
ssa( pos, PreVar1039, [go_up_st|Srest] ) :- !, (has_val(pos, PreVar1026, Srest), PreVar1026=[PreVar1031, PreVar1032]), (PreVar1036 is PreVar1032 + (1), PreVar1033 = PreVar1036), PreVar1039=[PreVar1031, PreVar1033].
ssa( visited(PreVar1054, PreVar1058), true, [go_up_st|Srest] ) :- !, (has_val(pos, PreVar1047, Srest), PreVar1047=[PreVar1052, PreVar1053]), PreVar1054 = PreVar1052, PreVar1059 is PreVar1053 + (1), PreVar1058 is PreVar1059.
ssa( epf_carry_item, PreVar1062, [pickup_item|Srest] ) :- !, PreVar1062=true.
ssa( new_user_utter, PreVar1066, [processed_utterance|Srest] ) :- !, PreVar1066=false.
ssa( assumed_action, PreVar1070, [reject|Srest] ) :- !, PreVar1070=nil.
ssa( assumed_arguments, PreVar1074, [reject|Srest] ) :- !, PreVar1074 = [].
ssa( assumed_objects, PreVar1078, [reject|Srest] ) :- !, PreVar1078 = [].
ssa( finished_action, PreVar1082, [reject|Srest] ) :- !, PreVar1082=true.
ssa( finished_assignments, PreVar1086, [reject|Srest] ) :- !, PreVar1086=true.
ssa( finished_objects, PreVar1090, [reject|Srest] ) :- !, PreVar1090=true.
ssa( interpretation_count, PreVar1094, [reject|Srest] ) :- !, PreVar1094=0.
ssa( visited(_31031, _31032), PreVar1098, [reset_visited|Srest] ) :- !, PreVar1098=false.
ssa( online, true, [setOnline|Srest] ) :- !, true.
ssa( assumed_objects, NewValue, [assign_argument(_P)|Srest] ) :- !, has_val(assumed_objects, PreVar1104, Srest), PreVar1104=[PreVar1109|NewValue].
ssa( assumed_arguments, PreVar1127, [assign_argument(PreVar1132)|Srest] ) :- !, (has_val(assumed_arguments, PreVar1113, Srest), PreVar1110 = PreVar1113), (has_val(assumed_objects, PreVar1119, Srest), PreVar1119=[[PreVar1125, PreVar1126]|L2]), PreVar1127=[[PreVar1132, PreVar1126]|PreVar1110].
ssa( finished_assignments, PreVar1143, [assign_argument(_P)|Srest] ) :- !, (has_val(assumed_objects, PreVar1137, Srest), PreVar1134 = PreVar1137), (length(PreVar1134, 1) -> PreVar1143=true ; PreVar1143=false).
ssa( interpretation_count, PreVar1171, [assign_argument(PreVar1169)|Srest] ) :- !, (has_val(assumed_action, PreVar1153, Srest), PreVar1153 = PreVar1152), (has_val(assumed_objects, PreVar1160, Srest), PreVar1160=[[PreVar1166, PreVar1167]|L2]), (preposition(PreVar1152, PreVar1169, PreVar1166) -> (has_val(interpretation_count, PreVar1173, Srest), PreVar1172 is PreVar1173 + (2)), PreVar1171 is PreVar1172 ; (has_val(interpretation_count, PreVar1180, Srest), PreVar1179 is PreVar1180 + (1)), PreVar1171 is PreVar1179).
ssa( new_user_utter, PreVar1185, [hear(_31070)|Srest] ) :- !, PreVar1185=true.
ssa( last_user_utterance, PreVar1189, [hear(PreVar1190)|Srest] ) :- !, PreVar1189 = PreVar1190.
ssa( verbphrases, NewValue, [init_ip(_31555)|Srest] ) :- !, has_val(verbphrases, PreVar1195, Srest), PreVar1195=[PreVar1200|NewValue].
ssa( vp_finished, PreVar1210, [init_ip(_31574)|Srest] ) :- !, (has_val(verbphrases, PreVar1203, Srest), PreVar1203 = PreVar1202), (length(PreVar1202, 1) -> PreVar1210=true ; PreVar1210=false).
ssa( assumed_action, PreVar1218, [init_ip(_31611)|Srest] ) :- !, PreVar1218=nil.
ssa( assumed_objects, PreVar1222, [init_ip(_31628)|Srest] ) :- !, PreVar1222 = [].
ssa( assumed_arguments, PreVar1226, [init_ip(_31645)|Srest] ) :- !, PreVar1226 = [].
ssa( interpretation_count, PreVar1230, [init_ip(_31662)|Srest] ) :- !, PreVar1230=0.
ssa( clarification_count, PreVar1234, [init_ip(_31679)|Srest] ) :- !, PreVar1234=0.
ssa( finished_action, PreVar1238, [init_ip(_31696)|Srest] ) :- !, PreVar1238=false.
ssa( finished_objects, PreVar1242, [init_ip(_31713)|Srest] ) :- !, PreVar1242=false.
ssa( finished_assignments, PreVar1246, [init_ip(_31730)|Srest] ) :- !, PreVar1246=false.
ssa( spoken_verb, PreVar1250, [init_ip([[PreVar1251|_31509]|_31507])|Srest] ) :- !, PreVar1250 = PreVar1251.
ssa( spoken_objects, PreVar1254, [init_ip([[_31530, [objects, PreVar1255]]|_31529])|Srest] ) :- !, PreVar1254 = PreVar1255.
ssa( verbphrases, PreVar1258, [init_vp(PreVar1259)|Srest] ) :- !, PreVar1258 = PreVar1259.
ssa( vp_finished, PreVar1262, [init_vp(_31488)|Srest] ) :- !, PreVar1262=false.
ssa( spoken_verb, PreVar1266, [interpret_action(_S)|Srest] ) :- !, PreVar1266=nil.
ssa( assumed_action, PreVar1270, [interpret_action(PreVar1271)|Srest] ) :- !, PreVar1270 = PreVar1271.
ssa( finished_action, PreVar1274, [interpret_action(_S)|Srest] ) :- !, PreVar1274=true.
ssa( interpretation_count, PreVar1278, [interpret_action(_S)|Srest] ) :- !, (has_val(interpretation_count, PreVar1280, Srest), PreVar1279 is PreVar1280 + (1)), PreVar1278 is PreVar1279.
ssa( assumed_objects, PreVar1304, [interpret_object(PreVar1310)|Srest] ) :- !, (has_val(spoken_objects, PreVar1288, Srest), [[PreVar1290, [PreVar1292, PreVar1293]]|L2] = PreVar1288), (has_val(assumed_objects, PreVar1300, Srest), PreVar1297 = PreVar1300), PreVar1304=[[PreVar1290, PreVar1310]|PreVar1297].
ssa( spoken_objects, NewValue, [interpret_object(_E)|Srest] ) :- !, has_val(spoken_objects, PreVar1313, Srest), PreVar1313=[PreVar1318|NewValue].
ssa( finished_objects, PreVar1328, [interpret_object(_E)|Srest] ) :- !, (has_val(spoken_objects, PreVar1322, Srest), PreVar1319 = PreVar1322), (length(PreVar1319, 1) -> PreVar1328=true ; PreVar1328=false).
ssa( interpretation_count, PreVar1336, [interpret_object(_E)|Srest] ) :- !, (has_val(interpretation_count, PreVar1338, Srest), PreVar1337 is PreVar1338 + (1)), PreVar1336 is PreVar1337.
ssa( start, NewValue, [setTime(NewValue)|Srest] ) :- !, true.
ssa( start, NewValue, [ccUpdate(_29808, NewValue)|Srest] ) :- !, true.
ssa( action, NewValue, [reply(action, NewValue)|Srest] ) :- !, true.
ssa( action, NewValue, [send(action, NewValue)|Srest] ) :- !, true.
ssa( Fluent, NewValue, [set(Fluent, NewValue)|Srest] ) :- !, true.
ssa( pos, PreVar1343, [teleport(PreVar1347, PreVar1348)|Srest] ) :- !, PreVar1343=[PreVar1347, PreVar1348].

/* 	___ dummy SSAs ___ */
ssa( action, NewValue, [set(action, NewValue)|_] ) :- !.
ssa( assumed_action, NewValue, [set(assumed_action, NewValue)|_] ) :- !.
ssa( assumed_arguments, NewValue, [set(assumed_arguments, NewValue)|_] ) :- !.
ssa( assumed_objects, NewValue, [set(assumed_objects, NewValue)|_] ) :- !.
ssa( clarification_count, NewValue, [set(clarification_count, NewValue)|_] ) :- !.
ssa( eval_exog_functions, NewValue, [set(eval_exog_functions, NewValue)|_] ) :- !.
ssa( eval_registers, NewValue, [set(eval_registers, NewValue)|_] ) :- !.
ssa( finished_action, NewValue, [set(finished_action, NewValue)|_] ) :- !.
ssa( finished_assignments, NewValue, [set(finished_assignments, NewValue)|_] ) :- !.
ssa( finished_objects, NewValue, [set(finished_objects, NewValue)|_] ) :- !.
ssa( interpretation_count, NewValue, [set(interpretation_count, NewValue)|_] ) :- !.
ssa( know_item_pos, NewValue, [set(know_item_pos, NewValue)|_] ) :- !.
ssa( last_user_utterance, NewValue, [set(last_user_utterance, NewValue)|_] ) :- !.
ssa( new_user_utter, NewValue, [set(new_user_utter, NewValue)|_] ) :- !.
ssa( num_visited, NewValue, [set(num_visited, NewValue)|_] ) :- !.
ssa( online, NewValue, [set(online, NewValue)|_] ) :- !.
ssa( spoken_objects, NewValue, [set(spoken_objects, NewValue)|_] ) :- !.
ssa( spoken_verb, NewValue, [set(spoken_verb, NewValue)|_] ) :- !.
ssa( start, NewValue, [set(start, NewValue)|_] ) :- !.
ssa( useAbstraction, NewValue, [set(useAbstraction, NewValue)|_] ) :- !.
ssa( verbphrases, NewValue, [set(verbphrases, NewValue)|_] ) :- !.
ssa( vp_finished, NewValue, [set(vp_finished, NewValue)|_] ) :- !.
ssa( bel(_73767), NewValue, [set(bel(_73767), NewValue)|_] ) :- !.
ssa( ltp(_73771), NewValue, [set(ltp(_73771), NewValue)|_] ) :- !.
ssa( pproj(_73755, _73756), NewValue, [set(pproj(_73755, _73756), NewValue)|_] ) :- !.
ssa( visited(_X, _Y), NewValue, [set(visited(_X, _Y), NewValue)|_] ) :- !.
ssa( lookahead(_73760, _73761, _73762, _73763), NewValue, [set(lookahead(_73760, _73761, _73762, _73763), NewValue)|_] ) :- !.
ssa( pll(_73748, _73749, _73750, _73751), NewValue, [set(pll(_73748, _73749, _73750, _73751), NewValue)|_] ) :- !.
ssa( epf_carry_item, NewValue, [set(epf_carry_item, NewValue)|_] ) :- !.
ssa( epf_item_pos, NewValue, [set(epf_item_pos, NewValue)|_] ) :- !.
ssa( epf_sense_item, NewValue, [set(epf_sense_item, NewValue)|_] ) :- !.
ssa( pos, NewValue, [set(pos, NewValue)|_] ) :- !.
ssa( epf_carry_item, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_carry_item, NewValue, Srest).
ssa( epf_item_pos, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_item_pos, NewValue, Srest).
ssa( epf_sense_item, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(epf_sense_item, NewValue, Srest).
ssa( pos, NewValue, [exogf_Update|Srest] ) :- !, exog_fluent_getValue(pos, NewValue, Srest).
ssa( action, NewValue, [_UnknownAction|Srest] ) :- !, has_val(action, NewValue, Srest).
ssa( assumed_action, NewValue, [_UnknownAction|Srest] ) :- !, has_val(assumed_action, NewValue, Srest).
ssa( assumed_arguments, NewValue, [_UnknownAction|Srest] ) :- !, has_val(assumed_arguments, NewValue, Srest).
ssa( assumed_objects, NewValue, [_UnknownAction|Srest] ) :- !, has_val(assumed_objects, NewValue, Srest).
ssa( clarification_count, NewValue, [_UnknownAction|Srest] ) :- !, has_val(clarification_count, NewValue, Srest).
ssa( eval_exog_functions, NewValue, [_UnknownAction|Srest] ) :- !, has_val(eval_exog_functions, NewValue, Srest).
ssa( eval_registers, NewValue, [_UnknownAction|Srest] ) :- !, has_val(eval_registers, NewValue, Srest).
ssa( finished_action, NewValue, [_UnknownAction|Srest] ) :- !, has_val(finished_action, NewValue, Srest).
ssa( finished_assignments, NewValue, [_UnknownAction|Srest] ) :- !, has_val(finished_assignments, NewValue, Srest).
ssa( finished_objects, NewValue, [_UnknownAction|Srest] ) :- !, has_val(finished_objects, NewValue, Srest).
ssa( interpretation_count, NewValue, [_UnknownAction|Srest] ) :- !, has_val(interpretation_count, NewValue, Srest).
ssa( know_item_pos, NewValue, [_UnknownAction|Srest] ) :- !, has_val(know_item_pos, NewValue, Srest).
ssa( last_user_utterance, NewValue, [_UnknownAction|Srest] ) :- !, has_val(last_user_utterance, NewValue, Srest).
ssa( new_user_utter, NewValue, [_UnknownAction|Srest] ) :- !, has_val(new_user_utter, NewValue, Srest).
ssa( num_visited, NewValue, [_UnknownAction|Srest] ) :- !, has_val(num_visited, NewValue, Srest).
ssa( online, NewValue, [_UnknownAction|Srest] ) :- !, has_val(online, NewValue, Srest).
ssa( spoken_objects, NewValue, [_UnknownAction|Srest] ) :- !, has_val(spoken_objects, NewValue, Srest).
ssa( spoken_verb, NewValue, [_UnknownAction|Srest] ) :- !, has_val(spoken_verb, NewValue, Srest).
ssa( start, NewValue, [_UnknownAction|Srest] ) :- !, has_val(start, NewValue, Srest).
ssa( useAbstraction, NewValue, [_UnknownAction|Srest] ) :- !, has_val(useAbstraction, NewValue, Srest).
ssa( verbphrases, NewValue, [_UnknownAction|Srest] ) :- !, has_val(verbphrases, NewValue, Srest).
ssa( vp_finished, NewValue, [_UnknownAction|Srest] ) :- !, has_val(vp_finished, NewValue, Srest).
ssa( bel(_73767), NewValue, [_UnknownAction|Srest] ) :- !, has_val(bel(_73767), NewValue, Srest).
ssa( ltp(_73771), NewValue, [_UnknownAction|Srest] ) :- !, has_val(ltp(_73771), NewValue, Srest).
ssa( pproj(_73755, _73756), NewValue, [_UnknownAction|Srest] ) :- !, has_val(pproj(_73755, _73756), NewValue, Srest).
ssa( visited(_X, _Y), NewValue, [_UnknownAction|Srest] ) :- !, has_val(visited(_X, _Y), NewValue, Srest).
ssa( lookahead(_73760, _73761, _73762, _73763), NewValue, [_UnknownAction|Srest] ) :- !, has_val(lookahead(_73760, _73761, _73762, _73763), NewValue, Srest).
ssa( pll(_73748, _73749, _73750, _73751), NewValue, [_UnknownAction|Srest] ) :- !, has_val(pll(_73748, _73749, _73750, _73751), NewValue, Srest).
ssa( epf_carry_item, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_carry_item, NewValue, Srest).
ssa( epf_item_pos, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_item_pos, NewValue, Srest).
ssa( epf_sense_item, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_sense_item, NewValue, Srest).
ssa( pos, NewValue, [_UnknownAction|Srest] ) :- !, has_val(pos, NewValue, Srest).
prolog_poss( send(_R, _74930) ).
prolog_poss( send(_R, _74930), S ) :- true.
prolog_poss( reply(_R, _74939) ).
prolog_poss( reply(_R, _74939), S ) :- true.
prolog_poss( setOnline ).
prolog_poss( setOnline, S ) :- true.
prolog_poss( clipOnline ).
prolog_poss( clipOnline, S ) :- true.
prolog_poss( setTime(_74957) ).
prolog_poss( setTime(_74957), S ) :- true.
prolog_poss( clipAbstraction ).
prolog_poss( clipAbstraction, S ) :- true.
prolog_poss( setAbstraction ).
prolog_poss( setAbstraction, S ) :- true.
prolog_poss( set(_Fluent, _Value) ).
prolog_poss( set(_Fluent, _Value), S ) :- true.
prolog_poss( exogf_Update ).
prolog_poss( exogf_Update, S ) :- true.
prolog_poss( say(_74989) ).
prolog_poss( say(_74989), S ) :- true.
prolog_poss( processed_utterance ).
prolog_poss( processed_utterance, S ) :- true.
prolog_poss( wait ).
prolog_poss( wait, S ) :- true.
prolog_poss( go_up ).
prolog_poss( go_up, S ) :- (has_val(pos, PreVar1351, S), PreVar1351=[PreVar1356, PreVar1357]), is_pos(PreVar1356, PreVar1357), (PreVar1361 is PreVar1357 + (1), PreVar1360 is PreVar1361), is_pos(PreVar1356, PreVar1360), not is_wall(PreVar1356, PreVar1357, PreVar1356, PreVar1360).
prolog_poss( go_down ).
prolog_poss( go_down, S ) :- (has_val(pos, PreVar1372, S), PreVar1372=[PreVar1377, PreVar1378]), is_pos(PreVar1377, PreVar1378), (PreVar1382 is PreVar1378 - (1), PreVar1381 is PreVar1382), is_pos(PreVar1377, PreVar1381), not is_wall(PreVar1377, PreVar1378, PreVar1377, PreVar1381).
prolog_poss( go_left ).
prolog_poss( go_left, S ) :- (has_val(pos, PreVar1393, S), PreVar1393=[PreVar1398, PreVar1399]), is_pos(PreVar1398, PreVar1399), (PreVar1403 is PreVar1398 - (1), PreVar1402 is PreVar1403), is_pos(PreVar1402, PreVar1399), not is_wall(PreVar1398, PreVar1399, PreVar1402, PreVar1399).
prolog_poss( go_right ).
prolog_poss( go_right, S ) :- (has_val(pos, PreVar1414, S), PreVar1414=[PreVar1419, PreVar1420]), is_pos(PreVar1419, PreVar1420), (PreVar1424 is PreVar1419 + (1), PreVar1423 is PreVar1424), is_pos(PreVar1423, PreVar1420), not is_wall(PreVar1419, PreVar1420, PreVar1423, PreVar1420).
prolog_poss( rest ).
prolog_poss( rest, S ) :- (has_val(pos, PreVar1435, S), PreVar1435=[PreVar1440, PreVar1441]), is_pos(PreVar1440, PreVar1441).
prolog_poss( go_up_st ).
prolog_poss( go_up_st, S ) :- (has_val(pos, PreVar1446, S), PreVar1446=[PreVar1451, PreVar1452]), is_pos(PreVar1451, PreVar1452), (PreVar1456 is PreVar1452 + (1), PreVar1455 is PreVar1456), is_pos(PreVar1451, PreVar1455), not is_wall(PreVar1451, PreVar1452, PreVar1451, PreVar1455).
prolog_poss( go_down ).
prolog_poss( go_down, S ) :- (has_val(pos, PreVar1467, S), PreVar1467=[PreVar1472, PreVar1473]), is_pos(PreVar1472, PreVar1473), (PreVar1477 is PreVar1473 - (1), PreVar1476 is PreVar1477), is_pos(PreVar1472, PreVar1476), not is_wall(PreVar1472, PreVar1473, PreVar1472, PreVar1476).
prolog_poss( go_left_st ).
prolog_poss( go_left_st, S ) :- (has_val(pos, PreVar1488, S), PreVar1488=[PreVar1493, PreVar1494]), is_pos(PreVar1493, PreVar1494), (PreVar1498 is PreVar1493 - (1), PreVar1497 is PreVar1498), is_pos(PreVar1497, PreVar1494), not is_wall(PreVar1493, PreVar1494, PreVar1497, PreVar1494).
prolog_poss( go_right_st ).
prolog_poss( go_right_st, S ) :- (has_val(pos, PreVar1509, S), PreVar1509=[PreVar1514, PreVar1515]), is_pos(PreVar1514, PreVar1515), (PreVar1519 is PreVar1514 + (1), PreVar1518 is PreVar1519), is_pos(PreVar1518, PreVar1515), not is_wall(PreVar1514, PreVar1515, PreVar1518, PreVar1515).
prolog_poss( pickup_item ).
prolog_poss( pickup_item, S ) :- (has_val(pos, PreVar1530, S), PreVar1530=[PreVar1535, PreVar1536]), is_pos(PreVar1535, PreVar1536), getval(real_item_pos, [PreVar1541, PreVar1542]), PreVar1535 =:= PreVar1541, PreVar1536 =:= PreVar1542, not has_val(epf_carry_item, true, S).
prolog_poss( drop_item ).
prolog_poss( drop_item, S ) :- has_val(epf_carry_item, true, S), (has_val(pos, PreVar1555, S), PreVar1555=[PreVar1560, PreVar1561]), is_pos(PreVar1560, PreVar1561), getval(real_item_pos, [PreVar1566, PreVar1567]), PreVar1560 =:= PreVar1566, PreVar1561 =:= PreVar1567.
prolog_poss( noop ).
prolog_poss( noop, S ) :- true.
prolog_poss( reset_visited ).
prolog_poss( reset_visited, S ) :- true.
prolog_poss( teleport(_X, _Y) ).
prolog_poss( teleport(_X, _Y), S ) :- true.
prolog_poss( hear(_75511) ).
prolog_poss( hear(_75511), S ) :- true.
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
prolog_poss( interpret_action(PreVar1580) ).
prolog_poss( interpret_action(PreVar1580), S ) :- (has_val(spoken_verb, PreVar1574, S), PreVar1574 = PreVar1573), synonym(PreVar1573, PreVar1580), not has_val(finished_action, true, S), not has_val(finished_objects, true, S), not has_val(finished_assignments, true, S).
prolog_poss( interpret_object(PreVar1603) ).
prolog_poss( interpret_object(PreVar1603), S ) :- (has_val(spoken_objects, PreVar1592, S), PreVar1592=[[PreVar1598, [PreVar1600, PreVar1601]]|_R]), synonym(PreVar1601, PreVar1603), has_val(finished_action, true, S), not has_val(finished_objects, true, S), not has_val(finished_assignments, true, S).
prolog_poss( assign_argument(PreVar1614) ).
prolog_poss( assign_argument(PreVar1614), S ) :- not (has_val(assumed_arguments, PreVar1616, S), member([PreVar1614, PreVar1615], PreVar1616)), (has_val(assumed_objects, PreVar1622, S), PreVar1622=[[PreVar1628, PreVar1629]|_R]), entity_attribute(PreVar1629, PreVar1631), (has_val(assumed_action, PreVar1634, S), PreVar1634 = PreVar1633), parameter(PreVar1633, PreVar1614, PreVar1641), parameter_attribute(PreVar1633, PreVar1614, PreVar1631), has_val(finished_action, true, S), has_val(finished_objects, true, S), not has_val(finished_assignments, true, S).
prolog_poss( init_vp(_75712) ).
prolog_poss( init_vp(_75712), S ) :- true.
prolog_poss( init_ip(_75719) ).
prolog_poss( init_ip(_75719), S ) :- not has_val(vp_finished, true, S).
prolog_poss( clarify ).
prolog_poss( clarify, S ) :- true.
prolog_poss( reject ).
prolog_poss( reject, S ) :- true.
events_list([e_loose_item]).
