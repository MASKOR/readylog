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
prolog_function( reward_interpretation, PreVar249, S ) :- (has_val(assumed_action, PreVar215, S), PreVar215=nil -> PreVar220=0 ; PreVar220=1), (has_val(assumed_objects, PreVar228, S), length(PreVar228, PreVar232)), (PreVar234 is PreVar232 + PreVar220, PreVar233 is PreVar234), ((has_val(preposition_count, PreVar240, S), PreVar238 is PreVar233 + PreVar240), PreVar237 is PreVar238), (has_val(assumed_arguments, PreVar244, S), length(PreVar244, PreVar248)), (PreVar252 is (2) * PreVar248, PreVar250 is PreVar237 + PreVar252), PreVar249 is PreVar250.
prolog_function( process_utterance(PreVar255) ).
prolog_function( process_utterance(PreVar255), PreVar256, S ) :- process_utterance_external(PreVar255, PreVar256).
prolog_function( split_essence(PreVar257) ).
prolog_function( split_essence(PreVar257), PreVar258, S ) :- split_essence_external(PreVar257, PreVar258).
prolog_function( form_command(PreVar259, PreVar261) ).
prolog_function( form_command(PreVar259, PreVar261), PreVar263, S ) :- skill(PreVar259, PreVar260), substitute_arguments(PreVar261, PreVar260, PreVar263).
prolog_function( filter_mandatory_parameters ).
prolog_function( filter_mandatory_parameters, PreVar276, S ) :- (has_val(assumed_arguments, PreVar264, S), flatten_argument_list(PreVar264, PreVar268)), (has_val(assumed_action, PreVar269, S), mandatory_parameters(PreVar269, PreVar273)), subtract(PreVar273, PreVar268, PreVar276).
prolog_function( filter_list_by_synonyms(PreVar277, PreVar278) ).
prolog_function( filter_list_by_synonyms(PreVar277, PreVar278), PreVar279, S ) :- flbs(PreVar277, PreVar278, PreVar279).
prolog_function( filter_list_by_attribute(PreVar280, PreVar281, PreVar282) ).
prolog_function( filter_list_by_attribute(PreVar280, PreVar281, PreVar282), PreVar285, S ) :- flba(PreVar280, PreVar281, PreVar282, PreVar283), remove_dups(PreVar283, PreVar285).
prolog_function( adl(PreVar286, PreVar287) ).
prolog_function( adl(PreVar286, PreVar287), PreVar288, S ) :- append(PreVar286, PreVar287, PreVar288).
stoch_proc(right_det).
stoch_proc(left_det).
stoch_proc(down_det).
stoch_proc(up_det).
stoch_proc(right_st).
stoch_proc(left_st).
stoch_proc(down_st).
stoch_proc(up_st).
stoch_proc(int_action_det(Skill)).
stoch_proc(int_object_det(Entity)).
stoch_proc(int_argument_det(Parameter)).
stoch_proc_outcomes(right_det, S, Outcomes, SenseEffect) :- Outcomes=[([go_right], 1.0, true)], SenseEffect = [], !.
stoch_proc_outcomes(left_det, S, Outcomes, SenseEffect) :- Outcomes=[([go_left], 1.0, true)], SenseEffect = [], !.
stoch_proc_outcomes(down_det, S, Outcomes, SenseEffect) :- Outcomes=[([go_down], 1.0, true)], SenseEffect = [], !.
stoch_proc_outcomes(up_det, S, Outcomes, SenseEffect) :- Outcomes=[([go_up], 1.0, true)], SenseEffect = [], !.
stoch_proc_outcomes(right_st, S, Outcomes, SenseEffect) :- (has_val(pos, PreVar291, S), PreVar291=[PreVar296, PreVar297]), (PreVar299 is PreVar296 + (1), PreVar298 is PreVar299), Outcomes=[(go_right, 0.5, pos=[PreVar298, PreVar297]), (noop, 0.5, pos=[PreVar296, PreVar297])], SenseEffect=[exogf_Update], !.
stoch_proc_outcomes(left_st, S, Outcomes, SenseEffect) :- (has_val(pos, PreVar304, S), PreVar304=[PreVar309, PreVar310]), (PreVar312 is PreVar309 - (1), PreVar311 is PreVar312), Outcomes=[(go_left, 0.5, pos=[PreVar311, PreVar310]), (noop, 0.5, pos=[PreVar309, PreVar310])], SenseEffect=[exogf_Update], !.
stoch_proc_outcomes(down_st, S, Outcomes, SenseEffect) :- (has_val(pos, PreVar317, S), PreVar317=[PreVar322, PreVar323]), (PreVar325 is PreVar323 - (1), PreVar324 is PreVar325), Outcomes=[(go_down, 0.5, pos=[PreVar322, PreVar324]), (noop, 0.5, pos=[PreVar322, PreVar323])], SenseEffect=[exogf_Update], !.
stoch_proc_outcomes(up_st, S, Outcomes, SenseEffect) :- (has_val(pos, PreVar330, S), PreVar330=[PreVar335, PreVar336]), (PreVar338 is PreVar336 + (1), PreVar337 is PreVar338), Outcomes=[(go_up, 0.5, pos=[PreVar335, PreVar337]), (noop, 0.5, pos=[PreVar335, PreVar336])], SenseEffect=[exogf_Update], !.
stoch_proc_outcomes(int_action_det(PreVar341), S, Outcomes, SenseEffect) :- Outcomes=[([interpret_action(PreVar341)], 1.0, true)], SenseEffect = [], !.
stoch_proc_outcomes(int_object_det(PreVar342), S, Outcomes, SenseEffect) :- Outcomes=[([interpret_object(PreVar342)], 1.0, true)], SenseEffect = [], !.
stoch_proc_outcomes(int_argument_det(PreVar343), S, Outcomes, SenseEffect) :- Outcomes=[([assign_argument(PreVar343)], 1.0, true)], SenseEffect = [], !.
stoch_proc_poss(right_det, S) :- true, !.
stoch_proc_poss(left_det, S) :- (has_val(pos, PreVar346, S), PreVar346=[PreVar351, PreVar352]), is_pos(PreVar351, PreVar352), (PreVar356 is PreVar351 - (1), PreVar355 is PreVar356), is_pos(PreVar355, PreVar352), not is_wall(PreVar351, PreVar352, PreVar355, PreVar352), !.
stoch_proc_poss(down_det, S) :- (has_val(pos, PreVar367, S), PreVar367=[PreVar372, PreVar373]), is_pos(PreVar372, PreVar373), (PreVar377 is PreVar373 - (1), PreVar376 is PreVar377), is_pos(PreVar372, PreVar376), not is_wall(PreVar372, PreVar373, PreVar372, PreVar376), !.
stoch_proc_poss(up_det, S) :- (has_val(pos, PreVar388, S), PreVar388=[PreVar393, PreVar394]), is_pos(PreVar393, PreVar394), (PreVar398 is PreVar394 + (1), PreVar397 is PreVar398), is_pos(PreVar393, PreVar397), not is_wall(PreVar393, PreVar394, PreVar393, PreVar397), !.
stoch_proc_poss(right_st, S) :- (has_val(pos, PreVar409, S), PreVar409=[PreVar414, PreVar415]), is_pos(PreVar414, PreVar415), (PreVar419 is PreVar414 + (1), PreVar418 is PreVar419), is_pos(PreVar418, PreVar415), not is_wall(PreVar414, PreVar415, PreVar418, PreVar415), !.
stoch_proc_poss(left_st, S) :- (has_val(pos, PreVar430, S), PreVar430=[PreVar435, PreVar436]), is_pos(PreVar435, PreVar436), (PreVar440 is PreVar435 - (1), PreVar439 is PreVar440), is_pos(PreVar439, PreVar436), not is_wall(PreVar435, PreVar436, PreVar439, PreVar436), !.
stoch_proc_poss(down_st, S) :- (has_val(pos, PreVar451, S), PreVar451=[PreVar456, PreVar457]), is_pos(PreVar456, PreVar457), (PreVar461 is PreVar457 - (1), PreVar460 is PreVar461), is_pos(PreVar456, PreVar460), not is_wall(PreVar456, PreVar457, PreVar456, PreVar460), !.
stoch_proc_poss(up_st, S) :- (has_val(pos, PreVar472, S), PreVar472=[PreVar477, PreVar478]), is_pos(PreVar477, PreVar478), (PreVar482 is PreVar478 + (1), PreVar481 is PreVar482), is_pos(PreVar477, PreVar481), not is_wall(PreVar477, PreVar478, PreVar477, PreVar481), !.
stoch_proc_poss(int_action_det(_18695), S) :- true, !.
stoch_proc_poss(int_object_det(_18702), S) :- true, !.
stoch_proc_poss(int_argument_det(_18709), S) :- true, !.
stoch_proc_costs(right_det, 1, S) :- true, !.
stoch_proc_costs(left_det, 1, S) :- true, !.
stoch_proc_costs(down_det, 1, S) :- true, !.
stoch_proc_costs(up_det, 1, S) :- true, !.
stoch_proc_costs(right_st, 1, S) :- true, !.
stoch_proc_costs(left_st, 1, S) :- true, !.
stoch_proc_costs(down_st, 1, S) :- true, !.
stoch_proc_costs(up_st, 1, S) :- true, !.
stoch_proc_costs(int_action_det(_18801), PreVar491, S) :- PreVar491=0, !.
stoch_proc_costs(int_object_det(_18815), PreVar495, S) :- PreVar495=0, !.
stoch_proc_costs(int_argument_det(_18829), PreVar499, S) :- PreVar499=0, !.
event_aux(e_loose_item).
event_outcomes(e_loose_item, S, Outcomes, SenseEffect) :- (has_val(epf_carry_item, PreVar506, S), PreVar503 = PreVar506), Outcomes=[(set(epf_carry_item, false), 0.3, epf_carry_item=false), ([], 0.7, epf_carry_item=true)], SenseEffect=[exogf_Update], !.
prolog_event_poss(e_loose_item, S) :- getval(agent_loose_item_enabled, true), has_val(epf_carry_item, PreVar514, S), PreVar514=true, !.
ssa( online, false, [clipOnline|Srest] ) :- !, true.
ssa( epf_carry_item, PreVar519, [drop_item|Srest] ) :- !, PreVar519=false.
ssa( num_visited, PreVar545, [go_down|Srest] ) :- !, (has_val(pos, PreVar525, Srest), PreVar525=[PreVar530, PreVar531]), PreVar532 = PreVar530, (PreVar537 is PreVar531 - (1), PreVar536 is PreVar537), (has_val(visited(PreVar532, PreVar536), true, Srest) -> has_val(num_visited, PreVar548, Srest), PreVar545 = PreVar548 ; (has_val(num_visited, PreVar554, Srest), PreVar553 is PreVar554 + (1)), PreVar545 is PreVar553).
ssa( pos, PreVar572, [go_down|Srest] ) :- !, (has_val(pos, PreVar561, Srest), PreVar561=[PreVar566, PreVar567]), (PreVar569 is PreVar567 - (1), PreVar568 is PreVar569), PreVar572=[PreVar566, PreVar568].
ssa( visited(PreVar587, PreVar591), true, [go_down|Srest] ) :- !, (has_val(pos, PreVar580, Srest), PreVar580=[PreVar585, PreVar586]), PreVar587 = PreVar585, PreVar592 is PreVar586 - (1), PreVar591 is PreVar592.
ssa( num_visited, PreVar617, [go_down_st|Srest] ) :- !, (has_val(pos, PreVar597, Srest), PreVar597=[PreVar602, PreVar603]), PreVar604 = PreVar602, (PreVar609 is PreVar603 - (1), PreVar608 is PreVar609), (has_val(visited(PreVar604, PreVar608), true, Srest) -> has_val(num_visited, PreVar618, Srest), PreVar617 is PreVar618 ; (has_val(num_visited, PreVar624, Srest), PreVar623 is PreVar624 + (1)), PreVar617 is PreVar623).
ssa( pos, PreVar644, [go_down_st|Srest] ) :- !, (has_val(pos, PreVar631, Srest), PreVar631=[PreVar636, PreVar637]), (PreVar641 is PreVar637 - (1), PreVar638 = PreVar641), PreVar644=[PreVar636, PreVar638].
ssa( visited(PreVar659, PreVar663), true, [go_down_st|Srest] ) :- !, (has_val(pos, PreVar652, Srest), PreVar652=[PreVar657, PreVar658]), PreVar659 = PreVar657, PreVar664 is PreVar658 - (1), PreVar663 is PreVar664.
ssa( num_visited, PreVar689, [go_left|Srest] ) :- !, (has_val(pos, PreVar669, Srest), PreVar669=[PreVar674, PreVar675]), (PreVar677 is PreVar674 - (1), PreVar676 is PreVar677), PreVar680 = PreVar675, (has_val(visited(PreVar676, PreVar680), true, Srest) -> has_val(num_visited, PreVar692, Srest), PreVar689 = PreVar692 ; (has_val(num_visited, PreVar698, Srest), PreVar697 is PreVar698 + (1)), PreVar689 is PreVar697).
ssa( pos, PreVar716, [go_left|Srest] ) :- !, (has_val(pos, PreVar705, Srest), PreVar705=[PreVar710, PreVar711]), (PreVar713 is PreVar710 - (1), PreVar712 is PreVar713), PreVar716=[PreVar712, PreVar711].
ssa( visited(PreVar731, PreVar735), true, [go_left|Srest] ) :- !, (has_val(pos, PreVar724, Srest), PreVar724=[PreVar729, PreVar730]), (PreVar732 is PreVar729 - (1), PreVar731 is PreVar732), PreVar735 = PreVar730.
ssa( num_visited, PreVar761, [go_left_st|Srest] ) :- !, (has_val(pos, PreVar741, Srest), PreVar741=[PreVar746, PreVar747]), (PreVar749 is PreVar746 - (1), PreVar748 is PreVar749), PreVar752 = PreVar747, (has_val(visited(PreVar748, PreVar752), true, Srest) -> has_val(num_visited, PreVar762, Srest), PreVar761 is PreVar762 ; (has_val(num_visited, PreVar768, Srest), PreVar767 is PreVar768 + (1)), PreVar761 is PreVar767).
ssa( pos, PreVar788, [go_left_st|Srest] ) :- !, (has_val(pos, PreVar775, Srest), PreVar775=[PreVar780, PreVar781]), (PreVar785 is PreVar780 - (1), PreVar782 = PreVar785), PreVar788=[PreVar782, PreVar781].
ssa( visited(PreVar803, PreVar807), true, [go_left_st|Srest] ) :- !, (has_val(pos, PreVar796, Srest), PreVar796=[PreVar801, PreVar802]), (PreVar804 is PreVar801 - (1), PreVar803 is PreVar804), PreVar807 = PreVar802.
ssa( num_visited, PreVar833, [go_right|Srest] ) :- !, (has_val(pos, PreVar813, Srest), PreVar813=[PreVar818, PreVar819]), (PreVar821 is PreVar818 + (1), PreVar820 is PreVar821), PreVar824 = PreVar819, (has_val(visited(PreVar820, PreVar824), true, Srest) -> has_val(num_visited, PreVar836, Srest), PreVar833 = PreVar836 ; (has_val(num_visited, PreVar842, Srest), PreVar841 is PreVar842 + (1)), PreVar833 is PreVar841).
ssa( pos, PreVar860, [go_right|Srest] ) :- !, (has_val(pos, PreVar849, Srest), PreVar849=[PreVar854, PreVar855]), (PreVar857 is PreVar854 + (1), PreVar856 is PreVar857), PreVar860=[PreVar856, PreVar855].
ssa( visited(PreVar875, PreVar879), true, [go_right|Srest] ) :- !, (has_val(pos, PreVar868, Srest), PreVar868=[PreVar873, PreVar874]), (PreVar876 is PreVar873 + (1), PreVar875 is PreVar876), PreVar879 = PreVar874.
ssa( num_visited, PreVar905, [go_right_st|Srest] ) :- !, (has_val(pos, PreVar885, Srest), PreVar885=[PreVar890, PreVar891]), (PreVar893 is PreVar890 + (1), PreVar892 is PreVar893), PreVar896 = PreVar891, (has_val(visited(PreVar892, PreVar896), true, Srest) -> has_val(num_visited, PreVar906, Srest), PreVar905 is PreVar906 ; (has_val(num_visited, PreVar912, Srest), PreVar911 is PreVar912 + (1)), PreVar905 is PreVar911).
ssa( pos, PreVar932, [go_right_st|Srest] ) :- !, (has_val(pos, PreVar919, Srest), PreVar919=[PreVar924, PreVar925]), (PreVar929 is PreVar924 + (1), PreVar926 = PreVar929), PreVar932=[PreVar926, PreVar925].
ssa( visited(PreVar947, PreVar951), true, [go_right_st|Srest] ) :- !, (has_val(pos, PreVar940, Srest), PreVar940=[PreVar945, PreVar946]), (PreVar948 is PreVar945 + (1), PreVar947 is PreVar948), PreVar951 = PreVar946.
ssa( num_visited, PreVar977, [go_up|Srest] ) :- !, (has_val(pos, PreVar957, Srest), PreVar957=[PreVar962, PreVar963]), PreVar964 = PreVar962, (PreVar969 is PreVar963 + (1), PreVar968 is PreVar969), (has_val(visited(PreVar964, PreVar968), true, Srest) -> has_val(num_visited, PreVar980, Srest), PreVar977 = PreVar980 ; (has_val(num_visited, PreVar986, Srest), PreVar985 is PreVar986 + (1)), PreVar977 is PreVar985).
ssa( pos, PreVar1004, [go_up|Srest] ) :- !, (has_val(pos, PreVar993, Srest), PreVar993=[PreVar998, PreVar999]), (PreVar1001 is PreVar999 + (1), PreVar1000 is PreVar1001), PreVar1004=[PreVar998, PreVar1000].
ssa( visited(PreVar1019, PreVar1023), true, [go_up|Srest] ) :- !, (has_val(pos, PreVar1012, Srest), PreVar1012=[PreVar1017, PreVar1018]), PreVar1019 = PreVar1017, PreVar1024 is PreVar1018 + (1), PreVar1023 is PreVar1024.
ssa( num_visited, PreVar1049, [go_up_st|Srest] ) :- !, (has_val(pos, PreVar1029, Srest), PreVar1029=[PreVar1034, PreVar1035]), PreVar1036 = PreVar1034, (PreVar1041 is PreVar1035 + (1), PreVar1040 is PreVar1041), (has_val(visited(PreVar1036, PreVar1040), true, Srest) -> has_val(num_visited, PreVar1050, Srest), PreVar1049 is PreVar1050 ; (has_val(num_visited, PreVar1056, Srest), PreVar1055 is PreVar1056 + (1)), PreVar1049 is PreVar1055).
ssa( pos, PreVar1076, [go_up_st|Srest] ) :- !, (has_val(pos, PreVar1063, Srest), PreVar1063=[PreVar1068, PreVar1069]), (PreVar1073 is PreVar1069 + (1), PreVar1070 = PreVar1073), PreVar1076=[PreVar1068, PreVar1070].
ssa( visited(PreVar1091, PreVar1095), true, [go_up_st|Srest] ) :- !, (has_val(pos, PreVar1084, Srest), PreVar1084=[PreVar1089, PreVar1090]), PreVar1091 = PreVar1089, PreVar1096 is PreVar1090 + (1), PreVar1095 is PreVar1096.
ssa( epf_carry_item, PreVar1099, [pickup_item|Srest] ) :- !, PreVar1099=true.
ssa( new_user_utter, PreVar1103, [processed_utterance|Srest] ) :- !, PreVar1103=false.
ssa( assumed_action, PreVar1107, [reject|Srest] ) :- !, PreVar1107=nil.
ssa( assumed_arguments, PreVar1111, [reject|Srest] ) :- !, PreVar1111 = [].
ssa( assumed_objects, PreVar1115, [reject|Srest] ) :- !, PreVar1115 = [].
ssa( finished_action, PreVar1119, [reject|Srest] ) :- !, PreVar1119=true.
ssa( finished_assignments, PreVar1123, [reject|Srest] ) :- !, PreVar1123=true.
ssa( finished_objects, PreVar1127, [reject|Srest] ) :- !, PreVar1127=true.
ssa( preposition_count, PreVar1131, [reject|Srest] ) :- !, PreVar1131=0.
ssa( visited(_33472, _33473), PreVar1135, [reset_visited|Srest] ) :- !, PreVar1135=false.
ssa( online, true, [setOnline|Srest] ) :- !, true.
ssa( assumed_objects, NewValue, [assign_argument(_P)|Srest] ) :- !, has_val(assumed_objects, PreVar1141, Srest), PreVar1141=[PreVar1146|NewValue].
ssa( assumed_arguments, PreVar1164, [assign_argument(PreVar1169)|Srest] ) :- !, (has_val(assumed_arguments, PreVar1150, Srest), PreVar1147 = PreVar1150), (has_val(assumed_objects, PreVar1156, Srest), PreVar1156=[[PreVar1162, PreVar1163]|L2]), PreVar1164=[[PreVar1169, PreVar1163]|PreVar1147].
ssa( finished_assignments, PreVar1180, [assign_argument(_P)|Srest] ) :- !, (has_val(assumed_objects, PreVar1174, Srest), PreVar1171 = PreVar1174), (length(PreVar1171, 1) -> has_val(finished_objects, PreVar1183, Srest), PreVar1180 = PreVar1183 ; PreVar1180=false).
ssa( preposition_count, PreVar1211, [assign_argument(PreVar1209)|Srest] ) :- !, (has_val(assumed_action, PreVar1193, Srest), PreVar1193 = PreVar1192), (has_val(assumed_objects, PreVar1200, Srest), PreVar1200=[[PreVar1206, PreVar1207]|L2]), (preposition(PreVar1192, PreVar1209, PreVar1206) -> (has_val(preposition_count, PreVar1213, Srest), PreVar1212 is PreVar1213 + (1)), PreVar1211 is PreVar1212 ; has_val(preposition_count, PreVar1219, Srest), PreVar1211 is PreVar1219).
ssa( new_user_utter, PreVar1223, [hear(_33511)|Srest] ) :- !, PreVar1223=true.
ssa( last_user_utterance, PreVar1227, [hear(PreVar1228)|Srest] ) :- !, PreVar1227 = PreVar1228.
ssa( verbphrases, NewValue, [init_ip(_34025)|Srest] ) :- !, has_val(verbphrases, PreVar1233, Srest), PreVar1233=[PreVar1238|NewValue].
ssa( vp_finished, PreVar1248, [init_ip(_34044)|Srest] ) :- !, (has_val(verbphrases, PreVar1241, Srest), PreVar1241 = PreVar1240), (length(PreVar1240, 1) -> PreVar1248=true ; PreVar1248=false).
ssa( assumed_action, PreVar1256, [init_ip(_34081)|Srest] ) :- !, PreVar1256=nil.
ssa( assumed_objects, PreVar1260, [init_ip(_34098)|Srest] ) :- !, PreVar1260 = [].
ssa( assumed_arguments, PreVar1264, [init_ip(_34115)|Srest] ) :- !, PreVar1264 = [].
ssa( preposition_count, PreVar1268, [init_ip(_34132)|Srest] ) :- !, PreVar1268=0.
ssa( clarification_count, PreVar1272, [init_ip(_34149)|Srest] ) :- !, PreVar1272=0.
ssa( finished_action, PreVar1276, [init_ip(_34166)|Srest] ) :- !, PreVar1276=false.
ssa( finished_objects, PreVar1280, [init_ip(_34183)|Srest] ) :- !, PreVar1280=false.
ssa( finished_assignments, PreVar1284, [init_ip(_34200)|Srest] ) :- !, PreVar1284=false.
ssa( spoken_verb, PreVar1288, [init_ip([[PreVar1289|_33979]|_33977])|Srest] ) :- !, PreVar1288 = PreVar1289.
ssa( spoken_objects, PreVar1292, [init_ip([[_34000, [objects, PreVar1293]]|_33999])|Srest] ) :- !, PreVar1292 = PreVar1293.
ssa( verbphrases, PreVar1296, [init_vp(PreVar1297)|Srest] ) :- !, PreVar1296 = PreVar1297.
ssa( vp_finished, PreVar1300, [init_vp(_33958)|Srest] ) :- !, PreVar1300=false.
ssa( spoken_verb, PreVar1304, [interpret_action(_S)|Srest] ) :- !, PreVar1304=nil.
ssa( assumed_action, PreVar1308, [interpret_action(PreVar1309)|Srest] ) :- !, PreVar1308 = PreVar1309.
ssa( finished_action, PreVar1312, [interpret_action(_S)|Srest] ) :- !, PreVar1312=true.
ssa( finished_objects, PreVar1325, [interpret_action(_33600)|Srest] ) :- !, (has_val(spoken_objects, PreVar1318, Srest), PreVar1318 = PreVar1317), (length(PreVar1317, 0) -> PreVar1325=true ; has_val(finished_objects, PreVar1332, Srest), PreVar1325 = PreVar1332).
ssa( finished_assignments, PreVar1345, [interpret_action(_33637)|Srest] ) :- !, (has_val(spoken_objects, PreVar1338, Srest), PreVar1338 = PreVar1337), (length(PreVar1337, 0) -> PreVar1345=true ; has_val(finished_assignments, PreVar1352, Srest), PreVar1345 = PreVar1352).
ssa( assumed_objects, PreVar1375, [interpret_object(PreVar1381)|Srest] ) :- !, (has_val(spoken_objects, PreVar1359, Srest), [[PreVar1361, [PreVar1363, PreVar1364]]|L2] = PreVar1359), (has_val(assumed_objects, PreVar1371, Srest), PreVar1368 = PreVar1371), PreVar1375=[[PreVar1361, PreVar1381]|PreVar1368].
ssa( spoken_objects, NewValue, [interpret_object(_E)|Srest] ) :- !, has_val(spoken_objects, PreVar1384, Srest), PreVar1384=[PreVar1389|NewValue].
ssa( finished_objects, PreVar1399, [interpret_object(_E)|Srest] ) :- !, (has_val(spoken_objects, PreVar1393, Srest), PreVar1390 = PreVar1393), (length(PreVar1390, 1) -> PreVar1399=true ; PreVar1399=false).
ssa( start, NewValue, [setTime(NewValue)|Srest] ) :- !, true.
ssa( start, NewValue, [ccUpdate(_32249, NewValue)|Srest] ) :- !, true.
ssa( action, NewValue, [reply(action, NewValue)|Srest] ) :- !, true.
ssa( action, NewValue, [send(action, NewValue)|Srest] ) :- !, true.
ssa( Fluent, NewValue, [set(Fluent, NewValue)|Srest] ) :- !, true.
ssa( pos, PreVar1407, [teleport(PreVar1411, PreVar1412)|Srest] ) :- !, PreVar1407=[PreVar1411, PreVar1412].

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
ssa( know_item_pos, NewValue, [set(know_item_pos, NewValue)|_] ) :- !.
ssa( last_user_utterance, NewValue, [set(last_user_utterance, NewValue)|_] ) :- !.
ssa( new_user_utter, NewValue, [set(new_user_utter, NewValue)|_] ) :- !.
ssa( num_visited, NewValue, [set(num_visited, NewValue)|_] ) :- !.
ssa( online, NewValue, [set(online, NewValue)|_] ) :- !.
ssa( preposition_count, NewValue, [set(preposition_count, NewValue)|_] ) :- !.
ssa( spoken_objects, NewValue, [set(spoken_objects, NewValue)|_] ) :- !.
ssa( spoken_verb, NewValue, [set(spoken_verb, NewValue)|_] ) :- !.
ssa( start, NewValue, [set(start, NewValue)|_] ) :- !.
ssa( useAbstraction, NewValue, [set(useAbstraction, NewValue)|_] ) :- !.
ssa( verbphrases, NewValue, [set(verbphrases, NewValue)|_] ) :- !.
ssa( vp_finished, NewValue, [set(vp_finished, NewValue)|_] ) :- !.
ssa( bel(_77295), NewValue, [set(bel(_77295), NewValue)|_] ) :- !.
ssa( ltp(_77299), NewValue, [set(ltp(_77299), NewValue)|_] ) :- !.
ssa( pproj(_77283, _77284), NewValue, [set(pproj(_77283, _77284), NewValue)|_] ) :- !.
ssa( visited(_X, _Y), NewValue, [set(visited(_X, _Y), NewValue)|_] ) :- !.
ssa( lookahead(_77288, _77289, _77290, _77291), NewValue, [set(lookahead(_77288, _77289, _77290, _77291), NewValue)|_] ) :- !.
ssa( pll(_77276, _77277, _77278, _77279), NewValue, [set(pll(_77276, _77277, _77278, _77279), NewValue)|_] ) :- !.
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
ssa( know_item_pos, NewValue, [_UnknownAction|Srest] ) :- !, has_val(know_item_pos, NewValue, Srest).
ssa( last_user_utterance, NewValue, [_UnknownAction|Srest] ) :- !, has_val(last_user_utterance, NewValue, Srest).
ssa( new_user_utter, NewValue, [_UnknownAction|Srest] ) :- !, has_val(new_user_utter, NewValue, Srest).
ssa( num_visited, NewValue, [_UnknownAction|Srest] ) :- !, has_val(num_visited, NewValue, Srest).
ssa( online, NewValue, [_UnknownAction|Srest] ) :- !, has_val(online, NewValue, Srest).
ssa( preposition_count, NewValue, [_UnknownAction|Srest] ) :- !, has_val(preposition_count, NewValue, Srest).
ssa( spoken_objects, NewValue, [_UnknownAction|Srest] ) :- !, has_val(spoken_objects, NewValue, Srest).
ssa( spoken_verb, NewValue, [_UnknownAction|Srest] ) :- !, has_val(spoken_verb, NewValue, Srest).
ssa( start, NewValue, [_UnknownAction|Srest] ) :- !, has_val(start, NewValue, Srest).
ssa( useAbstraction, NewValue, [_UnknownAction|Srest] ) :- !, has_val(useAbstraction, NewValue, Srest).
ssa( verbphrases, NewValue, [_UnknownAction|Srest] ) :- !, has_val(verbphrases, NewValue, Srest).
ssa( vp_finished, NewValue, [_UnknownAction|Srest] ) :- !, has_val(vp_finished, NewValue, Srest).
ssa( bel(_77295), NewValue, [_UnknownAction|Srest] ) :- !, has_val(bel(_77295), NewValue, Srest).
ssa( ltp(_77299), NewValue, [_UnknownAction|Srest] ) :- !, has_val(ltp(_77299), NewValue, Srest).
ssa( pproj(_77283, _77284), NewValue, [_UnknownAction|Srest] ) :- !, has_val(pproj(_77283, _77284), NewValue, Srest).
ssa( visited(_X, _Y), NewValue, [_UnknownAction|Srest] ) :- !, has_val(visited(_X, _Y), NewValue, Srest).
ssa( lookahead(_77288, _77289, _77290, _77291), NewValue, [_UnknownAction|Srest] ) :- !, has_val(lookahead(_77288, _77289, _77290, _77291), NewValue, Srest).
ssa( pll(_77276, _77277, _77278, _77279), NewValue, [_UnknownAction|Srest] ) :- !, has_val(pll(_77276, _77277, _77278, _77279), NewValue, Srest).
ssa( epf_carry_item, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_carry_item, NewValue, Srest).
ssa( epf_item_pos, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_item_pos, NewValue, Srest).
ssa( epf_sense_item, NewValue, [_UnknownAction|Srest] ) :- !, has_val(epf_sense_item, NewValue, Srest).
ssa( pos, NewValue, [_UnknownAction|Srest] ) :- !, has_val(pos, NewValue, Srest).
prolog_poss( send(_R, _78458) ).
prolog_poss( send(_R, _78458), S ) :- true.
prolog_poss( reply(_R, _78467) ).
prolog_poss( reply(_R, _78467), S ) :- true.
prolog_poss( setOnline ).
prolog_poss( setOnline, S ) :- true.
prolog_poss( clipOnline ).
prolog_poss( clipOnline, S ) :- true.
prolog_poss( setTime(_78485) ).
prolog_poss( setTime(_78485), S ) :- true.
prolog_poss( clipAbstraction ).
prolog_poss( clipAbstraction, S ) :- true.
prolog_poss( setAbstraction ).
prolog_poss( setAbstraction, S ) :- true.
prolog_poss( set(_Fluent, _Value) ).
prolog_poss( set(_Fluent, _Value), S ) :- true.
prolog_poss( exogf_Update ).
prolog_poss( exogf_Update, S ) :- true.
prolog_poss( say(_78517) ).
prolog_poss( say(_78517), S ) :- true.
prolog_poss( processed_utterance ).
prolog_poss( processed_utterance, S ) :- true.
prolog_poss( wait ).
prolog_poss( wait, S ) :- true.
prolog_poss( go_up ).
prolog_poss( go_up, S ) :- (has_val(pos, PreVar1415, S), PreVar1415=[PreVar1420, PreVar1421]), is_pos(PreVar1420, PreVar1421), (PreVar1425 is PreVar1421 + (1), PreVar1424 is PreVar1425), is_pos(PreVar1420, PreVar1424), not is_wall(PreVar1420, PreVar1421, PreVar1420, PreVar1424).
prolog_poss( go_down ).
prolog_poss( go_down, S ) :- (has_val(pos, PreVar1436, S), PreVar1436=[PreVar1441, PreVar1442]), is_pos(PreVar1441, PreVar1442), (PreVar1446 is PreVar1442 - (1), PreVar1445 is PreVar1446), is_pos(PreVar1441, PreVar1445), not is_wall(PreVar1441, PreVar1442, PreVar1441, PreVar1445).
prolog_poss( go_left ).
prolog_poss( go_left, S ) :- (has_val(pos, PreVar1457, S), PreVar1457=[PreVar1462, PreVar1463]), is_pos(PreVar1462, PreVar1463), (PreVar1467 is PreVar1462 - (1), PreVar1466 is PreVar1467), is_pos(PreVar1466, PreVar1463), not is_wall(PreVar1462, PreVar1463, PreVar1466, PreVar1463).
prolog_poss( go_right ).
prolog_poss( go_right, S ) :- (has_val(pos, PreVar1478, S), PreVar1478=[PreVar1483, PreVar1484]), is_pos(PreVar1483, PreVar1484), (PreVar1488 is PreVar1483 + (1), PreVar1487 is PreVar1488), is_pos(PreVar1487, PreVar1484), not is_wall(PreVar1483, PreVar1484, PreVar1487, PreVar1484).
prolog_poss( rest ).
prolog_poss( rest, S ) :- (has_val(pos, PreVar1499, S), PreVar1499=[PreVar1504, PreVar1505]), is_pos(PreVar1504, PreVar1505).
prolog_poss( go_up_st ).
prolog_poss( go_up_st, S ) :- (has_val(pos, PreVar1510, S), PreVar1510=[PreVar1515, PreVar1516]), is_pos(PreVar1515, PreVar1516), (PreVar1520 is PreVar1516 + (1), PreVar1519 is PreVar1520), is_pos(PreVar1515, PreVar1519), not is_wall(PreVar1515, PreVar1516, PreVar1515, PreVar1519).
prolog_poss( go_down ).
prolog_poss( go_down, S ) :- (has_val(pos, PreVar1531, S), PreVar1531=[PreVar1536, PreVar1537]), is_pos(PreVar1536, PreVar1537), (PreVar1541 is PreVar1537 - (1), PreVar1540 is PreVar1541), is_pos(PreVar1536, PreVar1540), not is_wall(PreVar1536, PreVar1537, PreVar1536, PreVar1540).
prolog_poss( go_left_st ).
prolog_poss( go_left_st, S ) :- (has_val(pos, PreVar1552, S), PreVar1552=[PreVar1557, PreVar1558]), is_pos(PreVar1557, PreVar1558), (PreVar1562 is PreVar1557 - (1), PreVar1561 is PreVar1562), is_pos(PreVar1561, PreVar1558), not is_wall(PreVar1557, PreVar1558, PreVar1561, PreVar1558).
prolog_poss( go_right_st ).
prolog_poss( go_right_st, S ) :- (has_val(pos, PreVar1573, S), PreVar1573=[PreVar1578, PreVar1579]), is_pos(PreVar1578, PreVar1579), (PreVar1583 is PreVar1578 + (1), PreVar1582 is PreVar1583), is_pos(PreVar1582, PreVar1579), not is_wall(PreVar1578, PreVar1579, PreVar1582, PreVar1579).
prolog_poss( pickup_item ).
prolog_poss( pickup_item, S ) :- (has_val(pos, PreVar1594, S), PreVar1594=[PreVar1599, PreVar1600]), is_pos(PreVar1599, PreVar1600), getval(real_item_pos, [PreVar1605, PreVar1606]), PreVar1599 =:= PreVar1605, PreVar1600 =:= PreVar1606, not has_val(epf_carry_item, true, S).
prolog_poss( drop_item ).
prolog_poss( drop_item, S ) :- has_val(epf_carry_item, true, S), (has_val(pos, PreVar1619, S), PreVar1619=[PreVar1624, PreVar1625]), is_pos(PreVar1624, PreVar1625), getval(real_item_pos, [PreVar1630, PreVar1631]), PreVar1624 =:= PreVar1630, PreVar1625 =:= PreVar1631.
prolog_poss( noop ).
prolog_poss( noop, S ) :- true.
prolog_poss( reset_visited ).
prolog_poss( reset_visited, S ) :- true.
prolog_poss( teleport(_X, _Y) ).
prolog_poss( teleport(_X, _Y), S ) :- true.
prolog_poss( hear(_79039) ).
prolog_poss( hear(_79039), S ) :- true.
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
prolog_poss( interpret_action(PreVar1653) ).
prolog_poss( interpret_action(PreVar1653), S ) :- not has_val(finished_action, true, S), not has_val(finished_objects, true, S), not has_val(finished_assignments, true, S), (has_val(spoken_verb, PreVar1647, S), PreVar1647 = PreVar1646), synonym(PreVar1646, PreVar1653).
prolog_poss( interpret_object(PreVar1676) ).
prolog_poss( interpret_object(PreVar1676), S ) :- has_val(finished_action, true, S), not has_val(finished_objects, true, S), not has_val(finished_assignments, true, S), (has_val(spoken_objects, PreVar1665, S), PreVar1665=[[PreVar1671, [PreVar1673, PreVar1674]]|_R]), synonym(PreVar1674, PreVar1676).
prolog_poss( assign_argument(PreVar1684) ).
prolog_poss( assign_argument(PreVar1684), S ) :- has_val(finished_action, true, S), not has_val(finished_assignments, true, S), not (has_val(assumed_arguments, PreVar1686, S), member([PreVar1684, PreVar1685], PreVar1686)), (has_val(assumed_objects, PreVar1692, S), PreVar1692=[[PreVar1698, PreVar1699]|_R]), entity_attribute(PreVar1699, PreVar1701), (has_val(assumed_action, PreVar1704, S), PreVar1704 = PreVar1703), parameter(PreVar1703, PreVar1684, PreVar1711), parameter_attribute(PreVar1703, PreVar1684, PreVar1701).
prolog_poss( init_vp(_79238) ).
prolog_poss( init_vp(_79238), S ) :- true.
prolog_poss( init_ip(_79245) ).
prolog_poss( init_ip(_79245), S ) :- not has_val(vp_finished, true, S).
prolog_poss( clarify ).
prolog_poss( clarify, S ) :- true.
prolog_poss( reject ).
prolog_poss( reject, S ) :- true.
events_list([e_loose_item]).
