/*******************************
* file generated from ReadyLog *
*******************************/

:- pragma(nodebug).
rules( [
%%% a overlappedby b %%%
production_rule( a, overlappedby, b, as,
	bfl,
	[],
	[after(as, bf)],
	[]
),
production_rule( a, overlappedby, b, as,
	not bfl,
	[[bs, as, [bs<as]]],
	[after(as, bf)],
	[between(bs, as, bfl)]
),
production_rule( a, overlappedby, b, af,
	bfl,
	[[bf, af, [bf<af]]],
	[],
	[]
),
%%% t_01 before sm_01 %%%
production_rule( t_01, before, sm_01, stop_t_01,
	true,
	[],
	[after(stop_t_01, sm_01)],
	[]
),
%%% t_02 after sm_02 %%%
production_rule( t_02, after, sm_02, start_t_02,
	and(not occurred(stop_sm_02), not sm_02),
	[[start_sm_02, stop_sm_02, [start_sm_02<stop_sm_02]], [stop_sm_02, start_t_02, [stop_sm_02<start_t_02]]],
	[],
	[]
),
production_rule( t_02, after, sm_02, start_t_02,
	and(not occurred(stop_sm_02), sm_02),
	[[stop_sm_02, start_t_02, [stop_sm_02<start_t_02]]],
	[],
	[]
),
%%% t_03 meets sm_03 %%%
production_rule( t_03, meets, sm_03, stop_t_03,
	not sm_03,
	[[stop_t_03, start_sm_03, [stop_t_03=start_sm_03]]],
	[after(start_sm_03, stop_sm_03)],
	[]
),
production_rule( t_03, meets, sm_03, stop_t_03,
	sm_03,
	[[stop_sm_03, start_sm_03, [stop_sm_03<start_sm_03]], [stop_t_03, start_sm_03, [stop_t_03=start_sm_03]]],
	[],
	[]
),
%%% t_04 metby sm_04 %%%
production_rule( t_04, metby, sm_04, start_t_04,
	and(not eoccurred(stop_sm_04), not sm_04),
	[[start_sm_04, stop_sm_04, [start_sm_04<stop_sm_04]], [stop_sm_04, start_t_04, [start_sm_04=start_t_04]]],
	[after(start_sm_04, stop_sm_04)],
	[]
),
production_rule( t_04, metby, sm_04, start_t_04,
	and(not eoccurred(stop_sm_04), sm_04),
	[[stop_sm_04, start_t_04, [stop_sm_04=start_t_04]]],
	[],
	[]
),
%%% t_05 starts sm_05 %%%
production_rule( t_05, starts, sm_05, start_t_05,
	sm_05,
	[[stop_sm_05, start_t_05, [stop_sm_05<start_t_05]], [stop_sm_05, start_sm_05, [stop_sm_05<start_sm_05]], [start_t_05, start_sm_05, [start_t_05=start_sm_05]]],
	[],
	[between(start_sm_05, stop_t_05, sm_05)]
),
production_rule( t_05, starts, sm_05, start_t_05,
	not sm_05,
	[[start_t_05, start_sm_05, [start_t_05=start_sm_05]]],
	[],
	[between(start_sm_05, stop_t_05, sm_05)]
),
production_rule( t_05, starts, sm_05, stop_t_05,
	true,
	[],
	[after(stop_t_05, stop_sm_05)],
	[]
),
%%% t_06 startedby sm_06 %%%
production_rule( t_06, startedby, sm_06, start_t_06,
	eoccurred(start_sm_06),
	[],
	[after(start_t_06, stop_sm_06)],
	[]
),
production_rule( t_06, startedby, sm_06, start_t_06,
	and(not eoccurred(start_sm_06), not sm_06),
	[[start_sm_06, start_t_06, [start_sm_06=start_t_06]]],
	[after(start_t_06, stop_sm_06)],
	[between(start_sm_06, start_t_06, sm_06)]
),
production_rule( t_06, startedby, sm_06, start_t_06,
	and(not eoccurred(start_sm_06), sm_06),
	[[stop_sm_06, start_sm_06, [stop_sm_06<start_sm_06]], [start_sm_06, start_t_06, [start_sm_06=start_t_06]]],
	[after(start_t_06, stop_sm_06)],
	[between(start_sm_06, start_t_06, sm_06)]
),
production_rule( t_06, startedby, sm_06, stop_t_06,
	sm_06,
	[[stop_sm_06, stop_t_06, [stop_sm_06<stop_t_06]]],
	[],
	[]
),
%%% t_07 finishes sm_07 %%%
production_rule( t_07, finishes, sm_07, start_t_07,
	not sm_07,
	[[start_sm_07, start_t_07, [start_sm_07<start_t_07]]],
	[],
	[between(start_sm_07, stop_t_07, sm_07)]
),
production_rule( t_07, finishes, sm_07, stop_t_07,
	sm_07,
	[[stop_t_07, stop_sm_07, [stop_t_07=stop_sm_07]]],
	[],
	[]
),
%%% t_08 finishedby sm_08 %%%
production_rule( t_08, finishedby, sm_08, start_t_08,
	true,
	[],
	[after(start_t_08, start_sm_08)],
	[]
),
production_rule( t_08, finishedby, sm_08, stop_t_08,
	and(not eoccurred(stop_sm_08), sm_08),
	[[stop_sm_08, stop_t_08, [stop_sm_08=stop_t_08]]],
	[],
	[]
),
production_rule( t_08, finishedby, sm_08, stop_t_08,
	and(not eoccurred(stop_sm_08), not sm_08),
	[[start_sm_08, stop_sm_08, [start_sm_08<stop_sm_08]], [stop_sm_08, stop_t_08, [stop_sm_08=stop_t_08]]],
	[],
	[]
),
%%% t_09 overlaps sm_09 %%%
production_rule( t_09, overlaps, sm_09, start_t_09,
	true,
	[],
	[after(start_t_09, start_sm_09)],
	[]
),
production_rule( t_09, overlaps, sm_09, stop_t_09,
	not sm_09,
	[[start_sm_09, stop_t_09, [start_sm_09<stop_t_09]]],
	[after(stop_t_09, stop_sm_09)],
	[between(start_sm_09, stop_t_09, sm_09)]
),
production_rule( t_09, overlaps, sm_09, stop_t_09,
	sm_09,
	[],
	[after(stop_t_09, stop_sm_09)],
	[]
),
%%% t_10 overlappedby sm_10 %%%
production_rule( t_10, overlappedby, sm_10, start_t_10,
	sm_10,
	[],
	[after(start_t_10, stop_sm_10)],
	[]
),
production_rule( t_10, overlappedby, sm_10, start_t_10,
	not sm_10,
	[[start_sm_10, start_t_10, [start_sm_10<start_t_10]]],
	[after(start_t_10, stop_sm_10)],
	[between(start_sm_10, start_t_10, sm_10)]
),
production_rule( t_10, overlappedby, sm_10, stop_t_10,
	sm_10,
	[[stop_sm_10, stop_t_10, [stop_sm_10<stop_t_10]]],
	[],
	[]
),
%%% t_11 during sm_11 %%%
production_rule( t_11, during, sm_11, start_t_11,
	not sm_11,
	[[start_sm_11, start_t_11, [start_sm_11<start_t_11]]],
	[],
	[between(start_sm_11, stop_t_11, sm_11)]
),
production_rule( t_11, during, sm_11, start_t_11,
	sm_11,
	[],
	[],
	[between(start_t_11, stop_t_11, sm_11)]
),
production_rule( t_11, during, sm_11, stop_t_11,
	sm_11,
	[],
	[after(stop_t_11, stop_sm_11)],
	[]
),
%%% t_12 contains sm_12 %%%
production_rule( t_12, contains, sm_12, start_t_12,
	true,
	[],
	[after(start_t_12, sm_12)],
	[]
),
production_rule( t_12, contains, sm_12, stop_t_12,
	not_started_within(start_sm_12, t_12),
	[[start_sm_12, stop_sm_12, [start_sm_12<stop_sm_12]], [stop_sm_12, stop_t_12, [stop_sm_12<stop_t_12]]],
	[],
	[]
),
production_rule( t_12, contains, sm_12, stop_t_12,
	started_not_finished_within(sm_12, t_12),
	[[stop_sm_12, stop_t_12, [stop_sm_12<stop_t_12]]],
	[],
	[]
),
%%% t_13 equals sm_13 %%%
production_rule( t_13, equals, sm_13, start_t_13,
	not sm_13,
	[[start_sm_13, start_t_13, [start_sm_13=start_t_13]]],
	[],
	[between(start_sm_13, stop_t_13, sm_13)]
),
production_rule( t_13, equals, sm_13, start_t_13,
	and(not eoccurred(start_sm_13), sm_13),
	[[stop_sm_13, start_sm_13, [stop_sm_13<start_sm_13]], [start_sm_13, start_t_13, [start_sm_13=start_t_13]]],
	[],
	[between(start_sm_13, stop_t_13, sm_13)]
),
production_rule( t_13, equals, sm_13, stop_t_13,
	sm_13,
	[[stop_t_13, stop_sm_13, [stop_t_13=stop_sm_13]]],
	[],
	[]
),
%%% t_14 metby sm_01 %%%
production_rule( t_14, metby, sm_01, start_t_14,
	and(not eoccurred(stop_sm_01), not sm_01),
	[[start_sm_01, stop_sm_01, [start_sm_01<stop_sm_01]], [stop_sm_01, start_t_14, [start_sm_01=start_t_14]]],
	[after(start_sm_01, stop_sm_01)],
	[]
),
production_rule( t_14, metby, sm_01, start_t_14,
	and(not eoccurred(stop_sm_01), sm_01),
	[[stop_sm_01, start_t_14, [stop_sm_01=start_t_14]]],
	[],
	[]
),
%%% t_14 metby sm_02 %%%
production_rule( t_14, metby, sm_02, start_t_14,
	and(not eoccurred(stop_sm_02), not sm_02),
	[[start_sm_02, stop_sm_02, [start_sm_02<stop_sm_02]], [stop_sm_02, start_t_14, [start_sm_02=start_t_14]]],
	[after(start_sm_02, stop_sm_02)],
	[]
),
production_rule( t_14, metby, sm_02, start_t_14,
	and(not eoccurred(stop_sm_02), sm_02),
	[[stop_sm_02, start_t_14, [stop_sm_02=start_t_14]]],
	[],
	[]
),
%%% t_14 metby sm_03 %%%
production_rule( t_14, metby, sm_03, start_t_14,
	and(not eoccurred(stop_sm_03), not sm_03),
	[[start_sm_03, stop_sm_03, [start_sm_03<stop_sm_03]], [stop_sm_03, start_t_14, [start_sm_03=start_t_14]]],
	[after(start_sm_03, stop_sm_03)],
	[]
),
production_rule( t_14, metby, sm_03, start_t_14,
	and(not eoccurred(stop_sm_03), sm_03),
	[[stop_sm_03, start_t_14, [stop_sm_03=start_t_14]]],
	[],
	[]
),
%%% t_14 metby sm_04 %%%
production_rule( t_14, metby, sm_04, start_t_14,
	and(not eoccurred(stop_sm_04), not sm_04),
	[[start_sm_04, stop_sm_04, [start_sm_04<stop_sm_04]], [stop_sm_04, start_t_14, [start_sm_04=start_t_14]]],
	[after(start_sm_04, stop_sm_04)],
	[]
),
production_rule( t_14, metby, sm_04, start_t_14,
	and(not eoccurred(stop_sm_04), sm_04),
	[[stop_sm_04, start_t_14, [stop_sm_04=start_t_14]]],
	[],
	[]
),
[]
] ).
