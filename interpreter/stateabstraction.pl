ignore_fluents([initial_pos, goal_pos]).
get_all_fluents(Result) :-
        % get all fluents
        findall(PrimF, prim_fluent(PrimF), PrimFList),
        findall(ContF, cont_fluent(ContF), ContFList),
        findall(ExogPrimF, exog_prim_fluent(ExogPrimF), ExogPrimFList),
        findall(ExogCrimF, exog_cont_fluent(ExogCrimF), ExogContFList),
        % get all registers (which are also fluents)
        findall(Reg, register(Reg), RegL),
        % define all built-in fluents
        BuiltInTemp=[online, start, pll(_, _, _, _), pproj(_, _),
                 lookahead(_, _, _, _), bel(_), ltp(_)],
	ignore_fluents(IFL), 
	append(BuiltInTemp, IFL, BuiltIn),
        % the list of all defined fluents
        FluentLTmp=[PrimFList, ContFList, ExogPrimFList, ExogContFList],
        flatten(FluentLTmp,FluentL),
        BuiltInTmp = [BuiltIn, RegL], flatten(BuiltInTmp, BuiltInL),
        subtract(FluentL, BuiltInL, ResultList),
	list_to_ord_set(ResultList, Result).
