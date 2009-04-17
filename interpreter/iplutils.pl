/* ***************************************************************************
 *                                            ####   ####           .-""-.    
 *       # #                             #   #    # #    #         /[] _ _\   
 *       # #                                 #    # #             _|_o_LII|_  
 * ,###, # #  ### ## ## ##   ###  ## ##  #   #    # #       ###  / | ==== | \ 
 * #   # # # #   # ## ## #  #   #  ## #  #   ###### #      #     |_| ==== |_| 
 * #   # # # ####  #  #  #  #   #  #  #  #   #    # #      ####   ||" ||  ||  
 * #   # # # #     #  #  #  #   #  #  #  #   #    # #    #    #   ||'----'||  
 * '###'# # # #### #  #  ##  ### # #  ## ## #      # ####  ###   /__|    |__\ 
 * ***************************************************************************
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; only version 2 of the License!
 * ***************************************************************************
 *
 *           $Id: iplutils.pl 183 2008-12-30 16:41:04Z dp $
 *         @date: 30.12.08
 *       @author: Dennis Pannhausen <Dennis.Pannhausen@rwth-aachen.de>
 *   description: utility predicates for (i)nductive (p)olicy (l)earning,
 *                used by the iplpreprocessor and the iplearner
 *
 * **************************************************************************/

% ================================================================== %
%  IPL UTILITIES                                                     %
% ================================================================== %

%  The IPL utilities can be used by both the iplpreprocessor and the
%  iplearner. The modular design allows other Readylog files to use
%  them as well.

:- write("** loading iplutils.pl\n"). 

% ---------------------------------------------------------- %
%   Utilities                                                %
% ---------------------------------------------------------- %

% ---------------------------------------------------------- %
%   String & Term Manipulation                               %
% ---------------------------------------------------------- %

%  During compilation, Prolog rips the cout arguments from their
%  quotation marks; fix_couts/2 adds them back. It does this in a
%  preprocessing step, as we would otherwise miss some
%  (e.g., in deterministic subprograms of if-clauses).
:- mode fix_couts(++, -).
fix_couts( [], Fixed ) :- !,
        Fixed = [].

fix_couts( Program, Fixed ) :-
        is_list(Program),
        !,
        Program = [Term | Rest],
        ( var(Term) ->
           FixedTerm = Term
        ;
           fix_couts( Term, FixedTerm )
        ),
        fix_couts( Rest, FixedRest ),
        Fixed = [FixedTerm | FixedRest].

fix_couts( Term, Fixed ) :-
        var(Term),
        !,
        Fixed = Term.

fix_couts( Term, Fixed ) :-
        Term =.. [Functor | Args],
        Functor = cout,
        !,
%        printf(stdout, "\nfix_couts() Found a cout in Term = %w\n", [Term]),
%        flush(stdout),
        length(Args, NumArgs),
        fix_couts_aux( Term, NumArgs, Fixed ).
        
fix_couts( Term, Fixed ) :-
        Term =.. [Functor | Args],
        not(length(Args, 0)),
        !,
        fix_couts( Args, FixedArgs ),
        Fixed =.. [Functor | FixedArgs].

fix_couts( Term, Fixed ) :-
        Fixed = Term.


:- mode fix_couts_aux(++, ++, -).
fix_couts_aux( Term, 1, Fixed ) :- !,
        arg(1, Term, V),
        term_string( V, VString ),
        replace_escape_chars( VString, VStringClean ),
        Fixed = cout(VStringClean).

fix_couts_aux( Term, 1, Fixed ) :- !,
        arg(1, Term, F),
        arg(2, Term, V),
        term_string( F, FString ),
        replace_escape_chars( FString, FStringClean ),
        Fixed = cout(FStringClean, V).

fix_couts_aux( Term, 3, Fixed ) :- 
        arg(1, Term, Color),
        arg(2, Term, F),
        arg(3, Term, V),
        term_string( F, FString ),
        replace_escape_chars( FString, FStringClean ),
        Fixed = cout(Color, FStringClean, V).


%  Replaces newlines and tabulator chars in a string by escaped chars.
:- mode replace_escape_chars(++, -).
replace_escape_chars( String, Result ) :-
        replace_character( String, "\n", "\\n", StringTmp ),
        replace_character( StringTmp, "\t", "\\t", Result ).


%  Replaces commas that separate consecutive actions by semicolons,
%  but ignores commas in tests, if-conditions, and while statements.
:- mode comma_to_semicolon(++, -).
comma_to_semicolon( "", String_New ) :- !,
        String_New = "".

comma_to_semicolon( String, String_New ) :-
        substring( String, 1, 3, "if(" ),
        !,
        comma_to_semicolon_aux( String, String_New ).
        
comma_to_semicolon( String, String_New ) :-
        substring( String, 1, 6, "while(" ),
        !,
        comma_to_semicolon_aux( String, String_New ).
        
comma_to_semicolon( String, String_New ) :-
        substring( String, 1, 2, "?(" ),
        !,
        comma_to_semicolon_aux( String, String_New ).

comma_to_semicolon( String, String_New ) :- 
        ( substring( String, IfPos, 3, "if(" );
          substring( String, WhilePos, 6, "while(" );
          substring( String, TestPos, 2, "?(" )
        ),
        !,
        comma_to_semicolon_aux( String, IfPos, WhilePos, TestPos,
                                String_New).

comma_to_semicolon( String, String_New ) :- 
        %  No special term found in the String
        %  Commas can all be replaced.
        replace_character(String, ",", ";", String_New).

%  Helper predicate for comma_to_semicolon_aux/2.
%  Differentiates between the cases, where IfPos, WhilePos, and TestPos
%  are instantiated or not.
%  If a position is instantiated (nonvar), then the corresponding pattern
%  has been found in the String at this position.
:- mode comma_to_semicolon_aux(++, ?, ?, ?, -).
comma_to_semicolon_aux( String, IfPos, WhilePos, TestPos, String_New) :-
        %  There is still an if, while, or test in the String.
        %  Find the position of the first occurence of such a statement.
        find_minimal_position(IfPos, WhilePos, TestPos, MinPos),
%        printf(stdout, "********** MinPos: %w\n", [MinPos]),
        string_length( String, StringLength ),
        %  String left from statement:
        %  Note that MinPos > 0.
        MinPosLeft is (MinPos - 1),
        substring( String, 1, MinPosLeft, Left ),
        replace_character( Left, ",", ";", LeftResult ),

        %  Rest string:
        MinPosRight is (MinPos + 1),
        RestLength is (StringLength - MinPosRight + 2),
        substring( String, MinPos, RestLength, Rest ),
%        printf(stdout, "********** Rest: %w\n", [Rest]),
        comma_to_semicolon( Rest, RestResult ),
        concat_strings( LeftResult, RestResult, String_New ).

%  Find the minimum of the three values IfPOs, WhilePos, and TestPos.
%  Note, that in our case at least one value will be initiated
%  (otherwise the disjunction before the call of comma_to_semicolon_aux/5
%  would have been false).
:- mode find_minimal_position(?, ?, ?, -).
find_minimal_position(IfPos, WhilePos, TestPos, MinPos) :-
        nonvar(IfPos),
        !,
        find_minimal_position_aux(IfPos, WhilePos, TestPos, MinPos).

find_minimal_position(_IfPos, WhilePos, TestPos, MinPos) :-
        %  IfPos is *not* instantiated.
        find_minimal_position_aux(WhilePos, TestPos, MinPos).
        
:- mode find_minimal_position_aux(++, ?, ?, -).
find_minimal_position_aux(IfPos, WhilePos, TestPos, MinPos) :-
        %  IfPos is instantiated.
        nonvar(WhilePos),
        !,
        min(IfPos, WhilePos, MinTmp),
        find_minimal_position_aux(MinTmp, TestPos, MinPos).

find_minimal_position_aux(IfPos, _WhilePos, TestPos, MinPos) :-
        %  IfPos is instantiated.
        %  WhilePos is *not* instantiated.
        find_minimal_position_aux(IfPos, TestPos, MinPos).

%  Only two candidates left.
:- mode find_minimal_position_aux(++, ?, -).
find_minimal_position_aux(Pos1, Pos2, MinPos) :-
        nonvar(Pos2),
        !,
        min(Pos1, Pos2, MinPos).

find_minimal_position_aux(Pos1, _Pos2, MinPos) :-
        %  Pos2 is *not* instantiated.
        MinPos = Pos1.

        
:- mode comma_to_semicolon_aux(++, -).
comma_to_semicolon_aux( String, String_New ) :-
        string_length( String, StringLength ),
        substring( String, ClosingBracketPos, 1, ")" ),

        %  Statement string:
        ClosingBracketPosLeft is (ClosingBracketPos - 1),
        substring( String, 1, ClosingBracketPosLeft, Left ),
        %  Replace commas in the statement by placeholder.
        %  Otherwise integrate/3 would interpret commas
        %  as separators for different programs.
        %  After integration, when converting the
        %  nondeterministic set into a list, we replace the
        %  placeholders by commas again
        replace_character( Left, ",", "__COMMA__", LeftResult ),

        %  Rest string:
        RightLength is (StringLength - ClosingBracketPos + 1),
        substring( String, ClosingBracketPos, RightLength, Right ),
        comma_to_semicolon( Right, RightResult ),

        concat_strings( LeftResult, RightResult, String_New ).

%  Converts a Prolog list to a string.
:- mode list_to_string(++, -).
list_to_string( [], ResultString ) :- !,
        ResultString = "".

list_to_string( List, ResultString ) :-
        %  Cut away brackets around the argument list.
        term_string(List, String),
        string_length(String, StringLength),
        ReducedLength is (StringLength - 2),
        substring( String, 2, ReducedLength, TmpString ),
        %  Remove all "'" that Prolog builds in when
        %  converting terms to strings with term_string/2.
        remove_character( TmpString, "'", ResultString ).        

%  Converts a string to a Prolog list.
:- mode string_to_list(++, -).
string_to_list( "", List ) :- !,
        List = [].

string_to_list( String, List ) :-
        split_string( String, ",", " \t", StringList),
        findall( X,
                 ( member(Y, StringList), atom_string(X,Y) ),
                 List ).

%  Converts the fluent values (with placeholders
%  COMMA and QUOTATION) from a C4.5-friendly format into
%  strings with commas and quotation marks.
:- mode fluent_values_to_string(++, -).
fluent_values_to_string( [], ResultString ) :- !,
        ResultString = "".

fluent_values_to_string( List, ResultString ) :-
        %  Cut away brackets around the argument list.
        term_string(List, String),
        string_length(String, StringLength),
        ReducedLength is (StringLength - 2),
        substring( String, 2, ReducedLength, TmpString1 ),
        %  Remove all " that Prolog builds in when
        %  converting (string)term->string.
        remove_character( TmpString1, "\"", TmpString2 ),
        %  Recover commas that were part of the fluent value.
        replace_string( TmpString2, "COMMA", "\\,",
                        TmpString3 ),
        %  Recover " that were part of the fluent value.
        replace_string( TmpString3, "QUOTATION", "\"",
                        ResultString ).        


%  Finds every occurrence of the string-pattern Pattern in the String and
%  replaces it by the string Value.
:- mode replace_string(++, ++, ++, -).
replace_string( String, Pattern, Value, StringNew ) :-
        replace_string_aux( String, Pattern, Value, "", StringNew ).

%  Helper predicate to carry through the already processed
%  part of the string ProcessedPart.
%  This avoids too much recursion and global stack overflows.
:- mode replace_string_aux(++, ++, ++, ++, -).
replace_string_aux( "", _Pattern, _Value, ProcessedPart, StringNew ) :- !,
        StringNew = ProcessedPart.

replace_string_aux( String, Pattern, Value, ProcessedPart, StringNew ) :-
        substring( String, Pattern, PatternPos ),
        !,
        %  Pattern is found in String.
        string_length( String, StringLength ),
        string_length( Pattern, PatternLength ),
        PatternPosRight is (PatternPos + PatternLength),
        ( PatternPos = 1 ->
           %  If Pattern is at the beginning.
           RestLength is (StringLength - PatternPosRight + 1),
           substring( String, PatternPosRight, RestLength, Right ),
           replace_string_aux( Right, Pattern, Value, Value, StringNew )
        ;
           %  Else.
           PatternPosLeft is (PatternPos - 1),
           substring( String, 1, PatternPosLeft, Left ),
           RestLength is (StringLength - PatternPosRight + 1),
           substring( String, PatternPosRight, RestLength, Right ),
           concat_string( [ProcessedPart, Left, Value],
                          ProcessedPartNew ),
           replace_string_aux( Right, Pattern, Value, ProcessedPartNew,
                               StringNew )
        ).

replace_string_aux( String, _Pattern, _Value, ProcessedPart, StringNew ) :-
        %  Pattern is not found in String.
        concat_strings( ProcessedPart, String, StringNew ).


%  Replaces all occurrences of the character OldChar in String
%  by the character NewChar.
:- mode replace_character(++, ++, ++, -).
replace_character( String, OldChar, NewChar, ResultString ) :-
        replace_string( String, OldChar, NewChar, ResultString ).


%  Removes a single Character from a String.
:- mode remove_character(++, ++, -).
remove_character( String, Character, ResultString ) :-
        replace_string( String, Character, "", ResultString ).


%  Finds every occurrence of the Prolog term in the Program and
%  replaces it by the Prolog term Value.
:- mode replace_term(++, ++, ++, -).
replace_term( Program, Term, Value, ProgramNew ) :-
        term_string(Term, TermS),
        term_string(Value, ValueS),
        term_string(Program, ProgramS),
        replace_string( ProgramS, TermS, ValueS, ProgramNewS ),
        term_string( ProgramNew, ProgramNewS ).




% ---------------------------------------------------------- %
%   Alpha* Expansion                                         %
% ---------------------------------------------------------- %

%  Expands the program Alpha.
%  Consume is the minimal horizon consumption of Alpha.
%  Omega_Prime is the rest program.
%  We do not have to take care of of cutting actions that reach
%  beyond the horizon, as too long final sequence will only be
%  considered up to the horizon by the Readylog interpreter.

:-mode expand_alpha(++, ++, ++, ++, -).         
expand_alpha( Alpha, Consume, Omega_Prime, Horizon, Star_Program ) :-
        expand_alpha_aux( Alpha, Consume, Omega_Prime, Horizon, [],
                          Star_Program ).

:-mode expand_alpha(++, ++, ++, ++, ++, -).         
expand_alpha_aux( Alpha, Consume, Omega_Prime, Horizon, Program,
                  Star_Program ) :-
%        printf( stdout, "Horizon consumption of %w: %w\n", [Alpha, Consume] ),
        ( (Consume > Horizon) ->
           %  End recursion.
           Star_Program = []
        ;
           %  Else keep appending Alpha.
           P_w_Alpha = [ Program | Alpha ],
           %  Conversion from comma to semicolon necessary to
           %  mark consecutive actions.
           list_to_string(P_w_Alpha, P_w_AlphaS),
           comma_to_semicolon( P_w_AlphaS, P_w_AlphaFinalS ),
           string_to_list( P_w_AlphaFinalS, P_w_AlphaFinal ),
%           printf(stdout, "Program with Alpha: %w\n", [P_w_AlphaFinal]),

           P_w_Alpha_Omega = [ P_w_Alpha | Omega_Prime ],
           %  Conversion from comma to semicolon necessary to
           %  mark consecutive actions.
           list_to_string(P_w_Alpha_Omega, P_w_Alpha_OmegaS),
           comma_to_semicolon( P_w_Alpha_OmegaS, P_w_Alpha_OmegaFinalS ),
           string_to_list( P_w_Alpha_OmegaFinalS, P_w_Alpha_OmegaFinal ),
%           printf(stdout, "Program with Alpha and Omega_Prime: %w\n",
%                  [P_w_Alpha_OmegaFinal]),

           Horizon_New is (Horizon - Consume),
           %  Continue appending Alpha. Note, that
           %  the parameter P_w_AlphaFinal does not
           %  contain Omega_Prime.
           expand_alpha_aux( Alpha, Consume, Omega_Prime, Horizon_New,
                             P_w_AlphaFinal, Star_Program_Tmp ),
           append( P_w_Alpha_OmegaFinal, Star_Program_Tmp, Star_Program )
        ).

%  Computes horizon consumption for a program. That will be the *minimal*
%  consumption for deterministic sub-programs, and the *horizon* for
%  solve sub-programs.
/** TODO: What about nondeterministic programs inside star? */
:- mode horizon_consumption(++, -).
horizon_consumption([], Consume) :- !,
        Consume = 0.

horizon_consumption([if(_Cond, Sigma1, Sigma2) | Rest], Consume) :- !, 
        horizon_consumption( Sigma1, Consume1 ),
%        printf(stdout, "if-clause... Horizon consumption of Sigma1=%w: %w\n",
%               [Sigma1, Consume1]),
        horizon_consumption( Sigma2, Consume2 ),
%        printf(stdout, "if-clause... Horizon consumption of Sigma2=%w: %w\n",
%               [Sigma2, Consume2]),
        min( Consume1, Consume2, ConsumeTmp ),
        horizon_consumption( Rest, ConsumeR ),
%        printf(stdout, "if-clause... Horizon consumption of Rest=%w: %w\n",
%               [Rest, ConsumeR]),
        Consume is ( ConsumeTmp + ConsumeR ).

horizon_consumption([if(Cond, Sigma) | Rest], Consume) :-
        horizon_consumption( [if(Cond, Sigma, [])], ConsumeIf ),
        horizon_consumption( Rest, ConsumeR ),
        Consume is ( ConsumeIf + ConsumeR ).

horizon_consumption([while(_Cond, Sigma) | Rest], Consume) :- !,
        horizon_consumption( Sigma, ConsumeWhile ),
        horizon_consumption( Rest, ConsumeR ),
        Consume is ( ConsumeWhile + ConsumeR ).

horizon_consumption([?(_Test) | Rest], Consume) :- !,
        horizon_consumption(Rest, Consume).

horizon_consumption([_Action | Rest], Consume) :-
        horizon_consumption(Rest, Consume_Rest),
%        printf(stdout, "action %w... Horizon consumption of Rest=%w: %w\n",
%               [Action, Rest, Consume_Rest]),
        Consume is (1 + Consume_Rest).

% ---------------------------------------------------------- %
%   Determination of Determinism for Procedures              %
% ---------------------------------------------------------- %

%  Determines recursively, if a proc is deterministic
%  or contains nondeterministic statements or procedures.
:- mode is_deterministic(++, -).
is_deterministic( Program, Result ) :-
        is_deterministic( Program, [], Result ).

:- mode is_deterministic(++, ++, -).
is_deterministic( [], _SeenProcs, Result ) :- !, 
        Result = true.

is_deterministic( [ProcName | Rest], SeenProcs, Result ) :-
        proc( ProcName, ProcBody ), !,
        %  To avoid an endless loop due to recursive procs,
        %  check, if we already have considered the current
        %  proc.
%        printf(stdout, "checking, if subproc %w is_deterministic...\n",
%              [ProcName]), flush(stdout),
%        printf(stdout, "SeenProcs: %w\n", [SeenProcs]), flush(stdout),
        ( memberchk( ProcName, SeenProcs ) ->
        %  Or maybe we should try unifying the argument list?
        %  member( ProcName(Args), SeenProcs ) ->
%           printf(stdout, "%w has already been considered.\n", [ProcName]),
%           flush(stdout),
           ResultTmp = true
        ;
           SeenProcsNew = [ProcName | SeenProcs],
           %  Or for unification
           %  SeenProcsNew = [SeenProcs | ProcName([args])]
           is_deterministic( ProcBody, SeenProcsNew, ResultTmp )
        ),
        ( ResultTmp = false -> 
%                printf(stdout, "... no, it isn't\n", []), flush(stdout),
                Result = false
        ;
%                printf(stdout, "... yes, it is\n", []), flush(stdout),
                is_deterministic( Rest, SeenProcsNew, Result )
        ).

is_deterministic( [if(_Cond, Sigma1, Sigma2) | Rest], SeenProcs, Result ) :- !,
        is_deterministic( Sigma1, SeenProcs, ResultTmp1 ),
        is_deterministic( Sigma2, SeenProcs, ResultTmp2 ),
        ( (ResultTmp1 = false; ResultTmp2 = false) -> 
                Result = false
        ;
                is_deterministic( Rest, SeenProcs, Result )
        ).

is_deterministic( [if(Cond, Sigma) | Rest], SeenProcs, Result ) :- !,
        is_deterministic( [if(Cond, Sigma, []) | Rest], SeenProcs, Result ).

is_deterministic( [while(_Cond, Sigma) | Rest], SeenProcs, Result ) :- !,
%        printf(stdout, "While! Sigma=%w\n", [Sigma]),
        is_deterministic( [Sigma], SeenProcs, ResultTmp ),
%        printf(stdout, "While result: %w\n", [ResultTmp]),
        ( ResultTmp = false -> 
                Result = false
        ;
                is_deterministic( Rest, SeenProcs, Result )
        ).
        
is_deterministic( [nondet(_Args) | _Rest], _SeenProcs, Result ) :- !,
        Result = false.

is_deterministic( [pickBest(_Args1, _Args2, _Args3) | _Rest], _SeenProcs,
                  Result ) :- !,
        Result = false.

is_deterministic( [star(_Args) | _Rest], _SeenProcs, Result ) :- !,
        Result = false.

is_deterministic( [_Term | Rest], SeenProcs, Result ) :- !,
        is_deterministic( Rest, SeenProcs, Result ).

is_deterministic( Prog, SeenProcs, Result ) :-
%        Prog \= [_Action],
        not(is_list(Prog)),
%        printf(stdout, "is_deterministic( [%w], SeenProcs,
%               Result)\n", [Prog, CurrentProc]),
        is_deterministic( [Prog], SeenProcs, Result ).

% ---------------------------------------------------------- %
%   Manipulation of Nondeterministic Program Sets/Lists      %
% ---------------------------------------------------------- %

%  Joins two program sets to another program set.
:- mode join_prog_lists(++, ++, -).
join_prog_lists( {P_String1}, {P_String2}, P_String_Joined ) :-
        %  Add brackets around first argument.
        concat_string( ["[", P_String1, "], "], ReducedString1 ),
        %  Add brackets around second argument.
        concat_string( ["[", P_String2, "]"], ReducedString2 ),
        %  Join argument lists.
        concat_string( [ReducedString1, ReducedString2],
                       P_String_Joined ).

%  Recursively integrates either a program set and a program list (argument
%  list for a nondeterministic choice) or two program sets into a
%  new program set by creating some kind of cross product.
:- mode integrate(++, ++, -).
integrate({P1_String}, {P2_String}, P_String_New ) :- 
%        string(P2_String),
        !,
        %  Cut away potential brackets [] inside the choice sets.
        %  So {[Program1, Program2]} would become {Program1, Program2}.
        remove_outer_list_brackets(P1_String, P1_String_Reduced),
        remove_outer_list_brackets(P2_String, P2_String_Reduced),
        integrate_aux({P1_String_Reduced}, {P2_String_Reduced}, P_String_New).

integrate({P_String}, List, P_String_New ) :- !,
        %  List is a list of programs to nondeterministically choose from.
        %  It contains commas to separate list elements, but the elements
        %  itself may contain commas (e.g., if-statements). Therefore,
        %  we first transform the program into a choice set with the help
        %  of the operator rho. It replaces all in-term commas by a
        %  placeholder __COMMA__.
        apply_rho(nondet(List), [], RhoList),
        %  Match again with a program *set* RhoList this time.
        integrate({P_String}, RhoList, P_String_New).

        
:- mode integrate_aux(++, ++, -).
%  Note, that we don't add {} to the result. It will be
%  done later to emphasise the workings of the transformation
%  operators.
integrate_aux({P_String}, {""}, P_String_New) :- !,
        P_String_New = P_String.

integrate_aux({""}, {P_String}, P_String_New) :- !,
        P_String_New = P_String.

integrate_aux({P1_String}, {P2_String}, P_String_New) :-
        string_to_list(P1_String, P1_List),
        string_to_list(P2_String, P2_List),
        %  Create some kind of cross-product.
        findall([X;Y],
                ( member(X, P1_List),               
                  member(Y, P2_List)
                ),
                P_List_Cross),
        list_to_string(P_List_Cross, P_String_Cross),
        %  Clean up string by removing unnecessary brackets.
        %  Programs are separated by commas while actions
        %  are separated by semicolons.
        %  Remove empty program (note that the empty program
        %  has been considered in the cross product already).
        replace_string(P_String_Cross, "[];", "", Tmp1 ),
        replace_string(Tmp1, ";[]", "", Tmp2 ),
        %  Remove brackets.
        remove_character(Tmp2, "[", Tmp3),
        remove_character(Tmp3, "]", P_String_Cross_Clean),
%        printf(stdout, "P_String_Cross_Clean: %w\n", [P_String_Cross_Clean]),
%        printf(stdout, "P_List_Cross: %w --> P_String_Cross: %w\n",
%               [P_List_Cross, P_String_Cross]),
        P_String_New = P_String_Cross_Clean.


%  Cuts away potential brackets [] inside the choice sets.
%  So {[Program1, Program2]} would become {Program1, Program2}.
:- mode remove_outer_list_brackets(++, -).
remove_outer_list_brackets("", Result) :- !,
        Result = "".

remove_outer_list_brackets(String, Result) :- !,
        %  Cut away [ and ]. Note that [] are not counted in StringLength.
        string_length(String, StringLength),
        %  Check if first character is [ and last character is ].
        ( (substring( String, 1, 1, "["),
           substring( String, StringLength, 1, "]") ) ->
                 %  Cut away brackets.
                 StringLengthReduced is (StringLength-2),
                 substring( String, 2, StringLengthReduced, Result )
           ;
                 %  No brackets found.
                 Result = String
        ).
        

:- writeln("** loading iplutils.pl\t\t DONE").

