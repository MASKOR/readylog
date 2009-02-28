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

fix_couts( [cout(V) | RestProgram], Fixed ) :- !,
        fix_couts( RestProgram, FixedRest ),
        term_string( V, VString ),
        Fixed = [ cout(VString) | FixedRest ].

fix_couts( [cout(F, V) | RestProgram], Fixed ) :- !,
        fix_couts( RestProgram, FixedRest ),
        term_string( F, FString ),
        Fixed = [ cout(FString, V) | FixedRest ].

fix_couts( [cout(Color, F, V) | RestProgram], Fixed ) :- !,
        fix_couts( RestProgram, FixedRest ),
        term_string( F, FString ),
        Fixed = [ cout(Color, FString, V) | FixedRest ].

fix_couts( [if(Cond, Sigma1, Sigma2) | RestProgram], Fixed ) :- !,
        fix_couts( Sigma1, FixedSigma1 ),
        fix_couts( Sigma2, FixedSigma2 ),
        fix_couts( RestProgram, FixedRest ),
        Fixed = [ if(Cond, FixedSigma1, FixedSigma2) | FixedRest ].

fix_couts( [if(Cond, Sigma) | RestProgram], Fixed ) :-
        fix_couts( [if(Cond, Sigma, []) | RestProgram], Fixed ).

fix_couts( [while(Cond, Sigma) | RestProgram], Fixed ) :- !,
        fix_couts( Sigma, FixedSigma ),
        fix_couts( RestProgram, FixedRest ),
        Fixed = [ while(Cond, FixedSigma) | FixedRest ].

fix_couts( Program, Fixed ) :-
        not(is_list(Program)),
        %  Program is a single action.
        !,
        Fixed = Program.

fix_couts( [Term | RestProgram], Fixed ) :-
        fix_couts( RestProgram, FixedRest ),
        %  TODO: remove additional outer [], when the program consists
        %  of more than a single action.
        Fixed = [Term | FixedRest].


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

%  Removes a single Character from a String.
:- mode remove_character(++, ++, -).
remove_character( String, Character, ResultString ) :-
        substring( String, Character, PrimePos ),
        !,
        %  There still is such a character in the string.
        PrimePosRight is (PrimePos + 1),
        string_length( String, StringLength ),
        RightLength is (StringLength - PrimePos),
        ( PrimePos = 1 ->
           %  If Character is the first character.
           substring( String, 2, RightLength, RightFromPrime),
           remove_character(RightFromPrime, Character, ResultString)
        ;
           %  Else.
           PrimePosLeft is (PrimePos - 1),
           substring( String, 1, PrimePosLeft, LeftFromPrime),
           ( PrimePos = StringLength ->
              %  If Character is the last character.
              remove_character(LeftFromPrime, Character, ResultString)
           ;
              %  Character is found somewhere in the middle.
              substring( String, PrimePosRight, RightLength, RightFromPrime),
              remove_character(LeftFromPrime, Character, LeftResult),
              remove_character(RightFromPrime, Character, RightResult),
              concat_strings(LeftResult, RightResult, ResultString)
           )
        ).

remove_character( String, _Character, ResultString ) :-
        %  Character is not found in the String.
        ResultString = String.

%  Replaces all occurrences of the character OldChar in String
%  by the character NewChar.
:- mode replace_character(++, ++, ++, -).
replace_character( String, OldChar, NewChar, ResultString ) :-
        substring( String, OldChar, PrimePos ),
        !,
        %  There still is such a character in the string.
        PrimePosRight is (PrimePos + 1),
        string_length( String, StringLength ),
        RightLength is (StringLength - PrimePos),
        ( PrimePos = 1 ->
           %  If Character is the first character.
           substring( String, 2, RightLength, RightFromPrime),
           replace_character(RightFromPrime, OldChar, NewChar, ResultStringTmp),
           concat_strings(NewChar, ResultStringTmp, ResultString)
        ;
           %  Else.
           PrimePosLeft is (PrimePos - 1),
           substring( String, 1, PrimePosLeft, LeftFromPrime),
           ( PrimePos = StringLength ->
              %  If Character is the last character.
              replace_character(LeftFromPrime, OldChar, NewChar,
                                ResultStringTmp),
              concat_strings(ResultStringTmp, NewChar, ResultString)
           ;
              %  Character is found somewhere in the middle.
              substring( String, PrimePosRight, RightLength, RightFromPrime),
              replace_character(LeftFromPrime, OldChar, NewChar, LeftResult),
              replace_character(RightFromPrime, OldChar, NewChar, RightResult),
              concat_string([LeftResult, NewChar, RightResult], ResultString)
           )
        ).

replace_character( String, _OldChar, _NewChar, ResultString ) :-
        %  OldChar is not found in the String.
        ResultString = String.

%  Finds every occurrence of the string-pattern Pattern in the String and
%  replaces it by the string Value.
:- mode replace_string(++, ++, ++, -).
replace_string( "", _Pattern, _Value, String_New ) :-!,
        String_New = "".

replace_string( String, Pattern, Value, String_New ) :-
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
           replace_string( Right, Pattern, Value, String_Tmp ),
           concat_strings( Value, String_Tmp, String_New )
        ;
           %  Else.
           PatternPosLeft is (PatternPos - 1),
           substring( String, 1, PatternPosLeft, Left ),
           replace_string( Left, Pattern, Value, String_Left_Tmp ),
           RestLength is (StringLength - PatternPosRight + 1),
           substring( String, PatternPosRight, RestLength, Right ),
           replace_string( Right, Pattern, Value, String_Right_Tmp ),
           concat_string( [String_Left_Tmp, Value, String_Right_Tmp],
                          String_New )
        ).

replace_string( String, _Pattern, _Value, String_New ) :-
        %  Pattern is not found in String.
        String_New = String.

%  Finds every occurrence of the Prolog term in the Program and
%  replaces it by the Prolog term Value.
:- mode replace_term(++, ++, ++, -).
replace_term( Program, Term, Value, Program_New ) :-
        term_string(Term, TermS),
        term_string(Value, ValueS),
%        list_to_string(Program, ProgramS),
        term_string(Program, ProgramS),
%        printf( stdout, "*1*replace_term_aux( %w, %w, %w, %w )\n",
%                [ProgramS, TermS, ValueS, Program_NewS]),
        replace_term_aux( ProgramS, TermS, ValueS, Program_NewS ),
%        split_string(ProgramS, ValueS, "", Program_Tmp),
%        string_to_list(Program_NewS, Program_New).
        term_string( Program_New, Program_NewS ).  % Correct?
                                                   % Or string_to_list w/o []?

:- mode replace_term_aux(++, ++, ++, -).
replace_term_aux( "", _Term, _Value, Program_New ) :- !,
        Program_New = "".

replace_term_aux( Program, Term, Value, Program_New ) :-
        substring( Program, Term, TermPos ),
        !,
        %  Term is found in the string Program.
%        printf( stdout, "Term is found...\n", [] ),
        string_length( Program, ProgramLength ),
        string_length( Term, TermLength ),
        TermPosRight is (TermPos + TermLength),
        ( TermPos = 1 ->
           %  If Term is at the beginning.
%           printf( stdout, "... at the beginning.\n", [] ),
           substring( Program, TermPosRight, TermLength, Right ),
           replace_term_aux( Right, Term, Value, Program_Tmp ),
           concat_strings( Value, Program_Tmp, Program_New )
        ;
           %  Else.
%           printf( stdout, "... in the middle.\n", [] ),
           TermPosLeft is (TermPos - 1),
           substring( Program, 1, TermPosLeft, Left ),
           replace_term_aux( Left, Term, Value, Program_Left_Tmp ),
%           printf(stdout, "Left string successfully replaced by %w\n",
%                  [Program_Left_Tmp] ),
           RestLength is (ProgramLength - TermPosRight + 1),
           substring( Program, TermPosRight, RestLength, Right ),
%           printf(stdout, "Right string: %w\n", [Right]),
           replace_term_aux( Right, Term, Value, Program_Right_Tmp ),
%           printf(stdout, "Right string successfully replaced by %w\n",
%          [Program_Right_Tmp] ),
           concat_string( [Program_Left_Tmp, Value, Program_Right_Tmp],
                          Program_New )
        ).

replace_term_aux( Program, _Term, _Value, Program_New ) :-
        %  Term is not found in the string Program.
        Program_New = Program.



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
is_deterministic( [], Result ) :- !, 
        Result = true.

is_deterministic( [ProcName | Rest], Result ) :-
        proc( ProcName, ProcBody ), !,
        is_deterministic( ProcBody, ResultTmp ),
        ( ResultTmp = false -> 
                Result = false
        ;
                is_deterministic( Rest, Result )
        ).

is_deterministic( [if(_Cond, Sigma1, Sigma2) | Rest], Result ) :- !,
        is_deterministic( [Sigma1], ResultTmp1 ),
        is_deterministic( [Sigma2], ResultTmp2 ),
        ( (ResultTmp1 = false; ResultTmp2 = false) -> 
                Result = false
        ;
                is_deterministic( Rest, Result )
        ).

is_deterministic( [if(Cond, Sigma) | Rest], Result ) :- !,
        is_deterministic( [if(Cond, Sigma, []) | Rest], Result ).


is_deterministic( [while(_Cond, Sigma) | Rest], Result ) :- !,
%        printf(stdout, "While! Sigma=%w\n", [Sigma]),
        is_deterministic( [Sigma], ResultTmp ),
%        printf(stdout, "While result: %w\n", [ResultTmp]),
        ( ResultTmp = false -> 
                Result = false
        ;
                is_deterministic( Rest, Result )
        ).
        
is_deterministic( [nondet(_Args) | _Rest], Result ) :- !,
        Result = false.

is_deterministic( [pickBest(_Args) | _Rest], Result ) :- !,
        Result = false.

is_deterministic( [star(_Args) | _Rest], Result ) :- !,
        Result = false.

is_deterministic( [_Term | Rest], Result ) :-
        is_deterministic( Rest, Result ).

% ---------------------------------------------------------- %
%   Manipulation of Nondeterministic Program Sets/Lists      %
% ---------------------------------------------------------- %

%  Joins two program sets to another program set.
:- mode join_prog_lists(++, ++, -).
join_prog_lists( {P_String1}, {P_String2}, {P_String_Joined} ) :-
        %  Add brackets around first argument.
        concat_string( ["[", P_String1, "], "], ReducedString1 ),
        %  Add brackets around second argument.
        concat_string( ["[", P_String2, "]"], ReducedString2 ),
        %  Join argument lists.
        concat_strings( ReducedString1, ReducedString2, P_String_Joined ).

%  Recursively integrates two program sets into a
%  new program set.
:- mode integrate(++, ++, -).
integrate({P1_String}, {P2_String}, {P_String_New} ) :- 
        string(P2_String),
        !,
        string_to_list(P2_String, P2_List),
        integrate({P1_String}, P2_List, {P_String_New}).

integrate({P_String}, List, {P_String_New} ) :-
%        printf(stdout, "*** Checking if there are brackets in %w...\n ",
%              [P_String]),
        %  Cut away [ and ]. Note that [] are not counted in StringLength.
        string_length(P_String, StringLength),
        %  Check if first character is [ and last character is ].
        ( (substring( P_String, 1, 1, "["),
           substring( P_String, StringLength, 1, "]") ) ->
%                printf(stdout, "There are brackets ***\n", []),
                 %  Cut away brackets.
                 StringLengthReduced is (StringLength-2),
                 substring( P_String, 2, StringLengthReduced, ReducedString ),
                 integrate({ReducedString}, List, {P_String_New})
           ;
%                printf(stdout, "No brackets found ***\n", []),
                 integrate_aux({P_String}, List, {P_String_New})
        ).

:- mode integrate_aux(++, ++, -).
integrate_aux({P_String}, [], {P_String_New}) :- !,
%        printf(stdout, "*integrate case 1*\n", []),
        P_String_New = P_String.

integrate_aux({""}, List, {P_String_New}) :- !,
%        printf(stdout, "*integrate case 4* with [Term]=[%w]\n", [Term]),
        P_List = [],
%        printf(stdout, "P_String: "" --> P_List: %w\n", [P_List]),
        append(P_List, List, P_List_New),
        list_to_string(P_List_New, P_String_New).
%        printf(stdout, "P_List_New: %w --> P_String_New: %w\n",
%               [P_List_New, P_String_New]).

integrate_aux({P_String}, List, {P_String_New}) :-
        %  Test, if List really is a list.
%        printf(stdout, "*integrating a program list*\n", []),
%        printf(stdout, "%w ---integrate--> {%w}\n", [List, P_String]),
        string_to_list(P_String, P_List),
%        printf(stdout, "P_List: %w\n", [P_List]),
        %  Create some kind of cross-product.
        findall([X;Y],
                ( member(X, P_List),               
                  member(Y, List) ),
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

        

:- writeln("** loading iplutils.pl\t\t DONE").

