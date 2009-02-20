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

/* ================================================================== */
/*  IPL UTILITIES                                                     */
/* ================================================================== */

/** The IPL utilities can be used by both the iplpreprocessor and the
 *  iplearner. The modular design allows other Readylog files to use
 *  them as well.
 */

:- write("** loading iplutils.pl\n"). 

/* ----------------------------------------------------------
   utilities                           
---------------------------------------------------------- */

/* ----------------------------------------------------------
   String & Term Manipulation                           
---------------------------------------------------------- */

/** During compilation, Prolog rips the cout arguments from their
 *  quotation marks; we now give them back. We do this in a
 *  preprocessing step, as we would otherwise miss some
 *  (e.g., in deterministic subprograms of if-clauses). */
fix_couts( [], Fixed ) :- !,
        Fixed = [].

fix_couts( [cout(V) | RestProgram], Fixed ) :- !,
%        printf( stdout, "\nEncountered cout(V).\n", [] ),
        fix_couts( RestProgram, FixedRest ),
        term_string( V, VString ),
        Fixed = [ cout(VString) | FixedRest ].

fix_couts( [cout(F, V) | RestProgram], Fixed ) :- !,
%        printf( stdout, "\nEncountered cout(F, V).\n", [] ),
        fix_couts( RestProgram, FixedRest ),
%        term_string( V, VString ),
        term_string( F, FString ),
        Fixed = [ cout(FString, V) | FixedRest ].

fix_couts( [cout(Color, F, V) | RestProgram], Fixed ) :- !,
%        printf( stdout, "\nEncountered cout(Color, F, V).\n", [] ),
        fix_couts( RestProgram, FixedRest ),
%        term_string( V, VString ),
        term_string( F, FString ),
        Fixed = [ cout(Color, FString, V) | FixedRest ].

fix_couts( [if(Cond, Sigma1, Sigma2) | RestProgram], Fixed ) :- !,
%        printf( stdout, "\nEncountered if.\n", [] ),
        fix_couts( Sigma1, FixedSigma1 ),
        fix_couts( Sigma2, FixedSigma2 ),
        fix_couts( RestProgram, FixedRest ),
        Fixed = [ if(Cond, FixedSigma1, FixedSigma2) | FixedRest ].

fix_couts( [if(Cond, Sigma) | RestProgram], Fixed ) :-
        fix_couts( [if(Cond, Sigma, []) | RestProgram], Fixed ).

fix_couts( [while(Cond, Sigma) | RestProgram], Fixed ) :- !,
%        printf( stdout, "\nEncountered while.\n", [] ),
        fix_couts( Sigma, FixedSigma ),
%        printf( stdout, "FixedSigma: %w\n", [FixedSigma] ),
        fix_couts( RestProgram, FixedRest ),
%        printf( stdout, "FixedRest: %w\n", [FixedRest] ),
        Fixed = [ while(Cond, FixedSigma) | FixedRest ].
%        printf( stdout, "FixedTmp: %w\n", [FixedTmp] ),
%        Fixed = FixedTmp.

fix_couts( Program, Fixed ) :-
        not(is_list(Program)),
        % Program \= [_Action],
        !,
%        printf( stdout, "\nEncountered single action %w.\n", [Program] ),
        Fixed = Program.

fix_couts( [Term | RestProgram], Fixed ) :- !,
%        printf( stdout, "\nEncountered Term %w.\n", [Term] ),
%        printf( stdout, "RestProgram: %w.\n", [RestProgram] ),
        fix_couts( RestProgram, FixedRest ),
%        printf( stdout, "FixedRest: %w.\n", [FixedRest] ),
%        ( ground(Fixed) ->
%                    printf( stdout, "Fixed already defined!\n", [] )
%        ;
%                    printf( stdout, "Fixed undefined.\n", [] )
%        ),
        /** TODO: remove additional outer [], when the program consists
         *  of more than a single action. */
        Fixed = [Term | FixedRest].

/** replace commas that separate consecutive actions by semicolons,
 *  but ignores commas in tests, if-conditions, and while statements
 *  by commas */
comma_to_semicolon( "", String_New ) :- !,
        String_New = "".

comma_to_semicolon( String, String_New ) :-
        substring( String, 1, 3, "if(" ),
%        printf( stdout, "if( pattern found\n", [] ),
        !,
        comma_to_semicolon_aux( String, String_New ).
        
comma_to_semicolon( String, String_New ) :-
        substring( String, 1, 6, "while(" ),
%        printf( stdout, "while( pattern found\n", [] ),
        !,
        comma_to_semicolon_aux( String, String_New ).
        
comma_to_semicolon( String, String_New ) :-
        substring( String, 1, 2, "?(" ),
%        printf( stdout, "?( pattern found\n", [] ),
        !,
        comma_to_semicolon_aux( String, String_New ).

comma_to_semicolon( String, String_New ) :- !, 
%        printf( stdout, "no special statement found\n", [] ),
        ( ( substring( String, IfPos, 3, "if(" ) ;
            substring( String, WhilePos, 6, "while(" ) ;
            substring( String, TestPos, 2, "?(" )
          ) ->
              /** If there still is any such term in the String */
              /** Check, if positions are instantiated. */
              ( ground(IfPos) -> ( ground(WhilePos) -> min(IfPos, WhilePos, MinTmp),
                                                         ( ground(TestPos) -> /** All variables are defined */
                                                                               min(TestPos, MinTmp, MinPos)
                                                         ;
                                                                               /** Only IfPos and WhilePos are defined */
                                                                               MinPos = MinTmp
                                                         )
                                  ;
                                  /** WhilePos is not defined */                        
                                                         ( ground(TestPos) -> /** Only IfPos and MinPos are defined */
                                                                               min(IfPos, TestPos, MinPos)
                                                         ;
                                                                               /** Only IfPos is defined */
                                                                               MinPos = IfPos
                                                         )
                                  )
              ;
                                  /** IfPos is not defined */
                                  ( ground(WhilePos) -> ( ground(TestPos) -> /** Only WhilePos and TestPos are defined */
                                                                               min(WhilePos, TestPos, MinPos)
                                                         ;
                                                                               /** Only WhilePos is defined */
                                                                               MinPos = WhilePos
                                                         )
                                  ;
                                                         /** TestPos must be defined (only) as a substring was found */
                                                         MinPos = TestPos
                                  )
              ),
%              printf( stdout, "********** MinPos: %w\n", [MinPos] ),
              string_length( String, StringLength ),
              /** String left from statement */
              /** Note that MinPos > 0. Otherwise, a previous clause would have matched. */
              MinPosLeft is (MinPos - 1),
              substring( String, 1, MinPosLeft, Left ),
              replace_character( Left, ",", ";", LeftResult ),

              /** Rest string */
              MinPosRight is (MinPos + 1),
              RestLength is (StringLength - MinPosRight + 2),
              substring( String, MinPos, RestLength, Rest ),
%              printf( stdout, "********** Rest: %w\n", [Rest] ),
              comma_to_semicolon( Rest, RestResult ),
              concat_strings( LeftResult, RestResult, String_New )
%              printf( stdout, "********** concat_strings( %w, %w, String_New )\n", [LeftResult, RestResult] )
        ;
              /** No special term found in the String */                                    
              /** Commas can all be replaced */
              replace_character(String, ",", ";", String_New)
        ).
        
comma_to_semicolon_aux( String, String_New ) :-
        string_length( String, StringLength ),
        substring( String, ClosingBracketPos, 1, ")" ),

        /** Statement string */
        ClosingBracketPosLeft is (ClosingBracketPos - 1),
        substring( String, 1, ClosingBracketPosLeft, Left ),
        /** Replace commas in the statement by placeholder.
         *  Otherwise integrate clause would interpret commas
         *  as separators for different programs.
         *  After integration, when converting the
         *  nondeterministic set into a vector, we replace the
         *  placeholders by commas again */
        replace_character( Left, ",", "__COMMA__", LeftResult ),

        /** Rest string */
        RightLength is (StringLength - ClosingBracketPos + 1),
        substring( String, ClosingBracketPos, RightLength, Right ),
        comma_to_semicolon( Right, RightResult ),

        concat_strings( LeftResult, RightResult, String_New ).

list_to_string( [], ResultString ) :- !,
        ResultString = "".

list_to_string( List, ResultString ) :- !,
        /** cut away brackets around the argument list */
        term_string(List, String),
        string_length(String, StringLength),
        ReducedLength is (StringLength - 2),
        substring( String, 2, ReducedLength, TmpString ),
        /** remove all "'" that Prolog builds in when
         *  converting term->string */
        remove_character( TmpString, "'", ResultString ).        

string_to_list( "", List ) :- !,
        List = [].

string_to_list( String, List ) :- !,
        split_string( String, ",", " \t", StringList),
        findall( X,
                 ( member(Y, StringList), atom_string(X,Y) ),
                 List ).

fluent_values_to_string( [], ResultString ) :- !,
        ResultString = "".

fluent_values_to_string( List, ResultString ) :- !,
        /** cut away brackets around the argument list */
        term_string(List, String),
        string_length(String, StringLength),
        ReducedLength is (StringLength - 2),
        substring( String, 2, ReducedLength, TmpString1 ),
        /** remove all " that Prolog builds in when
         *  converting (string)term->string */
        remove_character( TmpString1, "\"", TmpString2 ),
        /** recover commas that were part of the fluent value. */
        replace_string( TmpString2, "COMMA", "\\,",
                        TmpString3 ),
        /** recover " that were part of the fluent value. */
        replace_string( TmpString3, "QUOTATION", "\"",
                        ResultString ).        

remove_character( String, Character, ResultString ) :- !,
        ( substring( String, Character, PrimePos ) ->
              /** If there still is such a character in the string */
              PrimePosRight is (PrimePos + 1),
              string_length( String, StringLength ),
              RightLength is (StringLength - PrimePos),
              ( PrimePos = 1 ->
                     /** If Character is the first character */
                     substring( String, 2, RightLength, RightFromPrime),
                     remove_character(RightFromPrime, Character, ResultString)
              ;
                     /** Else */
                     PrimePosLeft is (PrimePos - 1),
                     substring( String, 1, PrimePosLeft, LeftFromPrime),
                     ( PrimePos = StringLength ->
                            /** If Character is the last character */
                            remove_character(LeftFromPrime, Character, ResultString)
                     ;
                            /** Character is found somewhere in the middle */
                            substring( String, PrimePosRight,
                                       RightLength, RightFromPrime),
                            remove_character(LeftFromPrime, Character, LeftResult),
                            remove_character(RightFromPrime, Character, RightResult),
                            concat_strings(LeftResult, RightResult, ResultString)
                     )
              )
        ;
              /** Otherwise end recursion */
              ResultString = String
        ).

replace_character( String, OldChar, NewChar, ResultString ) :- !,
        ( substring( String, OldChar, PrimePos ) ->
              /** If there still is such a character in the string */
              PrimePosRight is (PrimePos + 1),
              string_length( String, StringLength ),
              RightLength is (StringLength - PrimePos),
              ( PrimePos = 1 ->
                     /** If Character is the first character */
                     substring( String, 2, RightLength, RightFromPrime),
                     replace_character(RightFromPrime, OldChar, NewChar, ResultStringTmp),
                     concat_strings(NewChar, ResultStringTmp, ResultString)
              ;
                     /** Else */
                     PrimePosLeft is (PrimePos - 1),
                     substring( String, 1, PrimePosLeft, LeftFromPrime),
                     ( PrimePos = StringLength ->
                            /** If Character is the last character */
                            replace_character(LeftFromPrime, OldChar, NewChar, ResultStringTmp),
                            concat_strings(ResultStringTmp, NewChar, ResultString)
                     ;
                            /** Character is found somewhere in the middle */
                            substring( String, PrimePosRight,
                                       RightLength, RightFromPrime),
                            replace_character(LeftFromPrime, OldChar, NewChar, LeftResult),
                            replace_character(RightFromPrime, OldChar, NewChar, RightResult),
                            concat_string([LeftResult, NewChar, RightResult], ResultString)
                     )
              )
        ;
              /** Otherwise end recursion */
              ResultString = String
        ).

replace_string( "", _Pattern, _Value, String_New ) :-!,
        String_New = "".

replace_string( String, Pattern, Value, String_New ) :-!,
        ( substring( String, Pattern, PatternPos ) ->
              string_length( String, StringLength ),
              string_length( Pattern, PatternLength ),
              PatternPosRight is (PatternPos + PatternLength),
              ( PatternPos = 1 ->
                    /** If Pattern is at the beginning */
%                    printf( stdout, "%w found at the beginning.\n", [Pattern] ),
                    RestLength is (StringLength - PatternPosRight + 1),
                    substring( String, PatternPosRight, RestLength, Right ),
%                    printf( stdout, "replace_string( %w, %w, %w, String_Tmp )\n",
%                                    [Right, Pattern, Value] ),
                    replace_string( Right, Pattern, Value, String_Tmp ),
%                    printf( stdout, "String_Tmp: %w\n", [String_Tmp] ),
                    concat_strings( Value, String_Tmp, String_New )
              ;
                    /** Else */
                    PatternPosLeft is (PatternPos - 1),
                    substring( String, 1, PatternPosLeft, Left ),
                    replace_string( Left, Pattern, Value, String_Left_Tmp ),
                    RestLength is (StringLength - PatternPosRight + 1),
                    substring( String, PatternPosRight, RestLength, Right ),
                    replace_string( Right, Pattern, Value, String_Right_Tmp ),
                    concat_string( [String_Left_Tmp, Value, String_Right_Tmp], String_New )
              )
        ;
              /** If Pattern is not found in String */
              String_New = String
        ).

replace_term( Program, Term, Value, Program_New ) :-!,
        term_string(Term, TermS),
        term_string(Value, ValueS),
%        list_to_string(Program, ProgramS),
        term_string(Program, ProgramS),
%        printf( stdout, "*1*replace_term_aux( %w, %w, %w, %w )\n",
%                [ProgramS, TermS, ValueS, Program_NewS]),
        replace_term_aux( ProgramS, TermS, ValueS, Program_NewS ),
%        split_string(ProgramS, ValueS, "", Program_Tmp),
%        string_to_list(Program_NewS, Program_New).
        term_string( Program_New, Program_NewS ). /** Correct? Or string_to_list w/o []? */

replace_term_aux( "", _Term, _Value, Program_New ) :- !,
        Program_New = "".

replace_term_aux( Program, Term, Value, Program_New ) :- !,
%        printf( stdout, "*2* replace_term_aux(%w, %w, %w, Program_New, stdout)\n",
%                [Program, Term, Value] ),
        ( substring( Program, Term, TermPos ) ->
              /** If Term is found in the string Program */
%              printf( stdout, "Term is found...\n", [] ),
              string_length( Program, ProgramLength ),
              string_length( Term, TermLength ),
              TermPosRight is (TermPos + TermLength),
              ( TermPos = 1 ->
                    /** If Term is at the beginning */
%                    printf( stdout, "... at the beginning.\n", [] ),
                    substring( Program, TermPosRight, TermLength, Right ),
                    replace_term_aux( Right, Term, Value, Program_Tmp ),
                    concat_strings( Value, Program_Tmp, Program_New )
              ;
                    /** Else */
%                    printf( stdout, "... in the middle.\n", [] ),
                    TermPosLeft is (TermPos - 1),
                    substring( Program, 1, TermPosLeft, Left ),
                    replace_term_aux( Left, Term, Value, Program_Left_Tmp ),
%                    printf( stdout, "Left string successfully replaced by %w\n", [Program_Left_Tmp] ),
                    RestLength is (ProgramLength - TermPosRight + 1),
                    substring( Program, TermPosRight, RestLength, Right ),
%                    printf( stdout, "Right string: %w\n", [Right] ),
                    replace_term_aux( Right, Term, Value, Program_Right_Tmp ),
%                    printf( stdout, "Right string successfully replaced by %w\n", [Program_Right_Tmp] ),
                    concat_string( [Program_Left_Tmp, Value, Program_Right_Tmp], Program_New )
              )
        ;
              /** If Term is not found in the string Program */
%              printf( stdout, "Term is *not* found...\n", [] ),
              Program_New = Program
        ).


/* ----------------------------------------------------------
   Alpha* Expansion
---------------------------------------------------------- */

expand_alpha( Alpha, Consume, Omega_Prime, Horizon, Program, Star_Program ) :-
         /** Consume is the minimal horizon consumption of Alpha.
          *  If this number is =< Horizon, then append another Alpha.
          *  A too long final sequence will only be considered up to
          *  the horizon by the Readylog interpreter.
          */
%         printf( stdout, "Horizon consumption of %w: %w\n", [Alpha, Consume] ),
         ( (Consume > Horizon) -> /** End recursion. */
                                  Star_Program = []
         ;
                                  /** Else keep appending Alpha */
                                  P_w_Alpha = [ Program | Alpha ],
                                  /** Conversion from comma to semicolon necessary to
                                   *  mark consecutive actions. */
                                  list_to_string(P_w_Alpha, P_w_AlphaS),
                                  comma_to_semicolon( P_w_AlphaS, P_w_AlphaFinalS ),
                                  string_to_list( P_w_AlphaFinalS, P_w_AlphaFinal ),
%                                  printf(stdout, "Program with Alpha: %w\n", [P_w_AlphaFinal]),

                                  P_w_Alpha_Omega = [ P_w_Alpha | Omega_Prime ],
                                  /** Conversion from comma to semicolon necessary to
                                   *  mark consecutive actions. */
                                  list_to_string(P_w_Alpha_Omega, P_w_Alpha_OmegaS),
                                  comma_to_semicolon( P_w_Alpha_OmegaS, P_w_Alpha_OmegaFinalS ),
                                  string_to_list( P_w_Alpha_OmegaFinalS, P_w_Alpha_OmegaFinal ),
%                                  printf(stdout, "Program with Alpha and Omega_Prime: %w\n", [P_w_Alpha_OmegaFinal]),

                                  Horizon_New is (Horizon - Consume),
                                  /** Continue appending Alpha. Note, that
                                   *  the parameter P_w_AlphaFinal does not
                                   *  contain Omega_Prime. */
                                  expand_alpha( Alpha, Consume, Omega_Prime, Horizon_New, P_w_AlphaFinal,
                                                Star_Program_Tmp ),
                                  append( P_w_Alpha_OmegaFinal, Star_Program_Tmp, Star_Program )
         ).

/** Compute horizon consumption for program. That will be the *minimal*
 *  consumption for deterministic sub-programs, and the *horizon* for
 *  solve sub-programs. */
/** TODO: What about nondeterministic programs inside star? */
horizon_consumption([], Consume) :- !,
        Consume = 0.

horizon_consumption([if(_Cond, Sigma1, Sigma2) | Rest], Consume) :- !, 
        horizon_consumption( Sigma1, Consume1 ),
%         printf( stdout, "if-clause... Horizon consumption of Sigma1=%w: %w\n", [Sigma1, Consume1] ),
        horizon_consumption( Sigma2, Consume2 ),
%         printf( stdout, "if-clause... Horizon consumption of Sigma2=%w: %w\n", [Sigma2, Consume2] ),
        min( Consume1, Consume2, ConsumeTmp ),
        horizon_consumption( Rest, ConsumeR ),
%         printf( stdout, "if-clause... Horizon consumption of Rest=%w: %w\n", [Rest, ConsumeR] ),
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

horizon_consumption([_Action | Rest], Consume) :- !,
        horizon_consumption(Rest, Consume_Rest),
%         printf( stdout, "action %w... Horizon consumption of Rest=%w: %w\n", [Action, Rest, Consume_Rest] ),
        Consume is (1 + Consume_Rest).

/* ----------------------------------------------------------
   Determination of Determinism for Procedures
---------------------------------------------------------- */

is_deterministic( [], Result ) :-
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

is_deterministic( [if(Cond, Sigma) | Rest], Result ) :-
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

is_deterministic( [_Term | Rest], Result ) :- !,
        is_deterministic( Rest, Result ).

/* ----------------------------------------------------------
   Manipulation of Nondeterministic Program Sets/Lists
---------------------------------------------------------- */

join_prog_lists( {P_String1}, {P_String2}, {P_String_Joined} ) :-
        /** add brackets around first argument */
        concat_string( ["[", P_String1, "], "], ReducedString1 ),
        /** add brackets around second argument */
        concat_string( ["[", P_String2, "]"], ReducedString2 ),
        /** join argument lists */
        concat_strings( ReducedString1, ReducedString2, P_String_Joined ).

integrate({P1_String}, {P2_String}, {P_String_New} ) :- 
        string(P2_String),
        !,
        string_to_list(P2_String, P2_List),
        integrate({P1_String}, P2_List, {P_String_New}).

integrate({P_String}, List, {P_String_New} ) :- !,
%        printf(stdout, "*** Checking if there are brackets in %w...\n ", [P_String]),
        /** cut away [ and ]. Note that [] are not counted in StringLength */
        string_length(P_String, StringLength),
        /** check if first character is [ and last character is ] */
        ( (substring( P_String, 1, 1, "["),
           substring( P_String, StringLength, 1, "]") ) ->
%                printf(stdout, "There are brackets ***\n", []),
                 /** cut away brackets */
                 StringLengthReduced is (StringLength-2),
                 substring( P_String, 2, StringLengthReduced, ReducedString ),
                 integrate({ReducedString}, List, {P_String_New})
           ;
%                printf(stdout, "No brackets found ***\n", []),
                 integrate_aux({P_String}, List, {P_String_New})
        ).

integrate_aux({P_String}, [], {P_String_New}) :- !,
%        printf(stdout, "*integrate case 1*\n", []),
        P_String_New = P_String.

integrate_aux({""}, List, {P_String_New}) :- !,
%        printf(stdout, "*integrate case 4* with [Term]=[%w]\n", [Term]),
        P_List = [],
%        printf(stdout, "P_String: "" --> P_List: %w\n", [P_List]),
        append(P_List, List, P_List_New),
        list_to_string(P_List_New, P_String_New).
%        printf(stdout, "P_List_New: %w --> P_String_New: %w\n", [P_List_New, P_String_New]).

integrate_aux({P_String}, List, {P_String_New}) :- !,
        /** test, if List really is a list */ 
%        printf(stdout, "*integrating a program list*\n", []),
%        printf(stdout, "%w ---integrate--> {%w}\n", [List, P_String]),
        string_to_list(P_String, P_List),
%        printf(stdout, "P_List: %w\n", [P_List]),
        /** create some kind of cross-product */
        findall([X;Y],
                ( member(X, P_List),               
                  member(Y, List) ),
                P_List_Cross),
        list_to_string(P_List_Cross, P_String_Cross),
        /** Clean up string by removing unnecessary brackets.
         *  Programs are separated by commas while actions
         *  are separated by semicolons. */
        /** Remove empty program (note that the empty program
         *  has been considered in the cross product already */
        replace_string(P_String_Cross, "[];", "", Tmp1 ),
        replace_string(Tmp1, ";[]", "", Tmp2 ),
        /* remove brackets */
        remove_character(Tmp2, "[", Tmp3),
        remove_character(Tmp3, "]", P_String_Cross_Clean),
%        printf(stdout, "P_String_Cross_Clean: %w\n", [P_String_Cross_Clean]),

%        printf(stdout, "P_List_Cross: %w --> P_String_Cross: %w\n", [P_List_Cross, P_String_Cross]),
        P_String_New = P_String_Cross_Clean.

        

:- writeln("** loading iplutils.pl\t\t DONE").

