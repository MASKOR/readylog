
/***********************************************************************
 * Description: Contains programs for a convenient golog shell.
 *
 * last modified: $Date: 2006/04/11 13:59:17 $
 *            by: $Author: tim $
 *
 * $Id: icp_shell_2.pl,v 2.0 2006/04/11 13:59:17 tim Exp $
 **********************************************************************/

/*
 * icp_shell prolog implementation file by Carsten Gester & Stefan Jacobs
 * Copyright (C) 2004 KBSG, Aachen University <Stefan_J@gmx.de>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

:- write( "** Loading icp_shell.pl ...\n" ), flush( output ).

% load definitions and external c progs
:- load("icp_shell_interface").
:- ensure_loaded("icp_shell_interface.pl").

% disable syntax-error handling
:- setval(icp_shell_syntax_error,false).

% Return the current Eclipse Time. Kind'o'a timestamp.
%   Parameter 1: Returns current time.
currentTime2( CPUTime, RealTime ) :-
	statistics( times, [CPUTime, _SYS1, RealTime] ), !.

% Perform term under control of stopwatch and return the used time.
%   Parameter 1: Term to execute
%   Parameter 2: Returned time usage
stopWatch2( Term, CPUTime, RealTime ) :-
        currentTime2( CPUStart, RealStart ),
        Term,
        currentTime2( CPUEnd, RealEnd ),
        CPUTime is CPUEnd - CPUStart, 
        RealTime is RealEnd - RealStart,
        !.

% create list of readylog keywords for auto-completion
search_keywords(L) :-
        ( prolog_function(_Y01)->findall(X01,prolog_function(X01),L01); L01 = [] ),
	( stoch_proc(_Y02)->findall(X02,stoch_proc(X02),L02); L02 = [] ),
	( prim_action(_Y03)->findall(X03,prim_action(X03),L03); L03 = [] ),
	( exog_action(_Y04)->findall(X04,exog_action(X04),L04); L04 = [] ),
	( prim_fluent(_Y05)->findall(X05,prim_fluent(X05),L05); L05 = [] ),
	( special_fluent(_Y06)->findall(X06,special_fluent(X06),L06); L06 = [] ),
	( events_list(_Y07)->findall(X07,special_fluent(X07),L07); L07 = [] ),
	( cont_fluent(_Y08)->findall(X08,cont_fluent(X08),L08); L08 = [] ),
	( exog_prim_fluent(_Y09)->findall(X09,exog_prim_fluent(X09),L09); L09 = [] ),
	( exog_cont_fluent(_Y10)->findall(X10,exog_cont_fluent(X10),L10); L10 = [] ),
	( proc(_Y11,_Y11a)->findall(X11,proc(X11,_X11a),L11); L11 = [] ),
	flatten([L01,L02,L03,L04,L05,L06,L07,L08,L09,L10,L11,
		 pconc(_),
		 "if(_)",
		 while(_),
		 withCtrl(_),
		 applyPolicy(_),
		 tryAll(_),
		 withPol(_),
		 loop(_),
		 interrupt(_),
		 prob(_),
		 waitFor(_),
		 solve(_),
		 nondet(_),
		 pickBest(_),
		 whenever(_),
		 and(_),
		 or(_),
		 and(_,_),
		 or(_,_),
 		 neg(_),
		 not(_),
		 nil,
		 true,
		 false
		],L).

% syntaxerror handling
syntax_error :-
        printColor(red,"\aSYNTAX ERROR!\n",[]), setval(icp_shell_syntax_error,true),!.


% Icp Shell main predicate:
%  Parameter 1: Counter of current input
%  Parameter 2: all golog Keywords
%
% This predicate tries to match four cases:
%   Case 1: Empty input -> do nothing, call recursively again.
%   Case 2: Input equals 'quit' -> quit with true
%   Case 3: All other cases -> call icp with the string 
%                   interpreted as a term
icp_shell(  _KeywordList ) :-
	term_string(_KeywordList,_SKeywordList),
	% call external c_icp_shell
        c_icp_shell( _String, _SKeywordList ),
        (
            % empty string case:
            (
                append_strings( _String, "", "" ) ->
                (
                    icp_shell(  _KeywordList), !
                )
            );   
            % quit case:
            (
                append_strings( _String, "", "quit" ) ->
                (
                    true, !
                )
            );
            % program case:
            (
                term_string( _Term, _String ),
                getval(icp_shell_syntax_error,_Error),
                (
                    _Error = false ->
                    (
                        _IcpTerm = icp( [exogf_Update, _Term] ),
                        stopWatch2( _IcpTerm, CPUTime, RealTime ),
                        printf( "SUCCEED time usage: CPU:%w Real:%w\n", 
                                [CPUTime, RealTime] ), flush( output ),
                        icp_shell( _KeywordList ), !
                    );
                    (
                        setval(icp_shell_syntax_error,false),
                        icp_shell( _KeywordList ), !
                    )
                )
            )
        ).

	    
% Startup the icp_shell.
%
icp_shell :-
        write( " I C P - S H E L L   S T A R T U P !\n" ), flush( output ),
        write( "====================================\n\n" ), flush( output ),
        set_event_handler(7,syntax_error/0),
	search_keywords( _KeywordList ),
	icp_shell( _KeywordList ).


% Shortcut to start the icp_shell. 
%
i :-
	icp_shell.


:- write( "** Loading icp_shell.pl done!\n\n" ), flush( output ).
