/* ***************************************************************************
 *
 *           $Id:$
 *        author: Stefan Schiffer <schiffer@cs.rwth-aachen.de>
 *   description: eXecution TRAnsformation which maps primitive actions 
 *                to something meaningful in the real world ...
 *
 * ************************************************************************ */

xTra(cout(V), _H) :- !,
	printf("%w", [V]), flush(output).


xTra(cout(F, V), _H) :- !,
	printf(F, V), flush(output).

xTra(cout(Color,F,V),_H) :- !,
	printColor(Color,F,V).

xTra(Action,_S) :-
	printColor( red, " xTra: DON'T KNOW HOW TO HANDLE '%w'\n", [Action]).
