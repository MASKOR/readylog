/*************************************************************************/
/*								 	 */
/*	User interface for consulting trees and rulesets	 	 */
/*	------------------------------------------------	 	 */
/*			HEADER					 	 */
/*************************************************************************/
//
// Created by Stefan Jacobs <Stefan_J@gmx.de>
//
// Created:         2002-06-15
// Last Modified:   2002-06-15
//

#ifndef _C45_USERINT_H_
#define _C45_USERINT_H_


#define	SkipSpace	while ( (c = getchar()) == ' ' || c == '\t' )
#define Fuzz 0.01


#include "defns.i"
#include "types.i"
#include "extern.i"


typedef	struct	ValRange *RangeDescRec;
struct	ValRange
	{
	    Boolean	Known, Asked;
	    float	LowerBound, UpperBound, *Probability;
	};

extern	RangeDescRec	RangeDesc;
extern	char		Delimiter;


/* Ask for the value of attribute Att if necessary */
void CheckValue( Attribute Att, Tree T );


/* Print the range of values for attribute Att */
void PrintRange( Attribute Att );


/* Read a range of values for attribute Att or <cr> */
void ReadRange( Attribute Att, Tree T );


/* Read a discrete attribute value or range */
void ReadDiscr( Attribute Att, Tree T);


/* Read a continuous attribute value or range */
void ReadContin( Attribute Att, Tree T );


/* Clear the range description */
void Clear();


/* Skip to the end of the line of input */
void SkipLine( char c );


/* Try again to obtain a value for attribute Att */
void Retry( Attribute Att, Tree T );


#endif
