/*		HEADER  						 */
/*************************************************************************/
//
// Created by Stefan Jacobs <Stefan_J@gmx.de>
//
// Created:         2002-06-15
// Last Modified:   2002-06-15
//


#ifndef _C45_CONSULT_H_
#define _C45_CONSULT_H_


#include "defns.i"
#include "types.i"
#include "extern.i"
#include "getdata.h"
#include "trees.h"
#include "getnames.h"
#include "userint.h"
#include "header.h"



		/*  External data  -- see c4.5.c for meanings  */

short		MaxAtt, MaxClass, MaxDiscrVal;

ItemNo		MaxItem;

Description	*Item;

DiscrValue	*MaxAttVal;

String		*ClassName,
		*AttName,
		**AttValName,
		FileName = "DF";

char		*SpecialStatus;

short		VERBOSITY = 0,
		TRACE     = 0;


RangeDescRec RangeDesc;

Tree	DecisionTree;			/* tree being used */

float	*LowClassSum,			/* accumulated lower estimates */
	*ClassSum = Nil;		/* accumulated central estimates */

#define Fuzz	0.01			/* minimum weight */



/*  Classify the extended case description in RangeDesc using the	 */
/*  given subtree, by adjusting the values ClassSum and LowClassSum	 */
/*  for each class, indicating the likelihood of the case being  	 */
/*  of that class.						   	 */
void ClassifyCase( Tree Subtree, float Weight );


/*  Interpolate a single value between Lower, Cut and Upper		 */
float Interpolate( Tree t, float v );


/*  Compute the area under a soft threshold curve to the right of a	 */
/*  given value.							 */
float Area( Tree t, float v );


/*  Process a single case				  	 */
void InterpretTree();


/*  Print the chosen class with certainty factor and range	  	 */
void Decision( ClassNo c, float p, float lb, float ub);


#endif
