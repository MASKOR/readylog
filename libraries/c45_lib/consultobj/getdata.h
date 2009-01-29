/*************************************************************************/
/*									 */
/*	Get case descriptions from data file				 */
/*	--------------------------------------				 */
/*	HEADER  							 */
/*************************************************************************/
//
// Created by Stefan Jacobs <Stefan_J@gmx.de>
//
// Created:         2002-06-15
// Last Modified:   2002-06-15
//


#ifndef _C45_GETDATA_H_
#define _C45_GETDATA_H_


#include "defns.i"
#include "types.i"
#include "extern.i"

#define Inc 2048


/*  Read raw case descriptions from file with given extension.		 */
/*  On completion, cases are stored in array Item in the form		 */
/*  of Descriptions (i.e. arrays of attribute values), and		 */
/*  MaxItem is set to the number of data items.				 */
void GetData( String Extension );


/*  Read a raw case description from file Df.				 */
/*  For each attribute, read the attribute value from the file.		 */
/*  If it is a discrete valued attribute, find the associated no.	 */
/*  of this attribute value (if the value is unknown this is 0).	 */
/*  Returns the Description of the case (i.e. the array of		 */
/*  attribute values).							 */
Description GetDescription(FILE *Df);



#endif
