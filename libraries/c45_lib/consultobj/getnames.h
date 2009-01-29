/*************************************************************************/
/*									 */
/*	Get names of classes, attributes and attribute values		 */
/*	-----------------------------------------------------		 */
/*			HEADER   					 */
/*************************************************************************/
//
// Created by Stefan Jacobs <Stefan_J@gmx.de>
//
// Created:         2002-06-15
// Last Modified:   2002-06-15
//


#ifndef _C45_GETNAMES_H_
#define _C45_GETNAMES_H_


#include "defns.i"
#include "types.i"
#include "extern.i"

#include <sys/types.h>
#include <sys/stat.h>


#define  Space(s)	((s == ' ') || (s == '\n') || (s == '\t'))
#define  SkipComment	while ( ( c = getc(f) ) != '\n' )


/*  Read a name from file f into string s, setting Delimiter.		 */
/*  - Embedded periods are permitted, but periods followed by space	 */
/*    characters act as delimiters.					 */
/*  - Embedded spaces are permitted, but multiple spaces are replaced	 */
/*    by a single space.						 */
/*  - Any character can be escaped by '\'.				 */
/*  - The remainder of a line following '|' is ignored.			 */
Boolean ReadName(FILE *f, String s);


/*  Read the names of classes, attributes and legal attribute values.	 */
/*  On completion, these names are stored in:				 */
/*	ClassName	-  class names					 */
/*	AttName		-  attribute names				 */
/*	AttValName	-  attribute value names			 */
/*  with:								 */
/*	MaxAttVal	-  number of values for each attribute		 */
/*  Other global variables set are:					 */
/*	MaxAtt		-  maximum attribute number			 */
/*	MaxClass	-  maximum class number				 */
/*	MaxDiscrVal	-  maximum discrete values for any attribute	 */
/*  Note:  until the number of attributes is known, the name		 */
/*	   information is assembled in local arrays			 */
void GetNames();


/* Locate value Val in List[First] to List[Last] */
int Which( String Val, String List[], short First, short Last);


/* Allocate space then copy string into it */
String CopyString( String x );


/* Error messages */
void Error( short n, String s1, String s2);


#endif
