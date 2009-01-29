/*************************************************************************/
/*									 */
/*	Print header for all C4.5 programs				 */
/*	----------------------------------				 */
/*	       IMPLEMENTATION						 */
/*************************************************************************/
//
// Modified by Stefan Jacobs <Stefan_J@gmx.de>
//
// Created:         2002-06-15
// Last Modified:   2002-06-15
//


#ifndef _C45_TREES_C_
#define _C45_HEADER_C_


#include "header.h"
#include <stdio.h>


void PrintHeader( char* Title )
/*  -----------  */
{
    char *ctime(), TitleLine[80];
    long clock, time();
    short Underline;

    clock = time(0);
    sprintf(TitleLine, "C4.5 [release %s] %s", RELEASE, Title);
    printf("\n%s\t%s", TitleLine, ctime(&clock));

    Underline = strlen(TitleLine);
    while ( Underline-- ) 
      {
	putchar('-');
      }
    putchar('\n');

    return;
}


#endif
