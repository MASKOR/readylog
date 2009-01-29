/*************************************************************************/
/*									 */
/*	Routines for displaying, building, saving and restoring trees	 */
/*	-------------------------------------------------------------	 */
/*		HEADER  						 */
/*************************************************************************/
//
// Created by Stefan Jacobs <Stefan_J@gmx.de>
//
// Created:         2002-06-15
// Last Modified:   2002-06-15
//


#ifndef _C45_TREES_H_
#define _C45_TREES_H_


#include "defns.i"
#include "types.i"
#include "extern.i"


/* Display entire decision tree T */
void PrintTree( Tree T);


/* Display the tree T with offset Sh */
void Show( Tree T, short Sh);


/* Print a node T with offset Sh, branch value v, and continue */
void ShowBranch( short Sh, Tree T, DiscrValue v);


/* Find the maximum single line size for non-leaf subtree St. */
/* The line format is					      */
/*	<attribute> <> X.xx:[ <class (<Items>)], or */
/*	<attribute> = <DVal>:[ <class> (<Items>)]   */
short MaxLine( Tree St );

/* Indent Sh columns */
void Indent( short Sh, char* Mark);



/* Save entire decision tree T in file with extension Extension	*/
void SaveTree( Tree T, String Extension);


/* Save tree T as characters */
void OutTree( Tree T);


/* Retrieve entire decision tree with extension Extension */
Tree GetTree( String Extension );


/* Retrieve tree from saved characters */
Tree InTree();


/* Stream characters to/from file TRf from/to an address */
void StreamOut( String s, int n );
void StreamIn( String s, int n);


/* Free up space taken up by tree Node */
void ReleaseTree( Tree Node );


/* Construct a leaf in a given node */
Tree Leaf(ItemCount *ClassFreq, ClassNo NodeClass, 
	  ItemCount Cases, ItemCount Errors);


/* Insert branches in a node */
void Sprout( Tree Node, DiscrValue Branches );


/* Count the nodes in a tree */
int TreeSize( Tree Node );


/* Return a copy of tree T */
Tree CopyTree( Tree T );


/* Save attribute values read with "discrete N" */
void SaveDiscreteNames();


/*	Recover attribute values read with "discrete N"			 */
void RecoverDiscreteNames();







#endif
