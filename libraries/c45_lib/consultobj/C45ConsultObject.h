//*****************************************************************//
// Created by Stefan Jacobs <Stefan_J@gmx.de>                      //
//                                                                 //
// Created:         2002-06-16                                     //
// Last Modified:   2002-06-20                                     //
//                                                                 //
//*****************************************************************//
// This are modified methods by the original C4.5 code. Sometimes  //
//  even I don't know for what they are, and how they run. This    //
//  code was done, to check, if the DT idea works and if the       //
//  methods are working and fast enough.                           //
// This code can be made more efficient, by returning not only     //
//  strings, but structs, and so on.......                         //
//*****************************************************************//

/*! \file C45ConsultObject.h
<pre>
<b>File:</b>          C45ConsultObject.h
<b>Project:</b>       AllemaniACs Soccer Simulation League
<b>Authors:</b>       Stefan Jacobs <Stefan_J@gmx.de>
<b>Created:</b>       2002-06-16
<b>Last Revision:</b> 2002-06-20
<b>Contents:</b>      Contains an user based C4.5 consulting
                        object.
</pre>
*/




#ifndef _C45_CONSULTOBJECT_H_
#define _C45_CONSULTOBJECT_H_

#include <iostream>
#include <string>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>


#include "C45ConsultObjectDefinitions.h"

using namespace std;


// Consult a c4.5 DT.
//
class C45ConsultObject
{
 public:

  // Constructor. Load the decision Tree.
  C45ConsultObject();
  
  // Constructor with Filename.
  C45ConsultObject( char * fileName );

  // Destructor.
  ~C45ConsultObject();


  // Query the decision Tree user based.
  int Question();


 protected:

  // ====================================
  // Methods you don't have to care about
  // ====================================

  /** Load a C45 Consult File, with name written in 
   *   member variable.
   */
  int LoadConsultFile();


  // ====================================
  // Methods from Original C4.5 Code!!!!!
  // ====================================
  
  // GetName Routines from C4.5 Original Code
  //   -> can be found in C45ConsultObjectGetNames.cpp
  void  Error( short, char *, char * );
  char* CopyString( char * );
  int   Which( char *, char **, short, short );
  void  GetNames();
  bool  ReadName(FILE *, char *, char * );


  // Tree Routines from C4.5 Original Code
  //   -> can be found in C45ConsultObjectTrees.cpp
  void  PrintTree( Tree T);
  void  Show( Tree T, short Sh);
  void  ShowBranch( short Sh, Tree T, DiscrValue v);
  short MaxLine( Tree St );
  void  Indent( short Sh, char* Mark);
  void  SaveTree( Tree T, char * Extension);
  void  OutTree( Tree T);
  Tree  GetTree( char * Extension );
  Tree  InTree();
  void  StreamOut( char * s, int n );
  void  StreamIn( char * s, int n);
  void  ReleaseTree( Tree Node );
  Tree  Leaf(ItemCount *ClassFreq, ClassNo NodeClass, 
	     ItemCount Cases, ItemCount Errors);
  void  Sprout( Tree Node, DiscrValue Branches );
  int   TreeSize( Tree Node );
  Tree  CopyTree( Tree T );
  void  SaveDiscreteNames();
  void  RecoverDiscreteNames();

  
  // Consult Routines from C4.5 Original Code
  //   -> can be found in C45ConsultObjectConsultation.cpp
  void ClassifyCase( Tree Subtree, float Weight );
  float Interpolate( Tree t, float v );
  float Area( Tree t, float v );
  void InterpretTree();
  void Decision( ClassNo c, float p, float lb, float ub);

  
  // User Input Routines from C4.5 Original Code
  //   -> can be found in C45ConsultObjectUserint.cpp
  void CheckValue( Attribute Att, Tree T );
  void PrintRange( Attribute Att );
  void ReadRange( Attribute Att, Tree T );
  void ReadDiscr( Attribute Att, Tree T);
  void ReadContin( Attribute Att, Tree T );
  void Clear();
  void SkipLine( char c );
  void Retry( Attribute Att, Tree T );




  // ======================================
  // Variables you don't have to care about
  // ======================================

  // Own Data:

  string m_pC45ConsultFileName;



  // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ //
  // Global Data for C4.5 Routines!
  // Copied and modified that
  // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ //

  short MaxAtt;		/* max att number   */
  short MaxClass;       /* max class number */
  short MaxDiscrVal;	/* max discrete values for any att */

  ItemNo MaxItem;	/* max data item number */

  Description *Item;	/* data items */

  DiscrValue *MaxAttVal; /* number of values for each att */

  char *SpecialStatus;	/* special att treatment */

  char **ClassName;	/* class names */
  char **AttName;	/* att names   */
  char ***AttValName;	/* att value names */
  char *FileName;	/* family name of files */

  bool AllKnown;	/* true if there have been no splits */
			/* on atts with missing values above */
			/* the current position in the tree  */

  short TRIALS;		/* number of trees to be grown */

  bool GAINRATIO;	/* true=gain ratio, false=gain  */
  bool SUBSET;	        /* true if subset tests allowed */
  bool BATCH;		/* true if windowing turned off */
  bool UNSEENS;	        /* true if to evaluate on test data */
  bool PROBTHRESH;	/* true if to use soft thresholds   */

  ItemNo MINOBJS;	/* minimum items each side of a cut */
  ItemNo WINDOW;	/* initial window size */
  ItemNo INCREMENT;	/* max window increment each iteration */

  float CF;		/* confidence limit for tree pruning */

  
  // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ //
  // Tree Data for C4.5 Routines!
  // Copied and modified that
  // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ //
  short	Subtree;		/* highest subtree to be printed */
  Tree	Subdef[100];		/* pointers to subtrees */
  
  FILE	*TRf;	                /* file pointer for tree i/o */
  char	Fn[500];		/* file name */


  // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ //
  // Consulting Data for C4.5 Routines!
  // Copied and modified that
  // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ //

  short	VERBOSITY;
  short TRACE;
  RangeDescRec RangeDesc;
  Tree	DecisionTree;			/* tree being used */
  float	*LowClassSum;			/* accumulated lower estimates */
  float *ClassSum;		        /* accumulated central estimates */

  char Delimiter;

  // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ //
  // END OF GLOBAL C4.5 DATA
  // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ //


};


#endif
