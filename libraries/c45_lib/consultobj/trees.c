/*************************************************************************/
/*									 */
/*	Routines for displaying, building, saving and restoring trees	 */
/*	-------------------------------------------------------------	 */
/*		IMPLEMENTATION						 */
/*************************************************************************/
//
// Modified by Stefan Jacobs <Stefan_J@gmx.de>
//
// Created:         2002-06-15
// Last Modified:   2002-06-15
//


#ifndef _C45_TREES_C_
#define _C45_TREES_C_


#include "trees.h"
#include "getnames.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define	Tab		"|   "
#define	TabSize		4
#define	Width		80	/* approx max width of printed trees */

	/*  If lines look like getting too long while a tree is being
	    printed, subtrees are broken off and printed separately after
	    the main tree is finished	 */

short	Subtree;		/* highest subtree to be printed */
Tree	Subdef[100];		/* pointers to subtrees */

FILE	*TRf = 0, *fopen();	/* file pointer for tree i/o */
char	Fn[500];		/* file name */



/*************************************************************************/
/*									 */
/*	Display entire decision tree T					 */
/*									 */
/*************************************************************************/


void PrintTree( Tree T)
/*  ----------  */
{
    short s;

    Subtree=0;
    printf("Decision Tree:\n");
    Show(T, 0);
    printf("\n");

    ForEach(s, 1, Subtree)
    {
	printf("\n\nSubtree [S%d]\n", s);
	Show(Subdef[s], 0);
	printf("\n");
    }
    printf("\n");
    return;
}



/*************************************************************************/
/*									 */
/*	Display the tree T with offset Sh				 */
/*									 */
/*************************************************************************/


void Show( Tree T, short Sh)
/*  ---- */
{
    DiscrValue v, MaxV;

    if ( T->NodeType )
    {
	/*  See whether separate subtree needed  */

	if ( T != Nil && Sh && Sh * TabSize + MaxLine(T) > Width )
	{
	    if ( Subtree < 99 )
	    {
		Subdef[++Subtree] = T;
		printf("[S%d]", Subtree);
	    }
	    else
	    {
		printf("[S??]");
	    }
	}
	else
	{
	    MaxV = T->Forks;

	    /*  Print simple cases first */

	    ForEach(v, 1, MaxV)
	    {
		if ( ! T->Branch[v]->NodeType )
		{
		    ShowBranch(Sh, T, v);
		}
	    }

	    /*  Print subtrees  */

	    ForEach(v, 1, MaxV)
	    {
		if ( T->Branch[v]->NodeType )
		{
		    ShowBranch(Sh, T, v);
		}
	    }
	}
    }
    else
    {
	printf(" %s (%.1f", ClassName[T->Leaf], T->Items);
	if ( T->Errors > 0 ) printf("/%.1f", T->Errors);
	printf(")");
    }
    return;
}



/*************************************************************************/
/*									 */
/*	Print a node T with offset Sh, branch value v, and continue	 */
/*									 */
/*************************************************************************/


void ShowBranch( short Sh, Tree T, DiscrValue v)
/*  -----------  */
{
    DiscrValue Pv = 0;
    DiscrValue Last = 0;
    Attribute Att;
    Boolean FirstValue;
    short TextWidth, Skip, Values=0, i;
    
    Att = T->Tested;

    switch ( T->NodeType )
    {
	case BrDiscr:

	    Indent(Sh, Tab);

	    printf("%s = %s:", AttName[Att], AttValName[Att][v]);
	    break;

	case ThreshContin:

	    Indent(Sh, Tab);

	    printf("%s %s %g ",
		    AttName[Att], ( v == 1 ? "<=" : ">" ), T->Cut);

	    if ( T->Lower != T->Upper )
	    {
		printf("[%g,%g]", T->Lower, T->Upper);
	    }

	    printf(":");
	    break;

	case BrSubset:

	    /*  Count values at this branch  */

	    ForEach(Pv, 1, MaxAttVal[Att])
	    {
		if ( In(Pv, T->Subset[v]) )
		{
		    Last = Pv;
		    Values++;
		}
	    }
	    if ( ! Values ) return;

	    Indent(Sh, Tab);

	    if ( Values == 1 )
	    {
		printf("%s = %s:", AttName[Att], AttValName[Att][Last]);
		break;
	    }

	    printf("%s in {", AttName[Att]);
	    FirstValue = true;
	    Skip = TextWidth = strlen(AttName[Att]) + 5;

	    ForEach(Pv, 1, MaxAttVal[Att])
	    {
		if ( In(Pv, T->Subset[v]) )
		{
		    if ( ! FirstValue &&
			 TextWidth + strlen(AttValName[Att][Pv]) + 11 > Width )
		    {
		  	Indent(Sh, Tab);
			ForEach(i, 1, Skip) putchar(' ');

			TextWidth = Skip;
			FirstValue = true;
		    }

		    printf("%s%c", AttValName[Att][Pv], Pv == Last ? '}' : ',');
		    TextWidth += strlen(AttValName[Att][Pv]) + 1;
		    FirstValue = false;
		}
	    }
	    putchar(':');
    }

    Show(T->Branch[v], Sh+1);
    return;
}



/*************************************************************************/
/*									 */
/*	Find the maximum single line size for non-leaf subtree St.	 */
/*	The line format is						 */
/*			<attribute> <> X.xx:[ <class (<Items>)], or	 */
/*			<attribute> = <DVal>:[ <class> (<Items>)]	 */
/*									 */
/*************************************************************************/


short MaxLine( Tree St )
/*    --------  */
{
    Attribute a;
    DiscrValue v, MaxV, Next;
    short Ll, MaxLl=0;

    a = St->Tested;

    MaxV = St->Forks;
    ForEach(v, 1, MaxV)
    {
	Ll = ( St->NodeType == 2 ? 4 : strlen(AttValName[a][v]) ) + 1;

	/*  Find the appropriate branch  */

        Next = v;

	if ( ! St->Branch[Next]->NodeType )
	{
	    Ll += strlen(ClassName[St->Branch[Next]->Leaf]) + 6;
	}
	MaxLl = Max(MaxLl, Ll);
    }

    return strlen(AttName[a]) + 4 + MaxLl;
}



/*************************************************************************/
/*								   	 */
/*	Indent Sh columns					  	 */
/*								  	 */
/*************************************************************************/

void Indent( short Sh, char* Mark)
/*  ------  */
{
    printf("\n");
    while ( Sh-- ) 
      {
	printf("%s", Mark);
      }
    return;
}



/*************************************************************************/
/*									 */
/*	Save entire decision tree T in file with extension Extension	 */
/*									 */
/*************************************************************************/


void SaveTree( Tree T, String Extension)
/*  ---------  */
{
    static char *LastExt="";

    if ( strcmp(LastExt, Extension) )
    {
	LastExt = Extension;

	if ( TRf ) 
	  fclose(TRf);

	strcpy(Fn, FileName);
	strcat(Fn, Extension);
	if ( ! ( TRf = fopen(Fn, "w") ) )
	    Error(0, Fn, " for writing");
    }

    putc('\n', TRf);
    OutTree(T);

    SaveDiscreteNames();
    return;
}



/*************************************************************************/
/*									 */
/*	Save tree T as characters					 */
/*									 */
/*************************************************************************/


void OutTree( Tree T)
/*  --------  */
{
    DiscrValue v;
    int Bytes;

    StreamOut((char *) &T->NodeType, sizeof(short));
    StreamOut((char *) &T->Leaf, sizeof(ClassNo));
    StreamOut((char *) &T->Items, sizeof(ItemCount));
    StreamOut((char *) &T->Errors, sizeof(ItemCount));
    StreamOut((char *) T->ClassDist, (MaxClass + 1) * sizeof(ItemCount));

    if ( T->NodeType )
    {
	StreamOut((char *) &T->Tested, sizeof(Attribute));
	StreamOut((char *) &T->Forks, sizeof(short));

	switch ( T->NodeType )
	{
	    case BrDiscr:
		break;

	    case ThreshContin:
		StreamOut((char *) &T->Cut, sizeof(float));
		StreamOut((char *) &T->Lower, sizeof(float));
		StreamOut((char *) &T->Upper, sizeof(float));
		break;

	    case BrSubset:
		Bytes = (MaxAttVal[T->Tested]>>3) + 1;
		ForEach(v, 1, T->Forks)
		{
		    StreamOut((char *) T->Subset[v], Bytes);
		}
		break;
	}

	ForEach(v, 1, T->Forks)
	{
	    OutTree(T->Branch[v]);
	}
    }
    return;
}



/*************************************************************************/
/*									 */
/*	Retrieve entire decision tree with extension Extension		 */
/*									 */
/*************************************************************************/


Tree GetTree( String Extension )
/*   --------  */
{
    Tree Hold, InTree();
    static char *LastExt="";

    if ( strcmp(LastExt, Extension) )
    {
	LastExt = Extension;

	if ( TRf ) fclose(TRf);

	strcpy(Fn, FileName);
	strcat(Fn, Extension);
	if ( ! ( TRf = fopen(Fn, "r") ) ) Error(0, Fn, "");
    }

    if ( ! TRf || getc(TRf) == EOF ) return Nil;

    Hold = InTree();

    RecoverDiscreteNames();

    return Hold;
}



/*************************************************************************/
/*									 */
/*	Retrieve tree from saved characters				 */
/*									 */
/*************************************************************************/


Tree InTree()
/*   -------  */
{
    Tree T;
    DiscrValue v;
    int Bytes;

    T = (Tree) malloc(sizeof(TreeRec));

    StreamIn((char *) &T->NodeType, sizeof(short));
    StreamIn((char *) &T->Leaf, sizeof(ClassNo));
    StreamIn((char *) &T->Items, sizeof(ItemCount));
    StreamIn((char *) &T->Errors, sizeof(ItemCount));

    T->ClassDist = (ItemCount *) calloc(MaxClass+1, sizeof(ItemCount));
    StreamIn((char *) T->ClassDist, (MaxClass + 1) * sizeof(ItemCount));

    if ( T->NodeType )
    {
	StreamIn((char *) &T->Tested, sizeof(Attribute));
	StreamIn((char *) &T->Forks, sizeof(short));

	switch ( T->NodeType )
	{
	    case BrDiscr:
		break;

	    case ThreshContin:
		StreamIn((char *) &T->Cut, sizeof(float));
		StreamIn((char *) &T->Lower, sizeof(float));
		StreamIn((char *) &T->Upper, sizeof(float));
		break;

	    case BrSubset:
		T->Subset = (Set *) calloc(T->Forks + 1, sizeof(Set));

		Bytes = (MaxAttVal[T->Tested]>>3) + 1;
		ForEach(v, 1, T->Forks)
		{
		    T->Subset[v] = (Set) malloc(Bytes);
		    StreamIn((char *) T->Subset[v], Bytes);
		}
	}

	T->Branch = (Tree *) calloc(T->Forks + 1, sizeof(Tree));
	ForEach(v, 1, T->Forks)
	{
	    T->Branch[v] = InTree();
	}
    }

    return T;
}



/*************************************************************************/
/*									 */
/*	Stream characters to/from file TRf from/to an address		 */
/*									 */
/*************************************************************************/


void StreamOut( String s, int n )
/*  ---------  */
{
    while ( n-- ) 
      {
	putc(*s++, TRf);
      }
    return;
}



void StreamIn( String s, int n)
/*  ---------  */
{
    while ( n-- ) 
      {
	*s++ = getc(TRf);
      }
    return;
}



/*************************************************************************/
/*									 */
/*	Free up space taken up by tree Node				 */
/*									 */
/*************************************************************************/


void ReleaseTree( Tree Node )
/*  -------  */
{
    DiscrValue v;

    if ( Node->NodeType )
    {
	ForEach(v, 1, Node->Forks)
	{
	    ReleaseTree(Node->Branch[v]);
	}

	cfree(Node->Branch);

	if ( Node->NodeType == BrSubset )
	{
	    cfree(Node->Subset);
	}

    }

    cfree(Node->ClassDist);
    cfree(Node);
    return;
}



/*************************************************************************/
/*									 */
/*	Construct a leaf in a given node				 */
/*									 */
/*************************************************************************/


Tree Leaf(ItemCount *ClassFreq, ClassNo NodeClass, 
	  ItemCount Cases, ItemCount Errors)
/*   ----  */
{
    Tree Node;

    Node = (Tree) calloc(1, sizeof(TreeRec));

    Node->ClassDist = (ItemCount *) calloc(MaxClass+1, sizeof(ItemCount));
    memcpy(Node->ClassDist, ClassFreq, (MaxClass+1) * sizeof(ItemCount));
    
    Node->NodeType	= 0; 
    Node->Leaf		= NodeClass;
    Node->Items		= Cases;
    Node->Errors	= Errors;

    return Node; 
}



/*************************************************************************/
/*									 */
/*	Insert branches in a node 	                 		 */
/*									 */
/*************************************************************************/


void Sprout( Tree Node, DiscrValue Branches )
/*  ------  */
{
    Node->Forks = Branches;
    Node->Branch = (Tree *) calloc(Branches+1, sizeof(Tree));
    return;
}



/*************************************************************************/
/*									 */
/*	Count the nodes in a tree					 */
/*									 */
/*************************************************************************/

	
int TreeSize( Tree Node )
/*  --------  */
{
    int Sum=0;
    DiscrValue v;

    if ( Node->NodeType )
    {
	ForEach(v, 1, Node->Forks)
	{
	    Sum += TreeSize(Node->Branch[v]);
	}
    }

    return Sum + 1;
}



/*************************************************************************/
/*									 */
/*	Return a copy of tree T						 */
/*									 */
/*************************************************************************/


Tree CopyTree( Tree T )
/*   ---------  */
{
    DiscrValue v;
    Tree New;

    New = (Tree) malloc(sizeof(TreeRec));
    memcpy(New, T, sizeof(TreeRec));

    New->ClassDist = (ItemCount *) calloc(MaxClass+1, sizeof(ItemCount));
    memcpy(New->ClassDist, T->ClassDist, (MaxClass + 1) * sizeof(ItemCount));

    if ( T->NodeType )
    {
	New->Branch = (Tree *) calloc(T->Forks + 1, sizeof(Tree));
	ForEach(v, 1, T->Forks)
	{
	    New->Branch[v] = CopyTree(T->Branch[v]);
	}
    }

    return New;
}



/*************************************************************************/
/*									 */
/*	Save attribute values read with "discrete N"			 */
/*									 */
/*************************************************************************/


void SaveDiscreteNames()
/*  -----------------  */
{
    Attribute Att;
    DiscrValue v;
    int Length;

    ForEach(Att, 0, MaxAtt)
    {
	if ( SpecialStatus[Att] != DISCRETE ) continue;

	StreamOut((char *) &MaxAttVal[Att], sizeof(int));

	ForEach(v, 1, MaxAttVal[Att])
	{
	    Length = strlen(AttValName[Att][v]) + 1;

	    StreamOut((char *) &Length, sizeof(int));
	    StreamOut((char *) AttValName[Att][v], Length);
	}
    }
    return;
}



/*************************************************************************/
/*									 */
/*	Recover attribute values read with "discrete N"			 */
/*									 */
/*************************************************************************/


void RecoverDiscreteNames()
/*  --------------------  */
{
    Attribute Att;
    DiscrValue v;
    int Length;

    ForEach(Att, 0, MaxAtt)
    {
	if ( SpecialStatus[Att] != DISCRETE ) continue;

	StreamIn((char *)(&MaxAttVal[Att]), sizeof(int));

	ForEach(v, 1, MaxAttVal[Att])
	{
	    StreamIn((char *)(&Length), sizeof(int));

	    AttValName[Att][v] = (char *) malloc(Length);
	    StreamIn(AttValName[Att][v], Length);
	}
    }
    return;
}


#endif
