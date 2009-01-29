/*************************************************************************/
/*									 */
/*		Definitions used in C4.5				 */
/*              ------------------------				 */
/*									 */
/*************************************************************************/


#include <stdio.h>
#include <math.h>

#define	 Eof			EOF             /*char read on end of file*/
#define	 Nil			0               /*null pointer*/
#define	 false			0 
#define	 true			1 
#define	 None			-1
#define	 Epsilon                1E-3

long	 random();
#define	 Random			((random()&2147483647) / 2147483648.0)

#define	 Max(a,b)               ((a)>(b) ? a : b) 
#define	 Min(a,b)               ((a)<(b) ? a : b) 
#define	 Round(x)		((int) (x+0.5))
#define	 Log2			0.69314718055994530942
#define	 Log(x)			((x) <= 0 ? 0.0 : log((float)x) / Log2)

#define	 Bit(b)			(1 << (b))
#define	 In(b,s)		((s[(b) >> 3]) & Bit((b) & 07))
#define	 ClearBits(n,s)		memset(s,0,n)
#define	 CopyBits(n,f,t)	memcpy(t,f,n)
#define	 SetBit(b,s)		(s[(b) >> 3] |= Bit((b) & 07))

#define	 ForEach(v,f,l)		for(v=f ; v<=l ; ++v) 

#define	 Verbosity(d)		if(VERBOSITY >= d)

#define	 Check(v,l,h)\
	     if ( v<l||v>h ) {printf("\t** illegal value **\n"); exit(1);}
/*************************************************************************/
/*									 */
/*		Type definitions for C4.5				 */
/*              -------------------------				 */
/*									 */
/*************************************************************************/


typedef  char	Boolean, *String, *Set;

typedef  int	ItemNo;		/* data item number */
typedef  float	ItemCount;	/* count of (partial) items */

typedef  short	ClassNo,	/* class number, 0..MaxClass */
		DiscrValue;	/* discrete attribute value (0 = ?) */
typedef  short	Attribute;	/* attribute number, 0..MaxAtt */

typedef  union  _attribute_value
	 {
	    DiscrValue	_discr_val;
	    float	_cont_val;
	 }
	 	AttValue, *Description;

#define  CVal(Case,Attribute)   Case[Attribute]._cont_val
#define  DVal(Case,Attribute)   Case[Attribute]._discr_val
#define  Class(Case)		Case[MaxAtt+1]._discr_val

#define  Unknown  -999		/* unknown value for continuous attribute */


#define  BrDiscr	1	/* node types:	branch */
#define  ThreshContin	2	/*		threshold cut */
#define  BrSubset	3	/*		subset test */

typedef  struct _tree_record *Tree;
typedef  struct _tree_record
	 {
	    short	NodeType;	/* 0=leaf 1=branch 2=cut 3=subset */
	    ClassNo	Leaf;		/* most frequent class at this node */
	    ItemCount	Items,		/* no of items at this node */
			*ClassDist,	/* class distribution of items */
	    		Errors;		/* no of errors at this node */
	    Attribute	Tested; 	/* attribute referenced in test */
	    short	Forks;		/* number of branches at this node */
	    float	Cut,		/* threshold for continuous attribute */
		  	Lower,		/* lower limit of soft threshold */
		  	Upper;		/* upper limit ditto */
	    Set         *Subset;	/* subsets of discrete values  */
	    Tree	*Branch;	/* Branch[x] = (sub)tree for outcome x */
	 }
		TreeRec;

#define  IGNORE		1	/* special attribute status: do not use */
#define  DISCRETE	2	/* ditto: collect values as data read */


typedef short	RuleNo;			/* rule number */


typedef struct TestRec *Test;

struct TestRec
       {
	   short	NodeType;	/* test type (see tree nodes) */
	   Attribute	Tested;		/* attribute tested */
	   short	Forks;		/* possible branches */
	   float	Cut;		/* threshold (if relevant) */
	   Set		*Subset;	/* subset (if relevant) */
       };


typedef struct CondRec *Condition;

struct CondRec
       {
	   Test		CondTest;	/* test part of condition */
	   short	TestValue;	/* specified outcome of test */
       };


typedef struct ProdRuleRec PR;

struct ProdRuleRec
       {
	   short	Size;		/* number of conditions */
	   Condition	*Lhs;		/* conditions themselves */
	   ClassNo	Rhs;		/* class given by rule */
	   float	Error,		/* estimated error rate */
			Bits;		/* bits to encode rule */
	   ItemNo	Used,		/* times rule used */
			Incorrect;	/* times rule incorrect */
       };


typedef struct RuleSetRec RuleSet;

struct RuleSetRec
       {
	   PR		*SRule;		/* rules */
	   RuleNo	SNRules,	/* number of rules */
			*SRuleIndex;	/* ranking of rules */
	   ClassNo	SDefaultClass;	/* default class for this ruleset */
    };
/*************************************************************************/
/*									 */
/*	Main routine, c4.5						 */
/*	------------------						 */
/*									 */
/*************************************************************************/


#include "defns.i"
#include "types.i"


    /*  External data, described in extern.i  */

short		MaxAtt, MaxClass, MaxDiscrVal = 2;

ItemNo		MaxItem;

Description	*Item;

DiscrValue	*MaxAttVal;

char		*SpecialStatus;

String		*ClassName,
		*AttName,
		**AttValName,
		FileName = "DF";

short		VERBOSITY = 0,
		TRIALS    = 10;

Boolean		GAINRATIO  = true,
		SUBSET     = false,
		BATCH      = true,
		UNSEENS    = false,
		PROBTHRESH = false;

ItemNo		MINOBJS   = 2,
		WINDOW    = 0,
		INCREMENT = 0;

float		CF = 0.25;

Tree		*Pruned;

Boolean		AllKnown = true;


    main(Argc, Argv)
/*  ----  */
    int Argc;
    char *Argv[];
{
    int o;
    extern char *optarg;
    extern int optind;
    Boolean FirstTime=true;
    short Best, BestTree();

    PrintHeader("decision tree generator");

    /*  Process options  */

    while ( (o = getopt(Argc, Argv, "f:bupv:t:w:i:gsm:c:")) != EOF )
    {
	if ( FirstTime )
	{
	    printf("\n    Options:\n");
	    FirstTime = false;
	}

	switch (o)
	{
	case 'f':   FileName = optarg;
		    printf("\tFile stem <%s>\n", FileName);
		    break;
	case 'b':   BATCH = true;
		    printf("\tWindowing disabled (now the default)\n");
		    break;
	case 'u':   UNSEENS = true;
		    printf("\tTrees evaluated on unseen cases\n");
		    break;
	case 'p':   PROBTHRESH = true;
		    printf("\tProbability thresholds used\n");
		    break;
	case 'v':   VERBOSITY = atoi(optarg);
		    printf("\tVerbosity level %d\n", VERBOSITY);
		    break;
	case 't':   TRIALS = atoi(optarg);
		    printf("\tWindowing enabled with %d trials\n", TRIALS);
		    Check(TRIALS, 1, 10000);
		    BATCH = false;
		    break;
	case 'w':   WINDOW = atoi(optarg);
		    printf("\tInitial window size of %d items\n", WINDOW);
		    Check(WINDOW, 1, 1000000);
		    BATCH = false;
		    break;
	case 'i':   INCREMENT = atoi(optarg);
		    printf("\tMaximum window increment of %d items\n",
			   INCREMENT);
		    Check(INCREMENT, 1, 1000000);
		    BATCH = false;
		    break;
	case 'g':   GAINRATIO = false;
		    printf("\tGain criterion used\n");
		    break;
	case 's':   SUBSET = true;
		    printf("\tTests on discrete attribute groups\n");
		    break;
	case 'm':   MINOBJS = atoi(optarg);
		    printf("\tSensible test requires 2 branches with >=%d cases\n",
			    MINOBJS);
		    Check(MINOBJS, 1, 1000000);
		    break;
	case 'c':   CF = atof(optarg);
		    printf("\tPruning confidence level %g%%\n", CF);
		    Check(CF, Epsilon, 100);
		    CF /= 100;
		    break;
	case '?':   printf("unrecognised option\n");
		    exit(1);
	}
    }

    /*  Initialise  */

    GetNames();
    GetData(".data");
    printf("\nRead %d cases (%d attributes) from %s.data\n",
	   MaxItem+1, MaxAtt+1, FileName);

    /*  Build decision trees  */

    if ( BATCH )
    {
	TRIALS = 1;
	OneTree();
	Best = 0;
    }
    else
    {
	Best = BestTree();
    }

    /*  Soften thresholds in best tree  */

    if ( PROBTHRESH )
    {
	printf("Softening thresholds");
	if ( ! BATCH ) printf(" for best tree from trial %d", Best);
	printf("\n");
	SoftenThresh(Pruned[Best]);
	printf("\n");
	PrintTree(Pruned[Best]);
    }

    /*  Save best tree  */

    if ( BATCH || TRIALS == 1 )
    {
	printf("\nTree saved\n");
    }
    else
    {
	printf("\nBest tree from trial %d saved\n", Best);
    }
    SaveTree(Pruned[Best], ".tree");

    /*  Evaluation  */

    printf("\n\nEvaluation on training data (%d items):\n", MaxItem+1);
    Evaluate(false, Best);

    if ( UNSEENS )
    {   
        GetData(".test");
        printf("\nEvaluation on test data (%d items):\n", MaxItem+1);
        Evaluate(true, Best);
    }

    exit(0);
}
/*************************************************************************/
/*									 */
/*	Routines to manage tree growth, pruning and evaluation		 */
/*	------------------------------------------------------		 */
/*									 */
/*************************************************************************/


#include "defns.i"
#include "types.i"
#include "extern.i"


ItemNo		*TargetClassFreq;
Tree		*Raw;
extern Tree	*Pruned;



/*************************************************************************/
/*									 */
/*	Grow and prune a single tree from all data			 */
/*									 */
/*************************************************************************/


    OneTree()
/*  ---------  */
{
    Tree FormTree(), CopyTree();
    Boolean Prune();

    InitialiseTreeData();
    InitialiseWeights();

    Raw = (Tree *) calloc(1, sizeof(Tree));
    Pruned = (Tree *) calloc(1, sizeof(Tree));

    AllKnown = true;
    Raw[0] = FormTree(0, MaxItem);
    printf("\n");
    PrintTree(Raw[0]);

    SaveTree(Raw[0], ".unpruned");

    Pruned[0] = CopyTree(Raw[0]);
    if ( Prune(Pruned[0]) )
    {
	printf("\nSimplified ");
	PrintTree(Pruned[0]);
    }
}



/*************************************************************************/
/*									 */
/*	Grow and prune TRIALS trees and select the best of them		 */
/*									 */
/*************************************************************************/


short BestTree()
/*    --------  */
{
    Tree CopyTree(), Iterate();
    Boolean Prune();
    short t, Best=0;

    InitialiseTreeData();

    TargetClassFreq = (ItemNo *) calloc(MaxClass+1, sizeof(ItemNo));

    Raw    = (Tree *) calloc(TRIALS, sizeof(Tree));
    Pruned = (Tree *) calloc(TRIALS, sizeof(Tree));

    /*  If necessary, set initial size of window to 20% (or twice
	the sqrt, if this is larger) of the number of data items,
	and the maximum number of items that can be added to the
	window at each iteration to 20% of the initial window size  */

    if ( ! WINDOW )
    {
	WINDOW = Max(2 * sqrt(MaxItem+1.0), (MaxItem+1) / 5);
    }

    if ( ! INCREMENT )
    {
	INCREMENT = Max(WINDOW / 5, 1);
    }

    FormTarget(WINDOW);

    /*  Form set of trees by iteration and prune  */

    ForEach(t, 0, TRIALS-1 )
    {
        FormInitialWindow();

	printf("\n--------\nTrial %d\n--------\n\n", t);

	Raw[t] = Iterate(WINDOW, INCREMENT);
	printf("\n");
	PrintTree(Raw[t]);

	SaveTree(Raw[t], ".unpruned");

	Pruned[t] = CopyTree(Raw[t]);
	if ( Prune(Pruned[t]) )
	{
	    printf("\nSimplified ");
	    PrintTree(Pruned[t]);
	}

	if ( Pruned[t]->Errors < Pruned[Best]->Errors )
	{
	    Best = t;
	}
    }
    printf("\n--------\n");

    return Best;
}



/*************************************************************************/
/*									 */
/*  The windowing approach seems to work best when the class		 */
/*  distribution of the initial window is as close to uniform as	 */
/*  possible.  FormTarget generates this initial target distribution,	 */
/*  setting up a TargetClassFreq value for each class.			 */
/*									 */
/*************************************************************************/


    FormTarget(Size)
/*  -----------  */
    ItemNo Size;
{
    ItemNo i, *ClassFreq;
    ClassNo c, Smallest, ClassesLeft=0;

    ClassFreq = (ItemNo *) calloc(MaxClass+1, sizeof(ItemNo));

    /*  Generate the class frequency distribution  */

    ForEach(i, 0, MaxItem)
    {
	ClassFreq[ Class(Item[i]) ]++;
    }

    /*  Calculate the no. of classes of which there are items  */

    ForEach(c, 0, MaxClass)
    {
	if ( ClassFreq[c] )
	{
	    ClassesLeft++;
	}
	else
	{
	    TargetClassFreq[c] = 0;
	}
    }

    while ( ClassesLeft )
    {
	/*  Find least common class of which there are some items  */

	Smallest = -1;
	ForEach(c, 0, MaxClass)
	{
	    if ( ClassFreq[c] &&
		 ( Smallest < 0 || ClassFreq[c] < ClassFreq[Smallest] ) )
	    {
		Smallest = c;
	    }
	}

	/*  Allocate the no. of items of this class to use in the window  */

	TargetClassFreq[Smallest] = Min(ClassFreq[Smallest], Round(Size/ClassesLeft));

	ClassFreq[Smallest] = 0;

	Size -= TargetClassFreq[Smallest];
	ClassesLeft--;
    }

    cfree(ClassFreq);
}



/*************************************************************************/
/*									 */
/*  Form initial window, attempting to obtain the target class profile	 */
/*  in TargetClassFreq.  This is done by placing the targeted number     */
/*  of items of each class at the beginning of the set of data items.	 */
/*									 */
/*************************************************************************/


    FormInitialWindow()
/*  -------------------  */
{
    ItemNo i, Start=0, More;
    ClassNo c;
    void Swap();

    Shuffle();

    ForEach(c, 0, MaxClass)
    {
	More = TargetClassFreq[c];

	for ( i = Start ; More ; i++ )
	{
	    if ( Class(Item[i]) == c )
	    {
		Swap(Start, i);
		Start++;
		More--;
	    }
	}
    }
}



/*************************************************************************/
/*									 */
/*		Shuffle the data items randomly				 */
/*									 */
/*************************************************************************/


    Shuffle()
/*  -------  */
{
    ItemNo This, Alt, Left;
    Description Hold;

    This = 0;
    for( Left = MaxItem+1 ; Left ; )
    {
        Alt = This + (Left--) * Random;
        Hold = Item[This];
        Item[This++] = Item[Alt];
        Item[Alt] = Hold;
    }
}



/*************************************************************************/
/*									 */
/*  Grow a tree iteratively with initial window size Window and		 */
/*  initial window increment IncExceptions.				 */
/*									 */
/*  Construct a classifier tree using the data items in the		 */
/*  window, then test for the successful classification of other	 */
/*  data items by this tree.  If there are misclassified items,		 */
/*  put them immediately after the items in the window, increase	 */
/*  the size of the window and build another classifier tree, and	 */
/*  so on until we have a tree which successfully classifies all	 */
/*  of the test items or no improvement is apparent.			 */
/*									 */
/*  On completion, return the tree which produced the least errors.	 */
/*									 */
/*************************************************************************/


Tree Iterate(Window, IncExceptions)
/*   -------  */
    ItemNo Window, IncExceptions;
{
    Tree Classifier, BestClassifier=Nil, FormTree();
    ItemNo i, Errors, TotalErrors, BestTotalErrors=MaxItem+1,
	   Exceptions, Additions;
    ClassNo Assigned, Category();
    short Cycle=0;
    void Swap();

    printf("Cycle   Tree    -----Cases----");
    printf("    -----------------Errors-----------------\n");
    printf("        size    window   other");
    printf("    window  rate   other  rate   total  rate\n");
    printf("-----   ----    ------  ------");
    printf("    ------  ----  ------  ----  ------  ----\n");

    do
    {
	/*  Build a classifier tree with the first Window items  */

	InitialiseWeights();
	AllKnown = true;
	Classifier = FormTree(0, Window-1);

	/*  Error analysis  */

	Errors = Round(Classifier->Errors);

	/*  Move all items that are incorrectly classified by the
	    classifier tree to immediately after the items in the
	    current window.  */

	Exceptions = Window;
	ForEach(i, Window, MaxItem)
	{
	    Assigned = Category(Item[i], Classifier);
	    if ( Assigned != Class(Item[i]) )
	    {
		Swap(Exceptions, i);
		Exceptions++;
	    }
	}
        Exceptions -= Window;
	TotalErrors = Errors + Exceptions;

	/*  Print error analysis  */

	printf("%3d  %7d  %8d  %6d  %8d%5.1f%%  %6d%5.1f%%  %6d%5.1f%%\n",
	       ++Cycle, TreeSize(Classifier), Window, MaxItem-Window+1,
	       Errors, 100*(float)Errors/Window,
	       Exceptions, 100*Exceptions/(MaxItem-Window+1.001),
	       TotalErrors, 100*TotalErrors/(MaxItem+1.0));

	/*  Keep track of the most successful classifier tree so far  */

	if ( ! BestClassifier || TotalErrors < BestTotalErrors )
	{
	    if ( BestClassifier ) ReleaseTree(BestClassifier);
	    BestClassifier = Classifier;
	    BestTotalErrors = TotalErrors;
        }
	else
	{
	    ReleaseTree(Classifier);
	}

	/*  Increment window size  */

	Additions = Min(Exceptions, IncExceptions);
	Window = Min(Window + Max(Additions, Exceptions / 2), MaxItem + 1);
    }
    while ( Exceptions );

    return BestClassifier;
}



/*************************************************************************/
/*									 */
/*	Print report of errors for each of the trials			 */
/*									 */
/*************************************************************************/


    Evaluate(CMInfo, Saved)
/*  --------  */
    Boolean CMInfo;
    short Saved;
{
    ClassNo RealClass, PrunedClass, Category();
    short t;
    ItemNo *ConfusionMat, i, RawErrors, PrunedErrors;

    if ( CMInfo )
    {
	ConfusionMat = (ItemNo *) calloc((MaxClass+1)*(MaxClass+1), sizeof(ItemNo));
    }

    printf("\n");

    if ( TRIALS > 1 )
    {
	printf("Trial\t Before Pruning           After Pruning\n");
	printf("-----\t----------------   ---------------------------\n");
    }
    else
    {
	printf("\t Before Pruning           After Pruning\n");
	printf("\t----------------   ---------------------------\n");
    }
    printf("\tSize      Errors   Size      Errors   Estimate\n\n");

    ForEach(t, 0, TRIALS-1)
    {
	RawErrors = PrunedErrors = 0;

	ForEach(i, 0, MaxItem)
	{
	    RealClass = Class(Item[i]);

	    if ( Category(Item[i], Raw[t]) != RealClass ) RawErrors++;

	    PrunedClass = Category(Item[i], Pruned[t]);

	    if ( PrunedClass != RealClass ) PrunedErrors++;

	    if ( CMInfo && t == Saved )
	    {
		ConfusionMat[RealClass*(MaxClass+1)+PrunedClass]++;
	    }
	}
    
	if ( TRIALS > 1 )
	{
	    printf("%4d", t);
	}

	printf("\t%4d  %3d(%4.1f%%)   %4d  %3d(%4.1f%%)    (%4.1f%%)%s\n",
	       TreeSize(Raw[t]), RawErrors, 100.0*RawErrors / (MaxItem+1.0),
	       TreeSize(Pruned[t]), PrunedErrors, 100.0*PrunedErrors / (MaxItem+1.0),
	       100 * Pruned[t]->Errors / Pruned[t]->Items,
	       ( t == Saved ? "   <<" : "" ));
    }

    if ( CMInfo )
    {
	PrintConfusionMatrix(ConfusionMat);
	free(ConfusionMat);
    }
}
/*************************************************************************/
/*								 	 */
/*    Central tree-forming algorithm incorporating all criteria  	 */
/*    ---------------------------------------------------------	 	 */
/*								 	 */
/*************************************************************************/


#include "defns.i"
#include "types.i"
#include "extern.i"


ItemCount
	*Weight,	/* Weight[i]  = current fraction of item i */
	**Freq,		/* Freq[x][c] = no. items of class c with outcome x */
	*ValFreq,	/* ValFreq[x]   = no. items with outcome x */
	*ClassFreq;	/* ClassFreq[c] = no. items of class c */

float
	*Gain,		/* Gain[a] = info gain by split on att a */
	*Info,		/* Info[a] = potential info of split on att a */
	*Bar,		/* Bar[a]  = best threshold for contin att a */
	*UnknownRate;	/* UnknownRate[a] = current unknown rate for att a */

Boolean
	*Tested,	/* Tested[a] set if att a has already been tested */
	MultiVal;	/* true when all atts have many values */


	/*  External variables initialised here  */

extern float
	*SplitGain,	/* SplitGain[i] = gain with att value of item i as threshold */
	*SplitInfo;	/* SplitInfo[i] = potential info ditto */

extern ItemCount
	*Slice1,	/* Slice1[c]    = saved values of Freq[x][c] in subset.c */
	*Slice2;	/* Slice2[c]    = saved values of Freq[y][c] */

extern Set
	**Subset;	/* Subset[a][s] = subset s for att a */

extern short
	*Subsets;	/* Subsets[a] = no. subsets for att a */



/*************************************************************************/
/*								 	 */
/*		Allocate space for tree tables			 	 */
/*								 	 */
/*************************************************************************/


    InitialiseTreeData()
/*  ------------------  */
{ 
    DiscrValue v;
    Attribute a;

    Tested	= (char *) calloc(MaxAtt+1, sizeof(char));

    Gain	= (float *) calloc(MaxAtt+1, sizeof(float));
    Info	= (float *) calloc(MaxAtt+1, sizeof(float));
    Bar		= (float *) calloc(MaxAtt+1, sizeof(float));

    Subset = (Set **) calloc(MaxAtt+1, sizeof(Set *));
    ForEach(a, 0, MaxAtt)
    {
	if ( MaxAttVal[a] )
	{
	    Subset[a]  = (Set *) calloc(MaxDiscrVal+1, sizeof(Set));
	    ForEach(v, 0, MaxAttVal[a])
	    {
		Subset[a][v] = (Set) malloc((MaxAttVal[a]>>3) + 1);
	    }
	}
    }
    Subsets = (short *) calloc(MaxAtt+1, sizeof(short));

    SplitGain = (float *) calloc(MaxItem+1, sizeof(float));
    SplitInfo = (float *) calloc(MaxItem+1, sizeof(float));

    Weight = (ItemCount *) calloc(MaxItem+1, sizeof(ItemCount));

    Freq  = (ItemCount **) calloc(MaxDiscrVal+1, sizeof(ItemCount *));
    ForEach(v, 0, MaxDiscrVal)
    {
	Freq[v]  = (ItemCount *) calloc(MaxClass+1, sizeof(ItemCount));
    }

    ValFreq = (ItemCount *) calloc(MaxDiscrVal+1, sizeof(ItemCount));
    ClassFreq = (ItemCount *) calloc(MaxClass+1, sizeof(ItemCount));

    Slice1 = (ItemCount *) calloc(MaxClass+2, sizeof(ItemCount));
    Slice2 = (ItemCount *) calloc(MaxClass+2, sizeof(ItemCount));

    UnknownRate = (float *) calloc(MaxAtt+1, sizeof(float));

    /*  Check whether all attributes have many discrete values  */

    MultiVal = true;
    if ( ! SUBSET )
    {
	for ( a = 0 ; MultiVal && a <= MaxAtt ; a++ )
	{
	    if ( SpecialStatus[a] != IGNORE )
	    {
		MultiVal = MaxAttVal[a] >= 0.3 * (MaxItem + 1);
	    }
	}
    }
}



/*************************************************************************/
/*								 	 */
/*		Initialise the weight of each item		 	 */
/*								 	 */
/*************************************************************************/


    InitialiseWeights()
/*  -----------------  */
{
    ItemNo i;

    ForEach(i, 0, MaxItem)
    {
        Weight[i] = 1.0;
    }
}



/*************************************************************************/
/*								 	 */
/*  Build a decision tree for the cases Fp through Lp:		 	 */
/*								 	 */
/*  - if all cases are of the same class, the tree is a leaf and so	 */
/*      the leaf is returned labelled with this class		 	 */
/*								 	 */
/*  - for each attribute, calculate the potential information provided 	 */
/*	by a test on the attribute (based on the probabilities of each	 */
/*	case having a particular value for the attribute), and the gain	 */
/*	in information that would result from a test on the attribute	 */
/*	(based on the probabilities of each case with a particular	 */
/*	value for the attribute being of a particular class)		 */
/*								 	 */
/*  - on the basis of these figures, and depending on the current	 */
/*	selection criterion, find the best attribute to branch on. 	 */
/*	Note:  this version will not allow a split on an attribute	 */
/*	unless two or more subsets have at least MINOBJS items. 	 */
/*								 	 */
/*  - try branching and test whether better than forming a leaf	 	 */
/*								 	 */
/*************************************************************************/


Tree FormTree(Fp, Lp)
/*   ---------  */
    ItemNo Fp, Lp; 
{ 
    ItemNo i, Kp, Ep, Group();
    ItemCount Cases, NoBestClass, KnownCases, CountItems();
    float Factor, BestVal, Val, AvGain=0, Worth();
    Attribute Att, BestAtt, Possible=0;
    ClassNo c, BestClass;
    Tree Node, Leaf();
    DiscrValue v;
    Boolean PrevAllKnown;

    Cases = CountItems(Fp, Lp);

    /*  Generate the class frequency distribution  */

    ForEach(c, 0, MaxClass)
    {
	ClassFreq[c] = 0;
    }
    ForEach(i, Fp, Lp)
    { 
	ClassFreq[ Class(Item[i]) ] += Weight[i];
    } 

    /*  Find the most frequent class  */

    BestClass = 0;
    ForEach(c, 0, MaxClass)
    {
	if ( ClassFreq[c] > ClassFreq[BestClass] )
	{
	    BestClass = c;
	}
    }
    NoBestClass = ClassFreq[BestClass];

    Node = Leaf(ClassFreq, BestClass, Cases, Cases - NoBestClass);

    /*  If all cases are of the same class or there are not enough
	cases to divide, the tree is a leaf  */

    if ( NoBestClass == Cases  || Cases < 2 * MINOBJS )
    { 
	return Node;
    } 

    Verbosity(1)
    	printf("\n%d items, total weight %.1f\n", Lp - Fp + 1, Cases);

    /*  For each available attribute, find the information and gain  */

    ForEach(Att, 0, MaxAtt) 
    { 
	Gain[Att] = -Epsilon;

	if ( SpecialStatus[Att] == IGNORE ) continue;

        if ( MaxAttVal[Att] )
	{
	    /*  discrete valued attribute  */

	    if ( SUBSET && MaxAttVal[Att] > 2 )
	    {
		EvalSubset(Att, Fp, Lp, Cases);
	    }
	    else
	    if ( ! Tested[Att] )
	    {
	        EvalDiscreteAtt(Att, Fp, Lp, Cases);
	    }
	}
	else
	{ 
	    /*  continuous attribute  */

	    EvalContinuousAtt(Att, Fp, Lp);
	} 

	/*  Update average gain, excluding attributes with very many values  */

	if ( Gain[Att] > -Epsilon &&
	     ( MultiVal || MaxAttVal[Att] < 0.3 * (MaxItem + 1) ) )
	{
	    Possible++;
	    AvGain += Gain[Att];
	}
    } 

    /*  Find the best attribute according to the given criterion  */

    BestVal = -Epsilon;
    BestAtt = None;
    AvGain  = ( Possible ? AvGain / Possible : 1E6 );

    Verbosity(2)
    {
	if ( AvGain < 1E6 ) printf("\taverage gain %.3f\n", AvGain);
    }

    ForEach(Att, 0, MaxAtt) 
    { 
	if ( Gain[Att] > -Epsilon )
	{ 
	    Val = Worth(Info[Att], Gain[Att], AvGain);
	    if ( Val > BestVal ) 
	    { 
	        BestAtt  = Att; 
	        BestVal = Val;
	    } 
	} 
    } 

    /*  Decide whether to branch or not  */ 

    if ( BestAtt != None )
    { 
	Verbosity(1)
	{
	    printf("\tbest attribute %s", AttName[BestAtt]);
	    if ( ! MaxAttVal[BestAtt] )
	    {
		printf(" cut %.3f", Bar[BestAtt]);
	    }
	    printf(" inf %.3f gain %.3f val %.3f\n",
		   Info[BestAtt], Gain[BestAtt], BestVal);
	}	

	/*  Build a node of the selected test  */

        if ( MaxAttVal[BestAtt] )
	{
	    /*  Discrete valued attribute  */

	    if ( SUBSET && MaxAttVal[BestAtt] > 2 )
	    {
	        SubsetTest(Node, BestAtt);
	    }
	    else
	    {
	        DiscreteTest(Node, BestAtt);
	    }
	}
	else
	{ 
	    /*  Continuous attribute  */

	    ContinTest(Node, BestAtt);
	} 

	/*  Remove unknown attribute values  */

	PrevAllKnown = AllKnown;

	Kp = Group(0, Fp, Lp, Node) + 1;
	if ( Kp != Fp ) AllKnown = false;
	KnownCases = Cases - CountItems(Fp, Kp-1);
	UnknownRate[BestAtt] = (Cases - KnownCases) / (Cases + 0.001);

	Verbosity(1)
	{
	    if ( UnknownRate[BestAtt] > 0 )
	    {
		printf("\tunknown rate for %s = %.3f\n",
		       AttName[BestAtt], UnknownRate[BestAtt]);
	    }
	}

	/*  Recursive divide and conquer  */

	++Tested[BestAtt];

	Ep = Kp - 1;
	Node->Errors = 0;

	ForEach(v, 1, Node->Forks)
	{
	    Ep = Group(v, Kp, Lp, Node);

	    if ( Kp <= Ep )
	    {
		Factor = CountItems(Kp, Ep) / KnownCases;

		ForEach(i, Fp, Kp-1)
		{
		    Weight[i] *= Factor;
		}

		Node->Branch[v] = FormTree(Fp, Ep);
		Node->Errors += Node->Branch[v]->Errors;

		Group(0, Fp, Ep, Node);
		ForEach(i, Fp, Kp-1)
		{
		    Weight[i] /= Factor;
		}
	    }
	    else
	    {
		Node->Branch[v] = Leaf(Node->ClassDist, BestClass, 0.0, 0.0);
	    }
	}

	--Tested[BestAtt];
	AllKnown = PrevAllKnown;

	/*  See whether we would have been no worse off with a leaf  */

	if ( Node->Errors >= Cases - NoBestClass - Epsilon )
	{ 
	    Verbosity(1)
		printf("Collapse tree for %d items to leaf %s\n",
	                Lp - Fp + 1, ClassName[BestClass]);

	    Node->NodeType = 0;
	} 
    }
    else
    { 
	Verbosity(1)
	    printf("\tno sensible splits  %.1f/%.1f\n",
		   Cases, Cases - NoBestClass);
    } 

    return Node; 
} 



/*************************************************************************/
/*								 	 */
/*  Group together the items corresponding to branch V of a test 	 */
/*  and return the index of the last such			 	 */
/*								 	 */
/*  Note: if V equals zero, group the unknown values		 	 */
/*								 	 */
/*************************************************************************/


ItemNo Group(V, Fp, Lp, TestNode)
/*     -----  */
    DiscrValue V;
    ItemNo Fp, Lp;
    Tree TestNode;
{
    ItemNo i;
    Attribute Att;
    float Thresh;
    Set SS;
    void Swap();

    Att = TestNode->Tested;

    if ( V )
    {
	/*  Group items on the value of attribute Att, and depending
	    on the type of branch  */

	switch ( TestNode->NodeType )
	{
	    case BrDiscr:

		ForEach(i, Fp, Lp)
		{
		    if ( DVal(Item[i], Att) == V ) Swap(Fp++, i);
		}
		break;

	    case ThreshContin:

		Thresh = TestNode->Cut;
		ForEach(i, Fp, Lp)
		{
		    if ( (CVal(Item[i], Att) <= Thresh) == (V == 1) ) Swap(Fp++, i);
		}
		break;

	    case BrSubset:

		SS = TestNode->Subset[V];
		ForEach(i, Fp, Lp)
		{
		    if ( In(DVal(Item[i], Att), SS) ) Swap(Fp++, i);
		}
		break;
	}
    }
    else
    {
	/*  Group together unknown values  */

	switch ( TestNode->NodeType )
	{
	    case BrDiscr:
	    case BrSubset:

		ForEach(i, Fp, Lp)
		{
		    if ( ! DVal(Item[i], Att) ) Swap(Fp++, i);
		}
		break;

	    case ThreshContin:

		ForEach(i, Fp, Lp)
		{
		    if ( CVal(Item[i], Att) == Unknown ) Swap(Fp++, i);
		}
		break;
	}
    }

    return Fp - 1;
}



/*************************************************************************/
/*								 	 */
/*	Return the total weight of items from Fp to Lp		 	 */
/*								 	 */
/*************************************************************************/


ItemCount CountItems(Fp, Lp)
/*        ----------  */
    ItemNo Fp, Lp;
{
    register ItemCount Sum=0.0, *Wt, *LWt;

    if ( AllKnown ) return Lp - Fp + 1;

    for ( Wt = Weight + Fp, LWt = Weight + Lp ; Wt <= LWt ; )
    {
	Sum += *Wt++;
    }

    return Sum;
}



/*************************************************************************/
/*                                                               	 */
/*		Exchange items at a and b			 	 */
/*									 */
/*************************************************************************/


void Swap(a,b)
/*   ----  */
    ItemNo a, b;
{
    register Description Hold;
    register ItemCount HoldW;

    Hold = Item[a];
    Item[a] = Item[b];
    Item[b] = Hold;

    HoldW = Weight[a];
    Weight[a] = Weight[b];
    Weight[b] = HoldW;
}
/*************************************************************************/
/*									 */
/*	Calculate information, information gain, and print dists	 */
/*	--------------------------------------------------------	 */
/*									 */
/*************************************************************************/


#include "buildex.i"


/*************************************************************************/
/*									 */
/*  Determine the worth of a particular split according to the		 */
/*  operative criterion							 */
/*									 */
/*	    Parameters:							 */
/*		SplitInfo:	potential info of the split		 */
/*		SplitGain:	gain in info of the split		 */
/*		MinGain:	gain above which the Gain Ratio		 */
/*				may be used				 */
/*									 */
/*  If the Gain criterion is being used, the information gain of	 */
/*  the split is returned, but if the Gain Ratio criterion is		 */
/*  being used, the ratio of the information gain of the split to	 */
/*  its potential information is returned.				 */
/*									 */
/*************************************************************************/


float Worth(ThisInfo, ThisGain, MinGain)
/*    -----  */
    float ThisInfo, ThisGain, MinGain;
{
    if ( GAINRATIO )
    {
	if ( ThisGain >= MinGain - Epsilon && ThisInfo > Epsilon )
	{
	    return ThisGain / ThisInfo;
	}
	else
	{
	    return -Epsilon;
	}
    }
    else
    {
	return ( ThisInfo > 0 && ThisGain > -Epsilon ? ThisGain : -Epsilon );
    }
}



/*************************************************************************/
/*									 */
/*  Zero the frequency tables Freq[][] and ValFreq[] up to MaxVal	 */
/*									 */
/*************************************************************************/


    ResetFreq(MaxVal)
/*  ---------  */
    DiscrValue MaxVal;
{
    DiscrValue v;
    ClassNo c;

    ForEach(v, 0, MaxVal)
    { 
	ForEach(c, 0, MaxClass)
	{
	    Freq[v][c] = 0;
	}
	ValFreq[v] = 0;
    } 
}



/*************************************************************************/
/*									 */
/*  Given tables Freq[][] and ValFreq[], compute the information gain.	 */
/*									 */
/*	    Parameters:							 */
/*		BaseInfo:	average information for all items with	 */
/*				known values of the test attribute	 */
/*		UnknownRate:	fraction of items with unknown ditto	 */
/*		MaxVal:		number of forks				 */
/*		TotalItems:	number of items with known values of	 */
/*				test att				 */
/*									 */
/*  where Freq[x][y] contains the no. of cases with value x for a	 */
/*  particular attribute that are members of class y,			 */
/*  and ValFreq[x] contains the no. of cases with value x for a		 */
/*  particular attribute						 */
/*									 */
/*************************************************************************/


float ComputeGain(BaseInfo, UnknFrac, MaxVal, TotalItems)
/*    -----------  */
    float BaseInfo, UnknFrac;
    DiscrValue MaxVal;
    ItemCount TotalItems;
{
    DiscrValue v;
    float ThisInfo=0.0, ThisGain, TotalInfo();
    short ReasonableSubsets=0;

    /*  Check whether all values are unknown or the same  */

    if ( ! TotalItems ) return -Epsilon;

    /*  There must be at least two subsets with MINOBJS items  */

    ForEach(v, 1, MaxVal)
    {
	if ( ValFreq[v] >= MINOBJS ) ReasonableSubsets++;
    }
    if ( ReasonableSubsets < 2 ) return -Epsilon;

    /*  Compute total info after split, by summing the
	info of each of the subsets formed by the test  */

    ForEach(v, 1, MaxVal)
    {
	ThisInfo += TotalInfo(Freq[v], 0, MaxClass);
    }

    /*  Set the gain in information for all items, adjusted for unknowns  */

    ThisGain = (1 - UnknFrac) * (BaseInfo - ThisInfo / TotalItems);

    Verbosity(5)
        printf("ComputeThisGain: items %.1f info %.3f base %.3f unkn %.3f result %.3f\n",
    		TotalItems + ValFreq[0], ThisInfo, BaseInfo, UnknFrac, ThisGain);

    return ThisGain;
}



/*************************************************************************/
/*									 */
/*  Compute the total information in V[ MinVal..MaxVal ]		 */
/*									 */
/*************************************************************************/


float TotalInfo(V, MinVal, MaxVal)
/*    ---------  */
    ItemCount V[];
    DiscrValue MinVal, MaxVal;
{
    DiscrValue v;
    float Sum=0.0;
    ItemCount N, TotalItems=0;

    ForEach(v, MinVal, MaxVal)
    {
	N = V[v];

	Sum += N * Log(N);
	TotalItems += N;
    }

    return TotalItems * Log(TotalItems) - Sum;
}



/*************************************************************************/
/*									 */
/*	Print distribution table for given attribute			 */
/*									 */
/*************************************************************************/


    PrintDistribution(Att, MaxVal, ShowNames)
/*  -----------------  */
    Attribute Att;
    DiscrValue MaxVal;
    Boolean ShowNames;
{
    DiscrValue v;
    ClassNo c;
    String Val;

    printf("\n\t\t\t ");
    ForEach(c, 0, MaxClass)
    {
	printf("%7.6s", ClassName[c]);
    }
    printf("\n");

    ForEach(v, 0, MaxVal)
    {
	if ( ShowNames )
	{
	    Val = ( !v ? "unknown" :
		    MaxAttVal[Att] ? AttValName[Att][v] :
		    v == 1 ? "below" : "above" );
	    printf("\t\t[%-7.7s:", Val);
	}
	else
	{
	    printf("\t\t[%-7d:", v);
	}

	ForEach(c, 0, MaxClass)
	{
	    printf(" %6.1f", Freq[v][c]);
	}

	printf("]\n");
    }
}
/*************************************************************************/
/*									 */
/*	Evaluation of a test on a discrete valued attribute		 */
/*      ---------------------------------------------------		 */
/*									 */
/*************************************************************************/


#include "buildex.i"


/*************************************************************************/
/*									 */
/*  Set Info[] and Gain[] for discrete partition of items Fp to Lp	 */
/*									 */
/*************************************************************************/


    EvalDiscreteAtt(Att, Fp, Lp, Items)
/*  ---------------  */ 
    Attribute Att;
    ItemNo Fp, Lp; 
    ItemCount Items;
{ 
    ItemCount KnownItems;
    float DiscrKnownBaseInfo(), ComputeGain(), TotalInfo();

    ComputeFrequencies(Att, Fp, Lp);

    KnownItems = Items - ValFreq[0];

    /*  Special case when no known values of the attribute  */

    if ( Items <= ValFreq[0] )
    {
	Verbosity(2) printf("\tAtt %s: no known values\n", AttName[Att]);

	Gain[Att] = -Epsilon;
	Info[Att] = 0.0;
	return;
    }

    Gain[Att] = ComputeGain(DiscrKnownBaseInfo(KnownItems, MaxAttVal[Att]),
			    UnknownRate[Att], MaxAttVal[Att], KnownItems);
    Info[Att] = TotalInfo(ValFreq, 0, MaxAttVal[Att]) / Items;

    Verbosity(2)
    {
    	printf("\tAtt %s", AttName[Att]);
    	Verbosity(3) PrintDistribution(Att, MaxAttVal[Att], true);
    	printf("\tinf %.3f, gain %.3f\n", Info[Att], Gain[Att]);
    }

} 



/*************************************************************************/
/*									 */
/*  Compute frequency tables Freq[][] and ValFreq[] for attribute	 */
/*  Att from items Fp to Lp, and set the UnknownRate for Att		 */
/*									 */
/*************************************************************************/


    ComputeFrequencies(Att, Fp, Lp)
/*  ------------------  */
    Attribute Att;
    ItemNo Fp, Lp;
{
    Description Case; 
    ClassNo c;
    DiscrValue v;
    ItemCount CountItems();
    ItemNo p;

    ResetFreq(MaxAttVal[Att]);

    /*  Determine the frequency of each class amongst cases
	with each possible value for the given attribute  */

    ForEach(p, Fp, Lp)
    { 
	Case = Item[p];
	Freq[ DVal(Case,Att) ][ Class(Case) ] += Weight[p];
    } 

    /*  Determine the frequency of each possible value for the
	given attribute  */

    ForEach(v, 0, MaxAttVal[Att]) 
    { 
	ForEach(c, 0, MaxClass)
	{
	    ValFreq[v] += Freq[v][c];
	}
    }

    /*  Set the rate of unknown values of the attribute  */
 
    UnknownRate[Att] = ValFreq[0] / CountItems(Fp, Lp);
}



/*************************************************************************/
/*									 */
/*  Return the base info for items with known values of a discrete	 */
/*  attribute, using the frequency table Freq[][]			 */
/*	 								 */
/*************************************************************************/


float DiscrKnownBaseInfo(KnownItems, MaxVal)
/*    ------------------  */
    DiscrValue MaxVal;
    ItemCount KnownItems;
{
    ClassNo c;
    ItemCount ClassCount;
    double Sum=0;
    DiscrValue v;

    ForEach(c, 0, MaxClass)
    {
	ClassCount = 0;
	ForEach(v, 1, MaxVal)
	{
	    ClassCount += Freq[v][c];
	}
	Sum += ClassCount * Log(ClassCount);
    }

    return (KnownItems * Log(KnownItems) - Sum) / KnownItems;
}



/*************************************************************************/
/*									 */
/*  Construct and return a node for a test on a discrete attribute	 */
/*									 */
/*************************************************************************/


    DiscreteTest(Node, Att)
/*  ----------  */
    Tree Node;
    Attribute Att;
{
    ItemCount CountItems();

    Sprout(Node, MaxAttVal[Att]);

    Node->NodeType	= BrDiscr;
    Node->Tested	= Att;
    Node->Errors	= 0;
}
/*************************************************************************/
/*                                                                	 */
/*	Evaluation of a test on a continuous valued attribute	  	 */
/*	-----------------------------------------------------	  	 */
/*								  	 */
/*************************************************************************/


#include "buildex.i"


float
	*SplitGain,	/* SplitGain[i] = gain with att value of item i as threshold */
	*SplitInfo;	/* SplitInfo[i] = potential info ditto */



/*************************************************************************/
/*								  	 */
/*  Continuous attributes are treated as if they have possible values	 */
/*	0 (unknown), 1 (less than cut), 2(greater than cut)	  	 */
/*  This routine finds the best cut for items Fp through Lp and sets	 */
/*  Info[], Gain[] and Bar[]						 */
/*								  	 */
/*************************************************************************/


    EvalContinuousAtt(Att, Fp, Lp)
/*  -----------------  */ 
    Attribute Att;
    ItemNo Fp, Lp; 
{ 
    ItemNo i, BestI, Xp, Tries=0;
    ItemCount Items, KnownItems, LowItems, MinSplit, CountItems();
    ClassNo c;
    float AvGain=0, Val, BestVal, BaseInfo, ThreshCost,
	ComputeGain(), TotalInfo(), Worth();
    void Swap();

    Verbosity(2) printf("\tAtt %s", AttName[Att]);
    Verbosity(3) printf("\n");

    ResetFreq(2);

    /*  Omit and count unknown values */

    Items = CountItems(Fp, Lp);
    Xp = Fp;
    ForEach(i, Fp, Lp)
    {
	if ( CVal(Item[i],Att) == Unknown )
	{
	    Freq[ 0 ][ Class(Item[i]) ] += Weight[i];
	    Swap(Xp, i);
	    Xp++;
	}
    }

    ValFreq[0] = 0;
    ForEach(c, 0, MaxClass)
    {
	ValFreq[0] += Freq[0][c];
    }

    KnownItems = Items - ValFreq[0];
    UnknownRate[Att] = 1.0 - KnownItems / Items;

    /*  Special case when very few known values  */

    if ( KnownItems < 2 * MINOBJS )
    {
	Verbosity(2) printf("\tinsufficient cases with known values\n");

	Gain[Att] = -Epsilon;
	Info[Att] = 0.0;
	return;
    }

    Quicksort(Xp, Lp, Att, Swap);

    /*  Count base values and determine base information  */

    ForEach(i, Xp, Lp)
    {
	Freq[ 2 ][ Class(Item[i]) ] += Weight[i];
	SplitGain[i] = -Epsilon;
	SplitInfo[i] = 0;
    }

    BaseInfo = TotalInfo(Freq[2], 0, MaxClass) / KnownItems;

    /*  Try possible cuts between items i and i+1, and determine the
	information and gain of the split in each case.  We have to be wary
	of splitting a small number of items off one end, as we can always
	split off a single item, but this has little predictive power.  */

    MinSplit = 0.10 * KnownItems / (MaxClass + 1);
    if ( MinSplit <= MINOBJS ) MinSplit = MINOBJS;
    else
    if ( MinSplit > 25 ) MinSplit = 25;

    LowItems = 0;
    ForEach(i, Xp, Lp - 1)
    {
	c = Class(Item[i]);
	LowItems   += Weight[i];
	Freq[1][c] += Weight[i];
	Freq[2][c] -= Weight[i];

	if ( LowItems < MinSplit ) continue;
	else
	if ( LowItems > KnownItems - MinSplit ) break;

	if ( CVal(Item[i],Att) < CVal(Item[i+1],Att) - 1E-5 )
	{
	    ValFreq[1] = LowItems;
	    ValFreq[2] = KnownItems - LowItems;
	    SplitGain[i] = ComputeGain(BaseInfo, UnknownRate[Att], 2, KnownItems);
	    SplitInfo[i] = TotalInfo(ValFreq, 0, 2) / Items;
	    AvGain += SplitGain[i];
	    Tries++;

	    Verbosity(3)
	    {	printf("\t\tCut at %.3f  (gain %.3f, val %.3f):",
	               ( CVal(Item[i],Att) + CVal(Item[i+1],Att) ) / 2,
	    	       SplitGain[i],
	    	       Worth(SplitInfo[i], SplitGain[i], Epsilon));
	    	       PrintDistribution(Att, 2, true);
	    }
	}
    }

    /*  Find the best attribute according to the given criterion  */

    ThreshCost = Log(Tries) / Items;

    BestVal = 0;
    BestI   = None;
    ForEach(i, Xp, Lp - 1)
    {
	if ( (Val = SplitGain[i] - ThreshCost) > BestVal )
	{
	    BestI   = i;
	    BestVal = Val;
	}
    }

    /*  If a test on the attribute is able to make a gain,
	set the best break point, gain and information  */ 

    if ( BestI == None )
    {
	Gain[Att] = -Epsilon;
	Info[Att] = 0.0;

	Verbosity(2) printf("\tno gain\n");
    }
    else
    {
	Bar[Att]  = (CVal(Item[BestI],Att) + CVal(Item[BestI+1],Att)) / 2;
	Gain[Att] = BestVal;
	Info[Att] = SplitInfo[BestI];

	Verbosity(2)
	    printf("\tcut=%.3f, inf %.3f, gain %.3f\n",
		   Bar[Att], Info[Att], Gain[Att]);
    }
} 



/*************************************************************************/
/*                                                                	 */
/*  Change a leaf into a test on a continuous attribute           	 */
/*                                                                	 */
/*************************************************************************/


    ContinTest(Node, Att)
/*  ----------  */
    Tree Node;
    Attribute Att;
{
    float Thresh, GreatestValueBelow();
    ItemCount CountItems();

    Sprout(Node, 2);

    Thresh = GreatestValueBelow(Att, Bar[Att]);

    Node->NodeType	= ThreshContin;
    Node->Tested	= Att;
    Node->Cut		=
    Node->Lower		=
    Node->Upper		= Thresh;
    Node->Errors        = 0;
}



/*************************************************************************/
/*                                                                	 */
/*  Return the greatest value of attribute Att below threshold t  	 */
/*                                                                	 */
/*************************************************************************/


float GreatestValueBelow(Att, t)
/*    ------------------  */
    Attribute Att;
    float t;
{
    ItemNo i;
    float v, Best;
    Boolean NotYet=true;

    ForEach(i, 0, MaxItem)
    {
	v = CVal(Item[i], Att);
	if ( v != Unknown && v <= t && ( NotYet || v > Best ) )
	{
	    Best = v;
	    NotYet = false;
	}
    }

    return Best;
}
/*************************************************************************/
/*									 */
/*      Evaluation of the subsetting of a discrete attribute		 */
/*      ----------------------------------------------------		 */
/*									 */
/*************************************************************************/


#include "buildex.i"


ItemCount
	*Slice1,	/* Slice1[c]    = saved values of Freq[x][c] in subset.c */
	*Slice2;	/* Slice2[c]    = saved values of Freq[y][c] */

Set
	**Subset;	/* Subset[a][s] = subset s for att a */

short
	*Subsets;	/* Subsets[a] = no. subsets for att a */



/*************************************************************************/
/*									 */
/*  Evaluate subsetting a discrete attribute and form the chosen	 */
/*  subsets Subset[Att][], setting Subsets[Att] to the number of	 */
/*  subsets, and the Info[] and Gain[] of a test on the attribute	 */
/*									 */
/*************************************************************************/


    EvalSubset(Att, Fp, Lp, Items)
/*  ----------  */ 
    Attribute Att;
    ItemNo Fp, Lp; 
    ItemCount Items;
{ 
    DiscrValue V1, V2, BestV1, BestV2, Barred;
    ItemCount KnownItems;
    ClassNo c;
    float BaseInfo, MinGain, ThisGain, ThisInfo,
	Val, BestVal, BestGain, BestInfo,
	PrevVal, PrevGain, PrevInfo,
	DiscrKnownBaseInfo(), Worth(), ComputeGain(), TotalInfo();
    short Blocks=0, MissingValues=0, ReasonableSubsets, Bytes, b;
    Boolean MergedSubsets = false;
    int SaveMINOBJS;

    SaveMINOBJS = MINOBJS;
    MINOBJS = 1;

    /*  First compute Freq[][], ValFreq[], base info, and the gain
	and total info of a split on discrete attribute Att  */

    ComputeFrequencies(Att, Fp, Lp);

    KnownItems = Items - ValFreq[0];
    if ( KnownItems < Epsilon )
    {
	Verbosity(2) printf("\tAtt %s: no known values\n", AttName[Att]);

	Gain[Att] = -Epsilon;
	Info[Att] = 0;
	return;
    }

    BaseInfo = DiscrKnownBaseInfo(KnownItems, MaxAttVal[Att]);

    PrevGain = ComputeGain(BaseInfo, UnknownRate[Att], MaxAttVal[Att],KnownItems);
    PrevInfo = TotalInfo(ValFreq, 0, MaxAttVal[Att]) / Items;
    PrevVal = Worth(PrevInfo, PrevGain, Epsilon);

    Verbosity(2)
    {
	printf("\tAtt %s", AttName[Att]);

	Verbosity(3) PrintDistribution(Att, MaxAttVal[Att], true);

	printf("\tinf %.3f, gain %.3f, val=%.3f\n",
		PrevInfo, PrevGain, PrevVal);
    }

    /*  Eliminate unrepresented attribute values from Freq[] and ValFreq[]
	and form a separate subset for each represented attribute value  */

    Bytes = (MaxAttVal[Att]>>3) + 1;
    ClearBits(Bytes, Subset[Att][0]);

    ForEach(V1, 1, MaxAttVal[Att])
    {
	if ( ValFreq[V1] > 0.5 )
	{
	    if ( ++Blocks < V1 )
	    {
		ValFreq[Blocks] = ValFreq[V1];
		ForEach(c, 0, MaxClass)
		{
		    Freq[Blocks][c] = Freq[V1][c];
		}
	    }
	    ClearBits(Bytes, Subset[Att][Blocks]);
	    SetBit(V1, Subset[Att][Blocks]);
	}
	else
	{
	    SetBit(V1, Subset[Att][0]);
	    MissingValues++;
	}
    }

    /*  Merge any single-class subsets with others of the same class  */
    /*  Note: have ValFreq[V] > 0 for all V  */

    ForEach(V1, 1, Blocks-1)
    {
	for ( c = 0 ; Freq[V1][c] < 0.1 ; c++ )
	    ;

	if ( Freq[V1][c] < ValFreq[V1] - 0.1 ) continue;

	/*  Now have a single class -- look for others  */

	for ( V2 = V1+1 ; V2 <= Blocks ; )
	{
	    if ( Freq[V2][c] < ValFreq[V2] - 0.1 )
	    {
		V2++;
	    }
	    else
	    {
		/*  Merge these subsets  */

		Combine(V1, V2, Blocks);

		ForEach(b, 0, Bytes-1)
		{
		    Subset[Att][V1][b] |= Subset[Att][V2][b];
		    Subset[Att][V2][b] = Subset[Att][Blocks][b];
		}

		Blocks--;
		MergedSubsets = true;
	    }
	}
    }

    if ( MergedSubsets )
    {
	PrevGain = ComputeGain(BaseInfo, UnknownRate[Att], Blocks, KnownItems);
	PrevInfo = TotalInfo(ValFreq, 0, Blocks) / Items;
	PrevVal = Worth(PrevInfo, PrevGain, Epsilon);

	Verbosity(2)
	{
	    printf("\tAfter merging single-class subsets:");

	    Verbosity(3) PrintDistribution(Att, Blocks, false);

	    printf("\tinf %.3f, gain %.3f, val=%.3f\n",
		    PrevInfo, PrevGain, PrevVal);
	}
    }

    /*  Examine possible pair mergers and hill-climb  */

    MinGain = PrevGain / 2;

    while ( Blocks > 2 )
    {
	BestVal = BestV1 = 0;
	BestGain = -Epsilon;

	/*  Check reasonable subsets; if less than 3, bar mergers
	    involving the largest block  */

	ReasonableSubsets = 0;
	Barred = 1;

	ForEach(V1, 1, Blocks)
	{
	    if ( ValFreq[V1] >= SaveMINOBJS ) ReasonableSubsets++;

	    if ( ValFreq[V1] > ValFreq[Barred] ) Barred = V1;
	}

	if ( ReasonableSubsets >= 3 ) Barred = 0;

	/*  For each possible pair of values, calculate the gain and
	    total info of a split in which they are treated as one.
	    Keep track of the pair with the best gain.  */

	ForEach(V1, 1, Blocks-1)
	{
	    ForEach(V2, V1+1, Blocks)
	    {
		if ( V1 == Barred || V2 == Barred ) continue;

		Combine(V1, V2, Blocks);

		ThisGain = ComputeGain(BaseInfo, UnknownRate[Att],
					Blocks-1, KnownItems);
		ThisInfo = TotalInfo(ValFreq, 0, Blocks-1) / Items;
		Val      = Worth(ThisInfo, ThisGain, Epsilon);

		Verbosity(4)
		{
		    printf("\tcombine %d %d info %.3f gain %.3f val %.3f",
		           V1, V2, ThisInfo, ThisGain, Val);
		    PrintDistribution(Att, Blocks-1, false);
		}

		/*  Force a split if
			less than two reasonable subsets, or
			using GAIN criterion
		    Prefer this split to the previous one if
			gain >= MinGain (and previous < MinGain), or
			val >= previous best val  */

		if ( ThisGain >= MinGain && BestGain < MinGain ||
		     Val >= BestVal ||
		     ! BestV1 && ( ! GAINRATIO || ReasonableSubsets < 2 ) )
		{
		    BestVal  = Val;
		    BestGain = ThisGain;
		    BestInfo = ThisInfo;
		    BestV1   = V1;
		    BestV2   = V2;
		}

		Uncombine(V1, V2);
	    }
	}

	if ( GAINRATIO &&
	     ReasonableSubsets >= 2 &&
	     ( ! BestV1 ||
	       BestVal < PrevVal + 1E-5 ||
	       BestVal == PrevVal && BestGain < PrevGain ) ) break;

	PrevGain = BestGain;
	PrevInfo = BestInfo;
        PrevVal = BestVal;

	Combine(BestV1, BestV2, Blocks);

	ForEach(b, 0, Bytes-1)
	{
	    Subset[Att][BestV1][b] |= Subset[Att][BestV2][b];
	    Subset[Att][BestV2][b] = Subset[Att][Blocks][b];
	}

	Blocks--;

	Verbosity(2)
	{
	    printf("\t\tform subset ");
	    PrintSubset(Att, Subset[Att][BestV1]);
	    printf(": %d subsets, inf %.3f, gain %.3f, val %.3f\n",
		   Blocks, BestInfo, BestGain, BestVal);
	    Verbosity(3)
	    {
		printf("\t\tcombine %d, %d", BestV1, BestV2);
		PrintDistribution(Att, Blocks, false);
	    }
	}
    }

    MINOBJS = SaveMINOBJS;

    if ( PrevVal <= 0 )
    {
	Gain[Att] = -Epsilon;
	Info[Att] = 0;
    }
    else
    {
	Gain[Att] = ComputeGain(BaseInfo, UnknownRate[Att], Blocks, KnownItems);
	Info[Att] = PrevInfo;

	if ( MissingValues )
	{
	    Blocks++;
	    CopyBits(Bytes, Subset[Att][0], Subset[Att][Blocks]);
	}

	Subsets[Att] = Blocks;

	Verbosity(2) printf("\tFinal subsets:");
	Verbosity(3) PrintDistribution(Att, Blocks, false);
	Verbosity(2)
	    printf("\tinf %.3f gain %.3f val %.3f\n", 
		   Info[Att], Gain[Att], Worth(Info[Att], Gain[Att], Epsilon));
    }
}



/*************************************************************************/
/*									 */
/*  Combine the distribution figures of discrete attribute values	 */
/*  x and y, putting the combined figures in Freq[x][] and		 */
/*  ValFreq[x][], and saving old values in Slice1 and Slice2		 */
/*									 */
/*************************************************************************/


    Combine(x, y, Last)
/*  -------  */
    DiscrValue x, y, Last;
{
    ClassNo c;

    ForEach(c, 0, MaxClass)
    {
	Slice1[c] = Freq[x][c];
	Slice2[c] = Freq[y][c];

	Freq[x][c] += Freq[y][c];
	Freq[y][c]  = Freq[Last][c];
    }

    Slice1[MaxClass+1] = ValFreq[x];
    Slice2[MaxClass+1] = ValFreq[y];

    ValFreq[x] += ValFreq[y];
    ValFreq[y]  = ValFreq[Last];
}



/*************************************************************************/
/*									 */
/*  Restore old class distribution figures of discrete attribute	 */
/*  values x and y from Slice1 and Slice2				 */
/*									 */
/*************************************************************************/


    Uncombine(x, y)
/*  ---------  */
    DiscrValue x, y;
{
    ClassNo c;

    ForEach(c, 0, MaxClass)
    {
	Freq[x][c] = Slice1[c];
	Freq[y][c] = Slice2[c];
    }

    ValFreq[x] = Slice1[MaxClass+1];
    ValFreq[y] = Slice2[MaxClass+1];
}



/*************************************************************************/
/*									 */
/*  Print the values of attribute Att which are in the subset Ss	 */
/*									 */
/*************************************************************************/


    PrintSubset(Att, Ss)
/*  -----------  */
    Attribute Att;
    Set Ss;
{
    DiscrValue V1;
    Boolean First=true;

    ForEach(V1, 1, MaxAttVal[Att])
    {
	if ( In(V1, Ss) )
	{
	    if ( First )
	    {
		First = false;
	    }
	    else
	    {
		printf(", ");
	    }

	    printf("%s", AttValName[Att][V1]);
	}
    }
}



/*************************************************************************/
/*									 */
/*  Construct and return a node for a test on a subset of values	 */
/*									 */
/*************************************************************************/


    SubsetTest(Node, Att)
/*  -----------  */
    Tree Node;
    Attribute Att;
{ 
    ItemCount CountItems();
    short S, Bytes;

    Sprout(Node, Subsets[Att]);

    Node->NodeType	= BrSubset;
    Node->Tested	= Att;
    Node->Errors	= 0;
    
    Bytes = (MaxAttVal[Att]>>3) + 1;
    Node->Subset = (Set *) calloc(Subsets[Att] + 1, sizeof(Set));
    ForEach(S, 1, Node->Forks)
    {
	Node->Subset[S] = (Set) malloc(Bytes);
	CopyBits(Bytes, Subset[Att][S], Node->Subset[S]);
    }
} 
/*************************************************************************/
/*									 */
/*	Prune a decision tree and predict its error rate		 */
/*	------------------------------------------------		 */
/*									 */
/*************************************************************************/


#include "defns.i"
#include "types.i"
#include "extern.i"


extern	ItemCount	*Weight;

Set	*PossibleValues=Nil;
Boolean	Changed;

#define	LocalVerbosity(x)	if (Sh >= 0 && VERBOSITY >= x)
#define	Intab(x)		Indent(x, "| ")



/*************************************************************************/
/*									 */
/*  Prune tree T, returning true if tree has been modified		 */
/*									 */
/*************************************************************************/


Boolean Prune(T)
/*      -----  */
    Tree T;
{
    ItemNo i;
    float EstimateErrors();
    Attribute a;

    InitialiseWeights();
    AllKnown = true;

    Verbosity(1) printf("\n");

    Changed = false;

    EstimateErrors(T, 0, MaxItem, 0, true);

    if ( SUBSET )
    {
	if ( ! PossibleValues )
	{
	    PossibleValues = (Set *) calloc(MaxAtt+1, sizeof(Set));
	}

	ForEach(a, 0, MaxAtt)
	{
	    if ( MaxAttVal[a] )
	    {
		PossibleValues[a] = (Set) malloc((MaxAttVal[a]>>3) + 1);
		ClearBits((MaxAttVal[a]>>3) + 1, PossibleValues[a]);
		ForEach(i, 1, MaxAttVal[a])
		{
		    SetBit(i, PossibleValues[a]);
		}
	    }
	}

	CheckPossibleValues(T);
    }

    return Changed;
}




/*************************************************************************/
/*									 */
/*	Estimate the errors in a given subtree				 */
/*									 */
/*************************************************************************/


float EstimateErrors(T, Fp, Lp, Sh, UpdateTree)
/*    --------------  */
    Tree T;
    ItemNo Fp, Lp; 
    short Sh;
    Boolean UpdateTree;
{ 
    ItemNo i, Kp, Ep, Group();
    ItemCount Cases, KnownCases, *LocalClassDist, TreeErrors, LeafErrors,
	ExtraLeafErrors, BranchErrors, CountItems(), Factor, MaxFactor, AddErrs();
    DiscrValue v, MaxBr;
    ClassNo c, BestClass;
    Boolean PrevAllKnown;

    /*  Generate the class frequency distribution  */

    Cases = CountItems(Fp, Lp);
    LocalClassDist = (ItemCount *) calloc(MaxClass+1, sizeof(ItemCount));

    ForEach(i, Fp, Lp)
    { 
	LocalClassDist[ Class(Item[i]) ] += Weight[i];
    } 

    /*  Find the most frequent class and update the tree  */

    BestClass = T->Leaf;
    ForEach(c, 0, MaxClass)
    {
	if ( LocalClassDist[c] > LocalClassDist[BestClass] )
	{
	    BestClass = c;
	}
    }
    LeafErrors = Cases - LocalClassDist[BestClass];
    ExtraLeafErrors = AddErrs(Cases, LeafErrors);

    if ( UpdateTree )
    {
	T->Items = Cases;
	T->Leaf  = BestClass;
	memcpy(T->ClassDist, LocalClassDist, (MaxClass + 1) * sizeof(ItemCount));
    }

    if ( ! T->NodeType )	/*  leaf  */
    {
	TreeErrors = LeafErrors + ExtraLeafErrors;

	if ( UpdateTree )
	{
	    T->Errors = TreeErrors;

	    LocalVerbosity(1)
	    {
		Intab(Sh);
	    	printf("%s (%.2f:%.2f/%.2f)\n", ClassName[T->Leaf],
	    		T->Items, LeafErrors, T->Errors);
	    }
	}

	free(LocalClassDist);

	return TreeErrors;
    }

    /*  Estimate errors for each branch  */

    Kp = Group(0, Fp, Lp, T) + 1;
    KnownCases = CountItems(Kp, Lp);

    PrevAllKnown = AllKnown;
    if ( Kp != Fp ) AllKnown = false;

    TreeErrors = MaxFactor = 0;

    ForEach(v, 1, T->Forks)
    {
	Ep = Group(v, Kp, Lp, T);

	if ( Kp <= Ep )
	{
	    Factor = CountItems(Kp, Ep) / KnownCases;

	    if ( Factor >= MaxFactor )
	    {
		MaxBr = v;
		MaxFactor = Factor;
	    }

	    ForEach(i, Fp, Kp-1)
	    {
		Weight[i] *= Factor;
	    }

	    TreeErrors += EstimateErrors(T->Branch[v], Fp, Ep, Sh+1, UpdateTree);

	    Group(0, Fp, Ep, T);
	    ForEach(i, Fp, Kp-1)
	    {
		Weight[i] /= Factor;
	    }
	}
    }
 
    AllKnown = PrevAllKnown;

    if ( ! UpdateTree )
    {
	free(LocalClassDist);

	return TreeErrors;
    }

    /*  See how the largest branch would fare  */

    BranchErrors = EstimateErrors(T->Branch[MaxBr], Fp, Lp, -1000, false);

    LocalVerbosity(1)
    {
        Intab(Sh);
        printf("%s:  [%d%%  N=%.2f  tree=%.2f  leaf=%.2f+%.2f  br[%d]=%.2f]\n",
		AttName[T->Tested],
		(int) ((TreeErrors * 100) / (T->Items + 0.001)),
		T->Items, TreeErrors, LeafErrors, ExtraLeafErrors,
		MaxBr, BranchErrors);
    }

    /*  See whether tree should be replaced with leaf or largest branch  */

    if ( LeafErrors + ExtraLeafErrors <= BranchErrors + 0.1 &&
	 LeafErrors + ExtraLeafErrors <= TreeErrors + 0.1 )
    {
	LocalVerbosity(1)
	{
	    Intab(Sh);
	    printf("Replaced with leaf %s\n", ClassName[T->Leaf]);
	}

	T->NodeType = 0;
	T->Errors = LeafErrors + ExtraLeafErrors;
	Changed = true;
    }
    else
    if ( BranchErrors <= TreeErrors + 0.1 )
    {
	LocalVerbosity(1)
	{
	    Intab(Sh);
	    printf("Replaced with branch %d\n", MaxBr);
	}

	AllKnown = PrevAllKnown;
	EstimateErrors(T->Branch[MaxBr], Fp, Lp, Sh, true);
	memcpy((char *) T, (char *) T->Branch[MaxBr], sizeof(TreeRec));
	Changed = true;
    }
    else
    {
	T->Errors = TreeErrors;
    }

    AllKnown = PrevAllKnown;
    free(LocalClassDist);

    return T->Errors;
}



/*************************************************************************/
/*									 */
/*	Remove unnecessary subset tests on missing values		 */
/*									 */
/*************************************************************************/


    CheckPossibleValues(T)
/*  -------------------  */
    Tree T;
{
    Set HoldValues;
    int v, Bytes, b;
    Attribute A;
    char Any=0;

    if ( T->NodeType == BrSubset )
    {
	A = T->Tested;

	Bytes = (MaxAttVal[A]>>3) + 1;
	HoldValues = (Set) malloc(Bytes);

	/*  See if last (default) branch can be simplified or omitted  */

	ForEach(b, 0, Bytes-1)
	{
	    T->Subset[T->Forks][b] &= PossibleValues[A][b];
	    Any |= T->Subset[T->Forks][b];
	}

	if ( ! Any )
	{
	    T->Forks--;
	}

	/*  Process each subtree, leaving only values in branch subset  */

	CopyBits(Bytes, PossibleValues[A], HoldValues);

	ForEach(v, 1, T->Forks)
	{
	    CopyBits(Bytes, T->Subset[v], PossibleValues[A]);

	    CheckPossibleValues(T->Branch[v]);
	}

	CopyBits(Bytes, HoldValues, PossibleValues[A]);

	free(HoldValues);
    }
    else
    if ( T->NodeType )
    {
	ForEach(v, 1, T->Forks)
	{
	    CheckPossibleValues(T->Branch[v]);
	}
    }
}
/*************************************************************************/
/*									 */
/*  Statistical routines for C4.5					 */
/*  -----------------------------					 */
/*									 */
/*************************************************************************/


#include "defns.i"
#include "types.i"
#include "extern.i"

									
/*************************************************************************/
/*									 */
/*  Compute the additional errors if the error rate increases to the	 */
/*  upper limit of the confidence level.  The coefficient is the	 */
/*  square of the number of standard deviations corresponding to the	 */
/*  selected confidence level.  (Taken from Documenta Geigy Scientific	 */
/*  Tables (Sixth Edition), p185 (with modifications).)			 */
/*									 */
/*************************************************************************/


float Val[] = {  0,  0.001, 0.005, 0.01, 0.05, 0.10, 0.20, 0.40, 1.00},
      Dev[] = {4.0,  3.09,  2.58,  2.33, 1.65, 1.28, 0.84, 0.25, 0.00};


float AddErrs(N, e)
/*    -------  */
    ItemCount N, e;
{
    static float Coeff=0;
    float Val0, Pr;

    if ( ! Coeff )
    {
	/*  Compute and retain the coefficient value, interpolating from
	    the values in Val and Dev  */

	int i;

	i = 0;
	while ( CF > Val[i] ) i++;

	Coeff = Dev[i-1] +
		  (Dev[i] - Dev[i-1]) * (CF - Val[i-1]) /(Val[i] - Val[i-1]);
	Coeff = Coeff * Coeff;
    }

    if ( e < 1E-6 )
    {
	return N * (1 - exp(log(CF) / N));
    }
    else
    if ( e < 0.9999 )
    {
	Val0 = N * (1 - exp(log(CF) / N));
	return Val0 + e * (AddErrs(N, 1.0) - Val0);
    }
    else
    if ( e + 0.5 >= N )
    {
	return 0.67 * (N - e);
    }
    else
    {
	Pr = (e + 0.5 + Coeff/2
	        + sqrt(Coeff * ((e + 0.5) * (1 - (e + 0.5)/N) + Coeff/4)) )
             / (N + Coeff);
	return (N * Pr - e);
    }
}
/*************************************************************************/
/*									 */
/*	Soften thresholds for continuous attributes			 */
/*	-------------------------------------------			 */
/*									 */
/*************************************************************************/


#include "defns.i"
#include "types.i"
#include "extern.i"


Boolean *LHSErr,	/*  Does a misclassification occur with this value of an att  */
	*RHSErr;	/*  if the below or above threshold branches are taken  */

ItemNo	*ThreshErrs;	/*  ThreshErrs[i] is the no. of misclassifications if thresh is i  */

float	*CVals;		/*  All values of a continuous attribute  */


#define	Below(v,t)	(v <= t + 1E-6)


/*************************************************************************/
/*									 */
/*  Soften all thresholds for continuous attributes in tree T		 */
/*									 */
/*************************************************************************/


    SoftenThresh(T)
/*  ------------  */
    Tree T;
{
    CVals = (float *) calloc(MaxItem+1, sizeof(float));
    LHSErr = (Boolean *) calloc(MaxItem+1, sizeof(Boolean));
    RHSErr = (Boolean *) calloc(MaxItem+1, sizeof(Boolean));
    ThreshErrs = (ItemNo *) calloc(MaxItem+1, sizeof(ItemNo));

    InitialiseWeights();

    ScanTree(T, 0, MaxItem);

    cfree(ThreshErrs);
    cfree(RHSErr);
    cfree(LHSErr);
    cfree(CVals);
}



/*************************************************************************/
/*								  	 */
/*  Calculate upper and lower bounds for each test on a continuous	 */
/*  attribute in tree T, using data items from Fp to Lp			 */
/*								  	 */
/*************************************************************************/


    ScanTree(T, Fp, Lp)
/*  --------  */
    Tree T;
    ItemNo Fp, Lp;
{
    short v;
    float Val, Se, Limit, Lower, Upper, GreatestValueBelow();
    ItemNo i, Kp, Ep, LastI, Errors, BaseErrors;
    ClassNo CaseClass, Class1, Class2, Category();
    Boolean LeftThresh=false;
    Description CaseDesc;
    Attribute Att;
    void Swap();

    /*  Stop when get to a leaf  */

    if ( ! T->NodeType ) return;

    /*  Group the unknowns together  */

    Kp = Group(0, Fp, Lp, T);

    /*  Soften a threshold for a continuous attribute  */

    Att = T->Tested;

    if ( T->NodeType == ThreshContin )
    {
	printf("\nTest %s <> %g\n", AttName[Att], T->Cut);

	Quicksort(Kp+1, Lp, Att, Swap);

	ForEach(i, Kp+1, Lp)
	{
	    /*  See how this item would be classified if its
		value were on each side of the threshold  */

	    CaseDesc = Item[i];
	    CaseClass = Class(CaseDesc);
	    Val = CVal(CaseDesc, Att);
		
	    Class1 = Category(CaseDesc, T->Branch[1]);
	    Class2 = Category(CaseDesc, T->Branch[2]);

	    CVals[i] = Val;
	    LHSErr[i] = (Class1 != CaseClass ? 1 : 0);
	    RHSErr[i] = (Class2 != CaseClass ? 1 : 0);
	}

	/*  Set Errors to total errors if take above thresh branch,
	    and BaseErrors to errors if threshold has original value  */

	Errors = BaseErrors = 0;
	ForEach(i, Kp+1, Lp)
	{
	    Errors += RHSErr[i];

	    if ( Below(CVals[i], T->Cut) )
	    {
		BaseErrors += LHSErr[i];
	    }
	    else
	    {
		BaseErrors += RHSErr[i];
	    }
	}

	/*  Calculate standard deviation of the number of errors  */

	Se = sqrt( (BaseErrors+0.5) * (Lp-Kp-BaseErrors+0.5) / (Lp-Kp+1) );
	Limit = BaseErrors + Se;

	Verbosity(1)
	{
	    printf("\t\t\tBase errors %d, items %d, se=%.1f\n",
		   BaseErrors, Lp-Kp, Se);
	    printf("\n\tVal <=   Errors\t\t+Errors\n");
	    printf("\t         %6d\n", Errors);
	}

	/*  Set ThreshErrs[i] to the no. of errors if the threshold were i  */

	ForEach(i, Kp+1, Lp)
	{
	    ThreshErrs[i] = Errors = Errors + LHSErr[i] - RHSErr[i];

	    if ( i == Lp || CVals[i] != CVals[i+1] )
	    {
		Verbosity(1)
		    printf("\t%6g   %6d\t\t%7d\n",
			CVals[i], Errors, Errors - BaseErrors);
	    }
	}

	/*  Choose Lower and Upper so that if threshold were set to
	    either, the number of items misclassified would be one
	    standard deviation above BaseErrors  */

	LastI = Kp+1;
	Lower = Min(T->Cut, CVals[LastI]);
	Upper = Max(T->Cut, CVals[Lp]);
	while ( CVals[LastI+1] == CVals[LastI] ) LastI++;

	while ( LastI < Lp )
	{
	    i = LastI + 1;
	    while ( i < Lp && CVals[i+1] == CVals[i] ) i++;

	    if ( ! LeftThresh &&
		 ThreshErrs[LastI] > Limit &&
		 ThreshErrs[i] <= Limit &&
		 Below(CVals[i], T->Cut) )
	    {
		Lower = CVals[i] -
			(CVals[i] - CVals[LastI]) * (Limit - ThreshErrs[i]) /
			(ThreshErrs[LastI] - ThreshErrs[i]);
		LeftThresh = true;
	    }
	    else
	    if ( ThreshErrs[LastI] <= Limit &&
		 ThreshErrs[i] > Limit &&
		 ! Below(CVals[i], T->Cut) )
	    {
		Upper = CVals[LastI] +
			(CVals[i] - CVals[LastI]) * (Limit - ThreshErrs[LastI]) /
			(ThreshErrs[i] - ThreshErrs[LastI]);
		if ( Upper < T->Cut ) Upper = T->Cut;
	    }

	    LastI = i;
	}

	T->Lower = Lower;
	T->Upper = Upper;

	Verbosity(1) printf("\n");

	printf("\tLower = %g, Upper = %g\n", T->Lower, T->Upper);
    }

    /*  Recursively scan each branch  */

    ForEach(v, 1, T->Forks)
    {
	Ep = Group(v, Kp+1, Lp, T);

	if ( Kp < Ep )
	{
	    ScanTree(T->Branch[v], Kp+1, Ep);
	    Kp = Ep;
	}
    }
}
/*************************************************************************/
/*								  	 */
/*	Routine for printing confusion matrices				 */
/*	---------------------------------------				 */
/*								  	 */
/*************************************************************************/

#include "defns.i"
#include "types.i"
#include "extern.i"


    PrintConfusionMatrix(ConfusionMat)
/*  --------------------  */
    ItemNo *ConfusionMat;
{
    short Row, Col;

    if ( MaxClass > 20 ) return;  /* Don't print nonsensical matrices */

    /*  Print the heading, then each row  */

    printf("\n\n\t");
    ForEach(Col, 0, MaxClass)
    {
	printf("  (%c)", 'a' + Col);
    }

    printf("\t<-classified as\n\t");
    ForEach(Col, 0, MaxClass)
    {
	printf(" ----");
    }
    printf("\n");

    ForEach(Row, 0, MaxClass)
    {
	printf("\t");
	ForEach(Col, 0, MaxClass)
	{
	    if ( ConfusionMat[Row*(MaxClass+1) + Col] )
	    {
		printf("%5d", ConfusionMat[Row*(MaxClass+1) + Col]);
	    }
	    else
	    {
		printf("     ");
	    }
	}
	printf("\t(%c): class %s\n", 'a' + Row, ClassName[Row]);
    }
    printf("\n");
}
/*************************************************************************/
/*									 */
/*	Sorting utilities						 */
/*	-----------------						 */
/*									 */
/*************************************************************************/


#include "defns.i"
#include "types.i"
#include "extern.i"



/*************************************************************************/
/*									 */
/*	Sort items from Fp to Lp on attribute a				 */
/*									 */
/*************************************************************************/


    Quicksort(Fp, Lp, Att, Exchange)
/*  ---------  */
    ItemNo Fp, Lp;
    Attribute Att;
    void (*Exchange)();
{
    register ItemNo Lower, Middle;
    register float Thresh;
    register ItemNo i;

    if ( Fp < Lp )
    {
	Thresh = CVal(Item[Lp], Att);

	/*  Isolate all items with values <= threshold  */

	Middle = Fp;

	for ( i = Fp ; i < Lp ; i++ )
	{ 
	    if ( CVal(Item[i], Att) <= Thresh )
	    { 
		if ( i != Middle ) (*Exchange)(Middle, i);
		Middle++; 
	    } 
	} 

	/*  Extract all values equal to the threshold  */

	Lower = Middle - 1;

	for ( i = Lower ; i >= Fp ; i-- )
	{
	    if ( CVal(Item[i], Att) == Thresh )
	    { 
		if ( i != Lower ) (*Exchange)(Lower, i);
		Lower--;
	    } 
	} 

	/*  Sort the lower values  */

	Quicksort(Fp, Lower, Att, Exchange);

	/*  Position the middle element  */

	(*Exchange)(Middle, Lp);

	/*  Sort the higher values  */

	Quicksort(Middle+1, Lp, Att, Exchange);
    }
}
/*************************************************************************/
/*									 */
/*	Get names of classes, attributes and attribute values		 */
/*	-----------------------------------------------------		 */
/*									 */
/*************************************************************************/


#include "defns.i"
#include "types.i"
#include "extern.i"

#include <sys/types.h>
#include <sys/stat.h>


#define  Space(s)	(s == ' ' || s == '\n' || s == '\t')
#define  SkipComment	while ( ( c = getc(f) ) != '\n' )

char	Delimiter;
String	CopyString();



/*************************************************************************/
/*									 */
/*  Read a name from file f into string s, setting Delimiter.		 */
/*									 */
/*  - Embedded periods are permitted, but periods followed by space	 */
/*    characters act as delimiters.					 */
/*  - Embedded spaces are permitted, but multiple spaces are replaced	 */
/*    by a single space.						 */
/*  - Any character can be escaped by '\'.				 */
/*  - The remainder of a line following '|' is ignored.			 */
/*									 */
/*************************************************************************/


Boolean ReadName(f, s)
/*      ---------  */
    FILE *f;
    String s;
{
    register char *Sp=s;
    register int c;

    /*  Skip to first non-space character  */

    while ( ( c = getc(f) ) == '|' || Space(c) )
    {
	if ( c == '|' ) SkipComment;
    }

    /*  Return false if no names to read  */

    if ( c == EOF )
    {
	Delimiter = EOF;
	return false;
    }

    /*  Read in characters up to the next delimiter  */

    while ( c != ':' && c != ',' && c != '\n' && c != '|' && c != EOF )
    {
	if ( c == '.' )
	{
	    if ( ( c = getc(f) ) == '|' || Space(c) ) break;
	    *Sp++ = '.';
	}

	if ( c == '\\' )
	{
	    c = getc(f);
	}

	*Sp++ = c;

	if ( c == ' ' )
	{
	    while ( ( c = getc(f) ) == ' ' )
		;
	}
	else
	{
	    c = getc(f);
	}
    }

    if ( c == '|' ) SkipComment;
    Delimiter = c;

    /*  Strip trailing spaces  */

    while ( Space(*(Sp-1)) ) Sp--;

    *Sp++ = '\0';
    return true;
}



/*************************************************************************/
/*									 */
/*  Read the names of classes, attributes and legal attribute values.	 */
/*  On completion, these names are stored in:				 */
/*	ClassName	-  class names					 */
/*	AttName		-  attribute names				 */
/*	AttValName	-  attribute value names			 */
/*  with:								 */
/*	MaxAttVal	-  number of values for each attribute		 */
/*									 */
/*  Other global variables set are:					 */
/*	MaxAtt		-  maximum attribute number			 */
/*	MaxClass	-  maximum class number				 */
/*	MaxDiscrVal	-  maximum discrete values for any attribute	 */
/*									 */
/*  Note:  until the number of attributes is known, the name		 */
/*	   information is assembled in local arrays			 */
/*									 */
/*************************************************************************/


    GetNames()
/*  ---------  */
{
    FILE *Nf, *fopen();
    char Fn[100], Buffer[1000];
    DiscrValue v;
    int AttCeiling=100, ClassCeiling=100, ValCeiling;

    /*  Open names file  */

    strcpy(Fn, FileName);
    strcat(Fn, ".names");
    if ( ! ( Nf = fopen(Fn, "r") ) ) Error(0, Fn, "");

    /*  Get class names from names file  */

    ClassName = (String *) calloc(ClassCeiling, sizeof(String));
    MaxClass = -1;
    do
    {
	ReadName(Nf, Buffer);

	if ( ++MaxClass >= ClassCeiling)
	{
	    ClassCeiling += 100;
	    ClassName = (String *) realloc(ClassName, ClassCeiling*sizeof(String));
	}
	ClassName[MaxClass] = CopyString(Buffer);
    }
    while ( Delimiter == ',' );

    /*  Get attribute and attribute value names from names file  */

    AttName = (String *) calloc(AttCeiling, sizeof(String));
    MaxAttVal = (DiscrValue *) calloc(AttCeiling, sizeof(DiscrValue));
    AttValName = (String **) calloc(AttCeiling, sizeof(String *));
    SpecialStatus = (char *) malloc(AttCeiling);

    MaxAtt = -1;
    while ( ReadName(Nf, Buffer) )
    {
	if ( Delimiter != ':' ) Error(1, Buffer, "");

	if ( ++MaxAtt >= AttCeiling )
	{
	    AttCeiling += 100;
	    AttName = (String *) realloc(AttName, AttCeiling*sizeof(String));
	    MaxAttVal = (DiscrValue *) realloc(MaxAttVal, AttCeiling*sizeof(DiscrValue));
	    AttValName = (String **) realloc(AttValName, AttCeiling*sizeof(String *));
	    SpecialStatus = (char *) realloc(SpecialStatus, AttCeiling);
	}

	AttName[MaxAtt] = CopyString(Buffer);
	SpecialStatus[MaxAtt] = Nil;
	MaxAttVal[MaxAtt] = 0;
	ValCeiling = 100;
	AttValName[MaxAtt] = (String *) calloc(ValCeiling, sizeof(String));

	do
	{
	    if ( ! ( ReadName(Nf, Buffer) ) ) Error(2, AttName[MaxAtt], "");

	    if ( ++MaxAttVal[MaxAtt] >= ValCeiling )
	    {
		ValCeiling += 100;
		AttValName[MaxAtt] =
		    (String *) realloc(AttValName[MaxAtt], ValCeiling*sizeof(String));
	    }

	    AttValName[MaxAtt][MaxAttVal[MaxAtt]] = CopyString(Buffer);
	}
	while ( Delimiter == ',' );

	if ( MaxAttVal[MaxAtt] == 1 )
	{
	    /*  Check for special treatment  */

	    if ( ! strcmp(Buffer, "continuous") )
	    {}
	    else
	    if ( ! memcmp(Buffer, "discrete", 8) )
	    {
		SpecialStatus[MaxAtt] = DISCRETE;

		/*  Read max values, reserve space and check MaxDiscrVal  */

		v = atoi(&Buffer[8]);
		if ( v < 2 )
		{
		    printf("** %s: illegal number of discrete values\n",
			   AttName[MaxAtt]);
		    exit(1);
		}

		AttValName[MaxAtt] =
		    (String *) realloc(AttValName[MaxAtt], (v+2)*sizeof(String));
		AttValName[MaxAtt][0] = (char *) v;
		if ( v > MaxDiscrVal ) MaxDiscrVal = v;
	    }
	    else
	    if ( ! strcmp(Buffer, "ignore") )
	    {
		SpecialStatus[MaxAtt] = IGNORE;
	    }
	    else
	    {
		/*  Cannot have only one discrete value for an attribute  */

		Error(3, AttName[MaxAtt], "");
	    }

	    MaxAttVal[MaxAtt] = 0;
	}
	else
	if ( MaxAttVal[MaxAtt] > MaxDiscrVal ) MaxDiscrVal = MaxAttVal[MaxAtt];
    }

    fclose(Nf);
}



/*************************************************************************/
/*									 */
/*	Locate value Val in List[First] to List[Last]			 */
/*									 */
/*************************************************************************/


int Which(Val, List, First, Last)
/*  -----  */
    String Val, List[];
    short First, Last;
{
    short n=First;

    while ( n <= Last && strcmp(Val, List[n]) ) n++;

    return ( n <= Last ? n : First-1 );
}



/*************************************************************************/
/*									 */
/*	Allocate space then copy string into it				 */
/*									 */
/*************************************************************************/

String CopyString(x)
/*     -----------  */
    String x;
{
    char *s;

    s = (char *) calloc(strlen(x)+1, sizeof(char));
    strcpy(s, x);
    return s;
}



/*************************************************************************/
/*									 */
/*			Error messages					 */
/*									 */
/*************************************************************************/

    Error(n, s1, s2)
/*  -----  */
    short n;
    String s1, s2;
{
    static char Messages=0;

    printf("\nERROR:  ");
    switch(n)
    {
	case 0: printf("cannot open file %s%s\n", s1, s2);
		exit(1);

	case 1:	printf("colon expected after attribute name %s\n", s1);
		break;

	case 2:	printf("unexpected eof while reading attribute %s\n", s1);
		break;

	case 3: printf("attribute %s has only one value\n", s1);
		break;

	case 4: printf("case %d's value of '%s' for attribute %s is illegal\n",
		    MaxItem+1, s2, s1);
		break;

	case 5: printf("case %d's class of '%s' is illegal\n", MaxItem+1, s2);
    }

    if ( ++Messages > 10 )
    {
	printf("Error limit exceeded\n");
	exit(1);
    }
}
/*************************************************************************/
/*									 */
/*	Get case descriptions from data file				 */
/*	--------------------------------------				 */
/*									 */
/*************************************************************************/


#include "defns.i"
#include "types.i"
#include "extern.i"

#define Inc 2048



/*************************************************************************/
/*									 */
/*  Read raw case descriptions from file with given extension.		 */
/*									 */
/*  On completion, cases are stored in array Item in the form		 */
/*  of Descriptions (i.e. arrays of attribute values), and		 */
/*  MaxItem is set to the number of data items.				 */
/*									 */
/*************************************************************************/

    GetData(Extension)
/*  --------  */
    String Extension;
{
    FILE *Df, *fopen();
    char Fn[100];
    ItemNo i=0, j, ItemSpace=0;
    Description GetDescription();

    /*  Open data file  */

    strcpy(Fn, FileName);
    strcat(Fn, Extension);
    if ( ! ( Df = fopen(Fn, "r") ) ) Error(0, Fn, "");

    do
    {
	MaxItem = i;

	/*  Make sure there is room for another item  */

	if ( i >= ItemSpace )
	{
	    if ( ItemSpace )
	    {
		ItemSpace += Inc;
		Item = (Description *)
			realloc(Item, ItemSpace*sizeof(Description));
	    }
	    else
	    {
		Item = (Description *)
			malloc((ItemSpace=Inc)*sizeof(Description));
	    }
	}

	Item[i] = GetDescription(Df);

    } while ( Item[i] != Nil && ++i );

    fclose(Df);
    MaxItem = i - 1;
}



/*************************************************************************/
/*									 */
/*  Read a raw case description from file Df.				 */
/*									 */
/*  For each attribute, read the attribute value from the file.		 */
/*  If it is a discrete valued attribute, find the associated no.	 */
/*  of this attribute value (if the value is unknown this is 0).	 */
/*									 */
/*  Returns the Description of the case (i.e. the array of		 */
/*  attribute values).							 */
/*									 */
/*************************************************************************/


Description GetDescription(Df)
/*          ---------------  */
    FILE *Df;
{
    Attribute Att;
    char name[500], *endname, *CopyString();
    Boolean ReadName();
    int Dv;
    float Cv;
    Description Dvec;
    double strtod();

    if ( ReadName(Df, name) )
    {
	Dvec = (Description) calloc(MaxAtt+2, sizeof(AttValue));

        ForEach(Att, 0, MaxAtt)
        {
	    if ( SpecialStatus[Att] == IGNORE )
	    {
		/*  Skip this value  */

		DVal(Dvec, Att) = 0;
	    }
	    else
	    if ( MaxAttVal[Att] || SpecialStatus[Att] == DISCRETE )
	    {
		/*  Discrete value  */ 

	        if ( ! ( strcmp(name, "?") ) )
		{
		    Dv = 0;
		}
	        else
	        {
		    Dv = Which(name, AttValName[Att], 1, MaxAttVal[Att]);
		    if ( ! Dv )
		    {
			if ( SpecialStatus[Att] == DISCRETE )
			{
			    /*  Add value to list  */

			    Dv = ++MaxAttVal[Att];
			    if ( Dv > (int) AttValName[Att][0] )
			    {
				printf("\nToo many values for %s (max %d)\n",
					AttName[Att], (int) AttValName[Att][0]);
				exit(1);
			    }

			    AttValName[Att][Dv] = CopyString(name);
			}
			else
			{
			    Error(4, AttName[Att], name);
			}
		    }
	        }
	        DVal(Dvec, Att) = Dv;
	    }
	    else
	    {
		/*  Continuous value  */

	        if ( ! ( strcmp(name, "?") ) )
		{
		    Cv = Unknown;
		}
	        else
		{
		    Cv = strtod(name, &endname);
		    if ( endname == name || *endname != '\0' )
			Error(4, AttName[Att], name);
		}
		CVal(Dvec, Att) = Cv;
	    }

	    ReadName(Df, name);
        }

        if ( (Dv = Which(name, ClassName, 0, MaxClass)) < 0 )
        {
	    Error(5, "", name);
	    Dv = 0;
        }
	Class(Dvec) = Dv;

	return Dvec;
    }
    else
    {
	return Nil;
    }
}
/*************************************************************************/
/*                                                              	 */
/*  Determine the class of a case description from a decision tree	 */
/*  --------------------------------------------------------------	 */
/*                                                              	 */
/*************************************************************************/


#include "defns.i"
#include "types.i"
#include "extern.i"

float	*ClassSum=Nil;		/* ClassSum[c] = total weight of class c */



/*************************************************************************/
/*                                                              	 */
/*  Categorize a case description using the given decision tree		 */
/*                                                              	 */
/*************************************************************************/


ClassNo Category(CaseDesc, DecisionTree) 
/*       --------  */
    Description CaseDesc; 
    Tree DecisionTree; 
{ 
    ClassNo c, BestClass;

    if ( ! ClassSum )
    {
	ClassSum = (float *) malloc((MaxClass+1) * sizeof(float));
    }

    ForEach(c, 0, MaxClass)
    {
	ClassSum[c] = 0;
    }

    Classify(CaseDesc, DecisionTree, 1.0);

    BestClass = 0;
    ForEach(c, 0, MaxClass)
    {
	Verbosity(5) printf("class %s weight %.2f\n", ClassName[c], ClassSum[c]);

	if ( ClassSum[c] > ClassSum[BestClass] ) BestClass = c;
    }

    return BestClass;
}



/*************************************************************************/
/*                                                              	 */
/*  Classify a case description using the given subtree by adjusting	 */
/*  the value ClassSum for each class					 */
/*                                                              	 */
/*************************************************************************/


    Classify(CaseDesc, T, Weight)
/*  --------  */
    Description CaseDesc; 
    Tree T;
    float Weight;
{
    DiscrValue v, dv;
    float Cv;
    Attribute a;
    ClassNo c;

    switch ( T->NodeType )
    {
        case 0:  /* leaf */

	    if ( T->Items > 0 )
	    {
		/*  Update from ALL classes  */

		ForEach(c, 0, MaxClass)
		{
		    if ( T->ClassDist[c] )
		    {
			ClassSum[c] += Weight * T->ClassDist[c] / T->Items;
		    }
		}
	    }
	    else
	    {
		ClassSum[T->Leaf] += Weight;
	    }

	    return;

	case BrDiscr:  /* test of discrete attribute */

	    a = T->Tested;
	    v = DVal(CaseDesc, a);

	    if ( v && v <= T->Forks )	/*  Make sure not new discrete value  */
	    {
		Classify(CaseDesc, T->Branch[v], Weight);
	    }
	    else
	    {
		ForEach(v, 1, T->Forks)
		{
		    Classify(CaseDesc, T->Branch[v], 
			     (Weight * T->Branch[v]->Items) / T->Items);
		}
	    }

	    return;

	case ThreshContin:  /* test of continuous attribute */

	    a = T->Tested;
	    Cv = CVal(CaseDesc, a);

	    if ( Cv == Unknown )
	    {
		ForEach(v, 1, 2)
		{
		    Classify(CaseDesc, T->Branch[v], 
			     (Weight * T->Branch[v]->Items) / T->Items);
		}
	    }
	    else
	    {
		v = ( Cv <= T->Cut ? 1 : 2 );
		Classify(CaseDesc, T->Branch[v], Weight);
	    }

	    return;

	case BrSubset:  /* subset test on discrete attribute  */

	    a = T->Tested;
	    dv = DVal(CaseDesc, a);

	    if ( dv )
	    {
		ForEach(v, 1, T->Forks)
		{
		    if ( In(dv, T->Subset[v]) )
		    {
			Classify(CaseDesc, T->Branch[v], Weight);

			return;
		    }
		}
	    }

	    /*  Value unknown or not found in any of the subsets  */

	    ForEach(v, 1, T->Forks)
	    {
		Classify(CaseDesc, T->Branch[v], 
		         (Weight * T->Branch[v]->Items) / T->Items);
	    }

	    return;
    } 
}
/*************************************************************************/
/*									 */
/*	Routines for displaying, building, saving and restoring trees	 */
/*	-------------------------------------------------------------	 */
/*									 */
/*************************************************************************/


#include "defns.i"
#include "types.i"
#include "extern.i"


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


    PrintTree(T)
/*  ----------  */
    Tree T;
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
}



/*************************************************************************/
/*									 */
/*	Display the tree T with offset Sh				 */
/*									 */
/*************************************************************************/


    Show(T, Sh)
/*  ---- */
    Tree T;
    short Sh;
{
    DiscrValue v, MaxV;
    short MaxLine();

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
}



/*************************************************************************/
/*									 */
/*	Print a node T with offset Sh, branch value v, and continue	 */
/*									 */
/*************************************************************************/


    ShowBranch(Sh, T, v)
/*  -----------  */
    short Sh;
    Tree T;
    DiscrValue v;
{
    DiscrValue Pv, Last;
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
}



/*************************************************************************/
/*									 */
/*	Find the maximum single line size for non-leaf subtree St.	 */
/*	The line format is						 */
/*			<attribute> <> X.xx:[ <class (<Items>)], or	 */
/*			<attribute> = <DVal>:[ <class> (<Items>)]	 */
/*									 */
/*************************************************************************/


short MaxLine(St)
/*    --------  */
    Tree St;
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

    Indent(Sh, Mark)
/*  ------  */
    short Sh;
    char *Mark;
{
    printf("\n");
    while ( Sh-- ) printf("%s", Mark);
}



/*************************************************************************/
/*									 */
/*	Save entire decision tree T in file with extension Extension	 */
/*									 */
/*************************************************************************/


    SaveTree(T, Extension)
/*  ---------  */
    Tree T;
    String Extension;
{
    static char *LastExt="";

    if ( strcmp(LastExt, Extension) )
    {
	LastExt = Extension;

	if ( TRf ) fclose(TRf);

	strcpy(Fn, FileName);
	strcat(Fn, Extension);
	if ( ! ( TRf = fopen(Fn, "w") ) )
	    Error(0, Fn, " for writing");
    }

    putc('\n', TRf);
    OutTree(T);

    SaveDiscreteNames();
}



/*************************************************************************/
/*									 */
/*	Save tree T as characters					 */
/*									 */
/*************************************************************************/


    OutTree(T)
/*  --------  */
    Tree T;
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
}



/*************************************************************************/
/*									 */
/*	Retrieve entire decision tree with extension Extension		 */
/*									 */
/*************************************************************************/


Tree GetTree(Extension)
/*   --------  */
    String Extension;
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


    StreamOut(s, n)
/*  ---------  */
    String s;
    int n;
{
    while ( n-- ) putc(*s++, TRf);
}



    StreamIn(s, n)
/*  ---------  */
    String s;
    int n;
{
    while ( n-- ) *s++ = getc(TRf);
}



/*************************************************************************/
/*									 */
/*	Free up space taken up by tree Node				 */
/*									 */
/*************************************************************************/


    ReleaseTree(Node)
/*  -------  */
    Tree Node;
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
}



/*************************************************************************/
/*									 */
/*	Construct a leaf in a given node				 */
/*									 */
/*************************************************************************/


Tree Leaf(ClassFreq, NodeClass, Cases, Errors)
/*   ----  */
    ItemCount *ClassFreq;
    ClassNo NodeClass;
    ItemCount Cases, Errors;
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


    Sprout(Node, Branches)
/*  ------  */
    Tree Node;
    DiscrValue Branches;
{
    Node->Forks = Branches;
    
    Node->Branch = (Tree *) calloc(Branches+1, sizeof(Tree));
}



/*************************************************************************/
/*									 */
/*	Count the nodes in a tree					 */
/*									 */
/*************************************************************************/

	
    TreeSize(Node)
/*  --------  */
    Tree Node;
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


Tree CopyTree(T)
/*   ---------  */
    Tree T;
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


    SaveDiscreteNames()
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
}



/*************************************************************************/
/*									 */
/*	Recover attribute values read with "discrete N"			 */
/*									 */
/*************************************************************************/


    RecoverDiscreteNames()
/*  --------------------  */
{
    Attribute Att;
    DiscrValue v;
    int Length;

    ForEach(Att, 0, MaxAtt)
    {
	if ( SpecialStatus[Att] != DISCRETE ) continue;

	StreamIn(&MaxAttVal[Att], sizeof(int));

	ForEach(v, 1, MaxAttVal[Att])
	{
	    StreamIn(&Length, sizeof(int));

	    AttValName[Att][v] = (char *) malloc(Length);
	    StreamIn(AttValName[Att][v], Length);
	}
    }
}
/*************************************************************************/
/*									 */
/*	Print header for all C4.5 programs				 */
/*	----------------------------------				 */
/*									 */
/*************************************************************************/


#define  RELEASE "8"


    PrintHeader(Title)
/*  -----------  */
    char *Title;
{
    char *ctime(), TitleLine[80];
    long clock, time();
    short Underline;

    clock = time(0);
    sprintf(TitleLine, "C4.5 [release %s] %s", RELEASE, Title);
    printf("\n%s\t%s", TitleLine, ctime(&clock));

    Underline = strlen(TitleLine);
    while ( Underline-- ) putchar('-');
    putchar('\n');
}
