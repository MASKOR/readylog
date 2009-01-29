
#ifndef _C45CONSULTOBJECT_DEFINITIONS_H_
#define _C45CONSULTOBJECT_DEFINITIONS_H_

//from defines

#define	 SkipSpace	        while ( (c = getchar()) == ' ' || c == '\t' )

#define  CVal(Case,Attribute)   Case[Attribute]._cont_val
#define  DVal(Case,Attribute)   Case[Attribute]._discr_val
#define  Class(Case)		Case[MaxAtt+1]._discr_val

#define  Space(s)	        ((s == ' ') || (s == '\n') || (s == '\t'))
#define  SkipComment	        while ( ( c = getc(f) ) != '\n' )

#define  Unknown                -999		/* unknown value for continuous attribute */

#define  BrDiscr	        1	/* node types:	branch */
#define  ThreshContin	        2	/*		threshold cut */
#define  BrSubset	        3	/*		subset test */

#define  IGNORE		        1	/* special attribute status: do not use */
#define  DISCRETE	        2	/* ditto: collect values as data read */

#define	 Eof			EOF             /*char read on end of file*/
#define	 Nil			0               /*null pointer*/
#define	 false			0 
#define	 true			1 
#define	 None			-1
#define	 Epsilon                1E-3

long	 random();
#define	 C45Random		((random()&2147483647) / 2147483648.0)

/* DO NOT REMOVE THE C45 PREFIX OR YOU WILL BREAK THE SOFTWARE! */
#define	 C45Max(a,b)            ((a)>(b) ? a : b) 
#define	 C45Min(a,b)            ((a)<(b) ? a : b) 
#define	 C45Round(x)		((int) (x+0.5))
#define	 C45Log2		0.69314718055994530942
#define	 C45Log(x)		((x) <= 0 ? 0.0 : log((float)x) / Log2)

#define	 Bit(b)			(1 << (b))
#define	 In(b,s)		((s[(b) >> 3]) & Bit((b) & 07))
#define	 ClearBits(n,s)		memset(s,0,n)
#define	 CopyBits(n,f,t)	memcpy(t,f,n)
#define	 SetBit(b,s)		(s[(b) >> 3] |= Bit((b) & 07))
#define	 ForEach(v,f,l)		for(v=f ; v<=l ; ++v) 
#define	 Verbosity(d)		if(VERBOSITY >= d)

#define	 Check(v,l,h)           if ( v<l||v>h ) {printf("\t** illegal value **\n"); exit(1);}

// from trees

#define	Tab		        "|   "
#define	TabSize		        4
#define	Width		        80	/* approx max width of printed trees */
                                        /*  If lines look like getting too long 
				            while a tree is being printed, 
				            subtrees are broken off and printed 
				            separately after the main tree is 
				            finished	 */


// from consult

#define Fuzz	0.01			/* minimum weight */



//from types

typedef  char	Boolean;
typedef  char	*Set;

typedef  int	ItemNo;		/* data item number */
typedef  float	ItemCount;	/* count of (partial) items */

typedef  short	ClassNo;	/* class number, 0..MaxClass */
typedef  short	DiscrValue;	/* discrete attribute value (0 = ?) */

typedef  short	Attribute;	/* attribute number, 0..MaxAtt */

typedef  union  _attribute_value
{
  DiscrValue	_discr_val;
  float	_cont_val;
} AttValue, *Description;

typedef  struct _tree_record *Tree;

typedef  struct _tree_record
{
  short	NodeType;	/* 0=leaf 1=branch 2=cut 3=subset */
  ClassNo   Leaf;	/* most frequent class at this node */
  ItemCount Items;	/* no of items at this node */
  ItemCount *ClassDist;	/* class distribution of items */
  ItemCount Errors;	/* no of errors at this node */
  Attribute Tested; 	/* attribute referenced in test */
  short	    Forks;	/* number of branches at this node */
  float	    Cut;	/* threshold for continuous attribute */
  float     Lower;	/* lower limit of soft threshold */
  float     Upper;	/* upper limit ditto */
  Set      *Subset;	/* subsets of discrete values  */
  Tree	   *Branch;	/* Branch[x] = (sub)tree for outcome x */
} TreeRec;



typedef short	RuleNo;			/* rule number */


typedef struct TestRec *Test;

struct TestRec
{
  short	     NodeType;	/* test type (see tree nodes) */
  Attribute  Tested;	/* attribute tested */
  short	     Forks;	/* possible branches */
  float	     Cut;	/* threshold (if relevant) */
  Set	    *Subset;	/* subset (if relevant) */
};


typedef struct CondRec *Condition;

struct CondRec
{
  Test	CondTest;	/* test part of condition */
  short	TestValue;	/* specified outcome of test */
};


typedef struct ProdRuleRec PR;

struct ProdRuleRec
{
  short	Size;		/* number of conditions */
  Condition	*Lhs;	/* conditions themselves */
  ClassNo	Rhs;	/* class given by rule */
  float	Error;		/* estimated error rate */
  float Bits;		/* bits to encode rule */
  ItemNo Used;	        /* times rule used */
  ItemNo Incorrect;	/* times rule incorrect */
};


typedef struct RuleSetRec RuleSet;

struct RuleSetRec
{
  PR		*SRule;		/* rules */
  RuleNo	SNRules;	/* number of rules */
  RuleNo        *SRuleIndex;    /* ranking of rules */
  ClassNo	SDefaultClass;	/* default class for this ruleset */
};



typedef	struct	ValRange *RangeDescRec;

struct	ValRange
	{
	    Boolean	Known, Asked;
	    float	LowerBound, UpperBound, *Probability;
	};






#endif
