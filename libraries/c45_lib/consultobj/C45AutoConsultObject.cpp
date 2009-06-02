//*****************************************************************//
// Created by Stefan Jacobs <Stefan_J@gmx.de>                      //
//                                                                 //
// Created:         2002-06-16                                     //
// Last Modified:   2009-01-26 by Dennis Pannhausen                //
//                             <Dennis.Pannhausen@rwth-aachen.de>  //
//                                                                 //
//*****************************************************************//

/*! \file C45AutoConsultObject.cpp
<pre>
<b>File:</b>          C45AutoConsultObject.cpp
<b>Project:</b>       AllemaniACs Soccer Simulation League
<b>Authors:</b>       Stefan Jacobs <Stefan_J@gmx.de>
<b>Created:</b>       2002-06-16
<b>Last Revision:</b> 2009-01-26
<b>Contents:</b>      Contains implementation of automatic 
                        C4.5 consulting object.
</pre>
*/



#ifndef _C45_AUTOCONSULTOBJECT_CPP_
#define _C45_AUTOCONSULTOBJECT_CPP_


#include "C45AutoConsultObject.h"
#include <cstdlib>
#include <cstdio>


// in cstdio you can get methods:
//            int    atoi( const char * p ) 
//            double atof( const char * p )
//            long   atol( const char * p )


// ======================================================================================= //
//                              STANDARD METHODS                                           //
// ======================================================================================= //


// Constructor
C45AutoConsultObject::C45AutoConsultObject() 
  : C45ConsultObject()
{
  m_NumberOfAttributes = MaxAtt;
  cout << "C45AutoConsultObject(Constructor): Number of Attributes is " 
       << m_NumberOfAttributes+1 << endl;

  m_NumberOfClasses    = MaxClass;
  cout << "C45AutoConsultObject(Constructor): Number of Classes is " 
       << m_NumberOfClasses+1 << endl;

  cout << "C45AutoConsultObject(Constructor): Allocating Attribute values" << endl;
  m_vNumberOfTypesPerAttribute.resize( m_NumberOfAttributes+1 );
  for ( int i = 0; i <= m_NumberOfAttributes; i++)
    {
      m_vNumberOfTypesPerAttribute[i] = MaxAttVal[i];
    }

  cout << "C45AutoConsultObject(Constructor): Allocating AutoQueryAnswer Vector" << endl;
  m_MaxClassLength = 0;
  m_vAutoQueryAnswer.resize( m_NumberOfClasses+1 );
  for ( int i = 0; i <= m_NumberOfClasses; i++)
    {
      m_vAutoQueryAnswer[i].resize( 4 ); // 0: classe, 1: CF, 2: lower bound, 3: upper bound
      if ( strlen(ClassName[i]) > m_MaxClassLength)
	m_MaxClassLength = strlen(ClassName[i]) + 2;
    }

  cout << "C45AutoConsultObject(Constructor): Length of biggest class name is " 
       << m_MaxClassLength << endl;

  cout << "C45AutoConsultObject(Constructor): Initializing of AutoConsult is done... " 
       << endl << endl;
}


// Constructor
C45AutoConsultObject::C45AutoConsultObject( char * fileName )
  : C45ConsultObject( fileName )
{
  m_NumberOfAttributes = MaxAtt;
  cout << "C45AutoConsultObject(Constructor): Number of Attributes is " 
       << m_NumberOfAttributes+1 << endl;

  m_NumberOfClasses    = MaxClass;
  cout << "C45AutoConsultObject(Constructor): Number of Classes is " 
       << m_NumberOfClasses+1 << endl;

  cout << "C45AutoConsultObject(Constructor): Allocating Attribute values" << endl;
  m_vNumberOfTypesPerAttribute.resize( m_NumberOfAttributes+1 );
  for ( int i = 0; i <= m_NumberOfAttributes; i++)
    {
      m_vNumberOfTypesPerAttribute[i] = MaxAttVal[i];
    }

  cout << "C45AutoConsultObject(Constructor): Allocating AutoQueryAnswer Vector" << endl;
  m_MaxClassLength = 0;
  m_vAutoQueryAnswer.resize( m_NumberOfClasses+1 );
  m_vAutoQueryPrologAnswer.resize( m_NumberOfClasses+1 );
  for ( int i = 0; i <= m_NumberOfClasses; i++)
    {
      m_vAutoQueryAnswer[i].resize( 4 );
      if ( strlen(ClassName[i]) > m_MaxClassLength)
	m_MaxClassLength = strlen(ClassName[i]) + 2;
    }

  cout << "C45AutoConsultObject(Constructor): Length of biggest class name is " 
       << m_MaxClassLength << endl;

  cout << "C45AutoConsultObject(Constructor): Initializing of AutoConsult is done... " 
       << endl << endl;
}


// Destructor
C45AutoConsultObject::~C45AutoConsultObject()
{

}



// ======================================================================================= //
//                                OWN  METHODS                                             //
// ======================================================================================= //


// Put all attributes in a vector.
vector<string> C45AutoConsultObject::GetAttributes( )
{
  //  cout << "C45AutoConsultObject(GetAttributes): Entering" << endl;
  vector<string> attributes( m_NumberOfAttributes + 1 );
  for (int i = 0; i <= m_NumberOfAttributes; i++ )
    {
      attributes[i] = AttName[i];
    }
  //  cout << "C45AutoConsultObject(GetAttributes): Exiting" << endl;
  return attributes;
}



// Put all classes in a vector.
vector<string> C45AutoConsultObject::GetClasses( )
{
  //  cout << "C45AutoConsultObject(GetClasses): Entering" << endl;
  vector<string> classes( m_NumberOfClasses + 1 );
  for (int i = 0; i <= m_NumberOfClasses; i++ )
    {
      classes[i] = ClassName[i];
    }
  //  cout << "C45AutoConsultObject(GetClasses): Exiting" << endl;
  return classes;
}



// Put all known attribute values in this string vectors.
// If it is 0, it is continous data.
vector< vector<string> > C45AutoConsultObject::GetAttributeValues( )
{
  //  cout << "C45AutoConsultObject(GetAttributeValues): Entering" << endl;

  vector<vector<string> > attributeValues( m_NumberOfAttributes );
  for (int i = 0; i < m_NumberOfAttributes; i++)
    {
      attributeValues[i].resize( MaxAttVal[i] );
      for (int j = 1; j <= MaxAttVal[i]; ++j)
	{
	  // -1, cause the first is always empty in the C4.5 original
	  attributeValues[i][j-1] = AttValName[i][j];
	}
    }

  //  cout << "C45AutoConsultObject(GetAttributeValues): Exiting" << endl;
  return attributeValues;
}

// Put all known attribute values in this string vectors.
// If it is 0, it is continous data.
// The Prolog variant replaces "," by "\\," in values.
vector<string> C45AutoConsultObject::GetAttributeValuesProlog( short Att )
{
//    cout << "C45AutoConsultObject(GetAttributeValuesProlog): Entering" << endl;

  vector<string> attributeValues( m_NumberOfAttributes );
  string str;
  string searchString; 
  string replaceString;


  attributeValues.resize( MaxAttVal[Att] );
  for (int j = 1; j <= MaxAttVal[Att]; ++j)
    {
       str = AttValName[Att][j];
//       cout << "C45AutoConsultObject(GetAttributeValuesProlog): Original String: "
//            << str << endl;
       searchString = ",";
       replaceString = "COMMA";

       string::size_type pos = 0;
       while ( (pos = str.find(searchString, pos)) != string::npos )
         {
            str.replace( pos, searchString.size(), replaceString );
            pos++;
         }

       searchString = "COMMA";
       replaceString = "\\,";

       pos = 0;
       while ( (pos = str.find(searchString, pos)) != string::npos )
         {
            str.replace( pos, searchString.size(), replaceString );
            pos++;
         }
 
//       cout << "Getting value " << str << endl;     

       // -1, cause the first is always empty in the C4.5 original
       attributeValues[j-1] = str;
     }

//     cout << "C45AutoConsultObject(GetAttributeValuesProlog): Exiting" << endl;
  return attributeValues;
}

int C45AutoConsultObject::Question()
{
  return this->C45ConsultObject::Question();
}



// ======================================================================================= //
//                                AUTO QUERY METHODS                                       //
// ======================================================================================= //


// AutoQuery the DT.
vector< vector<string> > C45AutoConsultObject::Question( vector<string> values )
{
  // starting
  //  cout << "C45AutoConsultObject(Question): Entering Question the DT" << endl;
  //  cout << "C45AutoConsultObject(Question): Starting Interpreting the DT" << endl;

  // interpreting
  AutoInterpretTree( values );
  //  cout << "C45AutoConsultObject(Question): Ended Interpreting the DT" << endl;

  // output
  //  cout << "C45AutoConsultObject(Question): Output the Decision of the AutoQuery" << endl;
  //  for ( int i = 0; i < m_vAutoQueryAnswer.size(); i++ )
  //    {
      //      cout << "C45AutoConsultObject(Question) => Output Decision(" << i << "): Class      = "
      //	   << m_vAutoQueryAnswer[i][0] << endl;
      //      cout << "C45AutoConsultObject(Question) => Output Decision(" << i << "): Confidence = "
      //	   << m_vAutoQueryAnswer[i][1] << endl;
      //      cout << "C45AutoConsultObject(Question) => Output Decision(" << i << "): LowerBound = "
      //	   << m_vAutoQueryAnswer[i][2] << endl;
      //      cout << "C45AutoConsultObject(Question) => Output Decision(" << i << "): UpperBound = "
      //	   << m_vAutoQueryAnswer[i][3] << endl;
  //    }
  //  cout << "C45AutoConsultObject(Question): End of Output of AutoQuery" << endl;

  // exiting
  //  cout << "C45AutoConsultObject(Question): Exiting questioning DT, returning AutoQueryAnswer" 
  //       << endl << endl;

  // returning
  return m_vAutoQueryAnswer;
}

// AutoQuery the DT through an interprocess dialog with Prolog.
vector<string> C45AutoConsultObject::QuestionProlog( )
{
  // starting
  //  cout << "C45AutoConsultObject(QuestionProlog): Entering Question the DT" << endl;
  //  cout << "C45AutoConsultObject(QuestionProlog): Starting Interpreting the DT" << endl;

  // interpreting
  AutoInterpretTreeProlog( );
  //  cout << "C45AutoConsultObject(QuestionProlog): Ended Interpreting the DT" << endl;

  // output
  //  cout << "C45AutoConsultObject(QuestionProlog): Output the Decision of the AutoQuery" << endl;
  //  cout << "C45AutoConsultObject(QuestionProlog) => Output Class: << m_vAutoQueryPrologAnswer << endl;
  //  cout << "C45AutoConsultObject(QuestionProlog): End of Output of AutoQuery" << endl;

  // exiting
  //  cout << "C45AutoConsultObject(QuestionProlog): Exiting questioning DT, returning AutoQueryAnswer" 
  //       << endl << endl;

  // returning
  return m_vAutoQueryPrologAnswer;
}



void C45AutoConsultObject::AutoInterpretTree( vector<string> values )
{
  ClassNo c, BestClass;
  float Uncertainty=1.0;

  //  cout << "C45AutoConsultObject(AutoInterpretTree): Entering" << endl;
  AutoClear();

  /*  Find the likelihood of an item's being of each class  */
  AutoClassifyCase(DecisionTree, 1.0, values);

  /*  Find the best class and show decision made  */
  BestClass = 0;
  for ( c = 0; c <= MaxClass; ++c )
    {
      Uncertainty -= LowClassSum[c];
      if ( ClassSum[c] > ClassSum[BestClass] ) 
	{
	  BestClass = c;
	}
    }
  //  cout << "C45AutoConsultObject(AutoInterpretTree): Made my Decision!" << endl;
  AutoDecision(BestClass, ClassSum[BestClass], LowClassSum[BestClass], 
	       Uncertainty + LowClassSum[BestClass]);
  
  /*  Show the other significant classes, if more than two classes  */
  if ( MaxClass > 1 )
    {
      //    cout << "C45AutoConsultObject(AutoInterpretTree): More than one class probable!" 
      //         << endl;
      while ( true )
	{
	  ClassSum[BestClass] = 0;
	  BestClass = 0;
	  for ( c = 0; c <= MaxClass; ++c )
	    {
	      if ( ClassSum[c] > ClassSum[BestClass] ) 
		{
		  BestClass = c;
		}
	    }
	  if ( ClassSum[BestClass] < Fuzz ) 
	    {
	      break;
	    }
	  AutoDecision(BestClass, ClassSum[BestClass], LowClassSum[BestClass], 
		       Uncertainty + LowClassSum[BestClass]);
	}
    }
  
  //  cout << "C45AutoConsultObject(AutoInterpretTree): Exiting" << endl;
  return;
}

void C45AutoConsultObject::AutoInterpretTreeProlog( )
{
  ClassNo c, BestClass;
  float Uncertainty=1.0;

 //   cout << "C45AutoConsultObject(AutoInterpretTreeProlog): Entering" << endl;
  AutoClear();

  /*  Find the likelihood of an item's being of each class  */
  AutoClassifyCaseProlog(DecisionTree, 1.0);

  /*  Find the best class and show decision made  */
  BestClass = 0;
  for ( c = 0; c <= MaxClass; ++c )
    {
      Uncertainty -= LowClassSum[c];
      if ( ClassSum[c] > ClassSum[BestClass] ) 
	{
	  BestClass = c;
	}
    }
 //   cout << "C45AutoConsultObject(AutoInterpretTreeProlog): Made my Decision!" << endl;
  AutoDecisionProlog(BestClass, ClassSum[BestClass], LowClassSum[BestClass], 
	       Uncertainty + LowClassSum[BestClass]);
  
  /*  Show the other significant classes, if more than two classes  */
  if ( MaxClass > 1 )
    {
     //     cout << "C45AutoConsultObject(AutoInterpretTreeProlog): More than one class probable!" 
     //          << endl;
      while ( true )
	{
	  ClassSum[BestClass] = 0;
	  BestClass = 0;
	  for ( c = 0; c <= MaxClass; ++c )
	    {
	      if ( ClassSum[c] > ClassSum[BestClass] ) 
		{
		  BestClass = c;
		}
	    }
	  if ( ClassSum[BestClass] < Fuzz ) 
	    {
	      break;
	    }
	  AutoDecisionProlog(BestClass, ClassSum[BestClass], LowClassSum[BestClass], 
		       Uncertainty + LowClassSum[BestClass]);
	}
    }
  
 //   cout << "C45AutoConsultObject(AutoInterpretTreeProlog): Exiting" << endl;
  return;
}


// AutoClassify a case... This is the recursion
void C45AutoConsultObject::AutoClassifyCase( Tree Subtree, float Weight, vector<string> values )
{
  DiscrValue v;
  float BranchWeight;
  Attribute a;
  short s;
  ClassNo c;

  //  cout << "C45AutoConsultObject(AutoClassifyCase): Entering" << endl;

  /*  A leaf  */
  if ( ! Subtree->NodeType )
    {
      if ( Subtree->Items > 0 )
	{
	  /*  Adjust class sum of ALL classes, but adjust low class sum
	      of leaf class only  */
	  for ( c = 0; c <= MaxClass; ++c )
	    {
	      ClassSum[c] += Weight * Subtree->ClassDist[c] / Subtree->Items;
	    }
	  LowClassSum[Subtree->Leaf] +=	Weight * (1 - Subtree->Errors / Subtree->Items);
	}
      else
	{
	  ClassSum[Subtree->Leaf] += Weight;
	}
      return;
    }
 
  a = Subtree->Tested;
  AutoCheckValue(a, Subtree, values);
  
  /*  Unknown value  */
  if ( ! RangeDesc[a].Known )
    {
      for ( v = 1; v <= Subtree->Forks; ++v )
	{
	  AutoClassifyCase(Subtree->Branch[v],
			   (Weight * Subtree->Branch[v]->Items) / Subtree->Items, values);
	}
      return;
    }
  
  /*  Known value  */
  switch ( Subtree->NodeType )
    {

    case BrDiscr:  /* test of discrete attribute */
      for ( v = 1; v <= MaxAttVal[a]; ++v )
	{
	  BranchWeight = RangeDesc[a].Probability[v];
	  if ( BranchWeight > 0 )
	    {
	      AutoClassifyCase(Subtree->Branch[v], Weight * BranchWeight, values);
	    }
	}
      break;
      

    case ThreshContin:  /* test of continuous attribute */
      BranchWeight = 
	RangeDesc[a].UpperBound <= Subtree->Lower ? 1.0 :
	RangeDesc[a].LowerBound > Subtree->Upper ? 0.0 :
	RangeDesc[a].LowerBound != RangeDesc[a].UpperBound ?
	(Area(Subtree, RangeDesc[a].LowerBound) -
	 Area(Subtree, RangeDesc[a].UpperBound)) /
	(RangeDesc[a].UpperBound - RangeDesc[a].LowerBound) :
	Interpolate(Subtree, RangeDesc[a].LowerBound) ;

      if ( BranchWeight > Fuzz )
	{
	  AutoClassifyCase(Subtree->Branch[1], Weight * BranchWeight, values);
	}
      if ( BranchWeight < 1-Fuzz )
	{
	  AutoClassifyCase(Subtree->Branch[2], Weight * (1 - BranchWeight), values);
	}
      break;
      

    case BrSubset:  /* subset test on discrete attribute  */
      for ( s=1; s <= Subtree->Forks; ++s )
	{
	  BranchWeight = 0.0;
	  for ( v = 1; v <= MaxAttVal[a]; ++v )
	    {
	      if ( In(v, Subtree->Subset[s]) )  // SJ In ist ein define
		{
		  BranchWeight += RangeDesc[a].Probability[v];
		}
	    }
	  if ( BranchWeight > 0 )
	    {
	      AutoClassifyCase(Subtree->Branch[s], Weight * BranchWeight, values);
	    }
	}
      break;
    }
  //  cout << "C45AutoConsultObject(AutoClassifyCase): Exiting" << endl;
  return;
}

// AutoClassify a case... This is the recursion
void C45AutoConsultObject::AutoClassifyCaseProlog( Tree Subtree, float Weight )
{
  DiscrValue v;
  float BranchWeight;
  Attribute a;
  short s;
  ClassNo c;

//    cout << "C45AutoConsultObject(AutoClassifyCaseProlog): Entering" << endl;

  /*  A leaf  */
  if ( ! Subtree->NodeType )
    {
      if ( Subtree->Items > 0 )
	{
	  /*  Adjust class sum of ALL classes, but adjust low class sum
	      of leaf class only  */
	  for ( c = 0; c <= MaxClass; ++c )
	    {
	      ClassSum[c] += Weight * Subtree->ClassDist[c] / Subtree->Items;
	    }
	  LowClassSum[Subtree->Leaf] +=	Weight * (1 - Subtree->Errors / Subtree->Items);
	}
      else
	{
	  ClassSum[Subtree->Leaf] += Weight;
	}
      return;
    }
 
  a = Subtree->Tested;
  AutoCheckValueProlog(a, Subtree);
  
  /*  Unknown value  */
  if ( ! RangeDesc[a].Known )
    {
      for ( v = 1; v <= Subtree->Forks; ++v )
	{
	  AutoClassifyCaseProlog(Subtree->Branch[v],
			   (Weight * Subtree->Branch[v]->Items) / Subtree->Items);
	}
      return;
    }
  
  /*  Known value  */
  switch ( Subtree->NodeType )
    {

    case BrDiscr:  /* test of discrete attribute */
      for ( v = 1; v <= MaxAttVal[a]; ++v )
	{
	  BranchWeight = RangeDesc[a].Probability[v];
	  if ( BranchWeight > 0 )
	    {
	      AutoClassifyCaseProlog(Subtree->Branch[v], Weight * BranchWeight);
	    }
	}
      break;
      

    case ThreshContin:  /* test of continuous attribute */
      BranchWeight = 
	RangeDesc[a].UpperBound <= Subtree->Lower ? 1.0 :
	RangeDesc[a].LowerBound > Subtree->Upper ? 0.0 :
	RangeDesc[a].LowerBound != RangeDesc[a].UpperBound ?
	(Area(Subtree, RangeDesc[a].LowerBound) -
	 Area(Subtree, RangeDesc[a].UpperBound)) /
	(RangeDesc[a].UpperBound - RangeDesc[a].LowerBound) :
	Interpolate(Subtree, RangeDesc[a].LowerBound) ;

      if ( BranchWeight > Fuzz )
	{
	  AutoClassifyCaseProlog(Subtree->Branch[1], Weight * BranchWeight);
	}
      if ( BranchWeight < 1-Fuzz )
	{
	  AutoClassifyCaseProlog(Subtree->Branch[2], Weight * (1 - BranchWeight));
	}
      break;
      

    case BrSubset:  /* subset test on discrete attribute  */
      for ( s=1; s <= Subtree->Forks; ++s )
	{
	  BranchWeight = 0.0;
	  for ( v = 1; v <= MaxAttVal[a]; ++v )
	    {
	      if ( In(v, Subtree->Subset[s]) )  // SJ In ist ein define
		{
		  BranchWeight += RangeDesc[a].Probability[v];
		}
	    }
	  if ( BranchWeight > 0 )
	    {
	      AutoClassifyCaseProlog(Subtree->Branch[s], Weight * BranchWeight);
	    }
	}
      break;
    }
  //  cout << "C45AutoConsultObject(AutoClassifyCaseProlog): Exiting" << endl;
  return;
}



// Check whether the value is there '?', discrete, or continous
void C45AutoConsultObject::AutoCheckValue( Attribute Att, Tree T, vector<string> values )
{

  if ( RangeDesc[Att].Asked ) 
    {
      return;
    }
  RangeDesc[Att].Asked=true;

  if (    (strcmp( (char *)values[Att].c_str(), "" )  == 0)
       || (strcmp( (char *)values[Att].c_str(), "?" ) == 0)
       || (strcmp( (char *)values[Att].c_str(), " " ) == 0) )
    {
      RangeDesc[Att].Known = false;
      return;
    }
  else
    {
      RangeDesc[Att].Known = true;
      if ( MaxAttVal[Att] )
	{
	  AutoReadDiscr( Att, T, values );
	}
      else
	{
	  AutoReadContin( Att, T, values );
	}
    }
  return;
}

// Check whether the value is discrete, or continous
void C45AutoConsultObject::AutoCheckValueProlog( Attribute Att, Tree T )
{

  if ( RangeDesc[Att].Asked ) 
    {
      return;
    }
  RangeDesc[Att].Asked=true;

/** FIXME */
  RangeDesc[Att].Known = true;

  if ( MaxAttVal[Att] )
    {
       AutoReadDiscrProlog( Att, T );
    }
  else
    {
       AutoReadContinProlog( Att, T );
    }
    
  return;
}



// Autoread the discrete values out of the vector
void C45AutoConsultObject::AutoReadDiscr(Attribute Att, Tree T, vector<string> values)
{
    DiscrValue dv, PNo = 0;
    float P, PSum = 0.0;

    char * value = (char *)values[Att].c_str();
    char partOfString[m_MaxClassLength];
    int i = 0;
    int n = 0;
    P = 1.0; 

    for ( dv = 1; dv <= MaxAttVal[Att]; ++dv )
      {
	RangeDesc[Att].Probability[dv] = 0.0;
      }
    
    // Here follows parsing... This one is really dirty...
    while ( value[i] != '\0' )
      {
	n = 0;
	while ( Space(value[i]) ) ++i;
	// Attribute
	// ist kein Space mehr da, d.h. kopieren, bis : oder \n
	while ( value[i] != '\0' && value[i] != ':' )
	  {
	    partOfString[n] = value[i];
	    ++i;
	    ++n;
	  }
	partOfString[n] = '\0';   // Attribute geschrieben
//	cout << " Found That: " << partOfString << endl;
 	dv = Which( partOfString, AttValName[Att], 1, MaxAttVal[Att] );
 	if ( !dv )
 	  {
 	    cout << "C45AutoConsultObject(AutoReadDiscr): Cannot find attribute in the" 
 		 << " attributelist while scanning for attribute!" << endl;
 	    exit(0);
 	  }

	// W'keiten
	n = 0;
	while ( Space(value[i]) ) ++i;
	if ( (value[i] != ':') && (P == 1.0) )
	  {
	    RangeDesc[Att].Probability[dv] = P;	  
	    break;
	  }
	else if ( (value[i] != ':' ) && (P != 1.0 ) )
	  {
	    cout << "C45AutoConsultObject(AutoReadDiscr): Error in parsing..." << endl;
	    exit(0);
	  }
	else if ( value[i] == ':' )
	  {
	    i++;
	    while ( Space(value[i]) ) ++i;
	    while ( value[i] != '\0' && value[i] != ',' )
	      {
		partOfString[n] = value[i];
		++i;
		++n;
	      }
	    if ( value[i] == ',' ) ++i;
	    if ( n == 0 )
	      {
		cout << "C45AutoConsultObject(AutoReadDiscr): Error in parsing..." << endl;
		exit(0);
	      }
	    partOfString[n] = '\0';   // Attribute geschrieben
	    P = atof( partOfString );
	  }
	else
	  {
	    cout << "There was no : where I waited for one..." << endl;
	    exit(0);
	  }
//	cout << " and Probability " << P << endl;
	RangeDesc[Att].Probability[dv] = P;
      }

    /*  Check that sum of probabilities is not > 1  */
    PNo = MaxAttVal[Att];
    PSum = 1.0;
    for ( dv = 1; dv <= MaxAttVal[Att]; ++dv)
      {
	if ( RangeDesc[Att].Probability[dv] > Fuzz )
	  {
	    PSum -= RangeDesc[Att].Probability[dv];
	    PNo--;
	  }
      }
    
    // SJ previous was:  if ( PSum < 0 || ! PNo && PSum > Fuzz )
    if ( (PSum < 0) || ((!PNo) && (PSum > Fuzz)) )
      {
	printf("SJ ERROR USERINT.C:: ReadDiscr: Probability values must sum to 1\n");
	exit(0);
      }
    
    /*  Distribute the remaining probability equally among
	the unspecified attribute values  */
    PSum /= PNo;
    for ( dv = 1; dv <= MaxAttVal[Att]; ++dv)
      {
	if ( RangeDesc[Att].Probability[dv] < Fuzz )
	  {
	    RangeDesc[Att].Probability[dv] = PSum;
	  }
      }
    return;

}

int C45AutoConsultObject::CppWhich( string Val, short Att, short First, short Last)
{
    short n=First;
    vector<string> AttributeValueList;
//    cout << "C45AutoConsultObject(CppWhich): this->GetAttributeValuesProlog( " << Att << " )" << endl;
//    cout << "C45AutoConsultObject(CppWhich): Val = " << Val << ", First = " << First << ", Last = " << Last << endl;
    AttributeValueList = this->GetAttributeValuesProlog( Att );
 
//         cout << "n is " << n << ", Val is " << Val << ", AttributeValueList[n] is "
//         << AttributeValueList[n] << endl;
    while ( (n <= Last) && Val.compare(AttributeValueList[n]) )
      {
//         cout << "Comparing " << Val << " with " << AttributeValueList[n] << endl;
         n++;
      }

    return ( (n <= Last) ? n + 1 : First );
}


// Autoread the discrete values by asking the Prolog program
void C45AutoConsultObject::AutoReadDiscrProlog(Attribute Att, Tree T)
{
    // Note, that this is a very simplistic version. We don't take
    // into account any probability values like C4.5 usually does.
    DiscrValue dv;
    char * value;
    
    for ( dv = 1; dv <= MaxAttVal[Att]; ++dv )
      {
	RangeDesc[Att].Probability[dv] = 0.0;
      }

    cout << "#### Here comes the attribute name ####" << endl;
    cout << AttName[Att] << endl;
    string valueString;
    getline(cin, valueString);
//        cin >> valueString;
    cout << "[C4.5] Prolog says the value of " << AttName[Att] << " is: "
         << valueString << endl;
    cout << "--------" << endl << endl;
//    cout << "[C4.5] The Attribute " << Att << " can take " << MaxAttVal[Att] << " values." << endl;

    value = (char *)valueString.c_str();

    dv = this->CppWhich( value, Att, 0, MaxAttVal[Att]-1 );
    if ( !dv )
 	  {
 	    cout << "#### Error ####" << endl;
 	    cout << "C45AutoConsultObject(AutoReadDiscr): Cannot find attribute " << valueString
                 << " in the attributelist while scanning for attribute!" << endl;
 	    cout << "Permissible values are ";
            vector<string> AttributeValueList = this->GetAttributeValuesProlog( Att );
            for( int k = 0; k < MaxAttVal[Att]-1; k++ )
 	       cout << AttributeValueList[k] << ", ";
 	    cout << AttributeValueList[(MaxAttVal[Att]-1)] << endl;
 	    exit(0);
 	  }
    else
    {
     // We are sure that the attribute value is correct with P = 1.0.
     RangeDesc[Att].Probability[dv] = 1.0;
    }

    return;
}



// Auto Read Continous data out of the vector.
void C45AutoConsultObject::AutoReadContin( Attribute Att, Tree T, vector<string> values )
{
  char * value = (char *)values[Att].c_str();
  char * pointerToMinus;
  pointerToMinus = strchr(value,'-');
  if ( pointerToMinus == NULL )       // there is no '-' in it...
    {
      RangeDesc[Att].UpperBound = atof( value );
      RangeDesc[Att].LowerBound = atof( value );
    }
  else                                // there is a '-'
    {
      RangeDesc[Att].LowerBound = atof( value );
      RangeDesc[Att].UpperBound = atof( pointerToMinus+1 );
    }
  if ( RangeDesc[Att].LowerBound > RangeDesc[Att].UpperBound )
    {
      cout << "C45AutoConsultObject(AutoReadContin): Reading Continous Range failed."
	   << "Upperbound < Lowerbound!" << endl;
      exit(0);
    }
  return;
}

// Auto Read Continous data by asking the Prolog program.
void C45AutoConsultObject::AutoReadContinProlog( Attribute Att, Tree T )
{
    char * value;

    cout << "#### Here comes the attribute name ####" << endl;
    cout << AttName[Att] << endl;
    string valueString;
    getline(cin, valueString);
//        cin >> valueString;
    cout << "[C4.5] Prolog says the value of " << AttName[Att] << " is: "
         << valueString << endl;
    cout << "--------" << endl << endl;

    value = (char *)valueString.c_str();
 
   return;
}



// has to be done before the next question!
void C45AutoConsultObject::AutoClear()
{
  for ( Attribute a = 0; a <= MaxAtt; ++a )
    {
      RangeDesc[a].Asked = false;
      RangeDesc[a].Known = false;
    }
  for ( ClassNo c = 0; c <= MaxClass; ++c )
    {
      LowClassSum[c] = 0;
      ClassSum[c] = 0;
    }
  for ( int i = 0; i <= m_NumberOfClasses; i++)
    {
      m_vAutoQueryAnswer[i][0] = "";
      m_vAutoQueryAnswer[i][1] = "";
      m_vAutoQueryAnswer[i][2] = "";
      m_vAutoQueryAnswer[i][3] = "";
    }
  m_AutoQueryAnswerCount = 0;

  return;
}

// has to be done before the next question!
void C45AutoConsultObject::AutoClearProlog()
{
  for ( Attribute a = 0; a <= MaxAtt; ++a )
    {
      RangeDesc[a].Asked = false;
      RangeDesc[a].Known = false;
    }
  for ( ClassNo c = 0; c <= MaxClass; ++c )
    {
      LowClassSum[c] = 0;
      ClassSum[c] = 0;
    }
  for ( int i = 0; i <= m_NumberOfClasses; i++)
    {
      m_vAutoQueryPrologAnswer[0] = "";
    }

  return;
}

// prints the chosen decision
// and puts it in the AutoQueryAnswer String
void C45AutoConsultObject::AutoDecision( ClassNo c, float p, float lb, float ub)
{
  char puffer[10];
  //  cout << "C45AutoConsultObject(AutoDecision): Making decision" << endl;
  m_vAutoQueryAnswer[m_AutoQueryAnswerCount][0] = ClassName[c];
  //  cout << "C45AutoConsultObject(AutoDecision): Classname = " 
  //       << m_vAutoQueryAnswer[m_AutoQueryAnswerCount][0] << endl;

  if ( (p < 1-Fuzz) || (lb < ub - Fuzz) )
    {
      gcvt(p,3,puffer);  
      m_vAutoQueryAnswer[m_AutoQueryAnswerCount][1] = puffer;
      if ( lb < ub - Fuzz ) {
	gcvt(lb,3,puffer);  
	m_vAutoQueryAnswer[m_AutoQueryAnswerCount][2] = puffer;
	gcvt(ub,3,puffer);  
	m_vAutoQueryAnswer[m_AutoQueryAnswerCount][3] = puffer;
      }
    }
  m_AutoQueryAnswerCount++;
  //  cout << "C45AutoConsultObject(AutoDecision): Decision written and done" << endl;
  return;
}

// prints the chosen decision
// and puts it in the AutoQueryAnswer String
void C45AutoConsultObject::AutoDecisionProlog( ClassNo c, float p, float lb, float ub)
{
  char puffer[10];
//    cout << "C45AutoConsultObject(AutoDecisionProlog): Making decision" << endl;
//    cout << "C45AutoConsultObject(AutoDecisionProlog): ClassName[c] = " << ClassName[c] << endl;
  
  m_vAutoQueryPrologAnswer[0] = ClassName[c];
//    cout << "C45AutoConsultObject(AutoDecisionProlog): Classname = " 
//         << m_vAutoQueryPrologAnswer[0] << endl;

/*  if ( (p < 1-Fuzz) || (lb < ub - Fuzz) )
    {
      gcvt(p,3,puffer);  
      m_vAutoQueryPrologAnswer[1] = puffer;
      if ( lb < ub - Fuzz ) {
	gcvt(lb,3,puffer);  
	m_vAutoQueryPrologAnswer[2] = puffer;
	gcvt(ub,3,puffer);  
	m_vAutoQueryPrologAnswer[3] = puffer;
      }
    }
*/
/*  if ( p < 1-Fuzz || lb < ub - Fuzz )
    {
      cout << "  CF = " << p;
      if ( lb < ub - Fuzz )
      {
          cout << "  [ " << lb << " - " << ub << " ]" << endl;
      }
  }
*/
  //  cout << "C45AutoConsultObject(AutoDecisionProlog): Decision written and done" << endl;
  return;
}



#endif
