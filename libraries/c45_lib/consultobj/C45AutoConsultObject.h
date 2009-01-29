//*****************************************************************//
// Created by Stefan Jacobs <Stefan_J@gmx.de>                      //
//                                                                 //
// Created:         2002-06-16                                     //
// Last Modified:   2009-01-26 by Dennis Pannhausen                //
//                             <Dennis.Pannhausen@rwth-aachen.de>  //
//                                                                 //
//*****************************************************************//
// This are modified methods by the original C4.5 code. Sometimes  //
//  even I don't know for what they are, and how they run. This    //
//  code was done, to check, if the DT idea works and if the       //
//  methods are working and fast enough.                           //
// This code can be made more efficient, by returning not only     //
//  strings, but structs, and so on.......                         //
//*****************************************************************//

/*! \file C45AutoConsultObject.h
<pre>
<b>File:</b>          C45AutoConsultObject.h
<b>Project:</b>       AllemaniACs Soccer Simulation League
<b>Authors:</b>       Stefan Jacobs <Stefan_J@gmx.de>
<b>Created:</b>       2002-06-16
<b>Last Revision:</b> 2002-06-20
<b>Contents:</b>      Contains an automatic C4.5 consulting
                        object.
</pre>
*/



#ifndef _C45_AUTOCONSULTOBJECT_H_
#define _C45_AUTOCONSULTOBJECT_H_


#include "C45ConsultObject.h"
#include <vector>
/** For executing ECLiPSe predicates */
#include "../../../../eclipse/include/i386_linux/eclipseclass.h"

/** This is my C4.5 Auto Consult Object.
 *  This class is derived from the C4.5ConsultObject,
 *    which is based on userinput. This userinput is
 *    simulated in this class by a vector of string.
 */
class C45AutoConsultObject : public C45ConsultObject
{
 public:

  /** Constructor, load DT by user input
   */
  C45AutoConsultObject();


  /** Constructor, load DT by filename
   */
  C45AutoConsultObject( char * fileName );


  /** Destructor.
   *  Destruct all member variables...
   *  Nothing to be done here, yet.
   */
  ~C45AutoConsultObject();


  /** AutoQuery the decision Tree.
   *  @param Vector of attribute values:
   *         This string has to be of the length of the number
   *         of attributes. The Question method awaits, that in
   *         value[i] the values for attribute i are standing!
   *         A string in this vector can be the following:
   *           continous attributes: 
   *               "j"  \in IR,
   *               "j-k" with j,k \in IR, and j < k
   *           discrete attributes:
   *               "a"  \in AttributeValues,
   *               "a:i" with a \in AttributeValues, i \in IR,
   *                     representing the probability of a.
   *               "a:i, b:j" and so on for more attributevalues.
   *           unknown value for an attribute:
   *               "?"  represents unknow.
   *       
   *         space, newline and so on represent also unknown values,
   *          but for debugging its easier, to initialize all with "?"
   *
   *  @return a vector[classes] of vector[4] of strings:
   *          vector[i][0] is the class, given back.
   *          vector[i][1] is the confidence.
   *          vector[i][2] is the lower confidence interval bound.
   *          vector[i][3] is the upper confidence interval bound.
   */ 
  vector< vector<string> > Question( vector<string> values );

  /** AutoQuery the decision Tree on a single case by asking
   *  a Prolog program (only) for the necessary attribute values.
   *  @return a string representing the classification of
   *  the example
   */ 
  vector<string> QuestionProlog( );


  /** Is a link to the Question routine of the base class.
   *  This method will ask the user to put in the desired
   *  attribute values.
   */
  int Question ();


  /** GetAttributes.
   *  @doc This will return an array of attributes.
   *  @return Is a vector of strings containing all information about
   *           the attributes.
   */
  vector<string> GetAttributes( );

  /** GetClasses.
   *  @doc This will return an array of classes.
   *  @return Is a vector of strings containing all information about
   *           the classes.
   */
  vector<string> GetClasses( );

  /** GetAttributeValues.
   *  @doc This will return a vector of vectors of attribute values.
   *  @return Is a vector of vectors of strings. If a vector is empty,
   *           this is a special attribute and has not limited, discrete
   *           values.
   */
  vector< vector<string> > GetAttributeValues( );

  /** GetAttributeValuesProlog.
   *  @doc This will return a vector of attribute values.
   *  @return Is a vector of vectors of strings. If a vector is empty,
   *           this is a special attribute and has not limited, discrete
   *           values. The routine replaces "," in values by "\\,".
   */
  vector<string> GetAttributeValuesProlog( short Att );


 private:

  //                           METHODS                               //
  // =============================================================== //
  // This are modified methods by the original C4.5 code. Sometimes  //
  //  even I don't know for what they are, and how they run.
  //


  /** This method clears all relevant data and
   *   is invoked each time by the Question/QuestionProlog method.
   */
  void AutoClear();


  /** This Interprets the tree and is invoked
   *   by the Question Method.
   */
  void AutoInterpretTree( vector<string> values );

  /** This Interprets the tree and is invoked
   *   by the QuestionProlog Method.
   */
  void AutoInterpretTreeProlog( );


  /** This is the recursive method, to iterate the tree.
   *   It is invoked by the AutoInterpretTree method.
   */
  void AutoClassifyCase( Tree Subtree, float Weight, 
			 vector<string> values );

  /** This is the recursive method, to iterate the tree.
   *   It is invoked by the AutoInterpretTreeProlog method.
   */
  void AutoClassifyCaseProlog( Tree Subtree, float Weight );

  
  /** Reads out of values the attributes Att value.
   */
  void AutoReadContin  ( Attribute Att, Tree T, vector<string> values );

  /** Reads out (from Prolog input) the attributes Att value.
   */
  void AutoReadContinProlog  ( Attribute Att, Tree T );


  /** Reads out of values the attributes Att value.
   */
  void AutoReadDiscr   ( Attribute Att, Tree T, vector<string> values );

  /** Finds out the position of Val in List[]
   */
  int CppWhich( string Val, short Att, short First, short Last);

  /** Reads out (from Prolog input) the attributes Att value.
   */
  void AutoReadDiscrProlog   ( Attribute Att, Tree T );


  /** Checks, if the value is known, and distinguishes 
   *    between discrete and continous data and invokes
   *    the appopriate methods.
   */
  void AutoCheckValue  ( Attribute Att, Tree T, vector<string> values );

  /** Checks, if the value is known, and distinguishes 
   *    between discrete and continous data and invokes
   *    the appopriate methods.
   */
  void AutoCheckValueProlog  ( Attribute Att, Tree T );


  /** Is the last routine in Question, because it
   *    writes the answers of the C4.5 AutoQuestion
   *    the the AutoanswerQuery String.
   */
  void AutoDecision( ClassNo c, float p, float lb, float ub );

  /** Is the last routine in Question, because it
   *    writes the answers of the C4.5 AutoQuestion
   *    the the AutoanswerQuery String.
   */
  void AutoDecisionProlog( ClassNo c, float p, float lb, float ub );


  //                          VARIABLES                              //
  // =============================================================== //


  /// overall number of attributes
  int   m_NumberOfAttributes;
  
  /// overall number of classes
  int   m_NumberOfClasses;

  /// counting the number of answerd
  int   m_AutoQueryAnswerCount;

  /// is the maximum length of a class.
  unsigned int   m_MaxClassLength;

  /** Is the number of types per attribute.
   *  For continous data it is 0.
   */
  vector<int>  m_vNumberOfTypesPerAttribute;

  /** Is the vector for the autoqueryanswer,
   *   described above.
   */
  vector< vector<string> >   m_vAutoQueryAnswer;

  /** Is the classification of querying the
   *  Prolog program for a single case.
   */
  vector<string> m_vAutoQueryPrologAnswer;


};



#endif
