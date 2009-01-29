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

/*! \file C45ConsultObject.cpp
<pre>
<b>File:</b>          C45ConsultObject.cpp
<b>Project:</b>       AllemaniACs Soccer Simulation League
<b>Authors:</b>       Stefan Jacobs <Stefan_J@gmx.de>
<b>Created:</b>       2002-06-16
<b>Last Revision:</b> 2002-06-20
<b>Contents:</b>      Contains an user based C4.5 consulting
                        object.
</pre>
*/

#ifndef _C45_CONSULTOBJECT_CPP_
#define _C45_CONSULTOBJECT_CPP_

#include <sys/types.h>
#include <sys/stat.h>

#include "C45ConsultObject.h"
#include "C45ConsultObjectGetNames.cpp"
#include "C45ConsultObjectTrees.cpp"
#include "C45ConsultObjectConsultation.cpp"
#include "C45ConsultObjectUserint.cpp"


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//
// STANDARD METHODS                                                                  //
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//


C45ConsultObject::C45ConsultObject()
{
  cout << "C45ConsultObject(Constructor): Starting to construct myself" << endl;
  VERBOSITY = 0;
  TRACE = 0;
  TRf = 0;

  //char * fileName;
  cout << "C45ConsultObject(Constructor): Which file to consult? ";
  cin >> m_pC45ConsultFileName;
  FileName = (char *)m_pC45ConsultFileName.c_str();
  LoadConsultFile();
  cout << "C45ConsultObject(Constructor): Constructing done" << endl << endl;
}


C45ConsultObject::C45ConsultObject( char * fileName )
{
  cout << "C45ConsultObject(Constructor): Starting to construct myself" << endl;
  VERBOSITY = 0;
  TRACE = 0;
  TRf = 0;

  m_pC45ConsultFileName = fileName;
  FileName = (char *)m_pC45ConsultFileName.c_str();
  LoadConsultFile();
  cout << "C45ConsultObject(Constructor): Constructing done" << endl << endl;
}


C45ConsultObject::~C45ConsultObject()
{
  cout << endl;
  cout << "C45ConsultObject(Destructor): =======================>" << endl;
  cout << "C45ConsultObject(Destructor): Destructed myself" << endl;
  cout << "C45ConsultObject(Destructor): This is my last message!" << endl << endl;
}


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//
// PUBLIC METHODS                                                                    //
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//

// Query the DT by user input
int C45ConsultObject::Question()
{
  cout << "C45ConsultObject(Question): Going to query the decision tree by user" << endl;

  InterpretTree();

  cout << "C45ConsultObject(Question): UserQuerying done....." << endl << endl;
  return 0;
}


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//
// PRIVATE METHODS                                                                   //
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//

int C45ConsultObject::LoadConsultFile()
{
  cout << "C45ConsultObject(LoadConsultFile): Going to open File " 
       << m_pC45ConsultFileName << endl;

  cout << "C45ConsultObject(LoadConsultFile): Going to call GetNames routine" << endl;
  GetNames();
  cout << "C45ConsultObject(LoadConsultFile): Exited the GetNames routine" << endl;

  cout << "C45ConsultObject(LoadConsultFile): Going to call GetTree routine" << endl;
  DecisionTree = GetTree(".unpruned");
  cout << "C45ConsultObject(LoadConsultFile): Exited the GetTree routine" << endl;

  cout << "C45ConsultObject(LoadConsultFile): Going to allocate value ranges" << endl;
  RangeDesc = (struct ValRange *) calloc(MaxAtt+1, sizeof(struct ValRange));
  Attribute a;
  ForEach(a, 0, MaxAtt)
    {
      if ( MaxAttVal[a] )
	{
	  RangeDesc[a].Probability =
	    (float *) calloc(MaxAttVal[a]+1, sizeof(float));
	}
    }
  ClassSum = (float *) malloc((MaxClass+1) * sizeof(float));
  LowClassSum = (float *) malloc((MaxClass+1) * sizeof(float));
  cout << "C45ConsultObject(LoadConsultFile): Allocating value ranges done" << endl;
  cout << "C45ConsultObject(LoadConsultFile): Loading data done" << endl;
  return 0;
}


#endif
