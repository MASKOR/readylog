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

/*! \file C45ConsultObjectTest.cpp
<pre>
<b>File:</b>          C45ConsultObjectTest.cpp
<b>Project:</b>       AllemaniACs Soccer Simulation League
<b>Authors:</b>       Stefan Jacobs <Stefan_J@gmx.de>
<b>Created:</b>       2002-06-16
<b>Last Revision:</b> 2002-06-20
<b>Contents:</b>      Contains an user based C4.5 consulting
                        object.
</pre>
*/


#include <iostream>
#include <ctime>
#include "C45ConsultObject.h"
#include "C45AutoConsultObject.h"


int main()
{
  cout << "Main: Starting to construct" << endl << endl;

  //C45AutoConsultObject * consult = new C45AutoConsultObject( "../exampledata/soybean" );
  C45AutoConsultObject * consult = new C45AutoConsultObject( "../exampledata/crx" );

  vector<string> attributes = consult->GetAttributes( );
  vector<string> values ( attributes.size() );
  vector<string> classes = consult->GetClasses( );
  vector< vector<string> > attribute_values = consult->GetAttributeValues( );

  // Wenn das Beispiel == crx ist, gibt folgendes: +  CF = 1.00
  for ( int i = 0; i < attributes.size(); i++ )
    {
      if ( strcmp( (char *) attributes[i].c_str(), "A9" ) == 0 )
 	values[i] = "t";
      else if ( strcmp( (char *) attributes[i].c_str(), "A15" ) == 0 )
 	values[i] = "14";
      else if ( strcmp( (char *) attributes[i].c_str(), "A11" ) == 0 )
 	values[i] = "3-4";
      else if ( strcmp( (char *) attributes[i].c_str(), "A5" ) == 0 )
	values[i] = "g:0.12, p:0.3";
      else
	values[i] = "?";
      cout << "Main: Setting attribute " << attributes[i] << " to value " << values[i] << endl;
    }
  cout << "Main: Asking a classification" << endl << endl;

  vector< vector<string> > puffer;

  time_t t1 = time(0);
  for ( int i = 0; i < 10000; ++i )
    puffer = consult->Question( values );
  time_t t2 = time(0);

  cout << "Main: 10000 Fragen dauerten: " << difftime( t2, t1 ) 
       << " Zeit, gemessen mit time(0) und timediff...." << endl;


  //  int i = consult->Question( );
  delete consult;

  for ( int i = 0; i < puffer.size(); i++ )
    {
      for ( int j = 0; j < puffer[i].size(); j++ )
	{
	  cout << puffer[i][j] << " ";
	}
      cout << endl;
    }

  cout << "Main: Deleted. Bailing out...." << endl << endl;
  return 0;
}
