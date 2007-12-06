/* ***************************************************************************
 *  ,=³ .%%% %%%%%%%. .%%%%%.  .%%%%%.   %%%%%.%%. %. %%%%%%%% %% Rheinisch-
 * [==].%%%   %%   %% %%   %%  %%   %%   %%  %%'%%.%%.%% %% %% %% Westfälische
 *  //l%%%    %%   %% %%%%. ' %%       @ %%%%%' '%%%%%%% %% %%%%% Technische
 * [==]%%     %%|=%%   %%=>%  %%         %%  %%  '%%'%%% %% %% %% Hochschule
 * [==]%%%    %%   %% . '%%%% %%  '%%%   %%   %%  '% '%% %% %% %% Aachen
 * [==]'%%%   %%   %% %%   %%  %%   %%   http://kbsg.rwth-aachen.de/
 * o^^o '%%% %%%%%%%' '%%%%%'O '%%%%%'   Knowledge Based Systems Group
 * ***************************************************************************
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; version 2 of the License.
 *
 * ***************************************************************************
 *
 *           $Id$
 *        author: Alexander Ferrein <ferrein@cs.rwth-aachen.de>
 *        mod by: Stefan Schiffer <schiffer@cs.rwth-aachen.de>
 *   description: external interface to draw gridworlds (maze etc)
 *
 * ************************************************************************ */

//#include "getkey.h"
#include <eclipseclass.h>
#include <iostream>
#include <stdlib.h>
#include "display.h"

void CalculateOptPolicy();

// init external memory elements

std::vector<wall_t>   Walls;
wall_t WallPiece;

std::vector<value_t>  Values;
std::vector<value_t>  OptPolicy;
std::vector<action_t> Actions;

using std::cout;
using std::cerr;
using std::endl;

bool winited = false;

extern "C"
int p_StartDisplay()
{
  
  EC_word Arg1 = EC_word( EC_arg( 1 ) );
  EC_word Arg2 = EC_word( EC_arg( 2 ) );
  EC_word Arg3 = EC_word( EC_arg( 3 ) );

  long GridX, GridY;

  if ( Arg1.is_long( &GridX ) != EC_succeed) {
    cerr << "In p_StartDisplay(): Failed to get Arg 1 " << endl;
    return EC_fail;
  };
  
  if ( Arg2.is_long( &GridY ) != EC_succeed) {
    cerr << "In p_StartDisplay(): Failed to get Arg 2 " << endl;
    return EC_fail;
  };


  std::vector<int> list_elements;

  EC_word list = Arg3;
  EC_word sub_list, element;
  while(EC_succeed == list.arg(1, sub_list)) {

    while (EC_succeed == sub_list.arg(1, element)) {
      long elem = 0;
      //if (element.is_nil()) {
      //	cerr << " element is NIL!" << endl;
      //}	
      if (element.is_long(&elem) != EC_succeed) {
	cerr << "error in type in elem: '" << elem << "'" << endl;
	return EC_fail;
      }
      //cout << "got elem: '" << elem << "'" << endl;
      list_elements.push_back(elem);
      sub_list.arg(2,sub_list);
    }
    WallPiece.x1=list_elements[0];
    WallPiece.y1=list_elements[1];
    WallPiece.x2=list_elements[2];
    WallPiece.y2=list_elements[3];
    list_elements.clear();
    Walls.push_back(WallPiece);
    list.arg(2,list);
  }

  start_display(GridX, GridY);

  winited = true;

  return EC_succeed;
}

extern "C"
int p_DrawAction() 
{
  if( !winited ) {
    //cerr << " ***** ERROR ***** window not initialized yet, can't draw" << endl;
    return EC_succeed;
  }    

  EC_word ec_action = EC_word( EC_arg( 1 ) );
  EC_word ec_X = EC_word( EC_arg( 2 ) );
  EC_word ec_Y = EC_word( EC_arg( 3 ) );

  char * strAction;
  if ( ec_action.is_string( &strAction ) != EC_succeed) {
    cerr << "In p_StartDisplay(): Failed to get Arg 2 " << endl;
    return EC_fail;
  };

  long X;
  if ( ec_X.is_long( &X ) != EC_succeed) {
    cerr << "In p_StartDisplay(): Failed to get Arg 2 " << endl;
    return EC_fail;
  };


  long Y;
  if ( ec_Y.is_long( &Y ) != EC_succeed) {
    cerr << "In p_StartDisplay(): Failed to get Arg 2 " << endl;
    return EC_fail;
  };

  // to save action
  action_t a;

  switch(strAction[0]) {
  case 'R' :
    draw_dir(EAST, X, Y);
    // save action for redraw:
    a.X = X; a.Y = Y; a.action = EAST;
    Actions.push_back( a );
    break;
  case 'L' :
    draw_dir(WEST, X, Y);
    // save action for redraw:
    a.X = X; a.Y = Y; a.action = WEST;
    Actions.push_back( a );
    break;
  case 'U' :
    draw_dir(NORTH, X, Y);
    // save action for redraw:
    a.X = X; a.Y = Y; a.action = NORTH;
    Actions.push_back( a );
    break;
  case 'D' :
    draw_dir(SOUTH, X, Y);
    // save action for redraw:
    a.X = X; a.Y = Y; a.action = SOUTH;
    Actions.push_back( a );
    break;
  case '0' :
    draw_dir(REST, X, Y);
    // save action for redraw:
    a.X = X; a.Y = Y; a.action = REST;
    Actions.push_back( a );
    break;
  };
  return EC_succeed;
}

extern "C"
int p_DrawPolicy()
{
  if( !winited ) {
    //cerr << " ***** ERROR ***** window not initialized yet, can't draw" << endl;
    return EC_succeed;
  }    

  EC_word ec_action = EC_word( EC_arg( 1 ) );
  EC_word ec_X = EC_word( EC_arg( 2 ) );
  EC_word ec_Y = EC_word( EC_arg( 3 ) );

  char * strAction;
  if ( ec_action.is_string( &strAction ) != EC_succeed) {
    cerr << "In p_StartDisplay(): Failed to get Arg 2 " << endl;
    return EC_fail;
  };

  long X;
  if ( ec_X.is_long( &X ) != EC_succeed) {
    cerr << "In p_StartDisplay(): Failed to get Arg 2 " << endl;
    return EC_fail;
  };


  long Y;
  if ( ec_Y.is_long( &Y ) != EC_succeed) {
    cerr << "In p_StartDisplay(): Failed to get Arg 2 " << endl;
    return EC_fail;
  };


  switch(strAction[0]) {
  case 'R' :
    draw_dir(EAST, X, Y);
    break;
  case 'L' :
    draw_dir(WEST, X, Y);
    break;
  case 'U' :
    draw_dir(NORTH, X, Y);
    break;
  case 'D' :
    draw_dir(SOUTH, X, Y);
    break;
  };
  return EC_succeed;
}


extern "C"
int p_DrawAgent()
{
  if( !winited ) {
    //cerr << " ***** ERROR ***** window not initialized yet, can't draw" << endl;
    return EC_succeed;
  }    

  EC_word ec_X = EC_word( EC_arg( 1 ) );
  EC_word ec_Y = EC_word( EC_arg( 2 ) );

  long X,Y;
  if( ec_X.is_long( &X ) != EC_succeed ) {
    cerr << "failed to unify arg 1 in p_DrawAgent " << endl;
    return EC_fail;
  }
  if( ec_Y.is_long( &Y ) != EC_succeed ) {
    cerr << "failed to unify arg 2 in p_DrawAgent " << endl;
    return EC_fail;
  }

  draw_agent(X, Y);

  return EC_succeed;
}

extern "C"
int p_DrawItem()
{
  if( !winited ) {
    //cerr << " ***** ERROR ***** window not initialized yet, can't draw" << endl;
    return EC_succeed;
  }    

  EC_word ec_X = EC_word( EC_arg( 1 ) );
  EC_word ec_Y = EC_word( EC_arg( 2 ) );

  long X,Y;
  if( ec_X.is_long( &X ) != EC_succeed ) {
    cerr << "failed to unify arg 1 in p_DrawItem " << endl;
    return EC_fail;
  }
  if( ec_Y.is_long( &Y ) != EC_succeed ) {
    cerr << "failed to unify arg 2 in p_DrawItem " << endl;
    return EC_fail;
  }

  draw_item(X, Y);

  return EC_succeed;
  
}

extern "C"
int p_DrawGoal()
{
  if( !winited ) {
    //cerr << " ***** ERROR ***** window not initialized yet, can't draw" << endl;
    return EC_succeed;
  }    

  EC_word ec_X = EC_word( EC_arg( 1 ) );
  EC_word ec_Y = EC_word( EC_arg( 2 ) );

  long X,Y;
  if( ec_X.is_long( &X ) != EC_succeed ) {
    cerr << "failed to unify arg 1 in p_DrawGoal " << endl;
    return EC_fail;
  }
  if( ec_Y.is_long( &Y ) != EC_succeed ) {
    cerr << "failed to unify arg 2 in p_DrawGoal " << endl;
    return EC_fail;
  }

  draw_goal(X, Y);

  return EC_succeed;
  
}

extern "C"
int p_DrawStart()
{
  if( !winited ) {
    //cerr << " ***** ERROR ***** window not initialized yet, can't draw" << endl;
    return EC_succeed;
  }    

  EC_word ec_X = EC_word( EC_arg( 1 ) );
  EC_word ec_Y = EC_word( EC_arg( 2 ) );

  long X,Y;
  if( ec_X.is_long( &X ) != EC_succeed ) {
    cerr << "failed to unify arg 1 in p_DrawGoal " << endl;
    return EC_fail;
  }
  if( ec_X.is_long( &Y ) != EC_succeed ) {
    cerr << "failed to unify arg 2 in p_DrawGoal " << endl;
    return EC_fail;
  }

  draw_start(X, Y);

  return EC_succeed;
  
}


typedef struct {
  int i,j;
} tuple_t;


void
CalculateOptPolicy() 
{

}


extern "C"
int p_DrawValues()
{
  if( !winited ) {
    //cerr << " ***** ERROR ***** window not initialized yet, can't draw" << endl;
    return EC_succeed;
  }    

  EC_word ec_Arg1 = EC_word( EC_arg( 1 ) );

  value_t value;

  EC_word ec_value_list, ec_value, ec_pos_list, ec_pos, ec_action, ec_val, ec_prob;
  while(EC_succeed == ec_Arg1.arg(1, ec_value_list)) {
    
    // accessing the different sub-lists with value entries
    while (EC_succeed == ec_value_list.arg(1, ec_value)) {
      
      // now parsing different elements;


      // [[X, Y]]; the X
      if (EC_succeed == ec_value.arg(1, ec_pos_list)) {

	// the X
	if (EC_succeed == ec_pos_list.arg(1, ec_pos)) {
	  long l_pos;
	  if (ec_pos.is_long(&l_pos) != EC_succeed) {
	    cerr << "error in type " << endl;
	    return EC_fail;
	  }
	  value.X = l_pos;
	  }
	// next list entry }
	
	ec_pos_list.arg(2, ec_pos_list);
	
	if (EC_succeed == ec_value_list.arg(1, ec_pos)) {
	  
	  // the Y
	  if (EC_succeed == ec_pos_list.arg(1, ec_pos)) {
	    long l_pos;
	    if (ec_pos.is_long(&l_pos) != EC_succeed) {
	      cerr << "error in type " << endl;
	      return EC_fail;
	    }
	    value.Y = l_pos;
	  }
	}
      }

      // next entry from value list
      ec_value_list.arg(2, ec_value_list);
      
      // action string 
      if (EC_succeed == ec_value_list.arg(1, ec_action)) {
	char * chr_action;
	if (ec_action.is_string( &chr_action ) != EC_succeed) {
	  cerr << "error in type parsig action" << endl;
	  return EC_fail;
	}
	value.act = std::string(chr_action);
	if( value.act == "go_right" ) value.act = "r";
 	if( value.act == "go_left" ) value.act = "l";
 	if( value.act == "go_up" ) value.act = "u";
 	if( value.act == "go_down" ) value.act = "d";
	
 	switch(value.act[0]) {
 	case 'r': value.action = EAST; break;
 	case 'l': value.action = WEST; break;
 	case 'u': value.action = NORTH; break;
 	case 'd': value.action = SOUTH; break;
 	}

      }

      
      // next entry	
      ec_value_list.arg(2, ec_value_list);
      
      if (EC_succeed == ec_value_list.arg(1, ec_val)) {
	double d_val;
	if( ec_val.is_double( &d_val ) != EC_succeed ) {
	  cerr << "error in type parsing value" << endl;
	  return EC_fail;
	}
	value.value = d_val;
      }
    

      
      // next entry	
      ec_value_list.arg(2, ec_value_list);
      
      if( EC_succeed == ec_value_list.arg(1, ec_prob)) {
	double d_prob;
	if( ec_prob.is_double( &d_prob ) != EC_succeed ) {
	  cerr << "error in type parsing prob" << endl;
	  return EC_fail;
	}
	value.prob = d_prob;
      }
      

      
      ec_value_list.arg(2, ec_value_list);
      Values.push_back(value);
    }
//     cout << "X " << value.X << " ";
//     cout << "Y " << value.Y << " ";
//     cout << "action: " << value.act << " ";
//     cout << " value " << value.value << " ";
//     cout << "prob: " << value.prob << endl;
    ec_Arg1.arg(2, ec_Arg1);
  }

  for(unsigned int i=0; i< Values.size(); i++) {

    OptPolicy.push_back(Values[i]);

    //    cout << OptPolicy[i].value<< endl;
  }

  draw_opt_vals();

  return EC_succeed;
}    

 
extern "C"
int p_Refresh()
{
  if( !winited ) {
    //cerr << " ***** ERROR ***** window not initialized yet, can't draw" << endl;
    return EC_succeed;
  }    

  j_refresh_window(WINDOW);
  return EC_succeed;
}


extern "C"
int p_Redraw()
{
  if( !winited ) {
    //cerr << " ***** ERROR ***** window not initialized yet, can't draw" << endl;
    return EC_succeed;
  }    
  
  //std::cout << " ***** ReDraw ***** redraw " << std::endl;
  redraw();
  //std::cout << " ***** ReDraw ***** draw_actions " << std::endl;
  draw_actions();

  //std::cout << " ***** ReDraw ***** refresh " << std::endl;
  j_refresh_window(WINDOW);

  return EC_succeed;
}


extern "C"
int p_ClearHistory()
{
  if( !winited ) {
    //cerr << " ***** ERROR ***** window not initialized yet, can't draw" << endl;
    return EC_succeed;
  }    

  clear_history();

  return EC_succeed;
}


extern "C"
int p_Clear()
{
  if( !winited ) {
    //cerr << " ***** ERROR ***** window not initialized yet, can't draw" << endl;
    return EC_succeed;
  }    

  clear();

  return EC_succeed;
}


