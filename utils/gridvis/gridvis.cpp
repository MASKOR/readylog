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
 *        mod by: Dennis Pannhausen <Dennis.Pannhausen@rwth-aachen.de>
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

std::vector<pit_t>    Pits;
pit_t Pit;

std::vector<breeze_t> Breezes;
breeze_t Breeze;

std::vector<stench_t> Stenches;
stench_t Stench;

std::vector<shade_t>  Shades;
shade_t Shade;

std::vector<cell_t>   Cells;
cell_t CellPiece;

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


  // Walls
  Walls.clear();
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
int p_StartDisplayNG()
{
  
  EC_word Arg1 = EC_word( EC_arg( 1 ) );
  EC_word Arg2 = EC_word( EC_arg( 2 ) );
  EC_word Arg3 = EC_word( EC_arg( 3 ) );
  EC_word Arg4 = EC_word( EC_arg( 4 ) );

  long GridX, GridY;

  if ( Arg1.is_long( &GridX ) != EC_succeed) {
    cerr << "In p_StartDisplayNG(): Failed to get Arg 1 " << endl;
    return EC_fail;
  };
  
  if ( Arg2.is_long( &GridY ) != EC_succeed) {
    cerr << "In p_StartDisplayNG(): Failed to get Arg 2 " << endl;
    return EC_fail;
  };


//  std::cout << "gridvis(p_StartDisplayNG): drawing walls" << std::endl;
  // Walls
  Walls.clear();
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

//  std::cout << "gridvis(p_StartDisplayNG): drawing cells" << std::endl;
  // Occupied Cells
  Cells.clear();
  std::vector<int> oc_list_elements;
  EC_word oc_list = Arg4;
  EC_word oc_sub_list, oc_element;
  while(EC_succeed == oc_list.arg(1, oc_sub_list)) {

    while (EC_succeed == oc_sub_list.arg(1, oc_element)) {
      long oc_elem = 0;
      //if (element.is_nil()) {
      //	cerr << " element is NIL!" << endl;
      //}	
      if (oc_element.is_long(&oc_elem) != EC_succeed) {
	cerr << "error in type in oc_elem: '" << oc_elem << "'" << endl;
	return EC_fail;
      }
      //cout << "got elem: '" << oc_elem << "'" << endl;
      oc_list_elements.push_back(oc_elem);
      oc_sub_list.arg(2,oc_sub_list);
    }
    CellPiece.x=oc_list_elements[0];
    CellPiece.y=oc_list_elements[1];
    oc_list_elements.clear();
    Cells.push_back(CellPiece);
    oc_list.arg(2,oc_list);
  }

  start_display(GridX, GridY);

  winited = true;

  return EC_succeed;
}

extern "C"
int p_StartDisplayWumpus()
{
  
  EC_word Arg1 = EC_word( EC_arg( 1 ) );
  EC_word Arg2 = EC_word( EC_arg( 2 ) );
  EC_word Arg3 = EC_word( EC_arg( 3 ) );
  EC_word Arg4 = EC_word( EC_arg( 4 ) );
  EC_word Arg5 = EC_word( EC_arg( 5 ) );
  EC_word Arg6 = EC_word( EC_arg( 6 ) );
 // EC_word Arg7 = EC_word( EC_arg( 7 ) );

  long GridX, GridY;

  if ( Arg1.is_long( &GridX ) != EC_succeed) {
    cerr << "In p_StartDisplayWumpus(): Failed to get Arg 1 " << endl;
    return EC_fail;
  };
  
  if ( Arg2.is_long( &GridY ) != EC_succeed) {
    cerr << "In p_StartDisplayWumpus(): Failed to get Arg 2 " << endl;
    return EC_fail;
  };


//  std::cout << "gridvis(p_StartDisplayWumpus(): drawing walls" << std::endl;
  // Walls
  Walls.clear();
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

//  std::cout << "gridvis(p_StartDisplayWumpus(): drawing pits" << std::endl;
  // Pits
  Pits.clear();
  std::vector<int> pit_list_elements;
  EC_word pit_list = Arg4;
  EC_word pit_sub_list, pit_element;
  while(EC_succeed == pit_list.arg(1, pit_sub_list)) {
    while (EC_succeed == pit_sub_list.arg(1, pit_element)) {
      long pit_elem = 0;
//        if (pit_element.is_nil()) {
//       	cerr << " pit_element is NIL!" << endl;
//       }	
      if (pit_element.is_long(&pit_elem) != EC_succeed) {
	cerr << "error in type in pit_elem: '" << pit_elem << "'" << endl;
	return EC_fail;
      }
//      cout << "got pit_elem: '" << pit_elem << "'" << endl;
      pit_list_elements.push_back(pit_elem);
      pit_sub_list.arg(2,pit_sub_list);
    }
    Pit.x=pit_list_elements[0];
    Pit.y=pit_list_elements[1];
    pit_list_elements.clear();
    Pits.push_back(Pit);
    pit_list.arg(2,pit_list);
  }

//  std::cout << "gridvis(p_StartDisplayWumpus(): drawing breezes" << std::endl;
  // Breezes
  Breezes.clear();
  std::vector<int> breeze_list_elements;
  EC_word breeze_list = Arg5;
  EC_word breeze_sub_list, breeze_element;
  while(EC_succeed == breeze_list.arg(1, breeze_sub_list)) {
    while (EC_succeed == breeze_sub_list.arg(1, breeze_element)) {
      long breeze_elem = 0;
//        if (breeze_element.is_nil()) {
//       	cerr << " breeze_element is NIL!" << endl;
//       }	
      if (breeze_element.is_long(&breeze_elem) != EC_succeed) {
	cerr << "error in type in breeze_elem: '" << breeze_elem << "'" << endl;
	return EC_fail;
      }
//      cout << "got breeze_elem: '" << breeze_elem << "'" << endl;
      breeze_list_elements.push_back(breeze_elem);
      breeze_sub_list.arg(2,breeze_sub_list);
    }
    Breeze.x=breeze_list_elements[0];
    Breeze.y=breeze_list_elements[1];
    breeze_list_elements.clear();
    Breezes.push_back(Breeze);
    breeze_list.arg(2,breeze_list);
  }

//  std::cout << "gridvis(p_StartDisplayWumpus(): drawing stench" << std::endl;
  // Stenches
  Stenches.clear();
  std::vector<int> stench_list_elements;
  EC_word stench_list = Arg6;
  EC_word stench_sub_list, stench_element;
  while(EC_succeed == stench_list.arg(1, stench_sub_list)) {
    while (EC_succeed == stench_sub_list.arg(1, stench_element)) {
      long stench_elem = 0;
//        if (stench_element.is_nil()) {
//       	cerr << " stench_element is NIL!" << endl;
//       }	
      if (stench_element.is_long(&stench_elem) != EC_succeed) {
	cerr << "error in type in stench_elem: '" << stench_elem << "'" << endl;
	return EC_fail;
      }
//      cout << "got stench_elem: '" << stench_elem << "'" << endl;
      stench_list_elements.push_back(stench_elem);
      stench_sub_list.arg(2,stench_sub_list);
    }
    Stench.x=stench_list_elements[0];
    Stench.y=stench_list_elements[1];
    stench_list_elements.clear();
    Stenches.push_back(Stench);
    stench_list.arg(2,stench_list);
  }
/*  
  std::cout << "gridvis(p_StartDisplayWumpus(): drawing shades" << std::endl;
  // Shades
  Shades.clear();
  std::vector<int> shade_list_elements;
  EC_word shade_list = Arg7;
  EC_word shade_sub_list, shade_element;
  while(EC_succeed == shade_list.arg(1, shade_sub_list)) {
    while (EC_succeed == shade_sub_list.arg(1, shade_element)) {
      long shade_elem = 0;
//        if (shade_element.is_nil()) {
//       	cerr << " shade_element is NIL!" << endl;
//       }	
      if (shade_element.is_long(&shade_elem) != EC_succeed) {
	cerr << "error in type in shade_elem: '" << shade_elem << "'" << endl;
	return EC_fail;
      }
//      cout << "got shade_elem: '" << shade_elem << "'" << endl;
      shade_list_elements.push_back(shade_elem);
      shade_sub_list.arg(2,shade_sub_list);
    }
    Shade.x=shade_list_elements[0];
    Shade.y=shade_list_elements[1];
    shade_list_elements.clear();
    Shades.push_back(Shade);
    shade_list.arg(2,shade_list);
  }
*/
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
  case '1' : // turn left
    draw_turn(TLEFT, X, Y);
    // save action for redraw:
    a.X = X; a.Y = Y; a.action = TLEFT;
    Actions.push_back( a );
    break;
  case '2' : // turn right
    draw_turn(TRIGHT, X, Y);
    // save action for redraw:
    a.X = X; a.Y = Y; a.action = TRIGHT;
    Actions.push_back( a );
    break;
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
int p_DrawGoto() 
{
  if( !winited ) {
    //cerr << " ***** ERROR ***** window not initialized yet, can't draw" << endl;
    return EC_succeed;
  }    

  EC_word ec_action = EC_word( EC_arg( 1 ) );
  EC_word ec_toX = EC_word( EC_arg( 2 ) );
  EC_word ec_toY = EC_word( EC_arg( 3 ) );
  EC_word ec_X = EC_word( EC_arg( 4 ) );
  EC_word ec_Y = EC_word( EC_arg( 5 ) );

  char * strAction;
  if ( ec_action.is_string( &strAction ) != EC_succeed) {
    cerr << "In p_DrawGoto(): Failed to get Arg 1 " << endl;
    return EC_fail;
  };

  long toX;
  if ( ec_toX.is_long( &toX ) != EC_succeed) {
    cerr << "In p_DrawGoto(): Failed to get Arg 2 " << endl;
    return EC_fail;
  };
  long toY;
  if ( ec_toY.is_long( &toY ) != EC_succeed) {
    cerr << "In p_DrawGoto(): Failed to get Arg 3 " << endl;
    return EC_fail;
  };

  long X;
  if ( ec_X.is_long( &X ) != EC_succeed) {
    cerr << "In p_DrawGoto(): Failed to get Arg 4 " << endl;
    return EC_fail;
  };
  long Y;
  if ( ec_Y.is_long( &Y ) != EC_succeed) {
    cerr << "In p_DrawGoto(): Failed to get Arg 5 " << endl;
    return EC_fail;
  };

  // to save action
  action_t a;

  switch(strAction[0]) {
  case 'R' :
    draw_goto(GOTOR, toX, toY, X, Y);
    // save action for redraw:
    a.toX = toX; a.toY = toY; a.X = X; a.Y = Y; a.action = GOTOR;
    Actions.push_back( a );
    break;
  case 'G' :
    draw_goto(GOTOG, toX, toY, X, Y);
    // save action for redraw:
    a.toX = toX; a.toY = toY; a.X = X; a.Y = Y; a.action = GOTOG;
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
  case '1' : // turn left
    draw_turn(TLEFT, X, Y);
    break;
  case '2' : // turn right
    draw_turn(TRIGHT, X, Y);
    break;
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
int p_DrawArrow()
{
  if( !winited ) {
    //cerr << " ***** ERROR ***** window not initialized yet, can't draw" << endl;
    return EC_succeed;
  }    

  EC_word ec_X = EC_word( EC_arg( 1 ) );
  EC_word ec_Y = EC_word( EC_arg( 2 ) );
  EC_word ec_D = EC_word( EC_arg( 3 ) );

  long X,Y;
  if( ec_X.is_long( &X ) != EC_succeed ) {
    cerr << "failed to unify arg 1 in p_DrawArrow " << endl;
    return EC_fail;
  }
  if( ec_Y.is_long( &Y ) != EC_succeed ) {
    cerr << "failed to unify arg 2 in p_DrawArrow " << endl;
    return EC_fail;
  }
  char * D;
  if( ec_D.is_string( &D ) != EC_succeed ) {
    cerr << "failed to unify arg 3 in p_DrawArrow " << endl;
    return EC_fail;
  }  

  draw_arrow(X, Y, D);

  return EC_succeed;
}

extern "C"
int p_DrawWumpusHunter()
{
  if( !winited ) {
    //cerr << " ***** ERROR ***** window not initialized yet, can't draw" << endl;
    return EC_succeed;
  }    

  EC_word ec_X = EC_word( EC_arg( 1 ) );
  EC_word ec_Y = EC_word( EC_arg( 2 ) );
  EC_word ec_D = EC_word( EC_arg( 3 ) );
  EC_word ec_A = EC_word( EC_arg( 4 ) );

  long X,Y;
  if( ec_X.is_long( &X ) != EC_succeed ) {
    cerr << "failed to unify arg 1 in p_DrawWumpusHunter " << endl;
    return EC_fail;
  }
  if( ec_Y.is_long( &Y ) != EC_succeed ) {
    cerr << "failed to unify arg 2 in p_DrawWumpusHunter " << endl;
    return EC_fail;
  }
  char * D;
  if( ec_D.is_string( &D ) != EC_succeed ) {
    cerr << "failed to unify arg 3 in p_DrawWumpusHunter " << endl;
    return EC_fail;
  }  
  long A;
  if( ec_A.is_long( &A ) != EC_succeed ) {
    cerr << "failed to unify arg 4 in p_DrawWumpusHunter " << endl;
    return EC_fail;
  }
  
//  // string to bool conversion
//  switch(A[0]) {
//  case 'T' : // has arrow
//    draw_wumpus_hunter(X, Y, D, A);
//    break;
//  case 'F' : // doesn't have arrow
//    draw_wumpus_hunter(X, Y, D, false);
//    break;
//  }

  draw_wumpus_hunter(X, Y, D, A);

  return EC_succeed;
}

extern "C"
int p_DrawWumpus()
{
  if( !winited ) {
    //cerr << " ***** ERROR ***** window not initialized yet, can't draw" << endl;
    return EC_succeed;
  }    

  EC_word ec_X = EC_word( EC_arg( 1 ) );
  EC_word ec_Y = EC_word( EC_arg( 2 ) );
  EC_word ec_A = EC_word( EC_arg( 3 ) );

  long X,Y;
  if( ec_X.is_long( &X ) != EC_succeed ) {
    cerr << "failed to unify arg 1 in p_DrawWumpus " << endl;
    return EC_fail;
  }
  if( ec_Y.is_long( &Y ) != EC_succeed ) {
    cerr << "failed to unify arg 2 in p_DrawWumpus " << endl;
    return EC_fail;
  }
  long A;
  if( ec_A.is_long( &A ) != EC_succeed ) {
    cerr << "failed to unify arg 3 in p_DrawWumpus " << endl;
    return EC_fail;
  }
  
//  // string to bool conversion
//  switch(A[0]) {
//  case 'T' : // has arrow
//    draw_wumpus(X, Y, true);
//    break;
//  case 'F' : // doesn't have arrow
//    draw_wumpus(X, Y, false);
//    break;
//  }

  draw_wumpus(X, Y, A);

  return EC_succeed;
}

extern "C"
int p_DrawHuman()
{
  if( !winited ) {
    //cerr << " ***** ERROR ***** window not initialized yet, can't draw" << endl;
    return EC_succeed;
  }    

  EC_word ec_X = EC_word( EC_arg( 1 ) );
  EC_word ec_Y = EC_word( EC_arg( 2 ) );

  long X,Y;
  if( ec_X.is_long( &X ) != EC_succeed ) {
    cerr << "failed to unify arg 1 in p_DrawHuman " << endl;
    return EC_fail;
  }
  if( ec_Y.is_long( &Y ) != EC_succeed ) {
    cerr << "failed to unify arg 2 in p_DrawHuman " << endl;
    return EC_fail;
  }

  draw_human(X, Y);

  return EC_succeed;
}

extern "C"
int p_DrawGold()
{
  if( !winited ) {
    //cerr << " ***** ERROR ***** window not initialized yet, can't draw" << endl;
    return EC_succeed;
  }    

  EC_word ec_X = EC_word( EC_arg( 1 ) );
  EC_word ec_Y = EC_word( EC_arg( 2 ) );

  long X,Y;
  if( ec_X.is_long( &X ) != EC_succeed ) {
    cerr << "failed to unify arg 1 in p_DrawGold " << endl;
    return EC_fail;
  }
  if( ec_Y.is_long( &Y ) != EC_succeed ) {
    cerr << "failed to unify arg 2 in p_DrawGold " << endl;
    return EC_fail;
  }

  draw_gold(X, Y);

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
  if( ec_Y.is_long( &Y ) != EC_succeed ) {
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
        if( value.act == "turn_left" ) value.act = "1";
        if( value.act == "turn_right" ) value.act = "2";
	if( value.act == "go_right" ) value.act = "r";
 	if( value.act == "go_left" ) value.act = "l";
 	if( value.act == "go_up" ) value.act = "u";
 	if( value.act == "go_down" ) value.act = "d";
	
 	switch(value.act[0]) {
        case '1': value.action = TLEFT; break;
        case '2': value.action = TRIGHT; break;
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
int p_UpdatePits()
{
  EC_word Arg1 = EC_word( EC_arg( 1 ) );

  //  std::cout << "updating pits" << std::endl;
  // Pits
  Pits.clear();
  std::vector<int> pit_list_elements;
  EC_word pit_list = Arg1;
  EC_word pit_sub_list, pit_element;
  while(EC_succeed == pit_list.arg(1, pit_sub_list)) {
    while (EC_succeed == pit_sub_list.arg(1, pit_element)) {
      long pit_elem = 0;
//        if (pit_element.is_nil()) {
//       	cerr << " pit_element is NIL!" << endl;
//       }	
      if (pit_element.is_long(&pit_elem) != EC_succeed) {
	cerr << "error in type in pit_elem: '" << pit_elem << "'" << endl;
	return EC_fail;
      }
//      cout << "got pit_elem: '" << pit_elem << "'" << endl;
      pit_list_elements.push_back(pit_elem);
      pit_sub_list.arg(2,pit_sub_list);
    }
    Pit.x=pit_list_elements[0];
    Pit.y=pit_list_elements[1];
    pit_list_elements.clear();
    Pits.push_back(Pit);
    pit_list.arg(2,pit_list);
  }

  return EC_succeed;
}

extern "C"
int p_UpdateBreezes()
{
  EC_word Arg1 = EC_word( EC_arg( 1 ) );

  //  std::cout << "updating breezes" << std::endl;
  // Breezes
  Breezes.clear();
  std::vector<int> breeze_list_elements;
  EC_word breeze_list = Arg1;
  EC_word breeze_sub_list, breeze_element;
  while(EC_succeed == breeze_list.arg(1, breeze_sub_list)) {
    while (EC_succeed == breeze_sub_list.arg(1, breeze_element)) {
      long breeze_elem = 0;
//        if (breeze_element.is_nil()) {
//       	cerr << " breeze_element is NIL!" << endl;
//       }	
      if (breeze_element.is_long(&breeze_elem) != EC_succeed) {
	cerr << "error in type in breeze_elem: '" << breeze_elem << "'" << endl;
	return EC_fail;
      }
//      cout << "got breeze_elem: '" << breeze_elem << "'" << endl;
      breeze_list_elements.push_back(breeze_elem);
      breeze_sub_list.arg(2,breeze_sub_list);
    }
    Breeze.x=breeze_list_elements[0];
    Breeze.y=breeze_list_elements[1];
    breeze_list_elements.clear();
    Breezes.push_back(Breeze);
    breeze_list.arg(2,breeze_list);
  }

  return EC_succeed;
}

extern "C"
int p_UpdateStenches()
{
  EC_word Arg1 = EC_word( EC_arg( 1 ) );

  //  std::cout << "updating stenches" << std::endl;
  // Stenches
  Stenches.clear();
  std::vector<int> stench_list_elements;
  EC_word stench_list = Arg1;
  EC_word stench_sub_list, stench_element;
  while(EC_succeed == stench_list.arg(1, stench_sub_list)) {
    while (EC_succeed == stench_sub_list.arg(1, stench_element)) {
      long stench_elem = 0;
//        if (stench_element.is_nil()) {
//       	cerr << " stench_element is NIL!" << endl;
//       }	
      if (stench_element.is_long(&stench_elem) != EC_succeed) {
	cerr << "error in type in stench_elem: '" << stench_elem << "'" << endl;
	return EC_fail;
      }
//      cout << "got stench_elem: '" << stench_elem << "'" << endl;
      stench_list_elements.push_back(stench_elem);
      stench_sub_list.arg(2,stench_sub_list);
    }
    Stench.x=stench_list_elements[0];
    Stench.y=stench_list_elements[1];
    stench_list_elements.clear();
    Stenches.push_back(Stench);
    stench_list.arg(2,stench_list);
  }

  return EC_succeed;
}

extern "C"
int p_UpdateShades()
{
  EC_word Arg1 = EC_word( EC_arg( 1 ) );
        
//  if (Arg1.is_nil()) {
 //       // empty list! Happens before initialisation
 //       cerr << " Shade list is NIL!" << endl;
//        cerr << " Shade list:" << Arg1 << endl;

 //       return EC_succeed;
 // }	
  
//  std::cout << "updating shades" << std::endl;
  // Shades
  Shades.clear();
  std::vector<int> shade_list_elements;
  EC_word shade_list = Arg1;
  EC_word shade_sub_list, shade_element;
  while(EC_succeed == shade_list.arg(1, shade_sub_list)) {
    while (EC_succeed == shade_sub_list.arg(1, shade_element)) {
      long shade_elem = 0;
//        if (shade_element.is_nil()) {
//            cerr << " shade_element is NIL!" << endl;
//        }	
      if (shade_element.is_long(&shade_elem) != EC_succeed) {
	cerr << "error in type in shade_elem: '" << shade_elem << "'" << endl;
	return EC_fail;
      }
//      cout << "got shade_elem: '" << shade_elem << "'" << endl;
      shade_list_elements.push_back(shade_elem);
      shade_sub_list.arg(2,shade_sub_list);
    }
    Shade.x=shade_list_elements[0];
    Shade.y=shade_list_elements[1];
    shade_list_elements.clear();
    Shades.push_back(Shade);
    shade_list.arg(2,shade_list);
  }
  
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


