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
 *           $Id$
 *        author: Alexander Ferrein <ferrein@cs.rwth-aachen.de>
 *   description: external interface to draw gridworlds (maze etc)
 *
 * ************************************************************************ */

#include "display.h"
#include <iostream>
#include <stdio.h>

#define min(A,B) (A<B ? A:B);
#define max(A,B) (A>B ? A:B);

jwin *WINDOW = NULL;
int GX, GY;

static int COLORS[MAX_COLORS];/* define an array for storing colors */

//COLORS = 0;

//extern "C" {

void init_win (int wd, int ht) /* Make window and allocate colors.     */
{
  //jwin *w;
  char name[10];
  int i;

  printf("Creating window ...");   /* make some size of window */
  fflush(stdout);
  WINDOW = j_make_window(wd, ht, (char *)"white", NULL, NULL, 1);

  printf("\nAllocating colors ..."); /* allocate gray values */
  fflush(stdout);
  for (i=0; i <= MAX_COLORS-1; i++)
    {
      sprintf(name, "gray%d", (MAX_COLORS - 1) - i);
      COLORS[i] = (int) find_color_val(name, WINDOW);
    } 
  COLORS[81] = (int) find_color_val((char *)"red", WINDOW);
  COLORS[82] = (int) find_color_val((char *)"navy", WINDOW);
  COLORS[83] = (int) find_color_val((char *)"yellow", WINDOW);
  COLORS[84] = (int) find_color_val((char *)"cyan", WINDOW);
  COLORS[85] = (int) find_color_val((char *)"green", WINDOW);
  COLORS[86] = (int) find_color_val((char *)"brown", WINDOW);
  COLORS[87] = (int) find_color_val((char *)"magenta", WINDOW);
  COLORS[88] = (int) find_color_val((char *)"gray60", WINDOW);
  COLORS[89] = (int) find_color_val((char *)"gold", WINDOW);
  printf("\n");

}

void draw_walls() {
  int start_x, start_y, end_x, end_y;

//  std::cout << "display(draw_walls): drawing " <<   Walls.size() << " walls." <<std::endl;

  for (unsigned int i = 0; i < Walls.size(); i++) {
    // is the wall horizontal or vertical?
    int delta_x = abs(Walls[i].x1-Walls[i].x2);
    int delta_y = abs(Walls[i].y1-Walls[i].y2);

  int x, y;
  
  // vertical wall
  if (delta_x != 0) {
    x = max(Walls[i].x1, Walls[i].x2);
    start_x = end_x = x*ROW_SIZE;
    // y axis has wrong direction
    start_y = GY - Walls[i].y1*ROW_SIZE;
    end_y = GY - (Walls[i].y1+1 )*ROW_SIZE;
  }

  // horizontal wall
  if (delta_y != 0 ) {
    start_x = Walls[i].x1 * ROW_SIZE;
    end_x = (Walls[i].x2 + 1) * ROW_SIZE;
    y = max(Walls[i].y1, Walls[i].y2);
    // y axis has wrong direction
    start_y = end_y = GY - y * ROW_SIZE;
  }

     j_draw_line(end_x, end_y, start_x, start_y,COLORS[80],7,0,0,WINDOW);
  }

//   draw_dir(EAST, 0, 0);
//   draw_dir(WEST, 2,1);
//   draw_dir(WEST, 3,1);

}

void
draw_pits() {
  int draw_x, draw_y;

//  std::cout << "display(draw_pits): drawing " <<  Pits.size() << " pits." <<std::endl;

  for (unsigned int i = 0; i < Pits.size(); i++) {
      int x, y;
      
      x = Pits[i].x;
      y = Pits[i].y;
  
      draw_x = x * ROW_SIZE;
      draw_y = GY - (y+1) * ROW_SIZE;

      j_draw_circle(draw_x+(ROW_SIZE/2), draw_y+(ROW_SIZE/2), 26, COLORS[80],0,0,WINDOW);
  }

}

void
draw_breezes() {
//  std::cout << "display(draw_breezes): drawing " <<  Breezes.size() << " breezes." <<std::endl;
  
  int start_x, start_y; //, end_x, end_y;
  //std::cout << X << " " <<Y <<std::endl;
  
  float scalex = 0.35;
  float scaley = -0.35;

  for (unsigned int i = 0; i < Breezes.size(); i++) {
      int x, y;
      
      x = Breezes[i].x;
      y = Breezes[i].y;

      start_x = x * ROW_SIZE + 10;
      start_y = GY - (y+1) * ROW_SIZE + 38;

    for (unsigned int i = 1; i < 4; i++)  {
       j_draw_line(start_x+(-7*scalex), start_y+(95*scaley)+5*i, start_x+(7*scalex),start_y+(106*scaley)+5*i,COLORS[84],3,0,0,WINDOW);
       j_draw_line(start_x+(7*scalex), start_y+(106*scaley)+5*i, start_x+(16*scalex),start_y+(109*scaley)+5*i,COLORS[84],3,0,0,WINDOW);
       j_draw_line(start_x+(16*scalex), start_y+(109*scaley)+5*i, start_x+(30*scalex),start_y+(109*scaley)+5*i,COLORS[84],3,0,0,WINDOW);
       j_draw_line(start_x+(30*scalex), start_y+(109*scaley)+5*i, start_x+(70*scalex),start_y+(100*scaley)+5*i,COLORS[84],3,0,0,WINDOW);
       j_draw_line(start_x+(70*scalex), start_y+(100*scaley)+5*i, start_x+(85*scalex),start_y+(100*scaley)+5*i,COLORS[84],3,0,0,WINDOW);
       j_draw_line(start_x+(85*scalex), start_y+(100*scaley)+5*i, start_x+(123*scalex),start_y+(111*scaley)+5*i,COLORS[84],3,0,0,WINDOW);
    }
  
  }

}

void
draw_stench() {
//  std::cout << "display(draw_stench): drawing " <<  Stenches.size() << " stenches." <<std::endl;

  int start_x, start_y; //, end_x, end_y;
  //std::cout << X << " " <<Y <<std::endl;
  
  float scalex = 0.35;
  float scaley = -0.35;

  for (unsigned int i = 0; i < Stenches.size(); i++) {
      int x, y;
      
      x = Stenches[i].x;
      y = Stenches[i].y;

      start_x = x * ROW_SIZE;
      start_y = GY - (y+1) * ROW_SIZE + 52;

    for (unsigned int i = 1; i <= 5; i++)  {
       // stench no. i
       j_draw_line(start_x+(0*scalex)+9*i, start_y+(56*scaley), start_x+(10*scalex)+9*i,start_y+(62*scaley),COLORS[85],3,0,0,WINDOW);
       j_draw_line(start_x+(10*scalex)+9*i, start_y+(62*scaley), start_x+(-7*scalex)+9*i,start_y+(71*scaley),COLORS[85],3,0,0,WINDOW);
       j_draw_line(start_x+(-7*scalex)+9*i, start_y+(71*scaley), start_x+(7*scalex)+9*i,start_y+(76*scaley),COLORS[85],3,0,0,WINDOW);
       j_draw_line(start_x+(7*scalex)+9*i, start_y+(76*scaley), start_x+(9*scalex)+9*i,start_y+(80*scaley),COLORS[85],3,0,0,WINDOW);
       j_draw_line(start_x+(9*scalex)+9*i, start_y+(80*scaley), start_x+(0*scalex)+9*i,start_y+(83*scaley),COLORS[85],3,0,0,WINDOW);
       j_draw_line(start_x+(0*scalex)+9*i, start_y+(83*scaley), start_x+(1*scalex)+9*i,start_y+(88*scaley),COLORS[85],3,0,0,WINDOW);
       j_draw_line(start_x+(1*scalex)+9*i, start_y+(88*scaley), start_x+(13*scalex)+9*i,start_y+(91*scaley),COLORS[85],3,0,0,WINDOW);
    }
  
  }

}

void
draw_shades() {
  int draw_x, draw_y;
  
//  std::cout << "display(draw_shades): drawing " <<   Shades.size() << " shades." <<std::endl;

  for (unsigned int i = 0; i < Shades.size(); i++) {
    int x, y;
    
    x = Shades[i].x;
    y = Shades[i].y;
    
    draw_x = x * ROW_SIZE;
    draw_y = GY - (y+1) * ROW_SIZE;
  
//    for (unsigned int j = 0; j < 65; j=j+5) {
//      j_draw_line(draw_x, draw_y+j, draw_x+60, draw_y+j, COLORS[80],2,1,0,WINDOW);
//    }
//       j_draw_rectangle(draw_x, draw_y, draw_x+ROW_SIZE, draw_y+ROW_SIZE, COLORS[80],0,0,WINDOW);
      

      j_draw_rectangle(draw_x, draw_y, ROW_SIZE, ROW_SIZE, COLORS[88],0,0,WINDOW);

  }
  
}

void
draw_cells() {
  int draw_x, draw_y;
  
//  std::cout << "display(draw_cells): drawing " <<   Cells.size() << " cells." <<std::endl;

  for (unsigned int i = 0; i < Cells.size(); i++) {
    int x, y;
    
    x = Cells[i].x;
    y = Cells[i].y;
    
    draw_x = x * ROW_SIZE;
    draw_y = GY - (y+1) * ROW_SIZE;
    
    j_draw_line(draw_x+55, draw_y+5, draw_x+5, draw_y+55, COLORS[80],5,0,0,WINDOW);
    j_draw_line(draw_x+55, draw_y+55, draw_x+5, draw_y+5, COLORS[80],5,0,0,WINDOW);
  }
  
}

void 
draw_agent( int X, int Y)
{
  int start_x, start_y; //, end_x, end_y;
  //std::cout << X << " " <<Y <<std::endl;
  
  start_x = X * ROW_SIZE + 25;
  start_y = GY - ((Y+1)*ROW_SIZE);

  j_draw_line(start_x+5,start_y+55,start_x+15,start_y+35,COLORS[86],3,0,0,WINDOW);
  j_draw_line(start_x+15,start_y+35,start_x+25,start_y+55,COLORS[86],3,0,0,WINDOW);
  j_draw_line(start_x+5,start_y+55,start_x+25,start_y+55,COLORS[86],3,0,0,WINDOW);


  j_refresh_window(WINDOW);
}

void 
draw_wumpus( int X, int Y, int A)
{
/*  if(A) {
    std::cout << "display(draw_wumpus): drawing alive wumpus at (" << X << "," << Y << ")." <<std::endl;
  }
  else {
    std::cout << "display(draw_wumpus): drawing dead wumpus at (" << X << "," << Y << ")." <<std::endl;
  }
*/
  int start_x, start_y; //, end_x, end_y;
  //std::cout << X << " " <<Y <<std::endl;
  
  start_x = X * ROW_SIZE + 15;
  start_y = GY - ((Y+1)*ROW_SIZE) + 52;

  float scalex = 0.45;
  float scaley = -0.45;

  if(A){
  // body
  j_draw_line(start_x+(0*scalex), start_y+(0*scaley), start_x+(8*scalex),start_y+(4*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(8*scalex), start_y+(4*scaley), start_x+(22*scalex),start_y+(2*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(22*scalex), start_y+(2*scaley), start_x+(22*scalex),start_y+(8*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(22*scalex), start_y+(8*scaley), start_x+(8*scalex),start_y+(11*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(8*scalex), start_y+(11*scaley), start_x+(1*scalex),start_y+(18*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(1*scalex), start_y+(18*scaley), start_x+(3*scalex),start_y+(55*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(3*scalex), start_y+(55*scaley), start_x+(-6*scalex),start_y+(71*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(-6*scalex), start_y+(71*scaley), start_x+(-16*scalex),start_y+(74*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(-16*scalex), start_y+(74*scaley), start_x+(-13*scalex),start_y+(78*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(-13*scalex), start_y+(78*scaley), start_x+(-15*scalex),start_y+(81*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(-15*scalex), start_y+(81*scaley), start_x+(-11*scalex),start_y+(83*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(-11*scalex), start_y+(83*scaley), start_x+(-12*scalex),start_y+(86*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(-12*scalex), start_y+(86*scaley), start_x+(-9*scalex),start_y+(87*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(-9*scalex), start_y+(87*scaley), start_x+(-6*scalex),start_y+(91*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(-6*scalex), start_y+(91*scaley), start_x+(-2*scalex),start_y+(86*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(-2*scalex), start_y+(86*scaley), start_x+(5*scalex),start_y+(67*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(5*scalex), start_y+(67*scaley), start_x+(12*scalex),start_y+(91*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(12*scalex), start_y+(91*scaley), start_x+(25*scalex),start_y+(101*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(25*scalex), start_y+(101*scaley), start_x+(42*scalex),start_y+(105*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(42*scalex), start_y+(105*scaley), start_x+(59*scalex),start_y+(99*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(59*scalex), start_y+(99*scaley), start_x+(66*scalex),start_y+(90*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(66*scalex), start_y+(90*scaley), start_x+(70*scalex),start_y+(68*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(70*scalex), start_y+(68*scaley), start_x+(77*scalex),start_y+(82*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(77*scalex), start_y+(82*scaley), start_x+(80*scalex),start_y+(89*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(80*scalex), start_y+(89*scaley), start_x+(82*scalex),start_y+(85*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(82*scalex), start_y+(85*scaley), start_x+(87*scalex),start_y+(87*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(87*scalex), start_y+(87*scaley), start_x+(86*scalex),start_y+(83*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(86*scalex), start_y+(83*scaley), start_x+(90*scalex),start_y+(81*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(90*scalex), start_y+(81*scaley), start_x+(87*scalex),start_y+(79*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(87*scalex), start_y+(79*scaley), start_x+(91*scalex),start_y+(76*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(91*scalex), start_y+(76*scaley), start_x+(88*scalex),start_y+(74*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(88*scalex), start_y+(74*scaley), start_x+(80*scalex),start_y+(70*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(80*scalex), start_y+(70*scaley), start_x+(71*scalex),start_y+(53*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(71*scalex), start_y+(53*scaley), start_x+(75*scalex),start_y+(19*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(75*scalex), start_y+(19*scaley), start_x+(70*scalex),start_y+(10*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(70*scalex), start_y+(10*scaley), start_x+(55*scalex),start_y+(8*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(55*scalex), start_y+(8*scaley), start_x+(55*scalex),start_y+(3*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(55*scalex), start_y+(3*scaley), start_x+(63*scalex),start_y+(2*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(63*scalex), start_y+(2*scaley), start_x+(75*scalex),start_y+(3*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(75*scalex), start_y+(3*scaley), start_x+(79*scalex),start_y+(-1*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(79*scalex), start_y+(-1*scaley), start_x+(75*scalex),start_y+(-6*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(75*scalex), start_y+(-6*scaley), start_x+(61*scalex),start_y+(-7*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(61*scalex), start_y+(-7*scaley), start_x+(41*scalex),start_y+(-3*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(41*scalex), start_y+(-3*scaley), start_x+(42*scalex),start_y+(2*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(42*scalex), start_y+(2*scaley), start_x+(45*scalex),start_y+(4*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(45*scalex), start_y+(4*scaley), start_x+(45*scalex),start_y+(8*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(45*scalex), start_y+(8*scaley), start_x+(33*scalex),start_y+(8*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(33*scalex), start_y+(8*scaley), start_x+(33*scalex),start_y+(4*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(33*scalex), start_y+(4*scaley), start_x+(37*scalex),start_y+(-3*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(37*scalex), start_y+(-3*scaley), start_x+(4*scalex),start_y+(-6*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(4*scalex), start_y+(-6*scaley), start_x+(0*scalex),start_y+(0*scaley),COLORS[85],3,0,0,WINDOW);

  // eyes
  j_draw_circle(start_x+(30*scalex), start_y+(61*scaley), 7*scalex, COLORS[85],0,0,WINDOW);
  j_draw_circle(start_x+(46*scalex), start_y+(61*scaley), 7*scalex, COLORS[85],0,0,WINDOW);

  // eye brows
  j_draw_line(start_x+(23*scalex), start_y+(72*scaley), start_x+(33*scalex),start_y+(72*scaley),COLORS[85],5,0,0,WINDOW);
  j_draw_line(start_x+(43*scalex), start_y+(74*scaley), start_x+(53*scalex),start_y+(70*scaley),COLORS[85],5,0,0,WINDOW);

  // mouth
  j_draw_line(start_x+(22*scalex), start_y+(45*scaley), start_x+(26*scalex),start_y+(42*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(26*scalex), start_y+(42*scaley), start_x+(33*scalex),start_y+(44*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(33*scalex), start_y+(44*scaley), start_x+(39*scalex),start_y+(42*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(39*scalex), start_y+(42*scaley), start_x+(45*scalex),start_y+(45*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(45*scalex), start_y+(45*scaley), start_x+(52*scalex),start_y+(42*scaley),COLORS[85],3,0,0,WINDOW);
  j_draw_line(start_x+(52*scalex), start_y+(42*scaley), start_x+(56*scalex),start_y+(46*scaley),COLORS[85],3,0,0,WINDOW);

  // teeth
  j_draw_line(start_x+(31*scalex), start_y+(43*scaley), start_x+(32*scalex),start_y+(35*scaley),COLORS[85],2,0,0,WINDOW);
  j_draw_line(start_x+(32*scalex), start_y+(35*scaley), start_x+(36*scalex),start_y+(36*scaley),COLORS[85],2,0,0,WINDOW);
  j_draw_line(start_x+(36*scalex), start_y+(36*scaley), start_x+(37*scalex),start_y+(42*scaley),COLORS[85],2,0,0,WINDOW);

  j_draw_line(start_x+(43*scalex), start_y+(43*scaley), start_x+(44*scalex),start_y+(36*scaley),COLORS[85],2,0,0,WINDOW);
  j_draw_line(start_x+(44*scalex), start_y+(36*scaley), start_x+(49*scalex),start_y+(36*scaley),COLORS[85],2,0,0,WINDOW);
  j_draw_line(start_x+(49*scalex), start_y+(36*scaley), start_x+(50*scalex),start_y+(42*scaley),COLORS[85],2,0,0,WINDOW);
  }
  else {
  // body
  j_draw_line(start_x+(0*scalex), start_y+(0*scaley), start_x+(8*scalex),start_y+(4*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(8*scalex), start_y+(4*scaley), start_x+(22*scalex),start_y+(2*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(22*scalex), start_y+(2*scaley), start_x+(22*scalex),start_y+(8*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(22*scalex), start_y+(8*scaley), start_x+(8*scalex),start_y+(11*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(8*scalex), start_y+(11*scaley), start_x+(1*scalex),start_y+(18*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(1*scalex), start_y+(18*scaley), start_x+(3*scalex),start_y+(55*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(3*scalex), start_y+(55*scaley), start_x+(-6*scalex),start_y+(71*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(-6*scalex), start_y+(71*scaley), start_x+(-16*scalex),start_y+(74*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(-16*scalex), start_y+(74*scaley), start_x+(-13*scalex),start_y+(78*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(-13*scalex), start_y+(78*scaley), start_x+(-15*scalex),start_y+(81*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(-15*scalex), start_y+(81*scaley), start_x+(-11*scalex),start_y+(83*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(-11*scalex), start_y+(83*scaley), start_x+(-12*scalex),start_y+(86*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(-12*scalex), start_y+(86*scaley), start_x+(-9*scalex),start_y+(87*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(-9*scalex), start_y+(87*scaley), start_x+(-6*scalex),start_y+(91*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(-6*scalex), start_y+(91*scaley), start_x+(-2*scalex),start_y+(86*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(-2*scalex), start_y+(86*scaley), start_x+(5*scalex),start_y+(67*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(5*scalex), start_y+(67*scaley), start_x+(12*scalex),start_y+(91*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(12*scalex), start_y+(91*scaley), start_x+(25*scalex),start_y+(101*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(25*scalex), start_y+(101*scaley), start_x+(42*scalex),start_y+(105*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(42*scalex), start_y+(105*scaley), start_x+(59*scalex),start_y+(99*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(59*scalex), start_y+(99*scaley), start_x+(66*scalex),start_y+(90*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(66*scalex), start_y+(90*scaley), start_x+(70*scalex),start_y+(68*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(70*scalex), start_y+(68*scaley), start_x+(77*scalex),start_y+(82*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(77*scalex), start_y+(82*scaley), start_x+(80*scalex),start_y+(89*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(80*scalex), start_y+(89*scaley), start_x+(82*scalex),start_y+(85*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(82*scalex), start_y+(85*scaley), start_x+(87*scalex),start_y+(87*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(87*scalex), start_y+(87*scaley), start_x+(86*scalex),start_y+(83*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(86*scalex), start_y+(83*scaley), start_x+(90*scalex),start_y+(81*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(90*scalex), start_y+(81*scaley), start_x+(87*scalex),start_y+(79*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(87*scalex), start_y+(79*scaley), start_x+(91*scalex),start_y+(76*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(91*scalex), start_y+(76*scaley), start_x+(88*scalex),start_y+(74*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(88*scalex), start_y+(74*scaley), start_x+(80*scalex),start_y+(70*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(80*scalex), start_y+(70*scaley), start_x+(71*scalex),start_y+(53*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(71*scalex), start_y+(53*scaley), start_x+(75*scalex),start_y+(19*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(75*scalex), start_y+(19*scaley), start_x+(70*scalex),start_y+(10*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(70*scalex), start_y+(10*scaley), start_x+(55*scalex),start_y+(8*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(55*scalex), start_y+(8*scaley), start_x+(55*scalex),start_y+(3*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(55*scalex), start_y+(3*scaley), start_x+(63*scalex),start_y+(2*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(63*scalex), start_y+(2*scaley), start_x+(75*scalex),start_y+(3*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(75*scalex), start_y+(3*scaley), start_x+(79*scalex),start_y+(-1*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(79*scalex), start_y+(-1*scaley), start_x+(75*scalex),start_y+(-6*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(75*scalex), start_y+(-6*scaley), start_x+(61*scalex),start_y+(-7*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(61*scalex), start_y+(-7*scaley), start_x+(41*scalex),start_y+(-3*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(41*scalex), start_y+(-3*scaley), start_x+(42*scalex),start_y+(2*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(42*scalex), start_y+(2*scaley), start_x+(45*scalex),start_y+(4*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(45*scalex), start_y+(4*scaley), start_x+(45*scalex),start_y+(8*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(45*scalex), start_y+(8*scaley), start_x+(33*scalex),start_y+(8*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(33*scalex), start_y+(8*scaley), start_x+(33*scalex),start_y+(4*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(33*scalex), start_y+(4*scaley), start_x+(37*scalex),start_y+(-3*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(37*scalex), start_y+(-3*scaley), start_x+(4*scalex),start_y+(-6*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(4*scalex), start_y+(-6*scaley), start_x+(0*scalex),start_y+(0*scaley),COLORS[81],3,0,0,WINDOW);

  // eyes
  j_draw_circle(start_x+(30*scalex), start_y+(61*scaley), 7*scalex, COLORS[81],0,0,WINDOW);
  j_draw_circle(start_x+(46*scalex), start_y+(61*scaley), 7*scalex, COLORS[81],0,0,WINDOW);

  // eye brows
  j_draw_line(start_x+(23*scalex), start_y+(72*scaley), start_x+(33*scalex),start_y+(72*scaley),COLORS[81],5,0,0,WINDOW);
  j_draw_line(start_x+(43*scalex), start_y+(74*scaley), start_x+(53*scalex),start_y+(70*scaley),COLORS[81],5,0,0,WINDOW);

  // mouth
  j_draw_line(start_x+(22*scalex), start_y+(45*scaley), start_x+(26*scalex),start_y+(42*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(26*scalex), start_y+(42*scaley), start_x+(33*scalex),start_y+(44*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(33*scalex), start_y+(44*scaley), start_x+(39*scalex),start_y+(42*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(39*scalex), start_y+(42*scaley), start_x+(45*scalex),start_y+(45*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(45*scalex), start_y+(45*scaley), start_x+(52*scalex),start_y+(42*scaley),COLORS[81],3,0,0,WINDOW);
  j_draw_line(start_x+(52*scalex), start_y+(42*scaley), start_x+(56*scalex),start_y+(46*scaley),COLORS[81],3,0,0,WINDOW);

  // teeth
  j_draw_line(start_x+(31*scalex), start_y+(43*scaley), start_x+(32*scalex),start_y+(35*scaley),COLORS[81],2,0,0,WINDOW);
  j_draw_line(start_x+(32*scalex), start_y+(35*scaley), start_x+(36*scalex),start_y+(36*scaley),COLORS[81],2,0,0,WINDOW);
  j_draw_line(start_x+(36*scalex), start_y+(36*scaley), start_x+(37*scalex),start_y+(42*scaley),COLORS[81],2,0,0,WINDOW);

  j_draw_line(start_x+(43*scalex), start_y+(43*scaley), start_x+(44*scalex),start_y+(36*scaley),COLORS[81],2,0,0,WINDOW);
  j_draw_line(start_x+(44*scalex), start_y+(36*scaley), start_x+(49*scalex),start_y+(36*scaley),COLORS[81],2,0,0,WINDOW);
  j_draw_line(start_x+(49*scalex), start_y+(36*scaley), start_x+(50*scalex),start_y+(42*scaley),COLORS[81],2,0,0,WINDOW);
  }



  j_refresh_window(WINDOW);
}

void
draw_arrow( int X, int Y, std::string D )
{
  int start_x, start_y; //, end_x, end_y;
  //std::cout << X << " " <<Y <<std::endl;
  
  start_x = X * ROW_SIZE + 25;
  start_y = GY - ((Y+1)*ROW_SIZE);

  if( D=="east")
  {
        j_draw_line(start_x,start_y+45,start_x+30,start_y+45,COLORS[86],3,0,0,WINDOW);
        j_draw_line(start_x+25,start_y+50,start_x+30,start_y+45,COLORS[86],3,0,0,WINDOW);
        j_draw_line(start_x+25,start_y+40,start_x+30,start_y+45,COLORS[86],3,0,0,WINDOW);
  }
  else if( D=="south" )
  {
        j_draw_line(start_x+15,start_y+30,start_x+15,start_y+60,COLORS[86],3,0,0,WINDOW);
        j_draw_line(start_x+10,start_y+55,start_x+15,start_y+60,COLORS[86],3,0,0,WINDOW);
        j_draw_line(start_x+20,start_y+55,start_x+15,start_y+60,COLORS[86],3,0,0,WINDOW);
  }
  else if( D=="west" )
  {
        j_draw_line(start_x,start_y+45,start_x+30,start_y+45,COLORS[86],3,0,0,WINDOW);
        j_draw_line(start_x,start_y+45,start_x+5,start_y+40,COLORS[86],3,0,0,WINDOW);
        j_draw_line(start_x,start_y+45,start_x+5,start_y+50,COLORS[86],3,0,0,WINDOW);
  }
  else if( D=="north" )
  {
        j_draw_line(start_x+15,start_y+25,start_x+15,start_y+55,COLORS[86],3,0,0,WINDOW);
        j_draw_line(start_x+15,start_y+25,start_x+10,start_y+30,COLORS[86],3,0,0,WINDOW);
        j_draw_line(start_x+15,start_y+25,start_x+20,start_y+30,COLORS[86],3,0,0,WINDOW);
  }
  else
  {
     std::cout << "unknown direction parameter for draw_arrow(X,Y,D)!" << std::endl;
  }

  j_refresh_window(WINDOW);
}

void
draw_wumpus_hunter( int X, int Y, std::string D, int A )
{
  int start_x, start_y; //, end_x, end_y;
  //std::cout << X << " " <<Y <<std::endl;
  
  start_x = X * ROW_SIZE + 25;
  start_y = GY - ((Y+1)*ROW_SIZE);

  if( D=="east")
  {
     j_draw_line(start_x+5,start_y+35,start_x+5,start_y+55,COLORS[86],3,0,0,WINDOW);
     j_draw_line(start_x+5,start_y+55,start_x+25,start_y+45,COLORS[86],3,0,0,WINDOW);
     j_draw_line(start_x+5,start_y+35,start_x+25,start_y+45,COLORS[86],3,0,0,WINDOW);
  }
  else if( D=="south" )
  {
     j_draw_line(start_x+5,start_y+35,start_x+15,start_y+55,COLORS[86],3,0,0,WINDOW);
     j_draw_line(start_x+5,start_y+35,start_x+25,start_y+35,COLORS[86],3,0,0,WINDOW);
     j_draw_line(start_x+15,start_y+55,start_x+25,start_y+35,COLORS[86],3,0,0,WINDOW);
  }
  else if( D=="west" )
  {
     j_draw_line(start_x+5,start_y+45,start_x+25,start_y+55,COLORS[86],3,0,0,WINDOW);
     j_draw_line(start_x+5,start_y+45,start_x+25,start_y+35,COLORS[86],3,0,0,WINDOW);
     j_draw_line(start_x+25,start_y+55,start_x+25,start_y+35,COLORS[86],3,0,0,WINDOW);
  }
  else if( D=="north" )
  {
     j_draw_line(start_x+5,start_y+50,start_x+25,start_y+50,COLORS[86],3,0,0,WINDOW);
     j_draw_line(start_x+5,start_y+50,start_x+15,start_y+30,COLORS[86],3,0,0,WINDOW);
     j_draw_line(start_x+25,start_y+50,start_x+15,start_y+30,COLORS[86],3,0,0,WINDOW);
  }
  else
  {
     std::cout << "unknown direction parameter for draw_wumpus_hunter(X,Y,D,A)!" << std::endl;
  }
     

  if( A ) // hunter still has his arrow
  {
     draw_arrow(X, Y, D);
  }

  j_refresh_window(WINDOW);
}

void 
draw_human( int X, int Y)
{
//  std::cout << "display(draw_human): drawing human at (" << X << "," << Y << ")." <<std::endl;

  int start_x, start_y; //, end_x, end_y;
  //std::cout << X << " " <<Y <<std::endl;
  
  start_x = X * ROW_SIZE + 25;
  start_y = GY - ((Y+1)*ROW_SIZE);

  // head
  j_draw_line(start_x+9, start_y+9, start_x+15,start_y+5,COLORS[87],3,0,0,WINDOW);
  j_draw_line(start_x+9, start_y+9,start_x+15,start_y+13,COLORS[87],3,0,0,WINDOW);
  j_draw_line(start_x+15, start_y+5, start_x+21,start_y+9,COLORS[87],3,0,0,WINDOW);
  j_draw_line(start_x+15, start_y+13,start_x+21,start_y+9,COLORS[87],3,0,0,WINDOW);

  // neck
  j_draw_line(start_x+15,start_y+13,start_x+15,start_y+17,COLORS[87],3,0,0,WINDOW);

  // hands
  j_draw_line(start_x+9,  start_y+17, start_x+9,start_y+15, COLORS[87],3,0,0,WINDOW);
  j_draw_line(start_x+21, start_y+17, start_x+21,start_y+15,COLORS[87],3,0,0,WINDOW);

  // body
  j_draw_line(start_x+10, start_y+26, start_x+21,start_y+16,COLORS[87],3,0,0,WINDOW);
  j_draw_line(start_x+9, start_y+16, start_x+20,start_y+26,COLORS[87],3,0,0,WINDOW);

  // feet
  j_draw_line(start_x+8, start_y+27, start_x+10,start_y+27,COLORS[87],4,0,0,WINDOW);
  j_draw_line(start_x+20, start_y+27, start_x+22,start_y+27,COLORS[87],4,0,0,WINDOW);

  j_refresh_window(WINDOW);
}

void draw_gold( int X, int Y) {
  int start_x, start_y; //, end_x, end_y;
  //std::cout << X << " " <<Y <<std::endl;
  
  start_x = X * ROW_SIZE + 15;
  start_y = GY - ((Y+1)*ROW_SIZE) + 52;

  float scalex = 0.35;
  float scaley = -0.35;

  // lump of gold
  j_draw_line(start_x+(0*scalex), start_y+(0*scaley), start_x+(10*scalex),start_y+(25*scaley),COLORS[89],3,0,0,WINDOW);
  j_draw_line(start_x+(10*scalex), start_y+(25*scaley), start_x+(73*scalex),start_y+(25*scaley),COLORS[89],3,0,0,WINDOW);
  j_draw_line(start_x+(73*scalex), start_y+(25*scaley), start_x+(83*scalex),start_y+(0*scaley),COLORS[89],3,0,0,WINDOW);
  j_draw_line(start_x+(83*scalex), start_y+(0*scaley), start_x+(0*scalex),start_y+(0*scaley),COLORS[89],3,0,0,WINDOW);
  j_draw_line(start_x+(10*scalex), start_y+(25*scaley), start_x+(29*scalex),start_y+(31*scaley),COLORS[89],3,0,0,WINDOW);
  j_draw_line(start_x+(29*scalex), start_y+(31*scaley), start_x+(93*scalex),start_y+(31*scaley),COLORS[89],3,0,0,WINDOW);
  j_draw_line(start_x+(93*scalex), start_y+(31*scaley), start_x+(104*scalex),start_y+(5*scaley),COLORS[89],3,0,0,WINDOW);
  j_draw_line(start_x+(104*scalex), start_y+(5*scaley), start_x+(83*scalex),start_y+(0*scaley),COLORS[89],3,0,0,WINDOW);

  // glitter
  j_draw_line(start_x+(-13*scalex), start_y+(24*scaley), start_x+(-1*scalex),start_y+(18*scaley),COLORS[89],3,0,0,WINDOW);
  j_draw_line(start_x+(5*scalex), start_y+(44*scaley), start_x+(13*scalex),start_y+(32*scaley),COLORS[89],3,0,0,WINDOW);
  j_draw_line(start_x+(30*scalex), start_y+(51*scaley), start_x+(31*scalex),start_y+(37*scaley),COLORS[89],3,0,0,WINDOW);
  j_draw_line(start_x+(52*scalex), start_y+(55*scaley), start_x+(52*scalex),start_y+(39*scaley),COLORS[89],3,0,0,WINDOW);
  j_draw_line(start_x+(75*scalex), start_y+(51*scaley), start_x+(73*scalex),start_y+(39*scaley),COLORS[89],3,0,0,WINDOW);
  j_draw_line(start_x+(106*scalex), start_y+(45*scaley), start_x+(97*scalex),start_y+(34*scaley),COLORS[89],3,0,0,WINDOW);
  j_draw_line(start_x+(120*scalex), start_y+(24*scaley), start_x+(107*scalex),start_y+(18*scaley),COLORS[89],3,0,0,WINDOW);


  j_refresh_window(WINDOW);
}

void draw_item( int X, int Y) {
  int start_x, end_x, start_y, end_y;

  //std::cout << X << " " <<Y <<std::endl;

  start_x = X * ROW_SIZE + 5;
  end_x = ROW_SIZE - 35 ;
  start_y = GY - (Y+1) * ROW_SIZE + 5;
  end_y = ROW_SIZE - 35;

  //  std::cout << start_x << " " << start_y << " " << end_x << " " << end_y << std::endl;

  j_draw_rectangle(start_x, start_y, end_x, end_y, COLORS[84] ,7,0,WINDOW);
  j_refresh_window(WINDOW);
}

void draw_goal( int X, int Y) {
  int start_x, end_x, start_y, end_y;

  //std::cout << X << " " <<Y <<std::endl;

  start_x = X * ROW_SIZE;
  end_x = ROW_SIZE;
  start_y = GY - (Y+1) * ROW_SIZE;
  end_y = ROW_SIZE;

  //  std::cout << start_x << " " << start_y << " " << end_x << " " << end_y << std::endl;

  j_draw_rectangle(start_x, start_y, end_x, end_y, COLORS[81] ,5,0,WINDOW);
  j_refresh_window(WINDOW);
}

void draw_start( int X, int Y) {
  int start_x, end_x, start_y, end_y;

  start_x = X * ROW_SIZE;
  end_x = ROW_SIZE;
  start_y = GY - (Y+1) * ROW_SIZE;
  end_y = ROW_SIZE;

  j_draw_rectangle(start_x, start_y, end_x, end_y, COLORS[82],5,0,WINDOW);
  j_refresh_window(WINDOW);
}


void draw_columns()
{
  int i,start_x,start_y,end_x,end_y;

  for (i=1; i<=SIZE-1; i++)
    {
      start_x = GRID_X + i*ROW_SIZE;
      start_y = GRID_Y;
      end_x = start_x;
      end_y = GRID_Y + GRID_SIZE;
      j_draw_line(start_x,start_y,end_x,end_y,COLORS[60],3,0,0,WINDOW);
    }
}

void draw_rows()
{
  int i,start_x,start_y,end_x,end_y;

  for (i=1; i<= SIZE-1; i++)
    {
      start_x = GRID_X;
      start_y = GRID_X + i*ROW_SIZE;
      end_x = start_x + GRID_SIZE;
      end_y = start_y;
      j_draw_line(start_x,start_y,end_x,end_y,COLORS[60],3,0,0,WINDOW);
    }
}

void draw_world() /* draw grid world */
{
  j_draw_rectangle(GRID_X,GRID_Y,GRID_X+GRID_SIZE,GRID_Y+GRID_SIZE,
		   COLORS[20],1,0,WINDOW); /* borders */
  draw_shades();
  draw_columns();
  draw_rows();
  draw_walls();
  draw_pits();
  draw_breezes();
  draw_stench();
//  draw_cells();
}

void draw_actions() /* draw history of actions */
{
  for ( unsigned int i = 0; i < Actions.size(); ++i ) {
    //cout << " GridVis: redrawing action #" << i << " ("
    //	 << Actions[i].action << "," << Actions[i].X 
    //	 << "," << Actions[i].Y << ")." << endl;
    if( Actions[i].action == GOTOR || Actions[i].action == GOTOG ) {
      draw_goto( Actions[i].action, Actions[i].toX, Actions[i].toY, Actions[i].X, Actions[i].Y );
    }
    else if (Actions[i].action ==TLEFT || Actions[i].action == TRIGHT ) {
      draw_turn( Actions[i].action, Actions[i].X, Actions[i].Y );
    }
    else {
      draw_dir( Actions[i].action, Actions[i].X, Actions[i].Y );
    }
  }
}

void redraw()
{
  j_clear_window(WINDOW);
  draw_world();
  draw_actions();
  //draw_shades();
}

void clear_history()
{
  Actions.clear();
}

void clear()
{
  Actions.clear();
  j_clear_window(WINDOW);
}

int state_x_location(STATE s)
{
  return(GRID_X + s.col*ROW_SIZE);
}

int state_y_location(STATE s)
{
  return(GRID_Y + s.row*ROW_SIZE);
}

void draw_goal(STATE g)
{
  int goal_x,goal_y;

  goal_x = state_x_location(g);
  goal_y = state_y_location(g);

  j_draw_rectangle(goal_x,goal_y,ROW_SIZE, ROW_SIZE,
		   COLORS[5],0,1,WINDOW); 
}

void draw_dir(ACTION a, int X, int Y) {
  
  //  std::cout << X<< " " << Y << std::endl;
  int start_x, start_y;
  
  start_x = X*ROW_SIZE;
  start_y = GY - ((Y+1)*ROW_SIZE);
  switch (a) {
  case NORTH: 
    j_draw_line(start_x+5,start_y+55,start_x+15,start_y+35,COLORS[81],3,0,0,WINDOW);
    j_draw_line(start_x+15,start_y+35,start_x+25,start_y+55,COLORS[81],3,0,0,WINDOW);
    break;
  case EAST: 
    j_draw_line(start_x+5,start_y+35,start_x+25,start_y+45,COLORS[81],3,0,0,WINDOW);
    j_draw_line(start_x+5,start_y+55,start_x+25,start_y+45,COLORS[81],3,0,0,WINDOW);
    break;
  case WEST: 
    j_draw_line(start_x+25,start_y+35,start_x+5,start_y+45,COLORS[81],3,0,0,WINDOW);
    j_draw_line(start_x+25,start_y+55,start_x+5,start_y+45,COLORS[81],3,0,0,WINDOW);
    break;
  case SOUTH: 
    j_draw_line(start_x+5,start_y+35,start_x+15,start_y+55,COLORS[81],3,0,0,WINDOW);
    j_draw_line(start_x+25,start_y+35,start_x+15,start_y+55,COLORS[81],3,0,0,WINDOW);
    break;
  case REST: 
    j_draw_line(start_x+25,start_y+35,start_x+5,start_y+55,COLORS[81],3,0,0,WINDOW);
    j_draw_line(start_x+25,start_y+55,start_x+5,start_y+35,COLORS[81],3,0,0,WINDOW);
    break;
  case GOTOR: 
  case GOTOG: 
  case TLEFT: 
  case TRIGHT: 
    break;
  }

  j_refresh_window(WINDOW);
  
}

void draw_turn(ACTION a, int X, int Y) {
  
  //  std::cout << X<< " " << Y << std::endl;
/*  int start_x, start_y;
  
  start_x = X*ROW_SIZE;
  start_y = GY - ((Y+1)*ROW_SIZE);

  switch (a) {
  case TLEFT: 
    j_draw_line(start_x+25,start_y+35,start_x+5,start_y+45,COLORS[80],3,0,0,WINDOW);
    j_draw_line(start_x+25,start_y+55,start_x+5,start_y+45,COLORS[80],3,0,0,WINDOW);
    break;
  case TRIGHT: 
    j_draw_line(start_x+5,start_y+35,start_x+25,start_y+45,COLORS[80],3,0,0,WINDOW);
    j_draw_line(start_x+5,start_y+55,start_x+25,start_y+45,COLORS[80],3,0,0,WINDOW);
    break;
  }
*/
  j_refresh_window(WINDOW);
  
}

void draw_goto(ACTION a, int toX, int toY, int X, int Y ) {
  
  //  std::cout << X<< " " << Y << std::endl;
  int start_x, start_y;
  
  start_x = X*ROW_SIZE;
  start_y = GY - ((Y+1)*ROW_SIZE);

  char buff [128];
  sprintf( buff, "(%i,%i)", toX, toY );

  switch (a) {
  case NORTH: 
  case EAST: 
  case WEST: 
  case SOUTH: 
  case REST: 
  case TLEFT: 
  case TRIGHT: 
    break;
  case GOTOR: 
    j_draw_string(start_x+5,start_y+35, (char *)"GR", COLORS[81],WINDOW);
    j_draw_string(start_x+5,start_y+55, buff, COLORS[81],WINDOW);
    break;
  case GOTOG: 
    j_draw_string(start_x+5,start_y+35, (char *)"GG", COLORS[81],WINDOW);
    j_draw_string(start_x+5,start_y+55, buff, COLORS[81],WINDOW);
    break;
  }

  j_refresh_window(WINDOW);
  
}


void draw_opt_vals() {

  for( unsigned int i = 0; i < OptPolicy.size(); i++) {
    //    std::cout << OptPolicy[i].X << std::endl;
    int start_x = OptPolicy[i].X * ROW_SIZE;
    int start_y = GY - ((OptPolicy[i].Y + 1) *ROW_SIZE);
    
    switch (OptPolicy[i].action) {
    case NORTH: 
      j_draw_line(start_x+5,start_y+25,start_x+15,start_y+5,COLORS[60],3,0,0,WINDOW);
      j_draw_line(start_x+15,start_y+5,start_x+25,start_y+25,COLORS[60],3,0,0,WINDOW);
      break;
    case EAST: 
      j_draw_line(start_x+5,start_y+5,start_x+25,start_y+15,COLORS[60],3,0,0,WINDOW);
      j_draw_line(start_x+5,start_y+25,start_x+25,start_y+15,COLORS[60],3,0,0,WINDOW);
      break;
    case WEST: 
      j_draw_line(start_x+25,start_y+5,start_x+5,start_y+15,COLORS[60],3,0,0,WINDOW);
      j_draw_line(start_x+25,start_y+25,start_x+5,start_y+15,COLORS[60],3,0,0,WINDOW);
      break;
    case SOUTH: 
      j_draw_line(start_x+5,start_y+5,start_x+15,start_y+25,COLORS[60],3,0,0,WINDOW);
      j_draw_line(start_x+25,start_y+5,start_x+15,start_y+25,COLORS[60],3,0,0,WINDOW);
      break;
    case REST: 
      j_draw_line(start_x+25,start_y+35,start_x+5,start_y+55,COLORS[60],3,0,0,WINDOW);
      j_draw_line(start_x+25,start_y+55,start_x+5,start_y+35,COLORS[60],3,0,0,WINDOW);
      break;
    case GOTOR: 
    case GOTOG: 
    case TLEFT: 
    case TRIGHT: 
      break;
    }
    
    
  }
  j_refresh_window(WINDOW);

}


void draw_pol(ACTION a, int X, int Y) {
  
  //  std::cout << X<< " " << Y << std::endl;
  int start_x, start_y;
  
  start_x = X*ROW_SIZE;
  start_y = GY - ((Y+1)*ROW_SIZE);

  switch (a) {
  case NORTH: 
    j_draw_line(start_x+5,start_y+25,start_x+15,start_y+5,COLORS[83],3,0,0,WINDOW);
    j_draw_line(start_x+15,start_y+5,start_x+25,start_y+25,COLORS[83],3,0,0,WINDOW);
    break;
  case EAST: 
    j_draw_line(start_x+5,start_y+5,start_x+25,start_y+15,COLORS[83],3,0,0,WINDOW);
    j_draw_line(start_x+5,start_y+25,start_x+25,start_y+15,COLORS[83],3,0,0,WINDOW);
    break;
  case WEST: 
    j_draw_line(start_x+25,start_y+5,start_x+5,start_y+15,COLORS[83],3,0,0,WINDOW);
    j_draw_line(start_x+25,start_y+25,start_x+5,start_y+15,COLORS[83],3,0,0,WINDOW);
    break;
  case SOUTH: 
    j_draw_line(start_x+5,start_y+5,start_x+15,start_y+25,COLORS[83],3,0,0,WINDOW);
    j_draw_line(start_x+25,start_y+5,start_x+15,start_y+25,COLORS[83],3,0,0,WINDOW);
    break;
  case REST: 
    j_draw_line(start_x+25,start_y+35,start_x+5,start_y+55,COLORS[83],3,0,0,WINDOW);
    j_draw_line(start_x+25,start_y+55,start_x+5,start_y+35,COLORS[83],3,0,0,WINDOW);
    break;
  case GOTOR: 
  case GOTOG: 
  case TLEFT: 
  case TRIGHT: 
    break;
  }

  j_refresh_window(WINDOW);
  
}


void draw_direction(ACTION a, STATE s)
{
  int start_x, start_y;

  start_x = state_x_location(s);
  start_y = state_y_location(s);

  switch (a) {
  case NORTH: 
    j_draw_line(start_x+5,start_y+25,start_x+15,start_y+5,COLORS[60],3,0,0,WINDOW);
    j_draw_line(start_x+15,start_y+5,start_x+25,start_y+25,COLORS[60],3,0,0,WINDOW);
    break;
  case EAST: 
    j_draw_line(start_x+5,start_y+5,start_x+25,start_y+15,COLORS[60],3,0,0,WINDOW);
    j_draw_line(start_x+5,start_y+25,start_x+25,start_y+15,COLORS[60],3,0,0,WINDOW);
    break;
  case WEST: 
    j_draw_line(start_x+25,start_y+5,start_x+5,start_y+15,COLORS[60],3,0,0,WINDOW);
    j_draw_line(start_x+25,start_y+25,start_x+5,start_y+15,COLORS[60],3,0,0,WINDOW);
    break;
  case SOUTH: 
    j_draw_line(start_x+5,start_y+5,start_x+15,start_y+25,COLORS[60],3,0,0,WINDOW);
    j_draw_line(start_x+25,start_y+5,start_x+15,start_y+25,COLORS[60],3,0,0,WINDOW);
    break;
  case REST: 
    j_draw_line(start_x+25,start_y+35,start_x+5,start_y+55,COLORS[60],3,0,0,WINDOW);
    j_draw_line(start_x+25,start_y+55,start_x+5,start_y+35,COLORS[60],3,0,0,WINDOW);
    break;
  case GOTOR: 
  case GOTOG: 
  case TLEFT: 
  case TRIGHT: 
    break;
  }
}

void display_policy(STATE g)
{

//   XEvent event;
//   int r,c,x,y;
//   STATE s;
//   ACTION a;

//   for (r=0;r<SIZE; r++)
//     for (c=0; c<SIZE; c++)
//       {
// 	s.row = r;
// 	s.col = c;
// 	a = best_qvalue_action(s);
// 	if ((g.row != r) || (g.col !=c))
// 	  draw_direction(a,s);
//       }
//   while (1) {
//     XNextEvent(WINDOW->disp,&event);    
//     switch (event.type) {
//     case ButtonPress: 
//       x = event.xbutton.x; 
//       y = event.xbutton.y;  
//       s.row = convert_xloc_to_row(y);
//       s.col = convert_xloc_to_row(x);
//       //display_state_statistics(s); 
//       break;
//     }    
//   }
} 

int convert_xloc_to_row(int y)
{
  int rs;
  rs = ROW_SIZE;
  return(y/rs);
}

int convert_yloc_to_col(int x)
{
  int rs;
  rs = ROW_SIZE;
  return(x/rs);
}
  

STATE get_goal_from_user()
{
  XEvent event;
  int x,y,done=0;
  STATE goal;

  printf("click on square to indicate goal\n");
  while (!done) {
    XNextEvent(WINDOW->disp,&event);    
    switch (event.type) {
    case ButtonPress: done=1; x= event.xbutton.x; y = event.xbutton.y; break;
    }
  }
/*  printf("clicked on x: %d y: %d\n", x, y); */
  goal.row = convert_xloc_to_row(y);
  goal.col = convert_yloc_to_col(x);
  printf("goal row: %d col: %d\n", goal.row, goal.col);
  return(goal);
}
  



void start_display(int X, int Y) {

  GX=(X+1)*SIZE_OF_GRIDCELL;
  GY=(Y+1)*SIZE_OF_GRIDCELL;

  init_win( (X +1) * SIZE_OF_GRIDCELL , (Y+1) * SIZE_OF_GRIDCELL );
  
  draw_world();
  
  j_name_window(WINDOW, (char *)"Grid World");
}
	
// STATE start_display()
// {

//   XEvent event;
//   STATE g;

//   init_win(WINDOW_WIDTH,WINDOW_HEIGHT); /* pop up a window to draw world */

//   draw_world();

//   XFlush(WINDOW->disp);

//   XSelectInput(WINDOW->disp,WINDOW->win,
// 	   ButtonPressMask|ButtonMotionMask|ButtonReleaseMask|ExposureMask);

//   j_name_window(WINDOW, "Grid World");

//   g = get_goal_from_user();

//   draw_goal(g);

//   return(g);
//}



//}; // extern "C"
