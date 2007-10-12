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
 *           $Id:$
 *        author: Alexander Ferrein <ferrein@cs.rwth-aachen.de>
 *   description: 
 *
 * ************************************************************************ */

//extern "C" {

#ifndef _decl_display_h_
#define _decl_display_h_

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "graphics.h" /* include graphics header file */
#include "grid-world.h"
//#include "qlearner.h"
#include <vector>
#include <string>

#define MAX_COLORS 100  /* lots of colors */

#define WINDOW_WIDTH 1200
#define WINDOW_HEIGHT 1200
#define GRID_X 0
#define GRID_Y 0
#define GRID_SIZE WINDOW_WIDTH
#define ROW_SIZE GRID_SIZE/SIZE
#define OFFSET ROW_SIZE/2

#define SIZE_OF_GRIDCELL WINDOW_WIDTH/SIZE


typedef struct {
  int x1,y1,x2,y2;
} wall_t;

typedef struct {
  int X; 
  int Y; 
  std::string act; 
  ACTION action;
  float value; 
  float prob;
} value_t;


extern std::vector<value_t> OptPolicy;
extern std::vector<value_t> Values;
extern std::vector<wall_t> Walls;

extern void start_display(int,  int );


extern jwin *WINDOW; ;/* define reference to a global variable for the window */

static int COLORS[MAX_COLORS];/* define an array for storing colors */

void init_win(int wd, int ht);

void draw_columns();

void draw_rows();

void draw_world(); /* draw grid world */

int state_x_location(STATE s);

int state_y_location(STATE s);

void draw_goal(STATE g);

void draw_direction(ACTION a, STATE s);

void display_policy(STATE g);

void start_up_window();

int convert_xloc_to_row(int x);

int convert_yloc_to_col(int y);

STATE get_goal_from_user();

void draw_dir(ACTION a, int X, int y);
void draw_pol(ACTION a, int X, int Y);
void draw_start( int X, int Y) ;
void draw_goal( int X, int Y);
void draw_opt_vals();
#endif
 

//}; // extern "C"
