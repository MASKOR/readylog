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
  WINDOW = j_make_window(wd, ht, "white", NULL, NULL, 1);

  printf("\nAllocating colors ..."); /* allocate gray values */
  fflush(stdout);
  for (i=0; i <= MAX_COLORS-1; i++)
    {
      sprintf(name, "gray%d", (MAX_COLORS - 1) - i);
      COLORS[i] = (int) find_color_val(name, WINDOW);
    } 
  COLORS[81] = (int) find_color_val("red", WINDOW);
  COLORS[82] = (int) find_color_val("navy", WINDOW);
  COLORS[83] = (int) find_color_val("yellow", WINDOW);
  COLORS[84] = (int) find_color_val("cyan", WINDOW);
  COLORS[85] = (int) find_color_val("green", WINDOW);
  COLORS[86] = (int) find_color_val("brown", WINDOW);
  COLORS[87] = (int) find_color_val("magenta", WINDOW);
  printf("\n");

}

void draw_walls() {
  int start_x, start_y, end_x, end_y;

  std::cout << "display(draw_walls): drawing " <<   Walls.size() << " walls." <<std::endl;

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
draw_cells() {
  int draw_x, draw_y;
  
  std::cout << "display(draw_cells): drawing " <<   Cells.size() << " cells." <<std::endl;

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
draw_human( int X, int Y)
{
  std::cout << "display(draw_human): drawing human at (" << X << "," << Y << ")." <<std::endl;

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
  draw_columns();
  draw_rows();
  draw_walls();
  draw_cells();
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
    break;
  }

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
    break;
  case GOTOR: 
    j_draw_string(start_x+5,start_y+35, "GR", COLORS[81],WINDOW);
    j_draw_string(start_x+5,start_y+55, buff, COLORS[81],WINDOW);
    break;
  case GOTOG: 
    j_draw_string(start_x+5,start_y+35, "GG", COLORS[81],WINDOW);
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
  
  j_name_window(WINDOW, "Grid World");
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
