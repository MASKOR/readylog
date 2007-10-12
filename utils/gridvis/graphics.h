/* Header file for basic graphics operations */

/* Written by Jon Connell, April 1993       */
/* Copyright 1993, IBM, All Rights Reserved */

//extern "C" {

#include <X11/Xlib.h>

#ifndef _BASIC_GRAPHICS_
#define _BASIC_GRAPHICS_

// /* ===================================================== */

// /* remember server, actual window, and graphics defaults */

#define FALSE 0

typedef struct
{
  Display *disp;
  Window win;
  GC gcon;
  unsigned long fg;
} jwin;


#endif


/* ===================================================== */

/* function prototypes */

jwin *j_make_window (int width, int height, char *color,
		     char *host, char *font, int expose);
unsigned long find_color_val (char *name, jwin *w);
unsigned long window_color (jwin *w);
void window_size (jwin *w, int *wd, int *ht);

void j_kill_window (jwin *w);
void j_clear_window (jwin *w);
void j_refresh_window (jwin *w);
void j_shape_window (jwin *w, int wd, int ht);
void j_move_window (jwin *w, int x, int y);
void j_name_window (jwin *w, char *title);

void j_draw_line (int x1, 
		  int y1, 
		  int x2, 
		  int y2,
		  unsigned long color, 
		  int thickness, 
		  int dashed, 
		  int Xor,
		  jwin *w);
void j_draw_rectangle (int xlo, int ylo, int width, int height,
		       unsigned long color, int thickness, int Xor,
		       jwin *w);
void j_draw_circle (int xcent, int ycent, int radius,
		    unsigned long color, int thickness, int Xor,
		    jwin *w);
void j_draw_ellipse (int xcent, int ycent,
		     int rmax, int rmin, float amax, int n,
		     unsigned long color, int thickness, int Xor,
		     jwin *w);

void j_draw_title (int xd, int yd, char *title, int bd,
		   unsigned long color, jwin *w);
void j_draw_string (int xleft, int ybase, char *text,
		    unsigned long color, jwin *w);
void string_box (char *text, int *wd, int *up, int *dn, jwin *w);
void change_font (char *family, int size,
		  int bold, int italic, jwin *w);

void j_draw_image (int x, int y, char *data,
		   unsigned int wd, unsigned int ht,
		   int force, jwin *w);

int mouse_coords (int *x, int *y, int *button, jwin *w);
int mouse_click (int *x, int *y, jwin *w);




//}; // extern "C"
