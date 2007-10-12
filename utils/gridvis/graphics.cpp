/* Some simple graphics primitives coded for X Windows.     */
/* Must be linked with "/usr/local/X11R5/lib/libX11.a"      */
/* Must be linked with "/lib/libm.a" to get math functions. */
/* Altered so that most functions do not call XFlush.       */

/* Written by Jon Connell, April 1993       */
/*   revised February 1995                  */
/* Copyright 1993, IBM, All Rights Reserved */

#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include <math.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

#include "graphics.h"


//extern "C" {

/* ======================================================== */

/* Make a window on the specified host and then initialize    */
/* graphical attributes to standard values. If host if NULL,  */
/* value of Unix DISPLAY variable is used. Type "xcolors" at  */
/* shell to see possible names (NULL = "paleturquoise"). Type */
/* "xfontsel" to see possible type fonts (NULL = Times-12).   */

jwin *j_make_window (int width, int height, char *color,
		     char *host, char *font, int expose)
{
  XSetWindowAttributes attr;
  Display *disp;
  Window win;
  GC gcon;
  XGCValues vals;
  jwin *ans;
  unsigned long amask, vmask, bg;
  char wcol[80] = "paleturquoise";
  char style[80] = "-*-times-medium-r-*-12-*";

  if ((ans = (jwin *) malloc(sizeof(jwin))) == NULL)
  {
    fprintf(stderr,
	    "\nJ_MAKE_WINDOW: Unable to allocate structure!\n");
    exit(-1);
  }

  if ((disp = XOpenDisplay(host)) == NULL)
  {
    fprintf(stderr,
	    "\nJ_MAKE_WINDOW: Cannot connect to host %s!\n",
	    XDisplayName(host));
    exit(-2);
  }
	
  attr.bit_gravity = NorthWestGravity;
/* preserve window contents */
  attr.backing_store = Always;
  attr.event_mask = ButtonPressMask | ExposureMask;
  attr.do_not_propagate_mask = ButtonPressMask;
  amask = CWBitGravity | CWEventMask | CWDontPropagate | CWBackingStore;

  win = XCreateWindow(disp, 
		      DefaultRootWindow(disp),//disp->screens->root,
		      2, 
		      2, 
		      width, 
		      height, 
		      0,
		      CopyFromParent, 
		      InputOutput, 
		      CopyFromParent,
		      amask, 
		      &attr);

  if (font != NULL) strcpy(style, font);
  vals.font = XLoadFont(disp, style);
  vals.cap_style = CapRound;
  vals.join_style = JoinRound;
  vmask = GCFont | GCCapStyle | GCJoinStyle;
  gcon = XCreateGC(disp, win, vmask, &vals);

  ans->disp = disp;
  ans->win = win;
  ans->gcon = gcon;
  ans->fg = find_color_val("black", ans);

  if (color != NULL) strcpy(wcol, color);
  bg = find_color_val(wcol, ans);
  XSetWindowBackground(disp, win, bg);
  XSetBackground(disp, gcon, bg);
  XStoreName(disp, win, "random");
  XMapWindow(disp, win);
  XClearWindow(disp, win);
  if (expose != 0) XFlush(disp);

  return(ans);
}


/* Find number of color closest to the one requested.   */
/* To see color value strings, type "xcolors" at shell. */

unsigned long find_color_val (char *name, jwin *w)
{
  XColor cell, exact;
  Display *disp;
  //  unsigned short er, eg, eb, cr, cg, cb;

  disp = w->disp;
  Colormap screen_colormap = DefaultColormap(disp, DefaultScreen(disp));


  //    if (XAllocNamedColor(disp, (disp->screens)->cmap, name, &cell, &exact)
    if (XAllocNamedColor(disp, screen_colormap, name, &cell, &exact)
        == 0)
    printf("\nFIND_COLOR_VAL: Could not allocate color %s!\n", name);
/*
  er = exact.red & 0xFF;
  eg = exact.green & 0xFF;
  eb = exact.blue & 0xFF;
  cr = cell.red & 0xFF;
  cg = cell.green & 0xFF;
  cb = cell.blue & 0xFF;
  if ((er != cr) || (eg != cg) || (eb != cb))
    printf("Color %s needs: (%u %u %u), got: (%u %u %u)\n",
	   name, er, eg, eb, cr, cg, cb);
*/
  return(cell.pixel);
}


/* Useful for erasing stuff (i.e. draw in this color) */

unsigned long window_color (jwin *w)
{
  XGCValues vals;
  unsigned long vmask;

  vmask = GCBackground;
  XGetGCValues(w->disp, w->gcon, vmask, &vals);
  return(vals.background);
}


/* find the current size of the window */

void window_size (jwin *w, int *wd, int *ht)
{
  XWindowAttributes attr;

  XGetWindowAttributes(w->disp, w->win, &attr);
  *wd = attr.width;
  *ht = attr.height;
}


/* ------------------------------------------------------- */

/* get rid of old window and update screen */

void j_kill_window (jwin *w)
{
  XDestroyWindow(w->disp, w->win);
  XFlush(w->disp);
  free(w);
}


/* erase whatever is drawn */

void j_clear_window (jwin *w)
{
  XClearWindow(w->disp, w->win);
  XFlush(w->disp);
}


/* make sure pending requests are drawn */

void j_refresh_window (jwin *w)
{
  XFlush(w->disp);
}


/* change the size of a window */

void j_shape_window (jwin *w, int wd, int ht)
{
  unsigned int vmask;
  XWindowChanges vals;

  vals.width = wd;
  vals.height = ht;
  vmask = CWWidth | CWHeight;
  XConfigureWindow(w->disp, w->win, vmask, &vals);
  XFlush(w->disp);
}


/* change the position of a window */

void j_move_window (jwin *w, int x, int y)
{
  unsigned int vmask;
  XWindowChanges vals;

  vals.x = x;
  vals.y = y;
  vmask = CWX | CWY;
  XConfigureWindow(w->disp, w->win, vmask, &vals);
  XFlush(w->disp);
}


/* assign a title to a window after it has been created */

void j_name_window (jwin *w, char *title)
{
  XStoreName(w->disp, w->win, title);
  XFlush(w->disp);
}


/* --------------------------------------------------------------- */

/* draw a line with some more variations    */
/* a negative color value means XOR drawing */

void j_draw_line (int x1, int y1, int x2, int y2,
		  unsigned long color, int thickness, int dashed, int Xor,
		  jwin *w)
{
  XGCValues vals;
  unsigned long vmask;

  if (Xor != 0)
    vals.function = GXinvert;
  else
    vals.function = GXcopy;
  if (dashed != 0)
    vals.line_style = LineOnOffDash;
  else
    vals.line_style = LineSolid;
  vals.foreground = color;
  vals.line_width = thickness;
  vmask = GCForeground | GCLineWidth | GCLineStyle | GCFunction;
  XChangeGC(w->disp, w->gcon, vmask, &vals);

  XDrawLine(w->disp, w->win, w->gcon, x1, y1, x2, y2);
/*  XFlush(w->disp); */
}


/* draw a box with some more parameters     */
/* thickness 0 means fill it                */

void j_draw_rectangle (int xlo, int ylo, int width, int height,
		       unsigned long color, int thickness, int Xor,
		       jwin *w)
{
  XGCValues vals;
  unsigned long vmask;

  if (Xor != 0)
    vals.function = GXinvert;
  else
    vals.function = GXcopy;
  vals.foreground = color;
  vals.line_width = thickness;
  vmask = GCForeground | GCLineWidth | GCFunction;
  XChangeGC(w->disp, w->gcon, vmask, &vals);

  if (thickness == 0)
    XFillRectangle(w->disp, w->win, w->gcon,
		   xlo, ylo, width+1, height+1);
  else
    XDrawRectangle(w->disp, w->win, w->gcon,
		   xlo, ylo, width, height);
/*  XFlush(w->disp); */
}


/* draw a circle with some more parameters  */
/* thickness 0 means fill it                */

void j_draw_circle (int xcent, int ycent, int radius,
		    unsigned long color, int thickness, int Xor,
		    jwin *w)
{
  XGCValues vals;
  unsigned long vmask;

  if (Xor != 0)
    vals.function = GXinvert;
  else
    vals.function = GXcopy;
  vals.foreground = color;
  vals.line_width = thickness;
  vmask = GCForeground | GCLineWidth | GCFunction;
  XChangeGC(w->disp, w->gcon, vmask, &vals);

  if (thickness == 0)
    XFillArc(w->disp, w->win, w->gcon,
	     xcent-radius, ycent-radius, 2*radius, 2*radius,
	     0, 360*64);
  else
    XDrawArc(w->disp, w->win, w->gcon,
	     xcent-radius, ycent-radius, 2*radius, 2*radius,
	     0, 360*64);
/*  XFlush(w->disp); */
}


/* draw an approximate ellipse (zero thickness = filled) */
/* amax is the angle (in degrees) of the major axis      */
/* n tells number of angular steps (e.g. 8) per quadrant */

void j_draw_ellipse (int xcent, int ycent,
		     int rmax, int rmin, float amax, int n,
		     unsigned long color, int thickness, int Xor,
		     jwin *w)
{
  XGCValues vals;
  unsigned long vmask;
  float xfpts[81], yfpts[81];
  XPoint pts[81];
  float da, bssq, ang, r00, r01, r10, r11;
  float ta, tabs, x, y;
  int i, pmax, phaf;

  /* derive total number of points and angular step size */
  if ((pmax = 4 * n) >= 81)
  {
    fprintf(stderr, "\nJ_DRAW_ELLIPSE: More than 80 points!\n");
    exit(-1);
  }
  pmax = 4 * n;
  phaf = 2 * n;
  da = 3.141592653 / phaf;
  bssq = (float)(rmax * rmax) / (float)(rmin * rmin);
  ang = (3.141592653 / -180.0) * amax;
  r00 = cos(ang);
  r01 = sin(ang);
  r10 = -r01;
  r11 = r00;

  /* build axis-aligned ellipse of the right size and shape */
  for (i=0; i <= n; i++)
  {
    ta = (float) tan(i * da);
    tabs = ta * ta * bssq;
    x = ((float) rmax) / sqrt(tabs + 1.0);
    y = ((float) rmin) / sqrt(1.0 + (1.0 / tabs));
    xfpts[i] = x;
    yfpts[i] = y;
    xfpts[phaf - i] = -x;
    yfpts[phaf - i] = y;
    xfpts[phaf + i] = -x;
    yfpts[phaf + i] = -y;
    xfpts[pmax - i] = x;
    yfpts[pmax - i] = -y;
  }

  /* rotate and translate base ellipse, then close curve */
  for (i=0; i < pmax; i++)
  {
    x = xfpts[i];
    y = yfpts[i];
    pts[i].x = (short)(0.5 + xcent + (r00 * x) + (r10 * y));
    pts[i].y = (short)(0.5 + ycent + (r01 * x) + (r11 * y));
  }
  pts[pmax].x = pts[0].x;
  pts[pmax].y = pts[0].y;

  /* draw collection of points as a polygon */
  if (Xor != 0)
    vals.function = GXinvert;
  else
    vals.function = GXcopy;
  vals.foreground = color;
  vals.line_width = thickness;
  vmask = GCForeground | GCLineWidth | GCFunction;
  XChangeGC(w->disp, w->gcon, vmask, &vals);
  if (thickness == 0)
    XFillPolygon(w->disp, w->win, w->gcon,
		 pts, pmax+1, Convex, CoordModeOrigin);
  else
    XDrawLines(w->disp, w->win, w->gcon,
	       pts, pmax+1, CoordModeOrigin);
/*  XFlush(w->disp); */
}


/* ------------------------------------------------------------- */


/* clears a box to the background color then writes string */
/* (xd yd) is the LOWER left corner for the whole label    */
/* bd is the size of the border area in pixels             */

void j_draw_title (int xd, int yd, char *title, int bd,
		   unsigned long color, jwin *w)
{
  unsigned long bg;
  int tw, tu, td, b2;

  bg = window_color(w);
  b2 = 2 * bd;
  string_box(title, &tw, &tu, &td, w);
/*
  j_draw_rectangle(xd-bd-1, yd-td-tu-b2-2, tw+b2+1, td+tu+b2,
		   bg, 0, FALSE, w);
  j_draw_string(xd, yd-bd-td-1, title, color, w);
*/
  j_draw_rectangle(xd, yd-td-tu-b2, tw+b2, td+tu+b2,
		   bg, 0, FALSE, w);
  j_draw_string(xd+bd, yd-bd-td, title, color, w);

}


/* draw a string with some adjustable attributes */
/* always uses default font for window           */

void j_draw_string (int xleft, int ybase, char *text,
		    unsigned long color, jwin *w)
{
  XGCValues vals;

  vals.foreground = color;
  XChangeGC(w->disp, w->gcon, GCForeground, &vals);
  XDrawString(w->disp, w->win, w->gcon, xleft, ybase,
	      text, strlen(text));
/*  XFlush(w->disp); */
}


/* find the pixel size of a string in the standard font */

void string_box (char *text, int *wd, int *up, int *dn, jwin *w)
{
  XGCValues gvals;
  XCharStruct overall;
  int dir, ascent, descent;

  XGetGCValues(w->disp, w->gcon, GCFont, &gvals);
  XQueryTextExtents(w->disp, gvals.font, text, strlen(text),
		    &dir, &ascent, &descent, &overall);
  *wd = overall.width;
  *up = overall.ascent;
  *dn = overall.descent;
}


/* For changing the size of strings drawn           */
/* family is "times", "helvetica", "symbol", etc.   */
/* size is measured in points, e.g. 10, 12, 18      */
/* bold and italic cause these effects when nonzero */

void change_font (char *family, int size,
		  int bold, int italic, jwin *w)
{
  XGCValues vals;
  char New[80];

  sprintf(New,
	  "-*-%s-%s-%s-*-*-%d-*",
	  family,
	  (bold ? "bold" : "medium"),
	  (italic ? "i" : "r"),
	  size);
  if ((vals.font = XLoadFont(w->disp, New)) == BadName)
  {
    fprintf(stderr, "\nCHANGE_FONT: Font %s is unknown!\n", New);
    exit(-1);
  }
  XChangeGC(w->disp, w->gcon, GCFont, &vals);
}


/* ------------------------------------------------------- */

/* dumps an array (arbitrary size) to the window       */
/* upper left corner is specified by x and y           */
/* w and h describe how to interpret the data array    */
/* if force is nonzero then update is done immediately */

void j_draw_image (int x, int y, char *data,
		   unsigned int wd, unsigned int ht,
		   int force, jwin *w)
{
  XImage *image;

  image = XCreateImage(w->disp, DefaultVisual(w->disp, 0),
		       8, ZPixmap, 0, data, wd, ht, 8, 0);
  XPutImage(w->disp, w->win, w->gcon,
	    image, 0, 0, x, y, wd, ht);
  if (force != 0) XFlush(w->disp);
  image->data = NULL;
  XDestroyImage(image);
}


/* ------------------------------------------------------- */

/* tell where in the window the mouse currently is */
/* and also tells which button (if any) is pressed */
/* returns zero if outside window, one otherwise   */

int mouse_coords (int *x, int *y, int *button, jwin *w)
{
  Window root, child;
  int rx, ry;
  unsigned int ww, wh, b, d;
  Bool valid;

  XGetGeometry(w->disp, w->win, &root,
	       &rx, &ry, &ww, &wh, &b, &d);
  valid = XQueryPointer(w->disp, w->win, &root, &child,
			&rx, &ry, x, y, &b);
  switch (b & (Button1Mask | Button2Mask | Button3Mask))
  {
    case Button1Mask:
      *button = 1;
      break;
    case Button2Mask:
      *button = 2;
      break;
    case Button3Mask:
      *button = 3;
      break;
    default:
      *button = 0;
      break;
    }
  if (valid && (*x >= 0) && (*y >= 0)
            && (*x < ww) && (*y < wh))
    return(1);
  else
    return(0);
}


/* Waits for mouse to be clicked in window then tells  */
/* where the click occured. Returns button number.     */
/* events did not work so this uses polling instead    */

int mouse_click (int *x, int *y, jwin *w)
{
  int button, valid, clear = 0;

  while (1)
  {
    valid = mouse_coords(x, y, &button, w);
    if ((valid == 1) && (clear == 1) && (button != 0))
      break;
    if ((valid == 1) && (button == 0))
      clear = 1;
    else
      clear = 0;
  }
  return(button);
}










//}; // extern "C"
