/*
 *  cursorx.c
 *
 *  Created by Goang-Tay Hsu (gth).
 *  Modified by Long-Ji Lin (ljl) at Aug 1, 1990.
 *
 */

#include "EZX11.h"

static XButtonEvent theButtonEvent;
static XKeyEvent theKeyEvent;  /* DZ 10-27-92 */
static XEvent theEvent;

                                    /* For holding the button event */


/* get cursor position when button pressed */
int EZX_GetCursor(xp, yp)
  int	*xp, *yp;
{
    do {
	XNextEvent(theDisplay, &theButtonEvent);
     } while (theButtonEvent.type != ButtonPress);

    *xp = theButtonEvent.x;
    *yp = theButtonEvent.y;

    if (theButtonEvent.button == Button1) return(LEFT_BUTTON);
    else if (theButtonEvent.button == Button2) return(MIDDLE_BUTTON);
    else if (theButtonEvent.button == Button3) return(RIGHT_BUTTON);
    else return(OTHER_BUTTON);
}

/* get cursor position in particular window when button pressed */
int EZX_GetWindowCursor(w, xp, yp)
  EZXW_p w;
  int	*xp, *yp;
{
    do {
	XNextEvent(theDisplay, &theButtonEvent);
     } while ((theButtonEvent.type != ButtonPress) ||
	      (theButtonEvent.window != w->w));

    *xp = theButtonEvent.x;
    *yp = theButtonEvent.y;

    if (theButtonEvent.button == Button1) return(LEFT_BUTTON);
    else if (theButtonEvent.button == Button2) return(MIDDLE_BUTTON);
    else if (theButtonEvent.button == Button3) return(RIGHT_BUTTON);
    else return(OTHER_BUTTON);
}

/* DZ 10-27-92 */

int
EZX_GetKey(cp)
char *cp;
{
    do {
	XNextEvent(theDisplay, &theKeyEvent);
     } while (theKeyEvent.type != KeyPress);

    return XLookupString(&theKeyEvent, cp, 8, NULL, NULL);
}

int
EZX_GetWindowKey(w, cp)
EZXW_p w;
char *cp;
{
    do {
	XNextEvent(theDisplay, &theKeyEvent);
     } while ((theKeyEvent.type != KeyPress) ||
	      (theKeyEvent.window != w->w));

    return XLookupString(&theKeyEvent, cp, 8, NULL, NULL);
}

/* get cursor position when button pressed */
int EZX_TestGetKey(cp)
  char	*cp;
{
    if(XCheckMaskEvent(theDisplay, KeyPressMask, &theKeyEvent) == True)
      return XLookupString(&theKeyEvent, cp, 8, NULL, NULL);
    return -1;
}

/* get cursor position when button pressed */
int EZX_TestGetWindowKey(w, cp)
EZXW_p w;
char	*cp;
{
    if(XCheckMaskEvent(theDisplay, KeyPressMask, &theKeyEvent) == True)
      if(theKeyEvent.window == w->w)
        return XLookupString(&theKeyEvent, cp, 8, NULL, NULL);
    return -1;
}
					     
/* get cursor position when button pressed */
int EZX_TestGetCursor(xp, yp)
  int	*xp, *yp;
{
      

    XCheckMaskEvent(theDisplay, ButtonPressMask, &theEvent);
					     
    if (theEvent.xbutton.type == ButtonPress){ 
      XNextEvent(theDisplay, &theButtonEvent);
      *xp = theButtonEvent.x;
      *yp = theButtonEvent.y;
      
      if (theButtonEvent.button == Button1) return(LEFT_BUTTON);
      else if (theButtonEvent.button == Button2) return(MIDDLE_BUTTON);
      else if (theButtonEvent.button == Button3) return(RIGHT_BUTTON);
      else return(OTHER_BUTTON);
    }
    *xp = -1;
    *yp = -1;
    return -1;
}
					     
/* get cursor position when button pressed */
int EZX_TestGetWindowCursor(w, xp, yp)
  EZXW_p w;
  int	*xp, *yp;
{

    XCheckMaskEvent(theDisplay, ButtonPressMask, &theEvent);
					     
    if (theEvent.xbutton.type == ButtonPress){ 
      XNextEvent(theDisplay, &theButtonEvent);
      if(theButtonEvent.window == w->w) {
        *xp = theButtonEvent.x;
        *yp = theButtonEvent.y;
      
        if (theButtonEvent.button == Button1) return(LEFT_BUTTON);
        else if (theButtonEvent.button == Button2) return(MIDDLE_BUTTON);
        else if (theButtonEvent.button == Button3) return(RIGHT_BUTTON);
        else return(OTHER_BUTTON);
      }
    }
    *xp = -1;
    *yp = -1;
    return -1;
}


/* get cursor positions when button pressed and released */
int EZX_GetCursors(xp, yp, xr, yr)
  int	*xp, *yp, *xr, *yr;
{
    do {
	XNextEvent(theDisplay, &theButtonEvent);
     } while (theButtonEvent.type != ButtonPress);

    *xp = theButtonEvent.x;
    *yp = theButtonEvent.y;

    do {
	XNextEvent(theDisplay, &theButtonEvent);
     } while (theButtonEvent.type != ButtonRelease);

    *xr = theButtonEvent.x;
    *yr = theButtonEvent.y;

    if (theButtonEvent.button == Button1) return(LEFT_BUTTON);
    else if (theButtonEvent.button == Button2) return(MIDDLE_BUTTON);
    else if (theButtonEvent.button == Button3) return(RIGHT_BUTTON);
    else return(OTHER_BUTTON);
}
