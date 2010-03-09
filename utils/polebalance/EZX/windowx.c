/*
 *  colorx.c	-   set up colors
 *  Created by Long-Ji Lin (ljl) at Aug 6, 1991.
 */


#include "EZX11.h"

#define  BORDER_WIDTH	2

GC		theGC;
XGCValues	theGCValues;

static int	theFirstWindow = True;

static Window EZX_OpenWindow(title, x, y, width, height, flag, theNewGC,bg)
     char title[];
     int x,y;				/* window position.
					 * if negative, specified at runtime
					 */
     int width, height;			/* window size  */
     int flag;				/* if > 0, window is a pop-up */
     GC  *theNewGC;
     long bg;
{
   XSetWindowAttributes	theWindowAttributes;
   XSizeHints		theSizeHints;
   unsigned long	theWindowMask;
   Window		theNewWindow;
   static XWMHints theWMHints = {	/* Let WM know how to handle window */
      (InputHint|StateHint),		/* flags */
      False,				/* input */
      NormalState,			/* initial_state */
      0,				/* icon pixmap */
      0,				/* icon window */
      0, 0,				/* icon location */
      0,				/* icon mask */
      0,				/* window group */
   };

   /* set up the attributes for the window.
    * Window manager (WM) may deny some of these resources.  
    * Setting override_redirect to True will tell WM to leave the window
    * alone.
    */
   theWindowAttributes.backing_store = Always;
   /* DZ 10-27-92 */
   theWindowAttributes.event_mask = KeyPressMask | ButtonPressMask | ButtonReleaseMask;
   theWindowAttributes.border_pixel  = theBlackPixel;
   theWindowAttributes.background_pixel = bg;
   theWindowMask = CWBackingStore | CWEventMask |
      		   CWBackPixel | CWBorderPixel;

   if (flag > 0) {
      theWindowAttributes.override_redirect = True;
      theWindowAttributes.save_under	    = True;
      theWindowMask |= (CWSaveUnder |  CWOverrideRedirect);
   } else {
      theWindowAttributes.override_redirect = False;
      theWindowMask |= CWOverrideRedirect;
   }


   /* Open a window on the display */
   theNewWindow = XCreateWindow(theDisplay,
				RootWindow(theDisplay, theScreen),
				x,y,
				width,height,
				BORDER_WIDTH,
				theDepth,
				InputOutput,
				CopyFromParent,
				theWindowMask,
				&theWindowAttributes);

   /* Send "Hints" to WM, such as where the window should go. */
   XSetWMHints(theDisplay, theNewWindow, &theWMHints);

   /* Tell WM about the size and location for the window */
   theSizeHints.flags	= PPosition | PSize;
   theSizeHints.x	= x;
   theSizeHints.y	= y;
   theSizeHints.width	= width;
   theSizeHints.height	= height;

   XSetStandardProperties(theDisplay, theNewWindow,
			  title, 	/* window_name */
			  title,	/* icon_name */
			  None,		/* icon_pixmap */
			  0,		/* argv */
			  0,		/* argc */
			  &theSizeHints);

   /* Create a graphic context (GC) for the window, only for the first time */
   if (theFirstWindow) {
      if (EZX_CreateGC(theNewWindow, theNewGC) == 0) {
	 XDestroyWindow(theDisplay, theNewWindow);
printf("error in creategc\n");
	 return((Window)0);
      }
      theFirstWindow = False;
   }

   /* Ask X to place the window visibly on the screen */
   XMapWindow(theDisplay, theNewWindow);
   XFlush(theDisplay);
   return(theNewWindow);
}


/* Returns 0 if error, 1 if Ok */

EZX_CreateGC(theNewWindow, theNewGC)
     Window	theNewWindow;
     GC		*theNewGC;
{
   /*
   theGCValues.foreground = theBlackPixel;
   theGCValues.background = theWhitePixel;
   */
   *theNewGC = XCreateGC(theDisplay, theNewWindow,
			 (long) 0, &theGCValues);
   if (*theNewGC == 0)			/* unable to create a GC */
      return(0);
   XSetForeground(theDisplay, *theNewGC, theBlackPixel);
   XSetBackground(theDisplay, *theNewGC, theWhitePixel);
   EZX_UseFont(*theNewGC, FONT_NAME);
   return 1;
}


EZXW_p EZX_MakeWindow(title, width, height, position, bg)
     char *title;
     int width, height;
     char *position;			/* eg, "+4-4" */
     int bg;
{
   int left=0, top=0;
   Window window;
   char geometry[32];
   EZXW_p W;

   EZX_InitX( NULL, "EZX");
   W = (EZXW_p)malloc(sizeof(EZXW_t));
   strcpy(W->title, title);
   W->width  = width;
   W->height = height;
   sprintf(geometry, "=%dx%d", width, height);
   if (position) {
      strcat(geometry, position);
      XParseGeometry(geometry, &left, &top, &width, &height);
   } 
   W->bg = bg;
   if(theDepth == 1) {
     switch(bg) {
       case C_WHITE:
       case C_YELLOW:
       case C_GOLD:
       case C_PINK:
       case C_GREY:
         W->background = thePixels[C_WHITE];
	 W->foreground = thePixels[C_BLACK];
	 break;
       default:
         W->background = thePixels[C_BLACK];
	 W->foreground = thePixels[C_WHITE];
	 break;
      }
    }
    else
      W->background = thePixels[bg];

   W->w = EZX_OpenWindow(title,left,top,width,height, 
			 0,		/* pop-up window ? */
			 &theGC, W->background);
     return(W);
}


void EZX_EndWindow(w)
     EZXW_p w;
{
    XFlush( theDisplay );
    XDestroyWindow( theDisplay, w->w );
}


void EZX_ClearWindow(w)
     EZXW_p w;
{
    XClearWindow( theDisplay, w->w );
    XFlush( theDisplay );
}

void EZX_Flush()
{
   XFlush( theDisplay );
}
