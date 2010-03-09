#include "EZX11.h"

int	    	theScreen;		/* Screen number */
Display	       *theDisplay;		/* X server connection */
int	     	theDepth;		/* 1 if monochrome */
Colormap	theColormap;
int	theBlackPixel;
int	theWhitePixel;


static int 	theFirstTime = True;


void EZX_InitX( display, program )
     char *display, *program;
{
   char *getenv();

   if (! theFirstTime) return;
   /* establish a connection to X server */
   if ( display == NULL )  display = getenv("DISPLAY");
   if ( (theDisplay = XOpenDisplay(display)) == NULL ) {
      fprintf(stderr, "\n%s: could not open display %s.\n",
	      program, XDisplayName(display));
      exit(1);
   }
   /* check for the default screen and color plane depth, etc */
   theScreen     = DefaultScreen( theDisplay );
   theDepth      = DefaultDepth( theDisplay, theScreen );
   theWhitePixel = WhitePixel( theDisplay, theScreen );
   theBlackPixel = BlackPixel( theDisplay, theScreen );
   theColormap   = DefaultColormap( theDisplay, theScreen );

   EZX_InitDefaultColors();
   EZX_InitPatterns();

   theFirstTime = False;
}



void EZX_EndX()
{
   if (theFirstTime) return;
   XFlush( theDisplay );
   XCloseDisplay( theDisplay );
   theFirstTime = True;
}
