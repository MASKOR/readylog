/*
 *  colorx.c	-   set up colors
 *  Created by Long-Ji Lin (ljl) at Aug 6, 1991.
 */


#include "EZX11.h"


unsigned long	thePixels[MAXCOLORS];
int foreground_color;


static char	*theColorNames[MAXCOLORS] = {
   "white", "black", "red", "lawngreen", "blue", 
   "yellow", "gold", "violet", "pink", "grey"
   };


void EZX_InitDefaultColors()
{
   XColor	theRGBColor, theHardwareColor;
   int		theStatus;
   int		i;
   
   if (theDepth==1) {
      /* monochrome system */
      thePixels[C_WHITE] = theWhitePixel;
      thePixels[C_BLACK] = theBlackPixel;
   } else {
      for(i=0;i<MAXCOLORS;++i) {
	 theStatus = XLookupColor(theDisplay, theColormap, theColorNames[i],
				  &theRGBColor, &theHardwareColor);
	 if (theStatus != 0) {
	    theStatus = XAllocColor(theDisplay, theColormap, 
				    &theHardwareColor);
	    if (theStatus != 0)
	       thePixels[i] = theHardwareColor.pixel;
	    else
	       thePixels[i] = theBlackPixel;
	 }
      }
   }
}



void EZX_SetColor(color)			/* set foreground color */
     int color;
{
   if (color >= MAXCOLORS || color < 0) {
      printf("Wrong color: %d\n", color);
      return;
   }
   foreground_color = color;
}
