/*
 *  colorx.c	-   set up colors
 *  Created by Long-Ji Lin (ljl) at Aug 6, 1991.
 */


#include "EZX11.h"

XFontStruct	*theFont;

void EZX_UseFont(theNewGC, fontname)
     GC   theNewGC;
     char fontname[];
{
   theFont = XLoadQueryFont(theDisplay, fontname);
   if (theFont != 0)
      XSetFont(theDisplay, theNewGC, theFont->fid);
}

void EZX_FreeFont()
{
   XFreeFont(theDisplay, theFont);
}




void EZX_DrawString(w, x, y, string)
     EZXW_p w;
     int x,y;
     char *string;
{
   if(theDepth == 1) {
     if(foreground_color == w->bg)
       XSetForeground(theDisplay, theGC, w->background);
     else
       XSetForeground(theDisplay, theGC, w->foreground);
   }
   else
     XSetForeground(theDisplay, theGC, thePixels[foreground_color]);
   XSetBackground(theDisplay, theGC, w->background);
   XDrawString(theDisplay, w->w, theGC, x, y, string, strlen(string));
}


void EZX_DrawText(w, x, y, string)
     EZXW_p w;
     int x,y;
     char *string;
{
   if(theDepth == 1) {
     if(foreground_color == w->bg)
       XSetForeground(theDisplay, theGC, w->background);
     else
       XSetForeground(theDisplay, theGC, w->foreground);
   }
   else
     XSetForeground(theDisplay, theGC, thePixels[foreground_color]);
   XSetBackground(theDisplay, theGC, w->background);
   XDrawImageString(theDisplay, w->w, theGC, x, y, string, strlen(string));
}


static void format_spec(x,y, string, style, newx,newy,width)
     int x,y, *newx, *newy, *width;
     char string[], style;
{
   *width = XTextWidth(theFont, string, strlen(string));
   *newx  = x;
   *newy  = y;
   switch( style ) {
        case 'l':	/* Left Alignment */
	case 'L':
	    break;
	case 'R':	/* Right Alignment */
	case 'r':
/*	    *newx = x + WSizeHints.width - *width;  */
	    *newx = x - *width;
	    break;
	case 'c':	/* Centered */ 
	case 'C':
/*	    *newx = x + (WSizeHints.width - *width) / 2; */
	    *newx = x - *width / 2;
/*	    *newy = y - EZX_GetFontHeight()/2; */
	    break;
	default:	/* use left alignment as default */
	    format_spec(x,y, string, 'L', newx,newy,width);
	 }
}


void EZX_DrawTextAt(w, x,y, string, style)
     EZXW_p w;
     int	  x,y;
     char	  style, *string;
{
   int	width,newx,newy;

   format_spec(x,y, string, style, &newx, &newy, &width);
   EZX_DrawText(w, newx, newy, string );
}


void EZX_DrawStringAt(w, x,y, string, style)
     EZXW_p w;
     int	  x,y;
     char	  style, *string;
{
   int	width,newx,newy;

   format_spec(x,y, string, style, &newx, &newy, &width);
   EZX_DrawString(w, newx, newy, string );
}


void EZX_FormatAt(w, x,y, string, style, background_filled, underlied)
     EZXW_p w;
     int	  x,y, background_filled, underlied;
     char	  style, *string;
{
   int	width,newx,newy;

   format_spec(x,y, string, style, &newx, &newy, &width);
   if( background_filled )
      EZX_DrawText(w, newx, newy, string );
   else
      EZX_DrawString(w, newx, newy, string );

   if( underlied )
      EZX_DrawLine(w, newx, newy+2, x+width, newy+2 );
}


int  EZX_GetFontHeight()
{
    return( theFont->ascent + theFont->descent );
}


int EZX_GetTextWidth(string)
     char string[];
{
   return( XTextWidth(theFont, string, strlen(string)) );
}
