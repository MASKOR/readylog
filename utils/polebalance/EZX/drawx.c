/*
 *  drawx.c
 *
 *  Created by Goang-Tay Hsu (gth).
 *  Modified by Long-Ji Lin (ljl) at Aug 1, 1990.
 *
 */

#include "EZX11.h"


void EZX_ClearRectangle(w, left, top, width, height)
     EZXW_p w;
     int left, top, width, height;
{
   XClearArea(theDisplay, w->w, left, top, width, height, False);
}


void EZX_DrawPoint(w, x, y)
     EZXW_p w;
     int x,y;
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
   XDrawPoint(theDisplay, w->w, theGC, x, y);
}


void EZX_DrawPoints(w, npoints, points)
     EZXW_p w;
     int npoints;
     XPoint *points;
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
   XDrawPoints(theDisplay, w->w, theGC, points, npoints, CoordModePrevious);
}


void EZX_DrawLine(w, x1, y1, x2, y2)
     EZXW_p w;
     int x1, y1, x2, y2;
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
   XDrawLine(theDisplay, w->w, theGC, x1, y1, x2, y2);
}





void EZX_DrawRectangle(w, x, y, width, height)
     EZXW_p w;
     int		x, y;
     unsigned int	width, height;
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
   XDrawRectangle( theDisplay, w->w, theGC, x, y, width, height );
}


void EZX_FillRectangle(w, x, y, width, height)
     EZXW_p w;
     int	 x, y;
     unsigned int	width, height;
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
   XFillRectangle( theDisplay, w->w, theGC, x, y, width, height );
}


void EZX_DrawCircle(w, x, y, r)
     EZXW_p w;
     int    x, y, r;
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
   XDrawArc( theDisplay, w->w, theGC, x - r, y - r, r<<1, r<<1, 0, 360*64);
}


void EZX_FillCircle(w, x, y, r)
     EZXW_p w;
     int	 x, y, r;
{
    int  r2 = (int) (r  /* / 1.41421356 */ + 0.5); /*(st) */
    unsigned int wh = 2 * r2;

   if(theDepth == 1) {
     if(foreground_color == w->bg)
       XSetForeground(theDisplay, theGC, w->background);
     else
       XSetForeground(theDisplay, theGC, w->foreground);
   }
   else
     XSetForeground(theDisplay, theGC, thePixels[foreground_color]);
   XSetBackground(theDisplay, theGC, w->background);
    XFillArc( theDisplay, w->w, theGC, x - r2, y - r2, wh, wh, 0, 360*64);
}


void EZX_DrawPolygon(w, npoints, x, y)
     EZXW_p w;
     int	  npoints, x[], y[];
{
   int x0, y0, i;

   x0 = x[npoints-1];
   y0 = y[npoints-1];

   for(i = 0; i < npoints; i++) {
      EZX_DrawLine(w, x0,y0, x[i], y[i]); 
      x0 = x[i];
      y0 = y[i];
   }
}

void EZX_FillPolygon(w, npoints, x, y)
EZXW_p w;
int npoints, x[], y[];
{
   XPoint pts[500];
   int count;

   if(theDepth == 1) {
     if(foreground_color == w->bg)
       XSetForeground(theDisplay, theGC, w->background);
     else
       XSetForeground(theDisplay, theGC, w->foreground);
   }
   else
     XSetForeground(theDisplay, theGC, thePixels[foreground_color]);
   XSetBackground(theDisplay, theGC, w->background);
   for(count = 0; count < npoints; count++) {
     pts[count].x = x[count];
     pts[count].y = y[count];
   }
   XFillPolygon(theDisplay, w->w, theGC, pts, npoints, Complex,CoordModeOrigin);
}

void EZX_SetLineWidth(line_width)
     int line_width;
{
   theGCValues.line_width = line_width;
   XChangeGC(theDisplay, theGC, (GCLineWidth), &theGCValues);
}

/* line_stype include:
 *   LineSolid, LineOnOffDash, LineDoubledash
 */

void EZX_SetLineStyle(line_style)
     int line_style;
{
   theGCValues.line_style = line_style;
   XChangeGC(theDisplay, theGC, (GCLineStyle), &theGCValues);
}


/* possible styles include:
 *   FillSolid, FillTiled, FillStippled, FillOpaqueStippled
 */

void EZX_SetFillStyle(style)
     int style;
{
   theGCValues.fill_style = style;
   XChangeGC(theDisplay, theGC, (GCFillStyle), &theGCValues);
}

/*
void EZX_SetStippleGrey(level)
int level;
{
   theGCValues.fill_style = style;
   XChangeGC(theDisplay, theGC, (GCFillStyle), &theGCValues);
}
*/
