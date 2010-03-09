/*
 *  EZX11.h - Header File for Multiple Window Graphics Interface to X11 (R3)
 *
 *  Created by gth.
 *  Modified by ljl (Aug 1, 1990).
 */

#ifndef EZX11_LOADED
#define EZX11_LOADED

#include <stdio.h>
#include <strings.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>


typedef struct {
   char title[64];
   int  width, height;
   Window w;
   long background;
   int  bg;          /* If black-and-white, the background color */
   long foreground;  /* If black-and-white, the usual actual foreground */
}  EZXW_t, *EZXW_p;


typedef struct {
  XPoint start;		/* Starting Point Coordinate */
  XPoint *points;	/* Array of XPoints */
  int	 npoints;	/* No. of Points */
} BitMapPoints;


/* #define FONT_NAME 	"fixed" */
/* #define FONT_NAME	"9x15" */
#define FONT_NAME	"9x15bold"

#define	LEFT_BUTTON	0	/* button detail */
#define MIDDLE_BUTTON	1
#define RIGHT_BUTTON	2
#define OTHER_BUTTON	3

#define NO_GREY_LEVELS	65
#define PATTERN_WIDTH	8
#define PATTERN_HEIGHT	8

#define PROMPT_AND_GET_RETURN(prompt) \
	XFlush( WDisplay); \
	printf("\n]]] Press Return to %s ...", prompt); \
	getchar();


extern	int	    	theScreen;
extern	Display	       *theDisplay;
extern	int	     	theDepth;
extern	int	     	foreground_color;;
extern	Colormap	theColormap;
extern	int theBlackPixel;
extern	int theWhitePixel;
extern	unsigned long	thePixels[];

extern	GC	     	theGC;
extern	XGCValues     	theGCValues;
extern	BitMapPoints 	theGrey[NO_GREY_LEVELS];
extern	XFontStruct    *theFont;

/* colors */

#define MAXCOLORS	10

#define C_WHITE		0
#define C_BLACK		1
#define C_RED		2
#define C_LAWNGREEN	3
#define C_BLUE		4

#define C_YELLOW	5
#define C_GOLD		6
#define C_VIOLET	7
#define C_PINK		8
#define C_GREY		9
#define C_GRAY		9


/* init routines */

void EZX_InitX();
void EZX_EndX();
void EZX_InitDefaultColors();

/* window creation/destruction routines */

EZXW_p EZX_MakeWindow();
void EZX_EndWindow();

/* cursor routines */

int EZX_GetCursor();
int EZX_TestGetCursor();
int EZX_GetCursors();

/* basic drawing routines */

void EZX_ClearWindow();
void EZX_Flush();
void EZX_ClearRectangle();
void EZX_DrawPoint();
void EZX_DrawPoints();
void EZX_DrawLine();
void EZX_DrawRectangle();
void EZX_FillRectangle();
void EZX_DrawCircle();
void EZX_FillCircle();
void EZX_DrawString();
void EZX_DrawText();

/* advanced drawing routines */

void EZX_DrawPolygon();
void EZX_DrawTextAt();
void EZX_DrawStringAt();
void EZX_FormatAt();

void EZX_InitPatterns();
void EZX_CreatePattern();
void EZX_DrawPattern();
void EZX_DrawGrey();

/* GC changes */

void EZX_SetColor();				/* set foreground color */
void EZX_SetLineWidth();
void EZX_SetLineStyle();
void EZX_SetFillStyle();



/* misc */

int  EZX_TextHeight();
int  EZX_TextWidth();
void EZX_UseFont();
void EZX__FreeFont();

#define EZX_SetToEraseMode()	EZX_SetColor(C_WHITE)
#define EZX_SetToDrawMode()	EZX_SetColor(C_BLACK)


#endif
