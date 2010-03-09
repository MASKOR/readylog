#include "EZX11.h"

main()
{
   EZXW_p w1;
   int x, y;

   w1 = EZX_MakeWindow("Click mouse button to exit", 200, 200, NULL, C_GOLD);

   EZX_SetColor(C_BLUE);
   EZX_DrawRectangle(w1, 30, 30, 20, 80);
   EZX_SetColor(C_RED);
   EZX_DrawRectangle(w1, 60, 60, 70, 30);
   EZX_Flush();
   EZX_GetCursor(&x, &y);  /* Wait for a button click */
}   
