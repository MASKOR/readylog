#include "EZX11.h"

main()
{
   EZXW_p w1, w2;

   w1 = EZX_MakeWindow("environment", 200, 200, NULL, C_GOLD);  /* "+200+200" */
   w2 = EZX_MakeWindow("environment", 200, 200, NULL, C_BLACK);


   EZX_SetColor(C_BLUE);
   EZX_DrawGrey(w1, 150,150,5);
   EZX_DrawGrey(w1, 150,180,20);
   EZX_DrawGrey(w1, 180,150,40);
   EZX_DrawGrey(w1, 180,180,64);
   EZX_DrawGrey(w2, 150,150,5);
   EZX_DrawGrey(w2, 150,180,20);
   EZX_DrawGrey(w2, 180,150,40);
   EZX_DrawGrey(w2, 180,180,64);

   EZX_DrawTextAt(w1, 30,30, "O E $", 'C');
   EZX_DrawTextAt(w2, 30,30, "O E $", 'C');

   EZX_UseFont(theGC, "6x10");
   EZX_DrawTextAt(w1, 130,130, "O E $", 'L');
   EZX_DrawRectangle(w1, 130, 130-EZX_GetFontHeight(), 20, 20);
   EZX_DrawTextAt(w2, 130,130, "O E $", 'L');
   EZX_DrawRectangle(w2, 130, 130-EZX_GetFontHeight(), 20, 20);

for(;;) {
   EZX_SetColor(C_RED);
   EZX_DrawRectangle(w1, 50, 50, 50, 50);
   EZX_DrawRectangle(w2, 50, 50, 50, 50);

   EZX_DrawTextAt(w1, 30,30, "O E $", 'C');
   EZX_DrawTextAt(w2, 30,30, "O E $", 'C');

   EZX_SetColor(C_LAWNGREEN);
   EZX_DrawCircle(w1, 50,50, 50);
   EZX_DrawCircle(w2, 50,50, 50);
   EZX_Flush();

   EZX_DrawTextAt(w1, 30,30, "O E $", 'C');
   EZX_DrawTextAt(w2, 30,30, "O E $", 'C');

   sleep(2);
   EZX_SetColor(C_BLUE);
   EZX_SetLineWidth(3);
   EZX_DrawRectangle(w1, 50, 50, 50, 50);
   EZX_DrawRectangle(w2, 50, 50, 50, 50);
   EZX_Flush();

   sleep(2);
   EZX_SetColor(C_WHITE);
/*
   EZX_SetLineWidth(1);
   EZX_SetLineStyle(LineDoubleDash);
   EZX_DrawRectangle(w1, 50, 50, 50, 50);
 */
   EZX_DrawCircle(w1, 50,50, 50);
   EZX_DrawCircle(w2, 50,50, 50);
   EZX_Flush();

   sleep(2);
   EZX_SetLineWidth(3);
   EZX_SetLineStyle(LineOnOffDash);
   EZX_DrawRectangle(w1, 50, 50, 50, 50);
   EZX_DrawRectangle(w2, 50, 50, 50, 50);
   EZX_Flush();

   sleep(2);
}   

/*
   w2 = EZX_MakeWindow("panel",       200, 200, NULL);
   EZX_DrawTextAt(w2, 50, 50, "hello", 'C');
   EZX_Flush();
   for(;;);
 */
}
