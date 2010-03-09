//#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
//
#include "pole_balance.h"

int main () {
  PoleCart pole_cart;
  //int x,y;
  char key;

  //srandom(time(0));

  initializePoleCart(&pole_cart,1);
  initializePoleCartPosition(&pole_cart);

  do {
    do {
      printf("Apply force (p=small+,P=med+,m=small-,M=med-,n=none,q=quit): ");
      scanf(" %c",&key);
      fflush(stdin);
    } while ((key != 'p') && (key != 'P') && (key != 'm') && (key != 'M') && (key != 'n') && (key != 'q'));
    switch (key) {
      case 'P':
        pushPoleCart(&pole_cart,10);
        break;
      case 'p':
        pushPoleCart(&pole_cart,20);
        break;
      case 'M':
        pushPoleCart(&pole_cart,-20);
        break;
      case 'm':
        pushPoleCart(&pole_cart,-10);
        break;
      case 'n':
        pushPoleCart(&pole_cart,0);
        break;
      case 'q':
        break;
    };
  } while ((key != 'q') && positionPoleCartOK(&pole_cart));
    
  destroyPoleCart(&pole_cart);
  return 0;
}
