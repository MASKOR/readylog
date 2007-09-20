/*###########################################################################
  #                                                                         #
  #  M O D U L N A M E : getkey.h                                           #
  #                                                                         #
  #  B E S C H R E I B U N G :                                              #
  #   "getkey" ist eine Funktion, die dem "Readkey" in Turbo Pascal	        #
  #   entspricht.                                                           #
  #                                                                         #
  #  R Ü C K G A B E W E R T:                                               #
  #                                                                         #
  #   Sie liefert einen Tastendruck SOFORT zurück (ohne ENTER!)	            #
  #                                                                         #
  ###########################################################################*/

#ifndef GETKEY_H
#define GETKEY_H

#include <fcntl.h>
#include <unistd.h>
#include <termios.h>
#include <stdio.h>

extern char getkey( int wait_for_key=0 );

#endif
