
/***********************************************************************
 * Description: Returns (immediately) the pressed key
 *
 * last modified: $Date$
 *            by: $Author$
 *
 * $Id$
 **********************************************************************/

/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "getkey.h"

#include <unistd.h>
#include <signal.h>
#include <iostream>
#include <fcntl.h>
#include <unistd.h>
#include <termios.h>
#include <stdio.h>

/** Sets O_NONBLOCK Flag to 1.
 *  Consequently, read() in getkey() does not block the program until
 *  a key is pressed
 */
static void set_nonblock_flag(void)
{
  int oldflags;
  
  oldflags  = fcntl(STDIN_FILENO, F_GETFL, 0);
  oldflags |= O_NONBLOCK;
  fcntl(STDIN_FILENO, F_SETFL, oldflags);
}


static void clear_nonblock_flag( void )
{
	int oldflags;

   oldflags  = fcntl( STDIN_FILENO, F_GETFL, 0 );
   oldflags &= ~O_NONBLOCK; 
   fcntl( STDIN_FILENO, F_SETFL, oldflags );
}


char getkey( int wait_for_key )
{
  char buf[10] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  ssize_t n = 0;
  // new terminal attributes
  struct termios tattr;
  // original terminal settings
  struct termios saved_attributes;              

  set_nonblock_flag();
  // save the original attributes
  tcgetattr( STDIN_FILENO, &saved_attributes );
  // set the new attributes for the terminal:
  tcgetattr( STDIN_FILENO, &tattr );
  // Clear ICANON ...
  tattr.c_lflag   &= ~(ICANON);
  // ... and ECHO
  tattr.c_lflag   &= ~(ECHO);
  // noncanonical, direction transmission
  tattr.c_cc[VMIN] = 0;
  // of input characters (MIN=0,TIME=0)
  tattr.c_cc[VTIME]= 0;
  tcsetattr( STDIN_FILENO, TCSANOW, &tattr );

  if ( wait_for_key == 1 )
  {
    while(n == 0)
    {
      n = read( STDIN_FILENO, buf, 1 );
      usleep(10);
    }
  }
  else
  {
    n = read( STDIN_FILENO, buf, 1 );
    usleep(10);
  }

  tcsetattr( STDIN_FILENO, TCSANOW, &saved_attributes );
  clear_nonblock_flag();

  return buf[0];
}


/** The handler for signals. 
 *  Here only SIGINT is handled graciously
 */
static void BreakHandler(int sig)
{
  switch (sig)
    {
    case SIGINT:
      std::cerr  << std::endl << "Use \"quit\" or CTRL+D to exit ICP-SHELL !" << std::endl << std::flush;
      clear_nonblock_flag();   
      break;
    }
  signal (sig, BreakHandler);
  return;
}


static struct sigaction* oldact;

/** Setup for the above defined break handler.
 */
void SetupBreakHandler()
{
  oldact = new struct sigaction;
  sigaction(SIGINT, 0, oldact);

  signal(SIGINT, BreakHandler);
  return;
}   

void RetrackBreakHandler()
{
  sigaction(SIGINT,oldact,0);
  return;
}


