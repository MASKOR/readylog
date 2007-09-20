
#include "getkey.h"
#include <eclipseclass.h>
#include <iostream>

extern "C"
int p_chkkey()
{
  char c = getkey();
  if (c != 0) {
    return EC_succeed;
  }
  
  return EC_fail;
}

extern "C"
int p_getkey()
{
  char c = getkey();
  if (c == 0) {
    return EC_fail;
  }

  if ( unify( EC_arg(1), EC_word(c) ) != EC_succeed)
    {
      std::cerr << "Error unifying key" << std::endl;
      return EC_fail;
    }
  
  return EC_succeed;
}
