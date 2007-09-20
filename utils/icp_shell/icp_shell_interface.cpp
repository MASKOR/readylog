/*
 ********************************************************************************
 *
 *     icp_shell prolog implementation file by Carsten Gester & Stefan Jacobs
 *     Copyright (C) 2004 KBSG, Aachen University <Stefan_J@gmx.de>
 *
 *     This program is free software; you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation; either version 2 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 ********************************************************************************
//     Note: It is located in license/GPL.license =)
//
//
//      ######                                 ######
//      #     #  ######    ##    #####   #   # #     #   ####   #####
//      #     #  #        #  #   #    #   # #  #     #  #    #    #
//      ######   #####   #    #  #    #    #   ######   #    #    #
//      #   #    #       ######  #    #    #   #     #  #    #    #
//      #    #   #       #    #  #    #    #   #     #  #    #    #
//      #     #  ######  #    #  #####     #   ######    ####     #
//    ================================================================ 
*/

/***********************************************************************
 *
 * $Id$
 *
 * Description:
 *
 *  Contains c-/golog-interface for c-extensions of utils.
 *
 * last modified: $Date$
 *            by: $Author$
 *
 **********************************************************************/

#include <string>
#include <iostream>
#include "c_icp_shell.h"
#include <eclipseclass.h>
#include <cmath>

#define  unify(ARGNR, ARG) ((check_unify(ARG)) && unify(ARGNR, ARG))

bool
check_unify(EC_word ec) {
  double tmp;
  if ( ec.is_double(&tmp) == EC_succeed ) {
    if ( isnan(tmp) ) {
      std::cerr << "*********                N A N            ********" << std::endl;
    }
    if ( abs( isinf(tmp) ) ) {
      std::cerr << "*********                I N F            ********" << std::endl;
    }
  }
  return true;
}

char*
get_EC_string(int arg_index, int &error) {
  char* d;
  EC_word ec_word = EC_word( EC_arg(arg_index) );
  if ( ec_word.is_string(&d) != EC_succeed ) {
    std::cerr << "** Argument " << arg_index << " is not a string as was expected!" << std::endl;
    error = 1;
    return 0;
  }
  return d;
}

extern "C" int
p_icp_shell() {
  int error = 0;
  std::string keyword_list = get_EC_string(2, error);

  if (error) return EC_fail;

  std::string command = c_icp_shell("ICP-Shell > ",keyword_list);

  if ( unify( EC_arg(1), EC_word( command.c_str() ) )!= EC_succeed ) {
    return EC_fail;
  }

  return EC_succeed;
}
