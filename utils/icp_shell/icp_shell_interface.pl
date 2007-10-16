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
 *  Contains c - golog interface for c-extensions of utils.
 *
 * last modified: $Date$
 *            by: $Author$
 *
 **********************************************************************/


:- external(c_icp_shell/2, "p_icp_shell").

