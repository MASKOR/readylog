/* *****************************************************************************
 *
 *   Copyright (C) 2002-2005  KBSG, RWTH Aachen
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * *****************************************************************************
 *                                                                             
 *                                             ####   ####           .-""-.    
 *        # #                             #   #    # #    #         /[] _ _\
 *        # #                                 #    # #             _|_o_LII|_  
 *  ,###, # #  ### ## ## ##   ###  ## ##  #   #    # #       ###  / | ==== | \
 *  #   # # # #   # ## ## #  #   #  ## #  #   ###### #      #     |_| ==== |_| 
 *  #   # # # ####  #  #  #  #   #  #  #  #   #    # #      ####   ||" ||  ||
 *  #   # # # #     #  #  #  #   #  #  #  #   #    # #    #    #   ||'----'||
 *  '###'# # # #### #  #  ##  ### # #  ## ## #      # ####  ###   /__|    |__\ 
 *
 * *****************************************************************************
 *
 *            $Id: doc.pl,v 1.1 2006/04/13 15:50:19 stf Exp $
 *
 *    description: make pretty printed source documentation
 *
 *         author: Stefan Jacobs     contact: <Stefan_J@gmx.de>
 *         author: Stefan Schiffer   contact: <dr.stf[at]web.de>
 *
 *  last modified: $Date: 2006/04/13 15:50:19 $
 *             by: $Author: stf $
 *
 *******************************************************************************/

 :- lib(pretty_printer).

makedoc :-
	argv(all, Args),
	Args = [_EclipeExe| Files],
	pretty_print(Files, [outdir:"./doc"]).




	
