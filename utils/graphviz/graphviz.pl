
/***************************************************************************
 *  graphviz.ecl - Definition of the Prolog predicates for the
 *                 Graphviz interface
 *
 *  Generated: Mon Sep 10 15:02:57 2007
 *  Copyright  2007  Daniel Beck
 *
 *  $Id$
 *
 ****************************************************************************/

/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

:- external( gvc_create_digraph/1, "p_create_digraph" ).
:- external( gvc_add_node/1, "p_add_node" ).
:- external( gvc_add_node/2, "p_add_labelled_node" ).
:- external( gvc_add_edge/2, "p_add_edge" ).
:- external( gvc_add_edge/3, "p_add_labelled_edge" ).
:- external( gvc_render/0, "p_render" ).
:- external( gvc_render/2, "p_render_as" ).
:- external( gvc_close/0, "p_close" ).