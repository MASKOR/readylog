
/***************************************************************************
 *  example.ecl - An example demonstrating the usage of the Graphviz
 *                interface
 *
 *  Generated: Mon Sep 10 15:06:33 2007
 *  Copyright  2007  Daniel Beck
 *
 *  $Id: kicker_plugin.h 155 2007-05-25 12:16:03Z tim $
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

:- load("graphviz").
:- ensure_loaded("graphviz.pl").

:- gvc_create_digraph(asdf).
:- gvc_add_node("labelX", X), gvc_add_node("labelY", Y), gvc_add_edge(X,Y,"asdf").
:- gvc_render("png", "output.png").
:- gvc_close.