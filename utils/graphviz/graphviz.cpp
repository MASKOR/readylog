
/***************************************************************************
 *  graphviz.cpp - A Graphviz interface for Eclipse
 *
 *  Generated: Mon Sep 10 15:17:40 2007
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

#include <eclipseclass.h>
#include <graphviz/gvc.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <map>
#include <iostream>

using namespace std;

struct str_cmp {
  bool operator()( const char* s1, const char* s2 ) const {
    return strcmp( s1, s2 ) < 0;
  }
};

Agraph_t* G = NULL;
GVC_t* gvc = NULL;
map<char*, Agnode_t*, str_cmp> nodes;

extern "C"
int p_create_digraph()
{
  gvc = gvContext();

  char* graph_name;
  if (EC_succeed == EC_arg(1).is_string(&graph_name))
    {
      G = agopen(graph_name, AGDIGRAPH);

      // cout << "Created graph " << graph_name << endl;

      //agedgeattr(G, "label", "");
      //agnodeattr(G, "label", "");
      agedgeattr(G, (char *)"label", (char *)"");
      agnodeattr(G, (char *)"label", (char *)"");

      return EC_succeed;
    }

  return EC_fail;
}

extern "C"
int p_add_node()
{
  if (NULL != G)
    {
      char* name = (char*) malloc(10);
      sprintf(name, "X%u", (unsigned int) nodes.size());
      Agnode_t* node = agnode(G, name);

      // cout << "Created node " << name << endl;

      nodes[name] = node;

      if ( unify( EC_arg(1), EC_word(name) ) != EC_succeed )
	{
	  return EC_fail;
	}

      return EC_succeed;
    }
  
  cout << "Graph not initialized" << endl;

  return EC_fail;
}

extern "C"
int p_add_labelled_node()
{
  if (NULL != G)
    {
      char* name = (char*) malloc(10);
      sprintf(name, "X%u", (unsigned int) nodes.size());
      Agnode_t* node = agnode(G, name);

      char* label;
      if (EC_succeed != EC_arg(1).is_string(&label))

	{
	  return EC_fail;
	}
      
      agset(node, (char *)"label", label);
	  
      // cout << "Created node " << name << endl;

      nodes[name] = node;

      if ( unify( EC_arg(2), EC_word(name) ) != EC_succeed )
	{
	  return EC_fail;
	}
            
      return EC_succeed;
    }

  cout << "Graph not initialized" << endl;
  
  return EC_fail;
}


extern "C"
int p_add_edge()
{
  if (NULL != G)
    {
      char* node_name1;
      char* node_name2;

      if (EC_succeed != EC_arg(1).is_string(&node_name1))
	{
	  return EC_fail;
	}

      if (EC_succeed != EC_arg(2).is_string(&node_name2))
	{
	  return EC_fail;
	}

      // cout << "Creating egde between node " << node_name1 
      //      << " and node " << node_name2 << endl;

      Agnode_t* node1;
      Agnode_t* node2;

      node1 = nodes[node_name1];
      node2 = nodes[node_name2];

      Agedge_t* edge;
      edge = agedge(G,node1,node2);
     
      return EC_succeed;
    }

  cout << "Graph not initialized" << endl;

  return EC_fail;
}

extern "C"
int p_add_labelled_edge()
{
  if (NULL != G)
    {
      char* node_name1;
      char* node_name2;

      if (EC_succeed != EC_arg(1).is_string(&node_name1))
	{
	  return EC_fail;
	}

      if (EC_succeed != EC_arg(2).is_string(&node_name2))
	{
	  return EC_fail;
	}

      // cout << "Creating egde between node " << node_name1 
      //      << " and node " << node_name2 << endl;

      Agnode_t* node1;
      Agnode_t* node2;

      node1 = nodes[node_name1];
      node2 = nodes[node_name2];

      Agedge_t* edge;
      edge = agedge(G,node1,node2);

      char* label;

      if (EC_succeed != EC_arg(3).is_string(&label))
	{
	  return EC_fail;
	}

      agset(edge, (char *)"label", label);
     
      return EC_succeed;
    }

  cout << "Graph not initialized" << endl;

  return EC_fail;
}

extern "C"
int p_render()
{
  gvLayout(gvc, G, "dot");
  gvRenderFilename(gvc, G, "png", "output.png");
  
  return EC_succeed;
}

extern "C"
int p_render_as()
{
  gvLayout(gvc, G, "dot");


  char* format;
  char* filename;

  if (EC_succeed != EC_arg(1).is_string(&format))
    {
      return EC_fail;
    }

  if (EC_succeed != EC_arg(2).is_string(&filename))
    {
      return EC_fail;
    }

  gvRenderFilename(gvc, G, format, filename);

  return EC_succeed;
}

extern "C"
int p_close()
{
  gvFreeLayout(gvc, G);
  agclose(G);
  gvFreeContext(gvc);

  map<char*, Agnode_t*, str_cmp>::iterator iter;

  for (iter = nodes.begin(); iter != nodes.end(); iter++)
    {
      free((void*) iter->first);
    }

  return EC_succeed;
}
