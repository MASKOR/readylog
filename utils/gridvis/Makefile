# ############################################################################
#   ,=³ .%%% %%%%%%%. .%%%%%.  .%%%%%.   %%%%%.%%. %. %%%%%%%% %% Rheinisch-
#  [==].%%%   %%   %% %%   %%  %%   %%   %%  %%'%%.%%.%% %% %% %% Westfälische
#   //l%%%    %%   %% %%%%. ' %%       @ %%%%%' '%%%%%%% %% %%%%% Technische
#  [==]%%     %%|=%%   %%=>%  %%         %%  %%  '%%'%%% %% %% %% Hochschule
#  [==]%%%    %%   %% . '%%%% %%  '%%%   %%   %%  '% '%% %% %% %% Aachen
#  [==]'%%%   %%   %% %%   %%  %%   %%   http://kbsg.rwth-aachen.de/
#  o^^o '%%% %%%%%%%' '%%%%%'O '%%%%%'   Knowledge Based Systems Group
# ############################################################################
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; version 2 of the License.
#
# ############################################################################
#
#            $Id: Makefile 1 2007-03-01 23:58:15Z tim $
#    description: Makefile for High-Level-Interface
#  last modified: $Date: 2007-03-02 00:58:15 +0100 (Fri, 02 Mar 2007) $
#             by: $Author: stf $
#
# ############################################################################

BASEDIR = ../..

TARGETS = gridvis

OBJS_gridvis =  gridvis.o \
		grid-world.o \
		graphics.o \
		display.o \

#		qlearner.o \
#		krandom.o \


LIBS_gridvis = stdc++ Xm Xt X11 m nsl eclipse

INSTALL_gridvis = gridvis.pl

CFLAGS_gridvis = 

#$(shell $(PKGCONFIG) libgvc --cflags)

#LDFLAGS_gridvis = -Wl, --rpath -Wl,$(shell $(PKGCONFIG) libgvc --variable=libdir) \
#		   $(shell $(PKGCONFIG) libgvc --libs)
#

include $(BASEDIR)/etc/buildsys/base.mk

CFLAGS_BASE = -Wall $(DEFAULT_INCLUDES) -fPIC
