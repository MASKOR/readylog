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
#            $Id$
#    description: Makefile for High-Level-Interface
#  last modified: $Date$
#             by: $Author$
#
# ############################################################################

BASEDIR = ../../..

TARGETS = libEZX11

OBJS_libEZX11 = \
		initx.o \
		windowx.o \
		colorx.o \
		grey.o \
		drawx.o \
		textx.o \
		cursorx.o

LIBS_libEZX11 = Xt m X11

include $(BASEDIR)/etc/buildsys/base.mk

# Since we have buggy third-party code we do not want -Werror, yes the TMP thing *is* needed!
CFLAGS_TMP := $(CFLAGS_BASE)
CFLAGS_BASE = $(subst -Werror,,$(CFLAGS_TMP))

