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

BASEDIR = ../..

ifneq ($(realpath /usr/bin/motif-config),)
#ifneq ($(realpath /usr/local/bin/motif-config),)
  HAVE_MOTIF    = 1
endif

ifneq ($(HAVE_MOTIF),1)
  WARN_TARGETS += warning_motif
else
  TARGETS = gridvis
  LDFLAGS_gridvis = $(shell motif-config --libs)
endif

ifneq ($(SRCDIR),)
all: $(WARN_TARGETS)

.PHONY: warning_motif
warning_motif:
	$(SILENT)echo -e "$(INDENT_PRINT)--- $(TRED)Omitting gridvis util$(TNORMAL) (no motif, lesstif-devel not installed)"
endif


OBJS_gridvis =  gridvis.o \
		grid-world.o \
		graphics.o \
		display.o

#		qlearner.o \
#		krandom.o \


LIBS_gridvis = stdc++ m nsl eclipse
ECL_gridvis = gridvis.pl

include $(BASEDIR)/etc/buildsys/base.mk

# Since we have buggy third-party code we do not want -Werror, yes the TMP thing *is* needed!
CFLAGS_TMP := $(CFLAGS_BASE)
CFLAGS_BASE = $(subst -Werror,,$(CFLAGS_TMP))

