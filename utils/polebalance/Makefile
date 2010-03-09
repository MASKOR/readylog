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

## ####################
## 
## ####################

TARGETS = polebalance

OBJS_polebalance = pole_balance.o polebalance.o
LIBS_polebalance = EZX11 Xt m X11 stdc++ eclipse
ECL_polebalance  = polebalance.pl

OBJS_all = $(OBJS_polebalance)
#LIBS_all += $(LIBDIR)/polebalance.so

## ####################
## 
## ####################

OBJS_polegame = pole_game.o pole_balance.o
LIBS_polegame = EZX11 Xt m X11

BINS_all =	$(BINDIR)/polegame

## ####################
## 
## ####################

include $(BASEDIR)/etc/buildsys/base.mk

INCDIRS += $(abspath $(BASEDIR)/ecl)
LIBDIRS += $(abspath $(BASEDIR)/ecl)
