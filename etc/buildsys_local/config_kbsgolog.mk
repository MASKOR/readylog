#*****************************************************************************
#          Makefile Build System for Fawkes: config for KBSGolog
#                            -------------------
#   Created on Sun Oct 12 02:02:02 2007
#   copyright (C) 2006-2007 by Tim Niemueller, AllemaniACs RoboCup Team
#
#   $Id$
#
#*****************************************************************************
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#*****************************************************************************

ECLIPSE_VERSION = 5.10
ECLLIBDIR = $(abspath $(BASEDIR)/ecl)
LIBDIR = $(ECLLIBDIR)
ECLINCDIR = $(abspath $(BASEDIR)/include)

DEFAULT_INCLUDES = -I$(ECLIPSE_INCDIR)
CFLAGS_BASE = -Wall -Werror $(DEFAULT_INCLUDES) -fPIC

LIBDIRS += $(ECLIPSE_LIBDIR)

# Yepp, if you think this should better go in rules_kbsgolog.mk then you are right.
# Unfortunately the TARGETS_all in the all target prerequesites is expanded too
# early and thus this won't work and we have to put it here.
ifneq ($(TARGETS),)
  TARGETS_all += $(foreach target,$(TARGETS),$(ECLLIBDIR)/$(target).so)
  TARGETS_all += $(foreach target,$(TARGETS),$(addprefix $(ECLINCDIR)/,$(ECL_$(target))))
  OBJS_all += $(foreach target,$(TARGETS),$(OBJS_$(target)))
endif

