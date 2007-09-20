#*****************************************************************************
#                      Makefile Build System for Fawkes
#                       adapted for usage in KBSGolog
#                            -------------------
#   Created on Sun Sep 03 14:14:14 2006
#   copyright (C) 2006 by Tim Niemueller, AllemaniACs RoboCup Team
#
#*****************************************************************************
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#*****************************************************************************
#
#           $Id$
# last modified: $Date$
#            by: $Author$
#
#*****************************************************************************

# see http://make.paulandlesley.org/multi-arch.html

.SUFFIXES:

include $(BASEDIR)/etc/buildsys/config.mk

MAKECMDGOALS ?= all
MAKETARGET = $(MAKE) --no-print-directory --no-keep-going -C $@ \
                     -f $(CURDIR)/Makefile \
                     SRCDIR=$(CURDIR) BASEDIR=../$(BASEDIR) $(MAKECMDGOALS)

.PHONY: $(OBJDIR)
$(OBJDIR):
	+@[ -d $@ ] || mkdir -p $@
	+@$(MAKETARGET)

% :: $(OBJDIR) ; @:

.PHONY: nothing
nothing: ; @:

