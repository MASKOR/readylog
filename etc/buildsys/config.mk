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

### Debugging related options
SILENT = @

### Directories
#BASEDIR ?= $(HOME)/robocup/fawkes
SRCDIR ?= .
OBJDIR = .objs
DEPDIR = $(abspath $(SRCDIR)/.deps)
INSTALLDIR= $(abspath $(BASEDIR)/include)
VPATH = $(SRCDIR)
df = $(DEPDIR)/$(subst ._,,$(subst /,_,$(subst ..,__,$(subst ./,,$(*D))))_)$(*F)

### Programs used, do not mention trivial stuff like ln, rm, ls as per Makefile manual
CC = gcc
PKGCONFIG = $(shell which pkg-config)

### GCC version information
GCC_VERSION=$(shell LANG=C $CC -v 2>&1 | grep "gcc version" | awk '{ print $3 }')
GCC_VERSION_MAJOR=$(shell LANG=C $(CC) -v 2>&1 | grep "gcc version" | awk '{ print $$3 }' | awk -F. '{ print $$1 }')

### Eclipse
ECLIPSE_VERSION = 5.10
ECLIPSE_ARCH    = $(shell uname -i)_$(shell uname -s | tr "[A-Z]" "[a-z]")
ECLIPSE_PATH    = /usr/local/eclipse_$(ECLIPSE_VERSION)
ECLIPSE_LIBDIR  = $(ECLIPSE_PATH)/lib/$(ECLIPSE_ARCH)
ECLIPSE_INCDIR  = $(ECLIPSE_PATH)/include/$(ECLIPSE_ARCH)

### CFLAGS, preprocessor, compiler and linker options
LDFLAGS_LIBDIRS = -Wl,-R$(LIBDIR),-R$(ECLIPSE_LIBDIR)
DEFAULT_INCLUDES = -I$(ECLIPSE_INCDIR)
CFLAGS_BASE = -Wall -Werror -pthread $(DEFAULT_INCLUDES) -DBINDIR=\"$(BINDIR)\" -DLIBDIR=\"$(LIBDIR)\" -DPLUGINDIR=\"$(PLUGINDIR)\" -DCONFDIR=\"$(CONFDIR)\"
LDFLAGS_BASE = -L$(LIBDIR)
LDFLAGS_SHARED = -shared
LIBDIRS += $(ECLIPSE_LIBDIR)

### colors, to be used as command, not via echo
BLACK		= tput setaf 0
BG_BLACK	= tput setab 0
DARKGREY	= tput bold ; tput setaf 0
LIGHTGREY	= tput setaf 7
BG_LIGHTGREY	= tput setab 7
WHITE		= tput bold ; tput setaf 7
RED		= tput setaf 1
BG_RED		= tput setab 1
BRIGHTRED	= tput bold ; tput setaf 1
GREEN		= tput setaf 2
BG_GREEN	= tput setab 2
BRIGHTGREEN	= tput bold ; tput setaf 2
BROWN		= tput setaf 3
BG_BROWN	= tput setab 3
YELLOW		= tput bold ; tput setaf 3
BLUE		= tput setaf 4
BG_BLUE		= tput setab 4
BRIGHTBLUE	= tput bold ; tput setaf 4
PURPLE		= tput setaf 5
BG_PURPLE	= tput setab 5
PINK		= tput bold ; tput setaf 5
CYAN		= tput setaf 6
BG_CYAN		= tput setab 6
BRIGHTCYAN	= tput bold ; tput setaf 6
NORMAL		= tput sgr0

TBOLDGRAY	= \033[1;30m
TBLUE		= \033[0;34m
TBOLDBLUE	= \033[1;34m
TGREEN		= \033[0;32m
TBOLDGREEN	= \033[1;32m
TBROWN		= \033[0;33m
TYELLOW		= \033[1;33m
TRED		= \033[0;31m
TBOLDRED	= \033[1;31m
TNORMAL		= \033[0;39m
TBLACKBG	= \033[40m
TREDBG		= \033[41m
TGREENBG	= \033[42m
TORANGEBG	= \033[43m
TBLUEBG		= \033[44m
TMAGENTABG	= \033[45m
TCYANBG		= \033[46m
TGREYBG		= \033[47m

### Check if there are special config additions
ifneq ($(realpath $(BASEDIR)/etc/buildsys/$(BUILD_TYPE)_config.mk),)
include $(BASEDIR)/etc/buildsys/$(BUILD_TYPE)_config.mk
endif

