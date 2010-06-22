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
#   the Free Software Foundation; either version 2 of the License or
#   (at your option) any later version.
#
#*****************************************************************************

ECLIPSEBIN = eclipse-clp

#ifeq ($(shell which eclipse 2>&1 | grep -v "which: no"),)
#ifneq ($(shell ls $(ECLIPSE_LOC)/eclipse.ini 2>&1 | grep -v "ls:"),)

#ifeq ($(shell which eclipse; if [ "$$?" = "0" ]; then echo "yes"; else echo "no"; fi),no)
#  #echo -e " **** have eclipse!" 
#  #$(shell echo " got eclipse" )
#else
#  #echo -e " **** DON'T have eclipse!"
#  #$(shell echo " got NO eclipse" )
#endif

#HAVE_ECLIPSE = $(if $(shell which eclipse; echo $${?/1/}),1,0)
HAVE_ECLIPSE = $(if $(shell which $(ECLIPSEBIN); echo $${?/1/}),1,0)

ifeq ($(HAVE_ECLIPSE),1)
   ## we got eclipse, but which one is it? 
   #$(warning blub)
   ECLIPSE_LOC = $(shell which $(ECLIPSEBIN))
   ECLIPSE_CLP = $(if $(shell ls $(ECLIPSE_LOC)/eclipse.ini > /dev/null 2>&1; echo $${?/1/}),1,0)
   ifeq ($(ECLIPSE_CLP),1)
     ## got the right eclipse
     ## override stuff from buildsys/config.mk
     ECLIPSE_ARCH    = $(shell $(ECLIPSEBIN) -e "get_flag('hostarch',V),write(V)")
     ECLIPSE_PATH    = $(shell $(ECLIPSEBIN) -e "get_flag('installation_directory',V),write(V)")
     ECLIPSE_VERSION = $(shell $(ECLIPSEBIN) -e "get_flag('version',V),write(V)")
   else
     ## got the wrong eclipse
     ECLIPSE_ARCH    = 
     ECLIPSE_PATH    = 
     ECLIPSE_VERSION = 
   endif
else
  ## got NO eclipse

endif


## #######################################
## ECLiPSe specifics
## generate sane paths from the above
ECLIPSE_INCDIR  = $(ECLIPSE_PATH)/include/$(ECLIPSE_ARCH)
ECLIPSE_LIBDIR  = $(ECLIPSE_PATH)/lib/$(ECLIPSE_ARCH)

## #######################################
## stuff for local ECLiPSe shared libs
ECLINCDIR = $(abspath $(BASEDIR)/include)
ECLLIBDIR = $(abspath $(BASEDIR)/ecl)
## override LIBDIR to put ecl-libs in
LIBDIR    = $(ECLLIBDIR)

## #######################################
##  CFLAGS, PREPOCESSOR & LINKER OPTIONS
##  overrides stuff from buildsys/config.mk
DEFAULT_INCLUDES += -I$(ECLIPSE_INCDIR)
CFLAGS_BASE       = -Wall -Werror $(DEFAULT_INCLUDES) -fPIC
LIBDIRS          += $(ECLIPSE_LIBDIR)

# Yepp, if you think this should better go in rules_kbsgolog.mk then you are right.
# Unfortunately the TARGETS_all in the all target prerequesites is expanded too
# early and thus this won't work and we have to put it here.
ifneq ($(TARGETS),)
  TARGETS_all += $(foreach target,$(TARGETS),$(ECLLIBDIR)/$(target).so)
  TARGETS_all += $(foreach target,$(TARGETS),$(addprefix $(ECLINCDIR)/,$(ECL_$(target))))
  OBJS_all    += $(foreach target,$(TARGETS),$(OBJS_$(target)))
endif

