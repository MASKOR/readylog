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

# see http://make.paulandlesley.org/autodep.html
# see http://make.paulandlesley.org/rules.html

# indentation definitions, dash (-) is replaced with space
INDENT_STRING = ---
INDENT_PRINT := $(subst -, ,$(INDENT))
ifeq ($(MAKECMDGOALS),clean)
  DISABLE_OBJS_all_WARNING = 1
  ifeq ($(shell test -d qa; if [ "$$?" = "0" ]; then echo "yes"; fi),yes)
    ifneq ($(findstring qa,$(SUBDIRS)),qa)
      SUBDIRS += qa
    endif
  endif
endif

# Dependencies
-include $(DEPDIR)/*.d

OBJS_all = $(foreach target,$(TARGETS),$(OBJS_$(target)))
ifneq ($(TARGETS),)
# Do not delete .o files to allow for incremental builds
.SECONDARY: $(OBJS_all)
# Whenever the Makefile is modified rebuild everything
$(OBJS_all): $(SRCDIR)/Makefile
else
  ifneq ($(LIBS_all)$(PLUGINS_all)$(BINS_all),)
    ifneq ($(DISABLE_OBJS_all_WARNING),1)
    $(warning OBJS_all is not set. This is probably a bug. If you intended this set DISABLE_OBJS_all_WARNING to 1 to get rid of this warning.)
    endif
  endif
endif

# One to build 'em all
TARGETS_all = $(foreach target,$(TARGETS),$(INSTALLDIR)/$(target)/$(target).so)
INSTALL_all = $(foreach target,$(TARGETS),$(addprefix $(INSTALLDIR)/$(target)/,$(INSTALL_$(target))))
.PHONY: all
all: presubdirs $(TARGETS_all) $(INSTALL_all) subdirs

.PHONY: clean
clean: subdirs	
	$(SILENT) echo -e "$(INDENT_PRINT)--> Cleaning up directory $(TBOLDGRAY)$(CURDIR)$(TNORMAL)"
	$(SILENT) if [ "$(SRCDIR)/$(OBJDIR)" != "/" ]; then rm -rf $(SRCDIR)/$(OBJDIR) ; fi
	$(SILENT) if [ "$(DEPDIR)" != "" ]; then rm -rf $(DEPDIR) ; fi
	$(SILENT) if [ "$(TARGETS_all)" != "" ]; then rm -rf $(TARGETS_all) ; fi

ifeq (,$(findstring qa,$(SUBDIRS)))
.PHONY: qa
qa: subdirs
	$(SILENT) if [ -d "$(subst /.objs,,$(realpath $(CURDIR)))/qa" ]; then \
		echo -e "$(INDENT_PRINT)--> Building QA in $(subst $(realpath $(CURDIR)/$(BASEDIR))/,,$(subst /.objs,,$(realpath $(CURDIR)))/qa)"; \
		$(MAKE) --no-print-directory -C $(subst /.objs,,$(CURDIR))/qa INDENT="$(INDENT)$(INDENT_STRING)"; \
	fi
endif

.PHONY: presubdirs $(PRESUBDIRS) subdirs $(SUBDIRS)
presubdirs: $(PRESUBDIRS)
subdirs: $(SUBDIRS)

ifneq ($(PRESUBDIRS)$(SUBDIRS),)
$(PRESUBDIRS) $(SUBDIRS):
	$(SILENT) if [ ! -d "$(realpath $(SRCDIR)/$(@))" ]; then \
		echo -e "$(INDENT_PRINT)---$(TRED)Directory $(TNORMAL)$(TBOLDRED)$@$(TNORMAL)$(TRED) does not exist, check [PRE]SUBDIRS variable$(TNORMAL) ---"; \
		exit 1; \
	else \
		echo -e "$(INDENT_PRINT)--> Entering sub-directory $(TBOLDBLUE)$@$(TNORMAL) ---"; \
		$(MAKE) --no-print-directory --no-keep-going -C "$(realpath $(SRCDIR)/$@)" \
		$(MFLAGS) $(MAKECMDGOALS) INDENT="$(INDENT)$(INDENT_STRING)"; \
		if [ "$(MAKECMDGOALS)" != "clean" ]; then \
			echo -e "$(INDENT_PRINT)$(subst -, ,$(INDENT_STRING))<-- Leaving $@"; \
		fi \
	fi
endif

# Cancel built-in implicit rules, they suck
%: %.cpp
%: %.c

# NOTE: .. are replaced by __. This is needed to have objects that are in upper relative
# directories are build in .objs, another change is needed below in bin, lib and plugin targets,
# mocs etc.
%.o: %.cpp
	$(SILENT) mkdir -p $(DEPDIR)
	$(SILENT) mkdir -p $(@D)
	$(SILENT) echo "$(INDENT_PRINT)--> Compiling $(subst $(SRCDIR)/,,$<) (C++)"
	$(SILENT) mkdir -p $(dir $(subst ..,__,$@))
	$(SILENT) $(CC) -MD -MF $(df).td $(CFLAGS_BASE) $(CFLAGS) $(CFLAGS_$*) \
	$(addprefix -I,$(INCS_$*)) $(addprefix -I,$(INCDIRS)) -c -o $(subst ..,__,$@) $<
	$(SILENT)sed -e 's/^\([^:]\+\): \(.*\)$$/$(subst /,\/,$(@D))\/\1: \2/' < $(df).td > $(df).d; \
	sed -e 's/#.*//' -e 's/^[^:]*: *//' -e 's/ *\\$$//' -e 's/^ *//' \
	    -e '/^$$/ d' -e 's/$$/ :/' < $(df).td >> $(df).d; \
	rm -f $(df).td

%.o: %.c
	$(SILENT) mkdir -p $(DEPDIR)
	$(SILENT) mkdir -p $(@D)
	$(SILENT) echo "$(INDENT_PRINT)--- Compiling $(subst $(SRCDIR)/,,$<) (C)"
	$(SILENT) mkdir -p $(dir $(subst ..,__,$@))
	$(SILENT) $(CC) -MD -MF $(df).td $(CFLAGS_BASE) $(CFLAGS) $(CFLAGS_$*) \
	$(addprefix -I,$(INCS_$*)) $(addprefix -I,$(INCDIRS)) -c -o $(subst ..,__,$@) $<
	$(SILENT)sed -e 's/^\([^:]\+\): \(.*\)$$/$(subst /,\/,$(@D))\/\1: \2/' < $(df).td > $(df).d; \
	sed -e 's/#.*//' -e 's/^[^:]*: *//' -e 's/ *\\$$//' -e 's/^ *//' \
	    -e '/^$$/ d' -e 's/$$/ :/' < $(df).td >> $(df).d; \
	rm -f $(df).td

.SECONDEXPANSION:
$(INSTALLDIR)/%.so: $$(OBJS_$$(notdir $$*))
	$(SILENT) mkdir -p $(INSTALLDIR)/$(notdir $*)
	$(SILENT) echo -e " --- Linking $*"
	$(SILENT) mkdir -p $(@D)
	$(SILENT) $(CC) $(LDFLAGS_SHARED) $(LDFLAGS) $(LDFLAGS_$(notdir $*)) \
	$(addprefix -l,$(LIBS_$(notdir $*))) $(addprefix -l,$(LIBS)) \
	$(addprefix -L,$(LIBDIRS_$(notdir $*))) $(addprefix -L,$(LIBDIRS)) \
	-o $@ $(subst ..,__,$^)

$(INSTALLDIR)/%.pl $(INSTALLDIR)/%.ecl: $$(INSTALL_$$(notdir $$@))
	$(SILENT) mkdir -p $(dir $@)
	$(SILENT) echo -e " --- Copying additional file $(notdir $@)"
	$(SILENT) cp $(SRCDIR)/$(notdir $@) $@

