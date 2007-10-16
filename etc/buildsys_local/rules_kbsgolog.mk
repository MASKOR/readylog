#*****************************************************************************
#          Makefile Build System for Fawkes: rules for KBSGolog
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

$(ECLINCDIR)/%.pl $(ECLINCDIR)/%.ecl: $$(SRCDIR)/$$(@F)
	$(SILENT) mkdir -p $(@D)
	$(SILENT) echo -e " --- Copying additional file $(notdir $@)"
	$(SILENT) cp -f $(SRCDIR)/$(@F) $@

