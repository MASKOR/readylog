
BASEDIR = ../..
include $(BASEDIR)/etc/buildsys/config.mk

ifneq ($(PKGCONFIG),)
  HAVE_GRAPHVIZ = $(if $(shell $(PKGCONFIG) --exists 'libgvc'; echo $${?/1/}),1,0)
endif

ifneq ($(HAVE_GRAPHVIZ),1)
  WARN_TARGETS += warning_graphviz
else
  TARGETS = graphviz
endif

ifneq ($(SRCDIR),.)
all: $(WARN_TARGETS)

.PHONY: warning_graphviz
warning_graphviz:
	$(SILENT)echo -e "$(INDENT_PRINT)--- $(TRED)Omitting graphviz util$(TNORMAL) (graphviz-devel not installed)"
endif

OBJS_graphviz = graphviz.o
LIBS_graphviz = stdc++ eclipse
ECL_graphviz = graphviz.pl

CFLAGS_graphviz = $(shell $(PKGCONFIG) libgvc --cflags)
LDFLAGS_graphviz = -Wl,--rpath -Wl,$(shell $(PKGCONFIG) 'libgvc' --variable=libdir) \
		   $(shell $(PKGCONFIG) 'libgvc' --libs)

include $(BASEDIR)/etc/buildsys/base.mk
