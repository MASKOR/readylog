
BASEDIR = ../..

TARGETS = graphviz

OBJS_graphviz = graphviz.o

LIBS_graphviz = stdc++ eclipse

INSTALL_graphviz = graphviz.pl

CFLAGS_graphviz = $(shell $(PKGCONFIG) libgvc --cflags)

LDFLAGS_graphviz = -Wl,--rpath -Wl,$(shell $(PKGCONFIG) libgvc --variable=libdir) \
		   $(shell $(PKGCONFIG) libgvc --libs)

include $(BASEDIR)/etc/buildsys/base.mk



