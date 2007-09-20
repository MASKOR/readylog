
BASEDIR = ../..

include $(BASEDIR)/etc/buildsys/config.mk

TARGETS = graphviz

OBJS_graphviz = graphviz.o

LIBS_graphviz = eclipse

INSTALL_graphviz = graphviz.pl

CFLAGS_graphviz = $(shell $(PKGCONFIG) libgvc --cflags)

LDFLAGS_graphviz = $(shell $(PKGCONFIG) libgvc --libs)

include $(BASEDIR)/etc/buildsys/base.mk



