
BASEDIR = ../..

include $(BASEDIR)/etc/buildsys/config.mk

TARGETS = icp_shell

OBJS_icp_shell = getkey.o \
		 c_icp_shell.o \
		 icp_shell_interface.o

LIBS_icp_shell = eclipse

INSTALL_icp_shell = icp_shell_interface.pl \
		    icp_shell.pl

include $(BASEDIR)/etc/buildsys/base.mk

