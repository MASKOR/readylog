# ############################################################################
#
#            $Id: Makefile 1 2007-03-01 23:58:15Z tim $
#    description: Makefile for ReadyLog programs
#  last modified: $Date: 2007-03-02 00:58:15 +0100 (Fri, 02 Mar 2007) $
#             by: $Author: tim $
#
# ############################################################################

BASEDIR = ../..

### VARIABLES #############

FILENAME     = agent.readylog
CONFIGFILE   = $(BASEDIR)/cfg/config.xml
#
PARAMFILE    = $(BASEDIR)/utils/config.pl
C_EXTENSIONS = $(BASEDIR)/include/c_extensions/
PREPROCESSOR = $(BASEDIR)/interpreter/readylog/preprocessor.pl
DOXYGENERATE = $(BASEDIR)/utils/doc.pl

### RULES #################

all: preprocess

##

preprocess:
	@ eclipse -b $(PREPROCESSOR) $(PWD)/$(FILENAME)

xml2prolog:
	@ echo "Parsing " $(CONFIGFILE) " to " $(PARAMFILE)
	@ eclipse -b xml2prolog.pl $(CONFIGFILE) $(PARAMFILE)
	@ sed 's%xmlns:xi("http://www.w3.org/2001/xinclude"), %%' <$(PARAMFILE) >$(PARAMFILE).tmp
	@ mv $(PARAMFILE).tmp $(PARAMFILE) 

clean:
	@ echo "Making clean c_extensions for utils"
	@ make clean  -sC $(C_EXTENSIONS)

doc: 
	@ eclipse -b $(DOXYGENERATE) -e makedoc $(DOCFILES)

DOCFILES = *.pl *.readylog

.PHONY: doc

