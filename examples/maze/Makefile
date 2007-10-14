# ############################################################################
#   ,=³ .%%% %%%%%%%. .%%%%%.  .%%%%%.   %%%%%.%%. %. %%%%%%%% %% Rheinisch-
#  [==].%%%   %%   %% %%   %%  %%   %%   %%  %%'%%.%%.%% %% %% %% Westfälische
#   //l%%%    %%   %% %%%%. ' %%       @ %%%%%' '%%%%%%% %% %%%%% Technische
#  [==]%%     %%|=%%   %%=>%  %%         %%  %%  '%%'%%% %% %% %% Hochschule
#  [==]%%%    %%   %% . '%%%% %%  '%%%   %%   %%  '% '%% %% %% %% Aachen
#  [==]'%%%   %%   %% %%   %%  %%   %%   http://kbsg.rwth-aachen.de/
#  o^^o '%%% %%%%%%%' '%%%%%'O '%%%%%'   Knowledge Based Systems Group
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


### #######################
PARAMFILE    = $(BASEDIR)/utils/config.pl
PREPROCESSOR = $(BASEDIR)/interpreter/readylog/preprocessor.pl
XMLTOPROLOG  = $(BASEDIR)/utils/xml2prolog.pl
DOXYGENERATE = $(BASEDIR)/utils/doc.pl

C_EXTENSIONS = $(BASEDIR)/include/c_extensions/

### CONFIG ################

SILENT = @

### COMMANDS ##############

ECHO = echo
ECLIPSE = eclipse

### RULES #################

all: preprocess

##

preprocess:
	$(SILENT) $(ECLIPSE) -b $(PREPROCESSOR) $(PWD)/$(FILENAME)

xml2prolog:
	$(SILENT) $(ECHO) " -- Parsing " $(CONFIGFILE) " to " $(PARAMFILE)
	$(SILENT) $(ECLIPSE) -b $(XMLTOPROLOG) $(CONFIGFILE) $(PARAMFILE)
	$(SILENT) sed 's%xmlns:xi("http://www.w3.org/2001/xinclude"), %%' <$(PARAMFILE) >$(PARAMFILE).tmp
	$(SILENT) mv $(PARAMFILE).tmp $(PARAMFILE) 

clean:
	$(SILENT) $(ECHO) " -- Removing '*~' & 'processed_*' ..."
	$(SILENT) rm *~ processed_*

clean_ext:
	$(SILENT) $(ECHO) " -- Making clean c_extensions for utils"
	$(SILENT) make clean -sC $(C_EXTENSIONS)

DOCFILES = *.pl *.readylog

doc: 
	$(SILENT) $(ECHO) " -- Generating DOCUMENTATION ..."
	$(SILENT) $(ECLIPSE) -b $(DOXYGENERATE) -e makedoc $(DOCFILES)

.PHONY: doc


run: 
	$(SILENT) make -s all
	$(SILENT) $(ECLIPSE) -b run.pl

