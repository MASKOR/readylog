#*************************************************************************#
#*									 *#
#*		Makefile for the C4.5 induction system			 *#
#*		--------------------------------------			 *#
#*									 *#
#*************************************************************************#


#	Set CFLAGS for your C compiler, e.g.
#
#	  * if you need to debug the programs (sorry about that!)
#	      CFLAGS = -g
#	    (You might also want to reactivate the lint commands that
#	    have been commented out below.)
#
#	  * if your C compiler supports higher optimization levels
#	      CFLAGS = -O3 etc


# ==================================================
# Edited by Stefan Jacobs <stefan_j@gmx.de>, 14.6.02
# ==================================================
# ========================================================================
# Edited by Dennis Pannhausen <Dennis.Pannhausen@rwth-aachen.de>, 26.01.09
# ========================================================================

BIN_DIR = ../bin
OBJ_DIR = ../obj
SRC_DIR = .


CFLAGS = -O4 -g3 #-Wall


#	Definitions of file sets

core = \
	$(OBJ_DIR)/getnames.o \
	$(OBJ_DIR)/getdata.o \
	$(OBJ_DIR)/trees.o \
	$(OBJ_DIR)/getopt.o \
	$(OBJ_DIR)/header.o 


trees = \
	$(OBJ_DIR)/besttree.o \
	$(OBJ_DIR)/build.o \
	$(OBJ_DIR)/info.o \
	$(OBJ_DIR)/discr.o \
	$(OBJ_DIR)/contin.o \
	$(OBJ_DIR)/subset.o \
	$(OBJ_DIR)/prune.o \
	$(OBJ_DIR)/stats.o \
	$(OBJ_DIR)/st-thresh.o \
	$(OBJ_DIR)/classify.o \
	$(OBJ_DIR)/confmat.o \
	$(OBJ_DIR)/sort.o \
	$(core)


rules = \
	$(OBJ_DIR)/rules.o \
	$(OBJ_DIR)/genlogs.o \
	$(OBJ_DIR)/genrules.o \
	$(OBJ_DIR)/makerules.o \
	$(OBJ_DIR)/prunerule.o \
	$(OBJ_DIR)/siftrules.o \
	$(OBJ_DIR)/testrules.o \
	$(OBJ_DIR)/stats.o \
	$(OBJ_DIR)/confmat.o \
	$(OBJ_DIR)/sort.o \
	$(core)


#  C4.5 decision tree generator

c4.5:	$(OBJ_DIR)/c4.5.o $(trees)
	cc -o $(BIN_DIR)/c4.5 $(OBJ_DIR)/c4.5.o $(trees) -lm


#  C4.5 production rule generator

c4.5rules: $(OBJ_DIR)/c4.5rules.o $(rules)
	cc -o $(BIN_DIR)/c4.5rules $(OBJ_DIR)/c4.5rules.o $(rules) -lm


#  C4.5 decision tree interpreter

consult: $(OBJ_DIR)/consult.o $(OBJ_DIR)/userint.o $(core)
	cc -o $(BIN_DIR)/consult $(OBJ_DIR)/consult.o $(OBJ_DIR)/userint.o $(core)


#  C4.5 production rule interpreter

consultr: $(OBJ_DIR)/consultr.o $(OBJ_DIR)/rules.o $(OBJ_DIR)/userint.o $(core)
	cc -o $(BIN_DIR)/consultr $(OBJ_DIR)/consultr.o $(OBJ_DIR)/rules.o $(OBJ_DIR)/userint.o $(core)



$(OBJ_DIR)/%.o : $(SRC_DIR)/%.c
	cc $(CFLAGS) -c $< -o $@



#  Make everything

all:
	make c4.5
	make c4.5rules
	make consult
	make consultr
	cc -o $(BIN_DIR)/xval-prep $(SRC_DIR)/xval-prep.c
	cc -o $(BIN_DIR)/average $(SRC_DIR)/average.c -lm


clean:
	rm -f $(BIN_DIR)/average
	rm -f $(BIN_DIR)/c4.5
	rm -f $(BIN_DIR)/c4.5rules
	rm -f $(BIN_DIR)/consult
	rm -f $(BIN_DIR)/consultr
	rm -f $(BIN_DIR)/xval-prep
	rm -f $(OBJ_DIR)/*.o


$(trees): $(SRC_DIR)/defns.i $(SRC_DIR)/types.i $(SRC_DIR)/extern.i
$(rules): $(SRC_DIR)/defns.i $(SRC_DIR)/types.i $(SRC_DIR)/extern.i
