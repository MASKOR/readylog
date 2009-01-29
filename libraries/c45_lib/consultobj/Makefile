#*************************************************************************#
#*									 *#
#*		Makefile for the consulting object			 *#
#*		--------------------------------------			 *#
#*									 *#
#*************************************************************************#
#
# Written by Stefan Jacobs <Stefan_J@gmx.de>
# Creation Date:     2002-06-15
# Last Modified:     2009-01-29 by Dennis Pannhausen
#                               <Dennis.Pannhausen@rwth-aachen.de>


# Will be the name of the executable
MODUL_NAME = Consult
MODUL_TEST_NAME = ConsultObjectTest2

# Set Paths for compiling
OBJ_DIR = .
SRC_DIR = .
BIN_DIR = .
ECLIPSE_DIR = ../../../../eclipse

# The flags to compile with
INCLUDES = -I/usr/include
CFLAGS   = -O4 -g3 -Wall
COMPILER = gcc
CPPCOMPILER = g++

# The original files to compile
FILES  =   $(OBJ_DIR)/consult.o  \
           $(OBJ_DIR)/getdata.o  \
           $(OBJ_DIR)/getnames.o \
           $(OBJ_DIR)/header.o   \
           $(OBJ_DIR)/trees.o    \
	   $(OBJ_DIR)/userint.o


# The new files to test
OBJ_FILES = 	$(OBJ_DIR)/C45ConsultObject.o \
		$(OBJ_DIR)/C45AutoConsultObject.o \
		$(OBJ_DIR)/C45ConsultObjectTest2.o 




#*************************************************************************#

all: 	$(OBJ_FILES)
	$(CPPCOMPILER) -o $(BIN_DIR)/$(MODUL_TEST_NAME) $(OBJ_FILES) $(CFLAGS)
	etags *.c *.i *.cpp

original:    	$(FILES)	
		$(COMPILER) -o $(BIN_DIR)/$(MODUL_NAME) $(FILES) $(CFLAGS)
		etags *.c *.i *.cpp


clean:
	rm -f $(OBJ_DIR)/*.o
	rm -f $(BIN_DIR)/a.out
	rm -f $(BIN_DIR)/$(MODUL_NAME)
	rm -f $(BIN_DIR)/$(MODUL_TEST_NAME)
	rm -f $(SRC_DIR)/*~


$(OBJ_DIR)/%.o : $(SRC_DIR)/%.c
	$(COMPILER) $(CFLAGS) -c $< -o $@ $(INCLUDES) -g3
