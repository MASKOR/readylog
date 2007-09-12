
TARGET = graphviz

DEBUG   = -DDEBUG
CXX     = g++
CFLAGS   = -g -Wall -W -fPIC `pkg-config libgvc --cflags`
CXXFLAGS = -g -Wall -W -fPIC
SHARED = -shared


######  Includes   ######

INC_DIR += -I$(HOME)/local/eclipse_5.10/include/x86_64_linux
INC_DIR += -I/usr/local/eclipse/include/i386_linux
INC_DIR += -I.


######  Linker flags   ######

LFLAGS =  -leclipse -Wl,--rpath -Wl,`pkg-config libgvc --variable=libdir` `pkg-config libgvc --libs`

######  Libraries  ######

APPL1_LIBS = -L/usr/local/eclipse/lib/i386_linux -L$(HOME)/local/eclipse_5.10/lib/x86_64_linux -L/usr/lib64/graphviz
APPL1_SYS_LIBS =  

######  Files  ######
GRAPHVIZ_PL  =  graphviz.pl
APPL1_FILES  =  graphviz.o


######################
# implicit rules

.SUFFIXES: .c .cpp

%.o: %.cpp
	@echo " --- Compiling $<"
	$(CXX) $(CFLAGS) -c $< -o $@ $(INC_DIR)

$(TARGET): $(APPL1_FILES)
	$(CXX) $(SHARED) $(APPL1_FILES) -o $(TARGET) $(INC_DIR) $(APPL1_LIBS) $(LFLAGS)

######################

build:  $(TARGET)

clean:
	-rm -f *.o *.bak *~ *%
	-rm -f $(TARGET)


