TARGET	:= integrate

CAPD_DIR := /usr/local

INCLUDES := -I$(CAPD_DIR)/include
CFLAGS	:= -O3 -g -ansi -pedantic -Wall -frounding-math $(INCLUDES)
LDFLAGS := -L$(CAPD_DIR)/lib -lcapd

CXX     := g++ #@CXX@
SED     := sed #@SED@
MV      := mv #@MV@
AR      := ar #@AR@
RM      := rm #@RM@
LN      := ln #@LN@
LEX     := flex #@FLEX@
YACC    := bison #@BISON@
DOXYGEN := doxygen #@DOXYGEN@

SRC     := $(wildcard *.c) $(wildcard *.cc) $(wildcard *.cpp) $(wildcard *.h)
OBJECT  := $(foreach file, $(SRC), $(basename $(file)).o)
OBJ_COMMON := MyIMap.o

all: 
	@for t in $(TARGET); do \
	if [ ! -z $$t ]; then ${MAKE} $$t; fi \
	done

clean:
	-$(RM) -f $(TARGET) $(OBJECT) $(DEP_FILE) *~

$(OBJECT): util.h

trajectory.o: MyIMap.h MyIMap.o

.c.o .cc.o .cpp.o:
	$(CXX) $(CFLAGS) -c $<

$(TARGET): $(OBJECT)
	$(CXX) $(CFLAGS) $(LDFLAGS) -o $@ $@.o $(OBJ_COMMON) $(LIBS)
