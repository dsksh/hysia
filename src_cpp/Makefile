.SUFFIXES: 
.SUFFIXES: .o .cpp .h .d .l .y
.PHONY : doc clean

CMD		:= integrate
LIB		:= libmycapd.a

TARGET	:= $(CMD) $(LIB)

CAPD_DIR := /usr/local

INCLUDES := -I$(CAPD_DIR)/include -I/opt/local/include
CFLAGS	:= -O3 -g -ansi -pedantic -Wall -frounding-math $(INCLUDES)
LDFLAGS := -L$(CAPD_DIR)/lib -lcapd

CXX     := g++ #@CXX@
SED     := sed #@SED@
MV      := mv #@MV@
AR      := ar #@AR@
RANLIB  := ranlib #@RANLIB@
RM      := rm #@RM@
LN      := ln #@LN@
LEX     := flex #@FLEX@
YACC    := bison #@BISON@
DOXYGEN := doxygen #@DOXYGEN@

SRC     := $(wildcard *.c) $(wildcard *.cc) $(wildcard *.cpp)
SRC_EX  := $(SRC) $(wildcard *.h)
OBJECT  := $(foreach file, $(SRC), $(basename $(file)).o)
OBJ_COMMON := MapEx.o parallelotope.o nodebuilder.o firstzero.o simulate.o

DEP_FILE := $(OBJECT:.o=.d)

all: 
	@for t in $(TARGET); do \
	if [ ! -z $$t ]; then ${MAKE} $$t; fi \
	done

clean:
	-$(RM) -f $(TARGET) $(OBJECT) $(DEP_FILE) *~

ifneq "$(MAKECMDGOALS)" "clean"
  -include $(DEP_FILE)
endif

$(CMD): $(OBJECT)
	$(CXX) $(CFLAGS) $(LDFLAGS) -o $@ $@.o $(OBJ_COMMON) $(LIBS)

$(LIB): $(OBJECT)
	$(AR) cru $@ $(OBJ_COMMON)
#	$(AR) d $@ integrate.o
	$(RANLIB) $@

#$(OBJECT): util.h

.c.o .cc.o .cpp.o:
	$(CXX) $(CFLAGS) -c $<

.c.d .cc.d .cpp.d:
	$(CXX) $(CFLAGS) -M $< | \
	$(SED) 's,\($(notdir $*)\.o\) *:,$(dir $@)\1 $@: $(MAKE_INC) $(MAKE_CFG) ,' >$@.tmp
	$(MV) $@.tmp $@

