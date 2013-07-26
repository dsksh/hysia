VERSION=0.0.1

VERBOSEMAKE ?= yes

ifeq ($(VERBOSEMAKE),yes)
  QUIET =
else
  QUIET = yes
endif

## install the binaries
#DESTDIR =
#
#prefix	    = /usr/local
#exec_prefix = ${prefix}
#datarootdir = ${prefix}/share
#
#BINDIR  = $(DESTDIR)${exec_prefix}/bin
#LIBDIR  = $(DESTDIR)${exec_prefix}/lib
#DATADIR = $(DESTDIR)${prefix}/share
#MANDIR  = $(DESTDIR)${datarootdir}/man

#OINCLUDES = -I /usr/lib/ocaml/oUnit
#OINCLUDES = -I /opt/local/lib/ocaml/site-lib/oUnit

CXXDIR  = ../src_cpp
LIBNAME = mycapd

# OS specific stuff
EXE   = 
STRIP = strip

# other variables
OCAMLC    = ocamlc
OCAMLOPT  = ocamlopt
OCAMLDEP  = ocamldep
OCAMLLEX  = ocamllex
OCAMLYACC = ocamlyacc
OCAMLDOC  = ocamldoc
OCAMLLIB  = /opt/local/lib/ocaml
#OCAMLLIB  = /usr/lib/ocaml
OCAMLBEST = opt

CAMLP5O   = camlp5o

BFLAGS = -w Aer-29 -dtypes -g -I . $(OINCLUDES)
OFLAGS = -w Aer-29 -dtypes    -I . $(OINCLUDES)

ifeq (no,yes)
OFLAGS += -g -p
STRIP = true
endif

# external libraries common to all binaries

EXTOBJS =
EXTLIBS = str unix nums dynlink

EXTCMA	= $(addsuffix .cma,$(EXTLIBS)) $(addsuffix .cmo,$(EXTOBJS))
EXTCMXA = $(addsuffix .cmxa,$(EXTLIBS)) $(addsuffix .cmx,$(EXTOBJS))

# C/C++
CC  = gcc
CXX = g++

CAPD_DIR := /usr/local

INCLUDES := -I$(OCAMLLIB) -I$(CAPD_DIR)/include -I../src_cpp
LDFLAGS := -L$(CAPD_DIR)/lib -lcapd

CFLAGS   = -O3 -g -ansi -pedantic -Wall -frounding-math
CXXFLAGS = $(CFLAGS)

###############
# main target
###############

all: opt

plugins: plugins.opt

.PHONY: byte opt clean depend all install
.PHONY: plugins plugins.byte plugins.opt

#############
# hss library
#############

LIBGENERATED = lexer.ml parser.mli parser.ml parser_atm.mli parser_atm.ml

LIBMODULES = hashcons model_common pretty ptree util model parser parser_atm lexer capd_sending simulating

LIBML  = $(addsuffix .ml,  $(LIBMODULES))
LIBMLI = $(addsuffix .mli, $(LIBMODULES)) capd_stubs.mli capd_simulating_stubs.mli
LIBCMO = $(addsuffix .cmo, $(LIBMODULES)) capd_stubs.cmi capd_simulating_stubs.cmi
LIBCMX = $(addsuffix .cmx, $(LIBMODULES)) capd_stubs.cmi capd_simulating_stubs.cmi

#$(LIBCMO) $(LIBCMX): OINCLUDES += $(LIBINCLUDES)
$(LIBCMX): OFLAGS += -for-pack Hss

COBJS = capd_stubs.o capd_simulating_stubs.o capd_integrator.o

# build targets

byte: hss.cma
opt:  hss.cmxa

CCLIBFLAGS := -cclib -lstdc++ -cclib -L$(CAPD_DIR)/lib -cclib -lcapd -cclib -L$(CXXDIR) -cclib -l$(LIBNAME)

hss.cma: hss.cmo $(COBJS)
	$(OCAMLC) -a $(BFLAGS) -o $@ $(CCLIBFLAGS) $^

hss.cmxa: hss.cmx $(COBJS)
	$(OCAMLOPT) -a $(OFLAGS) -o $@ $(CCLIBFLAGS) $^

hss.cmo: $(LIBCMO)
	$(OCAMLC) $(BFLAGS) -pack -o $@ $^

hss.cmx: $(LIBCMX)
	$(OCAMLOPT) $(OFLAGS) -pack -o $@ $^

# depend target

include .depend.lib

hss.cma hss.cmxa: $(CXXDIR)/lib$(LIBNAME).a

.depend.lib: $(LIBGENERATED)
	$(OCAMLDEP) -slash -I . $(LIBML) $(LIBMLI) > $@

depend: .depend.lib

# clean target

# clean target

LIBSDIRS = .
LIBCLEAN = $(addsuffix /*.cm[iox], $(LIBSDIRS)) \
	   $(addsuffix /*.annot, $(LIBSDIRS)) \
	   $(addsuffix /*.output, $(LIBSDIRS)) \
	   $(addsuffix /*.automaton, $(LIBSDIRS)) \
	   $(addsuffix /*.o, $(LIBSDIRS)) \
	   $(addsuffix /*~, $(LIBSDIRS))

clean::
	rm -f $(LIBCLEAN) $(LIBGENERATED)
	rm -f hss.cm[aiox] hss.[ao] hss.cmxa
	rm -f .depend.lib

##################
# hss binary
##################

byte: hss.byte
opt:  hss.opt

hss.opt: hss.cmxa main.cmx 
	$(if $(QUIET),@echo 'Linking  $@' &&) \
	    $(OCAMLOPT) $(OFLAGS) -o $@ $(EXTCMXA) $^
	$(STRIP) $@

hss.byte: hss.cma main.cmo
	$(if $(QUIET),@echo 'Linking  $@' &&) \
	    $(OCAMLC) $(BFLAGS) -o $@ $(EXTCMA) $^

hss: hss.opt
	ln -sf hss.opt $@

main.cmo: hss.cma
main.cmx: hss.cmxa

clean::
	rm -f main.cm[iox] main.annot main.o
	rm -f hss.byte hss.opt hss

################
# generic rules
################

%.cmi: %.mli
	$(if $(QUIET),@echo 'Ocamlc   $<' &&) $(OCAMLC) -c $(BFLAGS) $<

%.cmo: %.ml
	$(if $(QUIET),@echo 'Ocamlc   $<' &&) $(OCAMLC) -c $(BFLAGS) $<

%.cmx: %.ml
	$(if $(QUIET),@echo 'Ocamlopt $<' &&) $(OCAMLOPT) -c $(OFLAGS) $<

%.cmxs: %.ml
	$(if $(QUIET),@echo 'Ocamlopt $<' &&) $(OCAMLOPT) -shared $(OFLAGS) -o $@ $<

%.ml: %.mll
	$(OCAMLLEX) $<

%.ml %.mli: %.mly
	$(OCAMLYACC) -v $<

.c.o: 
	$(if $(QUIET),@echo 'Compiling $<' &&) $(CC) $(CFLAGS) $(INCLUDES) -c -o $@ $<

.cpp.o:
	$(if $(QUIET),@echo 'Compiling $<' &&) $(CXX) $(CXXFLAGS) $(INCLUDES) -c -o $@ $<

###################
# clean and depend
###################

.PHONY: distclean

distclean: clean
	rm -f config.status config.cache config.log Makefile config.ml

depend:
	rm -f $^
	$(MAKE) $^
