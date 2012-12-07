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

INCLUDES = -I /opt/local/lib/ocaml/site-lib/oUnit
#INCLUDES = -I /usr/lib/ocaml/oUnit

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

BFLAGS = -w Aer-29 -dtypes -g -I . $(INCLUDES)
OFLAGS = -w Aer-29 -dtypes    -I . $(INCLUDES)

ifeq (no,yes)
OFLAGS += -g -p
STRIP = true
endif

# external libraries common to all binaries

EXTOBJS =
EXTLIBS = str unix nums dynlink oUnit

EXTCMA	= $(addsuffix .cma,$(EXTLIBS)) $(addsuffix .cmo,$(EXTOBJS))
EXTCMXA = $(addsuffix .cmxa,$(EXTLIBS)) $(addsuffix .cmx,$(EXTOBJS))

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

LIBGENERATED = lexer.ml parser.mli parser.ml

LIBMODULES = ptree util parser lexer pretty

LIBML  = $(addsuffix .ml,  $(LIBMODULES))
LIBMLI = $(addsuffix .mli, $(LIBMODULES))
LIBCMO = $(addsuffix .cmo, $(LIBMODULES))
LIBCMX = $(addsuffix .cmx, $(LIBMODULES))

#$(LIBCMO) $(LIBCMX): INCLUDES += $(LIBINCLUDES)
$(LIBCMX): OFLAGS += -for-pack Hss

# build targets

byte: hss.cma
opt:  hss.cmxa

hss.cma: hss.cmo
	$(OCAMLC) -a $(BFLAGS) -o $@ $^

hss.cmxa: hss.cmx
	$(OCAMLOPT) -a $(OFLAGS) -o $@ $^

hss.cmo: $(LIBCMO)
	$(OCAMLC) $(BFLAGS) -pack -o $@ $^

hss.cmx: $(LIBCMX)
	$(OCAMLOPT) $(OFLAGS) -pack -o $@ $^

# depend target

include .depend.lib

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

# .ml4.ml:
# 	$(CAMLP4) pr_o.cmo -impl $< > $@

# lib/coq/%.vo: lib/coq/%.v
# 	$(COQC8) -I lib/coq $<

# lib/coq-v7/%.vo: lib/coq-v7/%.v
# 	$(COQC7) -I lib/coq-v7 $<

# jc/jc_ai.ml: jc/jc_annot_inference.ml jc/jc_annot_fail.ml Makefile
# 	if test "@enable_apron@" = "yes" ; then \
# 	  echo "# 1 \"jc/jc_annot_inference.ml\"" > jc/jc_ai.ml; \
# 	  cat jc/jc_annot_inference.ml >> jc/jc_ai.ml; \
# 	else \
# 	  echo "# 1 \"jc/jc_annot_fail.ml\"" > jc/jc_ai.ml; \
# 	  cat jc/jc_annot_fail.ml >> jc/jc_ai.ml; \
# 	fi

# %_why.v: %.mlw $(BINARY)
# 	$(BINARY) -coq $*.mlw

# %_why.pvs: %.mlw $(BINARY)
# 	$(BINARY) -pvs $*.mlw

###################
# clean and depend
###################

.PHONY: distclean

distclean: clean
	rm -f config.status config.cache config.log Makefile config.ml

depend:
	rm -f $^
	$(MAKE) $^
