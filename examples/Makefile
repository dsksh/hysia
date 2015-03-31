PPRINT:=camlp4 pr_o.cmo pa_o.cmo
PKG:=pcre,camomile,dynlink
JG_HOME:=/Users/ishii/tmp/jingoo
CMPL:=ocamlfind ocamlopt -g -thread -package $(PKG) -linkpkg -I $(JG_HOME)/src $(JG_HOME)/src/jingoo.cmxa
JG_CMPL:=$(JG_HOME)/src/compiler

TARGET:= template

#.PHONY:$(TARGET)
all: $(TARGET)

template: template.ml
	$(CMPL) -o $@ template.ml
