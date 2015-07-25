.PHONY: all clean

PP_FLAGS=$(shell ../check_deps Z3 4.4.0.0 mlbdd 0.5 cudd 2.2.0)

MAKE_DIR=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
LIB_DIR=$(MAKE_DIR)/../../lib

all: _tags
	SETR_LIB=$(LIB_DIR) ocamlbuild -use-ocamlfind Main.d.byte Main.native
	rm -f _tags

clean:
	ocamlbuild -clean

%:%.in
	  cppo -n $(PP_FLAGS) -o $@ $<
