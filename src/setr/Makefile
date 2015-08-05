.PHONY: all clean benchmark

PP_FLAGS=$(shell ../check_deps Z3 4.4.0.0 mlbdd 0.5 cudd 2.2.0)

FLAGS=-j 0 -cflag -short-paths -pp "cppo $(PP_FLAGS)"

TARGET=../../lib

_build/SETr.cma _build/SETr.cmxa : _tags SETr.mllib META
	ocamlbuild -use-ocamlfind $(FLAGS) SETr.cma SETr.cmxa
	rm _tags
	rm SETr.mllib
	mkdir -p $(TARGET)
	cp `ls _build/*.cmi` $(TARGET)
	cp `ls _build/*/*.cmi` $(TARGET)
	cp _build/SETr.cma _build/SETr.cmxa _build/SETr.a $(TARGET)
	cp META $(TARGET)
	rm META

%:%.in
	cppo -n $(PP_FLAGS) -o $@ $<

clean:
	ocamlbuild -clean
	rm -rf $(TARGET)

install: META
	ocamlfind install setr META $(addprefix _build/, SETr.cma SETr.cmxa SETr.a SETr.cmi)
	rm META

uninstall:
	ocamlfind remove setr
