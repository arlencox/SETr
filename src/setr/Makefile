.PHONY: all clean benchmark

#PP_FLAGS=$(shell ../check_deps Z3 4.4.0.0 mlbdd 0.5 cudd 2.2.0)
PP_FLAGS=$(shell ../check_deps mlbdd 0.5)

FLAGS=-j 0 -cflag -short-paths -pp "cppo $(PP_FLAGS)"

TARGET=../../lib

BUILD_TARGETS = $(addprefix SETr, .cma .cmxa)

TARGETS = $(addprefix _build/, $(BUILD_TARGETS))

$(TARGETS) : _tags SETr.mllib META
	ocamlbuild -use-ocamlfind $(FLAGS) $(BUILD_TARGETS)
	rm _tags
	rm SETr.mllib
	mkdir -p $(TARGET)
	cp `ls _build/*.cmi` $(TARGET)
	cp _build/SETr.cma _build/SETr.cmxa _build/SETr.a $(TARGET)
	cp META $(TARGET)
	rm META

%:%.in
	cppo -n $(PP_FLAGS) -o $@ $<

clean:
	ocamlbuild -clean
	rm -rf $(TARGET) _tags SETr.mllib META

install: META
	ocamlfind install setr META $(addprefix _build/, SETr.cma SETr.cmxa SETr.a SETr.cmi)
	rm META

uninstall:
	ocamlfind remove setr
