.PHONY: all clean example install uninstall

all:
	$(MAKE) -C src all

example:
	$(MAKE) -C src example
	ln -s src/main/Main.native
	ln -s src/main/Main.d.byte

clean:
	$(MAKE) -C src clean
	rm -f Main.native Main.d.byte


install:
	ocamlfind install setr lib/META $(filter-out lib/META,$(wildcard lib/*))

uninstall:
	  ocamlfind remove setr
