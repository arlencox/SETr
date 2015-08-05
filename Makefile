.PHONY: all clean example install uninstall

all:
	$(MAKE) -C src all

example:
	$(MAKE) -C src example
	ln -sf src/main/Main.native
	ln -sf src/main/Main.d.byte

clean:
	$(MAKE) -C src clean
	rm -f Main.native Main.d.byte


install:
	ocamlfind install setr lib/META $(filter-out lib/META,$(wildcard lib/*))

uninstall:
	  ocamlfind remove setr
