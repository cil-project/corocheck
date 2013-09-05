.PHONY: all install uninstall clean check

all:
	ocamlbuild -classic-display -use-ocamlfind corocheck.cma corocheck.cmxs

install: uninstall all
	ocamlfind install corocheck META _build/corocheck.cma _build/corocheck.cmxs

uninstall:
	ocamlfind remove corocheck

clean:
	ocamlbuild -clean
	$(MAKE) -C test clean

check: all
	$(MAKE) -C test
