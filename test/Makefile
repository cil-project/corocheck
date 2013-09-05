CILLY=cilly
COPTS=-Wno-attributes
CORO=../_build/corocheck.cma
# if CoroCheck has been installed, you can also use:
# CORO=corocheck

.PHONY: all clean
all: inference.pdf

inference.dot: inference.c
	$(CILLY) $(COPTS) --load=$(CORO) \
		--doCoroCheck --dotFile="$@" --coopAttr=coroutine --blockAttr=blocking \
		-c $<

inference.pdf: inference.dot
	dot -Tpdf -o $@ $<

clean:
	rm -f *.o *.i *.cil.* *.dot *.pdf
