
RUSTC=$(HOME)/.cargo/bin/rustc
RUSTFLAGS=-O

GHC=ghc
GHCFLAGS=-O2

OCAML=ocamlopt
OCAMLFLAGS=-O2

CC=gcc
CFLAGS=-O2 -flto -std=c11 -Wall -Werror

BIN=R S sytyaful msearch mtree search tree osearch otree
FAST=$(BIN) search.js
PROG=$(BIN) Opt.py search.lua

all: $(BIN)

run: $(FAST:%=run-%) FORCE
slow: run run-Opt.py run-search.lua

$(PROG:%=run-%): run-%: %
	time ./$* >/dev/null
run-%: %
	time ./$* >/dev/null

run-%.js:
	ulimit -s unlimited && \
	time node --stack-size=400000000 $*.js > /dev/null

%: %.hs
	$(GHC) $(GHCFLAGS) -main-is $* $<

%: %.rs
	$(RUSTC) $(RUSTFLAGS) $<

%: %.ml
	$(OCAML) $(OCAMLFLAGS) -o $@ $<

%: %.sml
	mlton $<

FORCE:

.PHONY: clean $(BIN:%=run-%) FORCE run
clean:
	rm -f $(BIN) *.hi *.o *.cmi *.cmx
