
RUSTC=$(HOME)/.cargo/bin/rustc
RUSTFLAGS=-O

GHC=ghc
GHCFLAGS=-O2

OCAML=ocamlopt
OCAMLFLAGS=-O2

BIN=R S search tree osearch otree mtree msearch
PROGS=$(BIN) search.js

all: $(BIN)

run: $(PROGS:%=run-%) FORCE

$(BIN:%=run-%): run-%: %
	time ./$* >/dev/null

run-%.js:
	ulimit -s unlimited && time node --stack-size=400000000 $*.js > /dev/null

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
