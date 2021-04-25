
RUSTC=$(HOME)/.cargo/bin/rustc
RUSTFLAGS=-O

GHC=ghc
GHCFLAGS=-O2

OCAML=ocamlopt
OCAMLFLAGS=-O2

BIN=R S search tree osearch otree

all: S search tree osearch otree

run: $(BIN:%=run-%) FORCE

$(BIN:%=run-%): run-%: %
	time ./$* >/dev/null

%: %.hs
	$(GHC) $(GHCFLAGS) -main-is $* $<

%: %.rs
	$(RUSTC) $(RUSTFLAGS) $<

%: %.ml
	$(OCAML) $(OCAMLFLAGS) -o $@ $<

FORCE:

.PHONY: clean $(BIN:%=run-%) FORCE run
clean:
	rm -f $(BIN) *.hi *.o *.cmi *.cmx
