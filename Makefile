.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec ansi/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f chessfinal.zip
	zip -r chessfinal.zip . -x _build/\* .git/\*

clean:
	dune clean
	rm -f chessfinal.zip

doc:
	dune build @doc

