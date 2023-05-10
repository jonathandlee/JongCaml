.PHONY: test check

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

run:
	OCAMLRUNPARAM=b dune exec bin/main.exe

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

loc:
	 cloc --by-file --include-lang=OCaml src bin

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh	