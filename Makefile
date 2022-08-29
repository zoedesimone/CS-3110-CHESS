.PHONY: test check

build:
	dune build

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

run:
	dune exec ./src/main.exe
	
utop:
	OCAMLRUNPARAM=b dune utop src
	
zip:
	rm -f chess.zip
	zip -r chess.zip . -x@exclude.lst

docs:
	dune build @doc	

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

clean: bisect-clean
	dune clean
	rm -f search.zip

loc:
	ocamlbuild -clean
	cloc --by-file --include-lang=OCaml .

