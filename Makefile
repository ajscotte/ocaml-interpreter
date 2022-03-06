.PHONY: test check

build:
	dune build src

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

zip:
	rm -f jocalf.zip
	zip -r jocalf.zip . -x@exclude.lst

clean: bisect-clean
	dune clean
	rm -f jocalf.zip

doc:
	dune build @doc

repl:
	rlwrap dune exec bin/main.exe
