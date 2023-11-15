## 3110Project

# Mahjong OCaml!

This is an implementation of Japanese Riichi Mahjong, built entirely from scratch in Ocaml as our final project for CS 3110 at Cornell University. 

Project Members:
1. Alexander Peek (ap2298)
2. Jonathan Yen (jfy7)
3. Jonathan Lee (jdl282)


## Running the Game

# Running
To run the game, simply navigate to the directory you installed the game in and run the command `make run`. That's it!

# Testing
To test the game, simply navigate to the directory you install the game in and run the command `make test`


# Other helpful commands:

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

loc:
	 cloc --by-file --include-lang=OCaml src bin test

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh
