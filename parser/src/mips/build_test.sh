ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core test.native
./test.native test1.s
./test.native test2.s
./test.native test3.s
