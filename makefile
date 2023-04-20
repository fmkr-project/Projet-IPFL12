all:
	find -name main.bin -exec rm {} \;
	ocamlc ./main.ml -o ./main.bin

man:
	groff -man -Tascii ./manpage | less
