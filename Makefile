all: tyxml_translator_cli.native

tyxml_translator_cli.native: tyxml_translator_cli.ml translator.ml
	ocamlbuild -use-ocamlfind $@

clean:
	ocamlbuild -clean

.PHONY: all clean
