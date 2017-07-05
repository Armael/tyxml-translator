all: tyxml_translator_cli.native

tyxml_translator_cli.native: src/tyxml_translator_cli.ml src/translator.ml
	ocamlbuild -use-ocamlfind $@

clean:
	ocamlbuild -clean

.PHONY: all clean
