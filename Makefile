all: tyxml_translator_cli.native static/tyxml_translator_web.js

tyxml_translator_cli.native: src/tyxml_translator_cli.ml src/translator.ml
	ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" $@

static/tyxml_translator_web.js: src/tyxml_translator_web.ml
	ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" tyxml_translator_web.js
	cp _build/src/tyxml_translator_web.js static/

clean:
	ocamlbuild -clean
	rm -f static/tyxml_translator_web.js

.PHONY: all clean
