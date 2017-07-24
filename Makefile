all: tyxml_translator_cli.native static/tyxml_translator_web.js

tyxml_translator_cli.native: src/tyxml_translator_cli.ml src/translator.ml
	ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" $@

static/tyxml_translator_web.js: src/tyxml_translator_web.ml src/mypprintast.ml
	ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" tyxml_translator_web.js
	cp _build/src/tyxml_translator_web.js static/

clean:
	ocamlbuild -clean
	rm -f static/tyxml_translator_web.js

gh-pages: static/tyxml_translator_web.js
	cp -r static _static
	git checkout gh-pages
	find . -maxdepth 1 -not -path '*/\.*' -type f -exec rm "{}" \;
	cp -r _static/* .
	rm -rf _static
	git add --all *

.PHONY: all clean gh-pages
