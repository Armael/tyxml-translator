## Tyxml-translator: a sugar-coated translation from HTML to Tyxml combinators

Web app: https://armael.github.io/tyxml-translator

### Rationale

Tyxml-translator implements a thin layer of sugar-coating on top of Tyxml's PPX.
Tyxml's PPX is able to parse HTML, and produce an equivalent OCaml expression
that uses Tyxml's combinators.

This is apparent e.g. when using the `-dsource` option with utop, which prints
the OCaml expression produced by the PPX, before evaluation:

```
$ utop -dsource
[...]
utop # #require "tyxml.ppx";;
[...]
utop # open Tyxml;;
[...]
utop # [%html {|<h1 id="foo">contents</h1>|}];;

;;Html.h1 ~a:[Html.a_id (Html.Xml.W.return "foo")]
   (Html.Xml.W.cons (Html.pcdata "contents") (Html.Xml.W.nil ()));;
- : [> Html_types.h1 ] Tyxml_html.elt = <h1 id="foo">contents</h1>
```


The present application uses Tyxml's PPX to do all the heavy lifting, and then
cleans up the output, and makes it available through a CLI tool and a web app.
The goal is to produce OCaml code that corresponds to what a human would write.

The intended use-case for this application is to be used as an example generator
or helper when writing UI components using Tyxml's combinators. Of course, it is
possible to directly use the Tyxml's PPX, but at the moment it is not as
flexible as the combinators.

In short, this app should be a nice way of answering the occasional questions:
"argh, how do I write \<this HTML thing\> in Tyxml already?".

### Building

```
opam pin add --dev-repo -n tyxml
opam pin add --dev-repo -n tyxml-ppx
opam reinstall tyxml tyxml-ppx
opam install ocamlbuild js_of_ocaml js_of_ocaml-tyxml js_of_ocaml-ocamlbuild
make
```

This should build the CLI program and the .js file for the web app.
