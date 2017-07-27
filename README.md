## Tyxml-translator: a sugar-coated translation from HTML to Tyxml combinators

Web app URL: https://armael.github.io/tyxml-translator

[Tyxml](https://github.com/ocsigen/tyxml) is an OCaml library that allows
building HTML documents using a set of combinators (i.e. OCaml functions). It
ensures that the HTML produced is always valid, by relying on the OCaml
typechecker.

Tyxml's combinators closely match the names of HTML elements, but in some cases,
it can be confusing to know how to express a particular HTML construct using
these combinators.

This simple webapp is designed as a documentation tool or examples generator for
Tyxml's combinators: given a piece of HTML, it returns the corresponding OCaml
expression that uses Tyxml and produces the given HTML.

### Some more details

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
opam install tyxml.dev tyxml-ppx.dev
opam install ocamlbuild js_of_ocaml js_of_ocaml-lwt js_of_ocaml-ppx js_of_ocaml-ocamlbuild react
make
```

This should build the CLI program and the .js file for the web app.

### Hacking the webapp design (html/css)

1. Install frontend dependencies (gulp task manager, less transpiler, css auto-autoprefixer) with npm: `npm install`
2. Launch OCaml build and CSS compilation with: `gulp`
3. You can run a local server (browsersync) that monitor changes and build OCaml or recompile CSS automatically with `gulp watch`
