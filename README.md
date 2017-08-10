# Uwt-js

An experiment
with [js_of_ocaml](https://github.com/ocsigen/js_of_ocaml)
and [node](https://github.com/nodejs/node)

[uwt](https://github.com/fdopen/uwt) is an OCaml binding to libuv, and
libuv is also used by node. So perhaps we can translate uwt code to
javascript with js_of_ocaml...

It's not really useful, if you want to interact with node or
JavaScript libraries - there is
already [ocaml-nodejs](https://github.com/fxfactorial/ocaml-nodejs)
for this purpose. But you could at least reuse a little bit of your own
code for either native OCaml or JavaScript.

It requires a very recent nodejs version (8.0 or later). And you have
to pass special flags to nodejs
(e.g. `--harmony_tailcalls`). JavaScript engines can't deal very well
with the continuation passing style of lwt. But there has apparently
been a lot of progress recently, that is not enabled by default.


## Known problems

- the memory management of node doesn't fit well with lwt's higher
  level interface and conventions.

- No finalizers, no `Weak` support. Resource management is more
  complicated.

- No support for `Lwt.pause` / `enter_iter_hooks` / `leave_iter_hooks`
