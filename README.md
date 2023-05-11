typegist — Represent the essence of OCaml types as values
=========================================================
%%VERSION%%

Typegist represents the essence of OCaml types as values. This dynamic
type representation can be used to devise generic type-indexed
functions – value printers, parsers, differs, generators, editors,
ffi, etc.

The mechanism is flexible: more than one representation can be provided
for a single type and any accessible type can be described up to the
limits defined by its public interface. Abstract types can expose
multiple versioned public representations (or none) which allows to
interface with older representations your program may be subjected to.

Typegist aims at providing an ergonomic interface for both producers
and consumers of the representation. As such it does not try to
capture the full complexity of OCaml's type definition language. It
focuses on a core structural type representation decorated with
type-indexed existential metadata for customizing and extending the
behaviour of generic functions.

Typegist is distributed under the ISC license. It has no dependencies. 

Homepage: <https://erratique.ch/software/typegist>

# Installation

Typegist can be installed with `opam`:

    opam install typegist

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

# Documentation

The documentation can be consulted [online][doc] or via `odig doc typegist`.

Questions are welcome but better asked on the [OCaml forum][ocaml-forum] 
than on the issue tracker.

[doc]: https://erratique.ch/software/typegist/doc
[ocaml-forum]: https://discuss.ocaml.org/





