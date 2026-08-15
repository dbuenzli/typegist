typegist — Represent the essence of OCaml types as values
=========================================================

Typegist represents the essence of OCaml types as values.

This dynamic type representation can be used to devise generic
type-indexed functions – value serializers, generators, differs,
editors, FFI glue, etc. Any accessible type can be described up to the
limits defined by its public interface.

Typegist does not model OCaml's type language in full detail. It
focuses on a core structural subset decorated with typed-indexed
metadata to provide an ergonomic interface for both producers and
processors of the representation.

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


## Acknowledgements 

A grant from the [OCaml Software Foundation] helped to bring the first
public release of `typegist`.

[OCaml Software Foundation]: http://ocaml-sf.org/




