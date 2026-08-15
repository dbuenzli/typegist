(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Generic functions.

    See the {{!page-cookbook.generic_funs_template}generic function template}
    to write your own.

    @canonical Typegist.Fun.Generic *)

(** {1:pp Pretty-printing} *)

module Fmt = Typegist__fun_fmt.Fmt

val pp : 'a Typegist__gist.t -> Format.formatter -> 'a -> unit
(** [pp g ppf v] formats [v] as described by [g] on [ppf] with a pseudo
    OCaml syntax similar to what you would see in the OCaml toplevel (REPL).

    {b Warning.} For the time being the output format should not be considered
    stable, it may change between minor versions of the library. Do not
    snapshot test these outputs in public CI runs like the opam repository.

    {b Overriding.} The function can be selectively overriden on a gist
    or a field with the {!Fmt} metadata key. This happens before ignoring
    (see below).

    {b Ignoring.} Gists or fields whose {!Type.Gist.Meta.Ignore} metadata
    key is [true] are not printed. If you want a gist or field to be ignored
    only for the purpose of printing you can override [pp] with {!Fmt.ignore}
    in the metadata. *)

(** {1:equal Equality} *)

module Equal = Typegist__fun_equal.Equal

val equal : 'a Typegist__gist.t -> 'a -> 'a -> bool
(** [equal g v0 v1] determines the equality of values [v0] and [v1] as
    described by [g].

    {b Overriding.} The function can be selectively overriden on
    a gist or a field with the {!Equal} metadata key. This happens
    before ignoring (see below).

    {b Ignoring.} Gists or fields whose {!Type.Gist.Meta.Ignore} metadata
    key is [true] are ignored for determining equality (the comparison
    returns [true]). If you want a gist or field to be ignored only for
    the purpose of equality checking you can override [equal] with
    {!Equal.ignore} in the metadata.

    {b Undefined.} Unless ignored (see above), the function raises
    [Invalid_argument] in these cases:
    {ul
    {- On functional values.}
    {- On {{!Type.Gist.Abstract.is_opaque}opaque} abstract types}
    {- On abstract types for which [v0] and [v1] return different
       {!Type.Gist.Abstract.version_index}.}} *)

(** {1:compare Comparison} *)

module Compare = Typegist__fun_compare.Compare

val compare : 'a Typegist__gist.t -> 'a -> 'a -> int
(** [compare g v0 v1] compares values [v0] and [v1] as described by [g].

    {b Overriding.} The function can be selectively overriden on
    a gist or field with the {!Compare} metadata key. This happens
    before ignoring (see below).

    {b Ignoring.} Gists or fields whose {!Type.Gist.Meta.Ignore} metadata
    key is [true] are ignored by the comparison (the comparison
    returns [0]). If you want a gist or field to be ignored only for
    the purpose of comparison you can override [compare] with
    {!Compare.ignore} in the metadata.

    {b Undefined.} Unless ignored (see above), the function raises
    [Invalid_argument] in these cases:
    {ul
    {- On functional values.}
    {- On {{!Type.Gist.Abstract.is_opaque}opaque} abstract types.}
    {- On abstract types for which [v0] and [v1] return different
     {!Type.Gist.Abstract.version_index}.}} *)

(** {1:random Random generation} *)

module Random = Typegist__fun_random.Random

val random :
  'a Typegist__gist.t -> ?size:int -> ?state:Stdlib.Random.State.t -> unit -> 'a
(** [random g ~state ()] generates a random value described by [g] with:
    {ul
    {- [state], the random state to start from, defaults to
       {!Random.State.make_self_init}.}
    {- [size], an indicative, strictly positive, size factor to let
        users control an upper bound on the size and complexity of
        generated values (defaults to [23]). It should be used by generators to
        proportionally bound the length of sequences or the number of
        nodes in trees. It can be altered by gists and fields via the
        {!Random.Size} key.}}

    {b Warning.} For the time being the generation should not be considered
    stable, it may change between minor versions of the library.

    {b Overriding.} The function can be selectively overriden on
    a gist or field with the {!Random.Gen} metadata key. The sizing
    factor can also be for a given gist or field by using the {!Random.Size}
    metadata key.

    {b Undefined.} Unless overriden with a {!Random.Gen.const} constant value
    the function raises [Invalid_argument] in these cases:
    {ul
    {- On functional values (may be lifted in the future).}
    {- On {{!Type.Gist.Abstract.is_opaque}opaque} abstract types.}} *)



(* Left out for the first release, see the [attic] in the repo

(** {1:unmagic Unmagic & unmarshal} *)

module Unmagic = Typegist__fun_unmagic.Unmagic

val unmagic : 'a Typegist__gist.t -> 'b -> ('a, string) result
(** [unmagic g v] is [Ok v] if the memory layout of [v] is compatible with
    type described, and as described, by [g].

    {b Warning.} Given that gists values may only partially model a given
    type this is a conservative check. Some value [v] may error even though
    they happen to belong to the type described by the gist [g]. Depending on
    how your value is described (e.g. variant case order descripition differing
    from actual type definition) it may also mixup values that have the same
    structure.

    {b Overriding.} The function can be selectively overriden on
    a gist or field with the {!Compare} metadata key. This happens
    before ignoring (see below).

    {b Undefined.} For now a few things are unsupposed and error
    systematically.
    {ul
    {- Functional values.}
    {- On {{!Type.Gist.Abstract.is_opaque}opaque} abstract types.}
    {- Bigarray values.}} *)

val safer_unmarshal :
  ?first:int -> 'a Typegist__gist.t -> string -> ('a, string) result
(** [safer_unmarshal g s] is [unmagic g (Marshall.from_string s first)].
    [first] defaults to [0]. *)

*)
