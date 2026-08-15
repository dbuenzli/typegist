(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Custom [compare] function metadata key.

    If a gist or field defines a value for this key, the
    {!Fun.Generic.val-compare} function uses it for comparing the values.

    Using a function that always returns [0] like {!Compare.ignore}
    can also be used to indicate that a value or field should not
    influence the comparison.

    @canonical Typegist.Fun.Generic.Compare *)
module Compare : sig

  type 'a t = 'a -> 'a -> int
  (** The type for custom [compare] functions. *)

  val ignore : 'a t
  (** [ignore v0 v1] is [0]. *)

  include Typegist__gist.Meta.KEY
    with type ('a, 'b) value := 'b t (** @inline *)
end

val compare : 'a Typegist__gist.t -> 'a -> 'a -> int
