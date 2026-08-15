(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Implementation of Fun.Generic.equal. *)

(** Custom [equal] function metadata key.

    If a gist or field metadata defines a value for this key, the
    {!Fun.Generic.val-equal} function uses it for determining equality.

    Using a function that always returns [true] like {!Equal.ignore}
    can also be used to indicate that a value or field should not influence
    the comparison.

    @canonical Typegist.Fun.Generic.Equal *)
module Equal : sig

  type 'a t = 'a -> 'a -> bool
  (** The type for custom [equal] functions. *)

  val ignore : 'a t
  (** [ignore v0 v1] is [true]. *)

  include Typegist__gist.Meta.KEY
    with type ('a, 'b) value := 'b t (** @inline *)
end

val equal : 'a Typegist__gist.t -> 'a -> 'a -> bool
