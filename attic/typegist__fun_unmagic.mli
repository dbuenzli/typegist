(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Fun.Generic.unmagic implementation *)

(** Custom [unmagic] function metadata key.

    If a gist or field metadata defines a value for this key, the
    {!Fun.Generic.val-unmagic} function uses it for unmagicking
    the value.

    @canonical Typegist.Fun.Generic.Unmagic *)
module Unmagic : sig

  type 'a t = Obj.t -> ('a, string) result
  (** The type for custom [unmagic] functions. *)

  include Typegist__gist.Meta.KEY with
    type ('a, 'b) value := 'b t (** @inline *)
end

val unmagic : 'a Typegist__gist.t -> 'b -> ('a, string) result
val safer_unmarshal :
  ?first:int -> 'a Typegist__gist.t -> string -> ('a, string) result
