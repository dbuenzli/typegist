(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Fun.Generic.random implementation *)

(** Custom [random] function and sizing metadata keys.

    These keys influence the {!Fun.Generic.val-random} function.

    @canonical Typegist.Fun.Generic.Random *)
module Random : sig

  (** Custom [random] function metadata key.

      If a gist or field metadata defines a value for this key, the
      {!Fun.Generic.val-random} function uses it for random generation.

      Using a function that always returns the same value like {!Gen.const}
      does can be used to have a substructure remaining constant. *)
  module Gen : sig

    type 'a t = size:int -> Random.State.t -> bound:int -> 'a
    (** The type for custom random generators.

        [bound] is a bounding factor that initally depends on [size] for
        recursive data structures. When the bound reaches [0] you should no
        longer recurse. *)

    val const : 'a -> 'a t
    (** [const v] leaves the random state untouched and always returns [v]. *)

    include Typegist__gist.Meta.KEY with
      type ('a, 'b) value := 'b t (** @inline *)
  end

  (** Sizing factor alteration key.

      If a gist or field metadata defines a value for this key, the
      {!Fun.Generic.val-random} function uses it as its [size] argument. *)
  module Size : sig
    type t = int
    include Typegist__gist.Meta.KEY with
      type ('a, 'b) value := t (** @inline *)
  end
end

val random :
  'a Typegist__gist.t -> ?size:int -> ?state:Stdlib.Random.State.t ->
  unit -> 'a
