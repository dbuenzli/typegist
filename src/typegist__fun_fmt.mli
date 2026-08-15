(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Fun.Generic.pp implementation *)

(** Custom [pp] function metadata key.

    If a gist of field metadata defines a value for this key the
    {!Fun.Generic.val-pp} function uses it for printing.

    Using a function that prints nothing like {!Fmt.ignore} can
    also be used to indicate a value or field should not
    be printed.

    @canonical Typegist.Fun.Generic.Fmt *)
module Fmt : sig

  type 'a t = Format.formatter -> 'a -> unit
  (** The type for custom [pp] functions. *)

  val ignore : 'a t
  (** [ignore] prints nothing. *)

  include Typegist__gist.Meta.KEY with
    type ('a, 'b) value := Format.formatter -> 'b -> unit (** @inline *)
end

val pp : 'a Typegist__gist.t -> Format.formatter -> 'a -> unit
