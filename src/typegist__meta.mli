(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Gist metadata.

    Metadata is associated both to type gists and the fields of products.
    It allows to associate arbitrary type-dependent key-value data to the
    description. Gist processors can create and expose new keys to allow
    descriptions to influence their results. See for example the generic
    function {{!page-cookbook.convention_generic_fun_metadata}metadata
    convention}.

    Since the dictionary value is polymorphic in the of the type of the gist
    or field you describe the interface is slightly unusual compared to a
    classic heterogeneous dictionary. A new key is the result of a functor
    instantiated with the data type of the value. For example:
    {[
      type 'a fmt = Format.formatter -> 'a -> unit
      module Meta_fmt = Type.Gist.Meta.Key (struct type ('a, 'b) t = 'b fmt end)
      module Meta_skip = Type.Gist.Meta.Key (struct type ('a, 'b) t = bool end)
    ]}
    you can then use the module's {{!Meta.KEY}key interface} to test
    key membership, add and remove data for the key (you can also
    use {!Meta.mem} etc. but it's likely more cumbersome
    as the key cannot be conveniently packed into a value). For example:
    {[
      let meta : int Meta.t =
        Type.Gist.Meta.empty
        |> Meta_fmt.add Format.pp_print_int
        |> Type.Gist.Meta.Min.add 0
        |> Type.Gist.Meta.Max.add 255
    ]}

    @canonical Typegist.Type.Gist.Meta *)

type ('p, 'f) t2
(** The type for metadata for types parameterized by ['p] and ['f]. Key values
    can depend on ['p] and ['f]. This is used for
    {{!Type.Gist.Field.t}[('p, 'f) Field.t]} values. It allows to talk about
    both the product type ['p] and the field type ['f]. *)

type 'a t = ('a, 'a) t2
(** The type for metadata for types parameterized by ['a]. Key values can
    depend on ['a]. This is used for {{!Type.Gist.t}['a Gist.t]} gist values.
    It allows to talk about the type ['a] represented by the gist. *)

(** Type signature to describe the type dependent value of a type.

    See the preamble of {!Type.Gist.Meta} for examples implementing
    this signature.  *)
module type VALUE = sig
  type ('a, 'b) t
  (** The type for the key value that depends on ['a] and ['b]. *)
end

(** The type for key modules.

    A key module is a module that handles a specific key. *)
module type KEY = sig

  type ('a, 'b) value
  (** The type for the key's value. *)

  val mem : ('a, 'b) t2 -> bool
  (** [mem m] is [true] iff [m] has a binding for the key. *)

  val add : ('a, 'b) value -> ('a, 'b) t2 -> ('a, 'b) t2
  (** [add v m] is [m] with the key bound to [v] *)

  val find : ('a, 'b) t2 -> ('a, 'b) value option
  (** [find m] is the binding for the key (if any). *)

  val remove : ('a, 'b) t2 -> ('a, 'b) t2
  (** [remove m] is [m] with the binding for the key removed (if
      bound). *)
end

(** [Key (V)] is a new key with values of type ['a V.t]. *)
module Key (V : VALUE) : KEY with type ('a, 'b) value := ('a, 'b) V.t

val empty : ('a, 'b) t2
(** [empty] is the empty metadata. *)

val is_empty : ('a, 'b) t2 -> bool
(** [is_empty m] is [true] iff [m] has no bindings. *)

val mem : (module Key : KEY) -> ('a, 'b) t2 -> bool
(** [mem k m] is [true] iff [m] has a binding for [k]. *)

val add : (module Key : KEY) -> ('a, 'b) Key.value -> ('a, 'b) t2 -> ('a, 'b) t2
(** [add k v m] is [m] with the key bound to [v] *)

val find : (module Key : KEY) -> ('a, 'b) t2 -> ('a, 'b) Key.value option
(** [find k m] is the binding for the [k] (if any). *)

val remove : (module Key : KEY) -> ('a, 'b) t2 -> ('a, 'b) t2
(** [remove k m] is the binding for the [k] (if any). *)

(** {1:std_keys Standard keys} *)

(** [Default] is a key to store a default value for the type or field value. *)
module Default : KEY with type ('a, 'b) value = 'b

(** [Deprecated] is a key for indicating to processors that the type or
    field is subject to an [@deprectated] attribute with given payload *)
module Deprecated : KEY with type ('a, 'b) value := string

(** [Ignore] is a key for indicating to processors to ignore the type or
    field (they may or may not abide by it). *)
module Ignore : KEY with type ('a, 'b) value := bool

(** [Immediate] is a key for specifying that the type definition is subject to
    an [[@immediate]] attribute (if [true]). *)
module Immediate : KEY with type ('a, 'b) value := bool

(** [Immediate64] is a key for specifying that the type definition is subject to
    an [[@immediate64]] attribute (if [true]). *)
module Immediate64 : KEY with type ('a, 'b) value := bool

(** [Min] is a key to store a minimal value for the type or field value. *)
module Min : KEY with type ('a, 'b) value = 'b

(** [Max] is a key to store a maximal value for the type or field value. *)
module Max : KEY with type ('a, 'b) value = 'b

(** [Unboxed] is a key for specifying that the type definition is subject to
    an [[@unboxed]] attribute (if [true]) or an [[@boxed]] attribute
    (if [false]). *)
module Unboxed : KEY with type ('a, 'b) value := bool
