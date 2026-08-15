(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Base representation. *)

module Fmt : sig
  type 'a t = Format.formatter -> 'a -> unit
  val pf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
  val str : ('a, Format.formatter, unit, string) format4 -> 'a
  val string : string t
  val char : char t
  val iter : ?pp_sep:unit t -> (('a -> unit) -> 'b -> unit) -> 'a t -> 'b t
  val sp : unit t
end

(* Names *)

type name = string
(** @canonical Typegist.Type.Gist.name *)

type qualified_name = string
(** @canonical Typegist.Type.Gist.qualified_name *)

type applied_name = string
(** @canonical Typegist.Type.Gist.applied_name *)

type variant_case_name = name
(** @canonical Typegist.Type.Gist.Case.name *)

type field_name = name
(** @canonical Typegist.Type.Gist.Field.name *)

type abstract_version_name = string
(** @canonical Typegist.Type.Gist.Abstract.Version.name *)

val name_module_path : applied_name -> qualified_name
val name_unqualified : applied_name -> name
val name_type_args : applied_name -> string

(* Interfaces *)

(** Array interface.

    @canonical Typegist.Type.Gist.Array_like.S  *)
module type ARRAY = sig

  val immutable : bool
  (** If [true] the values of the array type are immutable and
      using {!set} raises [Invalid_argument]. *)

  type t
  (** The type for arrays. *)

  type elt
  (** The type of elements of the array. *)

  val get : t -> int -> elt
  (** [get a i] is the [i]th zero-based element of [a]. *)

  val set : t -> int -> elt -> unit
  (** [set a i v] sets the [i]th zero-based element of [a] to [v].

      @raise Invalid_argument if {!immutable} is [true]. *)

  val length : t -> int
  (** [length a] is the length of [a]. *)

  val init : int -> (int -> elt) -> t
  (** [init n f] is an array of length [n] with [get v i = f i]. *)

  val iter : (elt -> unit) -> t -> unit
  (** [iter] iterates over all elements of the array in increasing
      index order. *)

  val fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a
  (** [fold_left f init a] folds [f] over [a]'s elements in increasing
      index order starting with [init]. *)

  val equal : (elt -> elt -> bool) -> t -> t -> bool
  (** [equal eq a0 a1] is [true] if and only if tests [a0] and [a1]
      have the same length [n] for all [i] in [[0];[n-1]],
      [eq (get a0 i) (get a1 i)] is [true]. *)

  val compare : (elt -> elt -> int) -> t -> t -> int
  (** [compare cmp a0 a1] compare [a0] and [a1] according to shortlex order,
      that is, short arrays are smaller and equal-sized arrays are compared
      in lexicographic order using [cmp] to compare elements. *)

  val pp_applied_name : Format.formatter -> unit ->  unit
  (** [pp_applied_name ppf ()] formats an applied name for the array type. *)
end

(** Hash table interface.

    @canonical Typegist.Type.Gist.Map_like.HASHTBL  *)
module type HASHTBL = sig
  type t
  (** The type for hash tables. *)

  type key
  (** The type for hash table keys. *)

  type value
  (** The type for hash table values. *)

  val create : int -> t
  (** [create n] is an empty hash table. *)

  val mem : t -> key -> bool
  (** [mem h k] is [true] iff [h] has a binding for [k]. *)

  val add : t -> key -> value -> unit
  (** [add h k v] adds a binding for [k] to [v] in [h]. *)

  val remove : t -> key -> unit
  (** [remove h k] remove the current binding of [k] in [h]. *)

  val find_opt : t -> key -> value option
  (** [find_opt h k] is the current binding for key [k] in [h] (if any). *)

  val fold : (key -> value -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  (** [fold f h init] folds over the bindings of [h] with [f] starting with
      [init]. *)

  val equal : (value -> value -> bool) -> t -> t -> bool
  (** [equal veq h0 h1] tests whether [h0] and [h1] contain the same keys
      and associate them with equal values according to [veq] *)

  val compare : (key -> key -> int) -> (value -> value -> int) -> t -> t -> int
  (** [compare kcmp vcmp h0 h1] totally order [h0] and [h1] with [kcmp] used
      to compare keys and [vcmp] used to compare the value of equal
      keys. *)

  val pp_applied_name : Format.formatter -> unit -> unit
  (** [pp_applied_name ppf ()] formats an applied name for the hash table
      type. *)
end

(** Map interface.

    @canonical Typegist.Type.Gist.Map_like.MAP  *)
module type MAP = sig
  type t
  (** The type for maps. *)

  type key
  (** The type for map keys. *)

  type value
  (** The type for map values. *)

  val empty : t
  (** [empty] is an empty map. *)

  val mem : key -> t -> bool
  (** [mem k m] is [true] iff [m] has a binding for [k]. *)

  val add : key -> value -> t -> t
  (** [add k v m] is [m] with [k] binding to [v]. *)

  val remove : key -> t -> t
  (** [remove k m] is [m] without a binding for [k]. *)

  val find_opt : key -> t -> value option
  (** [find_opt k m] is the binding for key [k] in [m] (if any). *)

  val fold : (key -> value -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  (** [fold f m init] folds over the bindings of [m] with [f] starting with
      [init]. *)

  val equal : (value -> value -> bool) -> t -> t -> bool
  (** [equal eq m0 m1] tests whether [m0] and [m1] contain equal keys and
      associate them with equal values according to [eq] *)

  val compare : (value -> value -> int) -> t -> t -> int
  (** [compare cmp m0 m1] totally order [m0] and [m1] with [cmp] used to
      compare the values of equal keys. *)

  val pp_applied_name : Format.formatter -> unit -> unit
  (** [pp_applied_name ppf ()] formats an applied name for the map type. *)
end

(** Cell interface.

    @canonical Typegist.Type.Gist.Cell_like.S  *)
module type CELL = sig

  val immutable : bool
  (** If [true] the cell is immutable and
      using {!set} raises [Invalid_argument]. *)

  type t
  (** The type for cells. *)

  type contents
  (** The type for contents of the cells. *)

  val make : contents -> t
  (** [make v] is a cell holding value [v]. *)

  val get : t -> contents
  (** [get c] is the value of [c]. *)

  val set : t -> contents -> unit
  (** [set c v] sets the value of [c] to [v].

      @raise Invalid_argument if {!immutable} is [true]. *)

  val pp_applied_name : Format.formatter -> unit -> unit
  (** [pp_applied_name] formats an applied name for the cell. *)
end

(* Module types *)

type ('elt, 'array) array_module =
  (module ARRAY with type elt = 'elt and type t = 'array)
(** @canonical Typegist.Type.Gist.Array_like.module' *)

type ('k, 'v, 'hashtbl) hashtbl_module =
  (module HASHTBL with type key = 'k and type value = 'v and type t = 'hashtbl)
(** @canonical Typegist.Type.Gist.Map_like.hasthbtl_module *)

type ('k, 'v, 'map) map_module =
  (module MAP with type key = 'k and type value = 'v and type t = 'map)
(** @canonical Typegist.Type.Gist.Map_like.map_module *)

type ('a, 'cell) cell_module =
  (module CELL with type contents = 'a and type t = 'cell)
(** @canonical Typegist.Type.Gist.Cell_like.module' *)

(* Gists *)

type bytes_encoding = | Binary | Utf_8 (** *)
(** @canonical Typegist.Type.Gist.Array.bytes_encoding *)

type 'a scalar =
| Unit : unit scalar
| Bool : bool scalar
| Char : char scalar
| Uchar : Uchar.t scalar
| Int : int scalar
| Int32 : int32 scalar
| Int64 : int64 scalar
| Nativeint : nativeint scalar
| Float : float scalar (** *)
(** @canonical Typegist.Type.Gist.Scalar.t *)

type ('p, 'a) field
(** @canonical Typegist.Type.Gist.Field.t *)

type ('p, _) fields =
| Ctor : 'ctor -> ('p, 'ctor) fields
| App : ('p, 'v -> 'ctor) fields * ('p, 'v) field -> ('p, 'ctor) fields (** *)
(** @canonical Typegist.Type.Gist.Product.fields *)

type 'p product = ('p, 'p) fields
(** @canonical Typegist.Type.Gist.Product.t *)

type 't tuple = 't product
(** @canonical Typegist.Type.Gist.Tuple.t *)

type 'r record = 'r product
(** @canonical Typegist.Type.Gist.Record.t *)

type 'v variant_case
(** @canonical Typegist.Type.Gist.Variant.Case.t *)

type 'v variant
(** @canonical Typegist.Type.Gist.Variant.t *)

type 's variant_like =
| Option : 'a t -> 'a option variant_like
| Either : 'a t * 'b t -> ('a, 'b) Either.t variant_like
| Result : 'a t * 'b t -> ('a, 'b) result variant_like
| List : 'a t -> 'a list variant_like
| Variant : 'a variant -> 'a variant_like (** *)
(** @canonical Typegist.Type.Gist.Variant_like.t *)

and ('elt, 'array) array_like =
| Bytes : bytes_encoding -> (char, bytes) array_like
| String : bytes_encoding -> (char, string) array_like
| Array : 'elt t -> ('elt, 'elt array) array_like
| Iarray : 'elt t -> ('elt, 'elt iarray) array_like
| Bigarray1 :
    'elt t * ('elt, 'b) Bigarray.kind * 'c Bigarray.layout ->
    ('elt, ('elt, 'b, 'c) Bigarray.Array1.t) array_like
| Module :
    'elt t * ('elt, 'array) array_module -> ('elt, 'array) array_like (** *)
(** @canonical Typegist.Type.Gist.Array_like.t *)

and ('k, 'v, 'm) map_like =
| Hashtbl_module :
    'k t * 'v t * ('k, 'v, 'h) hashtbl_module -> ('k, 'v, 'h) map_like (** *)
| Map_module :
    'k t * 'v t * ('k, 'v, 'map) map_module -> ('k, 'v, 'map) map_like (** *)
(** @canonical Typegist.Type.Gist.Map_like.t *)

and ('a, 'cell) cell_like =
| Lazy : 'a t -> ('a, 'a lazy_t) cell_like
| Ref : 'a t -> ('a, 'a ref) cell_like
| Atomic : 'a t -> ('a, 'a Atomic.t) cell_like
| Module : 'a t * ('a, 'cell) cell_module -> ('a, 'cell) cell_like (** *)
(** @canonical Typegist.Type.Gist.Cell_like.t *)

and ('a, 'b) func
(** @canonical Typegist.Type.Gist.Func.t *)

and 'a abstract_version
(** @canonical Typegist.Type.Gist.Abstract.Version.t *)

and 'a abstract
(** @canonical Typegist.Type.Gist.Abstract.t *)

and ('a, 'b) view
(** @canonical Typegist.Type.Gist.View.t *)

and 'a expr =
| Scalar : 'a scalar -> 'a expr
| Tuple : 't tuple -> 't expr
| Record : 'r record -> 'r expr
| Variant_like : 's variant_like -> 's expr
| Array_like : ('elt, 'array) array_like -> 'array expr
| Map_like : ('k, 'v, 'map) map_like -> 'map expr
| Cell_like : ('a, 'cell) cell_like -> 'cell expr
| Func : ('a, 'b) func -> ('a -> 'b) expr
| Abstract : 'a abstract -> 'a expr
| View : ('a, 'b) view -> 'a expr
| Rec : 'a t lazy_t -> 'a expr (** *)
(** @canonical Typegist.Type.Gist.Expr.t *)

(* Gists *)

and 'a t
(** @canonical Typegist.Type.Gist.t *)

type v = V : 'a t -> v (** *)
(** @canonical Typegist.Type.Gist.v *)

val make :
  ?name:applied_name -> ?doc:string -> ?id:'a Type.Id.t ->
  ?meta:'a Typegist__meta.t -> 'a expr -> 'a t

val name : 'a t -> applied_name
val doc : 'a t -> string
val id : 'a t -> 'a Type.Id.t
val meta : 'a t -> 'a Typegist__meta.t
val expr : 'a t -> 'a expr

val has_gist : 'b t -> 'a t -> bool
val is_recursive : 'a t -> bool
val is_nameless : 'a t -> bool
val update :
  ?name:qualified_name -> ?doc:string -> ?meta:'a Typegist__meta.t ->
  ?expr:'a expr -> 'a t -> 'a t

val of_gist :
  ?name:qualified_name -> ?doc:string -> ?meta:'a Typegist__meta.t ->
  ?expr:'a expr -> 'a t -> 'a t

val rebind : 'a t -> 'b t -> 'b t
val replace : 'a t -> by:'a t -> 'b t -> 'b t

(* Expressions *)

val expr_map_gists : ('a. 'a t -> 'a t) -> 'b expr -> 'b expr
val expr_fold_gists : ('a. 'acc -> 'a t -> 'acc) -> 'acc -> 'b expr -> 'acc

(* Scalars *)

val scalar_applied_name : 'a scalar -> applied_name
val char : char t

(* Fields *)

val make_field :
  ?name:field_name -> ?doc:string -> ?meta:('p, 'v) Typegist__meta.t2 ->
  ?set:('p -> 'v -> unit) -> ?iset:('p -> 'v -> 'p) ->
  'v t -> ('p -> 'v) -> ('p, 'v) field

val field_name : ('p, 'v) field ->  field_name
val field_doc : ('p, 'v) field ->  string
val field_meta : ('p, 'v) field -> ('p, 'v) Typegist__meta.t2
val field_gist :  ('p, 'v) field -> 'v t
val field_get : ('p, 'v) field ->  ('p -> 'v)
val field_set : ('p, 'v) field ->  ('p -> 'v -> unit) option
val field_iset :  ('p, 'v) field ->  ('p -> 'v -> 'p) option
val field_is_mutable : ('p, 'v) field -> bool

(* Products *)

val product_is_empty : ('p, 'ctor) fields -> bool
val product_is_singleton : ('p, 'ctor) fields -> bool
val product_fold_fields :
  ('v. 'acc -> ('p, 'v) field -> 'acc) -> 'acc -> ('p, 'ctor) fields -> 'acc

val product_field_count : ('p, 'ctor) fields -> int

(* Variants *)

val make_variant_case :
  ?doc:string -> variant_case_name -> 'v product -> 'v variant_case
val variant_case_name : 'v variant_case -> variant_case_name
val variant_case_doc : 'v variant_case -> string
val variant_case_product : 'v variant_case -> 'v product
val variant_case_is_inline_record : 'v variant_case -> bool

val make_variant : 'v variant_case list -> case_index:('v -> int) -> 'v variant
val variant_cases : 'v variant -> 'v variant_case iarray
val variant_case_index : 'v variant -> 'v -> int
val variant_is_polymorphic_variant : 'v variant -> bool

(* Array likes *)

val array_like_elt : ('elt, 'aarray) array_like -> 'elt t

(* Map likes *)

val map_like_key : ('k, 'v, 'map) map_like -> 'k t
val map_like_value : ('k, 'v, 'map) map_like -> 'v t

(* Cell likes *)

val cell_like_contents : ('a, 'cell) cell_like -> 'a t

(* Functions *)

val make_func : domain:'a t -> range:'b t -> ('a, 'b) func
val func_domain : ('a, 'b) func -> 'a t
val func_range : ('a, 'b) func -> 'b t

(* Abstract types *)

val make_abstract_version : name:string -> 'a t -> 'a abstract_version
val abstract_version_name : 'a abstract_version -> string
val abstract_version_gist : 'a abstract_version -> 'a t
val make_abstract :
  ?version_index:('a -> int) -> 'a abstract_version list -> 'a abstract

val abstract_is_opaque : 'a abstract -> bool
val abstract_versions : 'a abstract -> 'a abstract_version iarray
val abstract_version_index : 'a abstract -> ('a -> int)

(* Views *)

val make_view : inject:('a -> 'b) -> project:('b -> 'a) -> 'b t -> ('a, 'b) view
val view_inject : ('a, 'b) view -> ('a -> 'b)
val view_project : ('a, 'b) view -> ('b -> 'a)
val view_gist : ('a, 'b) view -> 'b t

(* Formatting *)

val pp_bigarray_layout :'a Bigarray.layout Fmt.t
val pp_bigarray_kind : ('a, 'b) Bigarray.kind Fmt.t
val pp_expr : 'a expr Fmt.t
val pp_def : 'a t Fmt.t
val pp_type_arg : single:bool -> 'a t Fmt.t
val pp_applied_name : v list -> qualified_name Fmt.t
