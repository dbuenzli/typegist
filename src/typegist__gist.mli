(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Type gists.

    Type gist values represents the essence of OCaml types as values.

    See the {{!page-index.quick_start}quick start} or the {{!page-cookbook}
    cookbook} for simple blueprints. Generic functions can be found in
    {!Fun.Generic} and there is a {{!page-cookbook.generic_funs_template}
    generic function template} to write your own.

    @canonical Typegist.Type.Gist *)

(** {1:names Names} *)

type name = string
(** The type for unqualified names (without the module path). For example [t],
    [int], [signal], [Left], [`Left]. See also {!type-qualified_name}. *)

type qualified_name = Typegist__repr.qualified_name
(** The type for qualified names. These are {!type-name}s prefixed by the
    module path to access them from the top-level scope.
    For example [Buffer.t], [int], [Sys.signal], [Either.Left],
    [`Left]. See also {!type-name}. *)

type applied_name = Typegist__repr.applied_name
(** The type for applied type names. This can be the qualified name
    of a type definition (e.g. [Buffer.t]) or a qualified type constructor
    name with instantiated variables (e.g. [(int, string) Hashtbl.t]). This is
    what we use for naming gists, in particular for monomorphic instances of
    polymorphic types. *)

(** Name munging. *)
module Name : sig
  val unqualified : applied_name -> name
  (** [unqualified_name n] is the unqualified name of [n] or [""] if [n]
      is [""]. For example ["t"] in ["Buffer.t"] or
      ["(string, int) Hashtbl.t"]. *)

  val module_path : applied_name -> qualified_name
  (** [module_path g] is the module path of [name g] or [""] if there is no
      module path. For example ["Buffer"] in ["Buffer.t"] or
      ["Hashtbl"] in ["(string, int) Hashtbl.t"]. *)

  val type_args : applied_name -> string
  (** [type_args] are the type arguments of polymorphic type instances.
      For example [""] in [Buffer.t] and ["(string, int)"] in
      ["(string, int) Hashtbl.t"]. *)

  val make_applied : Typegist__repr.v list -> qualified_name -> applied_name
  (** [make_applied type_args qname] makes an applied polymorphic name
      with given [type_args] arguments and the qualified name [qname]. *)

  val pp_applied :
    Typegist__repr.v list -> Format.formatter -> qualified_name -> unit
  (** [pp_applied type_args ppf qname] formats an applied polymorphic type
      with given [type_args] arguments and the qualified name [qname]. *)
end

(** {1:metadata Metadata} *)

module Meta = Typegist__meta

(** {1:gists Gists} *)

type 'a t = 'a Typegist__repr.t
(** The type for representing a type of type ['a]. *)

type 'a gist := 'a t
(** See {!t}. *)

type v = V : 'a t -> v (** *)
(** The type for existential gists. *)

(** {2:properties Properties} *)

val name : 'a t -> applied_name
(** [name g] is the applied name of [g] or [""] if [g] is a nameless
    type expression. *)

val doc : 'a t -> string
(** [doc g] is the documentation string of [g]. *)

val id : 'a t -> 'a Type.Id.t
(** [id g] is the typed identifier of [g]. *)

val meta : 'a t -> 'a Meta.t
(** [meta g] is the metadata of [g]. *)

(** Type expressions for gist processors.

    This is typegist's view on the OCaml type expression language. *)
module Expr : sig
  type 'a t = 'a Typegist__repr.expr =
  | Scalar : 'a Typegist__repr.scalar -> 'a t
  | Tuple : 't Typegist__repr.tuple -> 't t
  | Record : 'r Typegist__repr.record -> 'r t
  | Variant_like : 'v Typegist__repr.variant_like -> 'v t
  | Array_like : ('elt, 'array) Typegist__repr.array_like -> 'array t
  | Map_like : ('k, 'v, 'map) Typegist__repr.map_like -> 'map t
  | Cell_like : ('a, 'cell) Typegist__repr.cell_like -> 'cell t
  | Func : ('a, 'b) Typegist__repr.func -> ('a -> 'b) t
  | Abstract : 'a Typegist__repr.abstract -> 'a t
  | View : ('a, 'b) Typegist__repr.view -> 'a t
  | Rec : 'a gist lazy_t -> 'a t (** *)
  (** The type for type expressions. *)

  val fold_gists : ('a. 'acc -> 'a gist -> 'acc) -> 'acc -> 'b t -> 'acc
  (** [fold_gists f acc expr] recursively folds over all unique (as per
      {!Gist.id}) gists occurences in [expr]. A gist with given identity
      and its definition are folded over only once even if it is present
      multiple times. *)

  val map_gists : ('a. 'a gist -> 'a gist) -> 'b t -> 'b t
  (** [map_gists f expr] recursively maps all gists in [expr]. Expressions
      of gists returned by [f g] are also mapped. *)

  val pp : Format.formatter -> 'a t -> unit
  (** [pp] formats a {e likely} valid OCaml type expression for the type gist
      expression. Named typed gists found in the expression are not expanded
      to their definition. *)
end

val expr : 'a t -> 'a Expr.t
(** [expr g] is the type expression of [g]. *)

val gists : 'a t -> v list
(** [gists g] is the list of unique (as per {!id}) gists found in [g]'s
    expression. This includes [g] if [g] is recursive. *)

(** {2:predicates Predicates} *)

val is_recursive : 'a t -> bool
(** [is_recursive g] is [true] if and only if [g] is a recursive
    definition. This means that the {!id} of [g] can be found in the
    list returned by {!val-gists}. This talks about the gist definition, not the
    represented type. For example [is_recursive (list int)] is
    [false] despite [list] being a recursive type. *)

val has_gist : 'a t -> 'b t -> bool
(** [has_gist sub g] is [true] iff [sub] can be found in [g]'s definition. *)

val is_nameless : 'a t -> bool
(** [is_nameless g] is [true] iff [name g = ""]. *)


(** {2:updating Updating}

    {!update} and {!rebind}  allow to update gist metadata and make sure
    other gists use the up-to-date definition, see
    {{!page-cookbook.tips_metadata_update}this entry} of the cookbook.
    In contrast {!of_gist} updates and attributes a new identity to a gist,
    see {{!page-cookbook.generic_funs_override}this entry} in the
    cookbook for an example where this may be useful. *)

val update :
  ?name:applied_name -> ?doc:string -> ?meta:'a Meta.t ->
  ?expr:'a Expr.t -> 'a t -> 'a t
(** [update ?name ?doc ?meta g] update the given properties in [g].
    The result has the same {!id}. *)

val rebind : 'a t -> 'b t -> 'b t
(** [rebind r g] substitutes by [r] all gist occurences identified by [id r]
    in the type expression of [g]. Returns [r] if [id g = id r].
    See {{!page-cookbook.tips_metadata_update}this example}. *)

val replace : 'a t -> by:'a t -> 'b t -> 'b t
(** [replace g ~by g'] replaces all occurences of [g] in [g'] by the [by]
    gist. In constrast to {!rebind}, [by] may change the identity of
    [g]. *)

val of_gist :
  ?name:applied_name -> ?doc:string -> ?meta:'a Meta.t ->
  ?expr:'a Expr.t -> 'a t -> 'a t
(** [of_gist g] is like {!update} but it attributes a new identity to the
    result. Note that the result cannot be used to {!rebind} [g]. *)

(** {2:fmt Formatting} *)

val pp_def : Format.formatter -> 'a t -> unit
(** [pp_def g] formats an OCaml {e looking} type definition for [g]. If the
    {!val-name} of [g] is [""] this is {!Expr.pp}, otherwise it has
    the form [type applied_name = …]. *)

val pp_type_arg : single:bool -> Format.formatter -> 'a t -> unit
(** [pp_type_arg ~single g] prints [g] as a type argument for a
    polymorphic type. If [single] is [true] it is expected to be the
    single argument, if [false] it is expected to be in a list of
    arguments, this affects parenthesising which for example differs
    for ['a] between ['a list] and [('a, 'b) Hashtbl.t]. *)

(** {1:representations Representations} *)

val todo : ?doc:string -> ?meta:'a Meta.t -> qualified_name -> 'a t
(** [todo name] represents the type [name] but remains to be described.
    Generic functions raise [Invalid_argument] when they hit the stub. *)

(** {2:scalars Scalars}

    Scalar gist values represent the built-in scalar types. *)

(** Scalar types for gist processors. *)
module Scalar : sig

  (** {1:scalar Scalars} *)

  type 'a t = 'a Typegist__repr.scalar =
  | Unit : unit t
  | Bool : bool t
  | Char : char t
  | Uchar : Uchar.t t
  | Int : int t
  | Int32 : int32 t
  | Int64 : int64 t
  | Nativeint : nativeint t
  | Float : float t (** *)
  (** The type for representing scalar types of type ['a]. *)

  val applied_name : 'a t -> applied_name
  (** [applied_name s] is the OCaml nullary type constructor of scalar [s]
      – ["unit"], ["bool"], ["char"], etc. *)

  (** {1:gen Generic operations} *)

  (** {2:constants Constants} *)

  val zero : 'a t -> 'a
  (** [zero s] is a zero value for the scalar type [s]. *)

  val min_scalar : 'a t -> 'a
  (** [min_scalar s] is the minimal value for the scalar type [s]. *)

  val max_scalar : 'a t -> 'a
  (** [max_scalar s] is the maximal value for the scalar type [s]. *)

  (** {2:preds Predicates and comparisons} *)

  val equal : 'a t -> 'a -> 'a -> bool
  (** [equal s v0 v1] is [true] iff [v0] and [v1] are the same value as per
      the Stdlib's corresponding [M.equal] function. *)

  val compare : 'a t -> 'a -> 'a -> int
  (** [equal s v0 v1] is [true] iff [v0] and [v1] are the same
      value as per the Stdlib's corresponding [M.compare] function. *)

  (** {2:fmt Formatting} *)

  val pp_char : Format.formatter -> char -> unit
  (** [pp_char] formats [char] OCaml literals as follows:
      {ul
      {- {!Char.Ascii.is_print} except single quote and slash are
         printed verbatim between single quotes.}
      {- Characters ['\''], ['\\'], ['\n'], ['\r'] and ['\t'] are
         printed just as written here.}
      {- Other characters use hex escapes ['\xhh'].}} *)

  val pp_uchar : Format.formatter -> Uchar.t -> unit
  (** [pp_uchar] formats [Uchar.t] pseudo OCaml literals (the notation
      does not exist) between quotes. It escapes, with the OCaml Unicode
      escape notation: C0 control characters (U+0000-U+001F),
      C1 control characters (U+0080-U+009F), line separator (U+2028)
      and paragraph separator (U+2029). In practice lots of funny stuff
      can still happen. *)

  val pp : 'a t -> Format.formatter -> 'a -> unit
  (** [pp s] is a formatter for values of the scalar type [s]. This
      formats numbers in decimal, uses "%g" for floats and
      {!pp_char} and {!pp_uchar} for characters. *)
end

val unit : unit t
(** [unit] represents the [unit] type. *)

val bool : bool t
(** [bool] represents the [bool] type. *)

val char : char t
(** [char] represents the [char] type. *)

val uchar : Uchar.t t
(** [uchar] represents the [Uchar.t] type. *)

val int : int t
(** [int] represents the [int] type. *)

val int32 : int32 t
(** [int32] represent the [int32] type. *)

val int64 : int64 t
(** [int64] represents the [int64] type. *)

val nativeint : nativeint t
(** [nativeint] represents the [nativeint] type. *)

val float : float t
(** [float] represents the [float] type. *)

(** {2:products Fields and products}

    {{!tuples}Tuples}, {{!records}records} and {{!variants}variant cases} are
    all products of types. The representation distinguishes them in different
    top-level cases but otherwise they share the same representation: a
    product of typed and possibly named fields. *)

(** Fields.

    The field type represents an individual field of a {!Product.t} value
    which is used to represent tuple types, record types and variant cases
    types.

    Fields are named only for record types or variant case types with
    inline records. *)
module Field : sig

  (** {1:fields Fields} *)

  type name = Typegist__repr.field_name
  (** The type for record field names. These must be unqualified.
      For example for {!Stdlib.Complex}, [re], [im], see also {!type-name}.
      An empty string indicates the field is unnamed. *)

  type ('p, 'v) t = ('p, 'v) Typegist__repr.field
  (** The type for representing the field of type ['v] of a product of
      type ['p]. *)

  val make :
    ?name:name -> ?doc:string -> ?meta:('p, 'v) Typegist__meta.t2 ->
    ?set:('p -> 'v -> unit) -> ?iset:('p -> 'v -> 'p) -> 'v gist ->
    ('p -> 'v) -> ('p, 'v) t
  (** [make g get] is a field such that:
      {ul
      {- [g] is the type representation of the field.}
      {- [get] is the function to get the field value from the product.}
      {- [name] is the name for the field. Defaults to [""].}
      {- [doc] is the doc string for the field. Defaults to [""].}
      {- [meta] is the field metadata. Defaults to {!Meta.empty}.}
      {- [set] is a function to update the field value of the product
         in-place for mutable record fields. Defaults to [None], that is the
         field is immutable.}
      {- [iset] is a function for an immutable update of the field value
         of the product. Defaults to [None].}} *)

  val name : ('p, 'v) t -> name
  (** [name f] is the name of [f]. This is [""] on unnamed field. *)

  val doc : ('p, 'v) t -> string
  (** [doc f] is the doc string of [f]. *)

  val meta : ('p, 'v) t -> ('p, 'v) Typegist__meta.t2
  (** [meta f] is the meta of [f]. *)

  val gist :  ('p, 'v) t -> 'v gist
  (** [gist f] is the gist of [f]. *)

  val get : ('p, 'v) t ->  ('p -> 'v)
  (** [get f] is the function to get the field value from the product. *)

  val set : ('p, 'v) t ->  ('p -> 'v -> unit) option
  (** [set f] is a function to update the field value of the product
      in-place, if any. This is only relevant for record fields. *)

  val iset :  ('p, 'v) t ->  ('p -> 'v -> 'p) option
  (** [iset f] is a function for an immutable update of the field
      value of the product, if any. *)

  val is_mutable : ('p, 'v) t -> bool
  (** [is_mutable f] is [true] if and only if [set f] is [Some _]. *)
end

(** Products for gist processors.

    The {!Product.type-fields} type represent the ordered sequence of fields and
    the way to construct the product from ordered field values with a
    constructor function. *)
module Product : sig

  (** {1:fields Fields} *)

  type ('p, 'ctor) fields = ('p, 'ctor) Typegist__repr.fields =
  | Ctor : 'ctor -> ('p, 'ctor) fields
    (** Holds the product constructor. *)
  | App : ('p, 'v -> 'ctor) fields * ('p, 'v) Field.t -> ('p, 'ctor) fields
    (** Applies a field value to the constructor. *)
  (** The type for representing the fields of a product of type ['p] and
      its construction via a constructor function ['ctor].

      When a [fields] value types with [('p, 'p) fields] we know how to
      project each field of product ['p] and how to construct a value from
      its fields. *)

  type 'p t = ('p, 'p) fields
  (** The type for product types. A fully saturated constructor. *)

  val is_empty : ('p, 'ctor) fields -> bool
  (** [is_empty fs] is [true] if [fs] has no fields. *)

  val is_singleton : ('p, 'ctor) fields -> bool
  (** [is_singleton fs] is [true] if [fs] has a single field. *)

  val field_count : ('p, 'ctor) fields -> int
  (** [field_count fs] is the number of fields in [fs]. *)

  val fold_fields :
    ('v. 'acc -> ('p, 'v) Field.t -> 'acc) -> 'acc -> 'p t -> 'acc
  (** [fold_fields f acc p] folds over the fields of [p] starting
      with [acc]. *)

  (** {1:constructing Constructing products}

      This intermediate structure is used for constructing
      product descriptions. It's just here so that the metadata
      can be described at the start of the description in
      {!Gist.tuple}, {!Gist.record} and {!Gist.case} and the appropriate
      value be returned via {!Gist.finish}. *)

  type ('p, 'ctor, 'ret) cons
  (** The type for constructing fields. This {!type-fields} augmented
      with a return ['ret] which is the kind of value you get on
      {!Gist.finish}.  *)
end

val field' :
  ('p, 'v) Field.t -> ('p, 'v -> 'ctor, 'ret) Product.cons ->
  ('p, 'ctor, 'ret) Product.cons
(** [field f p] adds [f] to the construction of [p]. *)

val field :
  ?doc:string -> ?meta:('p, 'v) Meta.t2 -> ?set:('p -> 'v -> unit) ->
  ?iset:('p -> 'v -> 'p) -> string -> 'v t -> ('p -> 'v) ->
  ('p, 'v -> 'ctor, 'ret) Product.cons -> ('p, 'ctor, 'ret) Product.cons
(** [field name g get cons] defines a named field for a product ['v].
    This just combines {!Field.make} and {!field'}. This is for
    record fields and variant inline records. *)

val comp :
  ?doc:string -> ?meta:('p, 'v) Meta.t2 -> ?iset:('p -> 'v -> 'p) -> 'v t ->
  ('p -> 'v) -> ('p, 'v -> 'ctor, 'ret) Product.cons ->
  ('p, 'ctor, 'ret) Product.cons
(** [comp] is like {!field} but it is nameless. Use it for tuple
    or variant case components. *)

val finish : ('p, 'p, 'ret) Product.cons -> 'ret
(** [finish] finishes the construction of the product. *)

(** {2:tuples Tuples}

    Tuple gist values represent finite arity tuples.
    See {{!page-cookbook.blueprints_tuples}examples}. *)

(** Tuples for gist processors. *)
module Tuple : sig
  type 'a t = 'a Typegist__repr.tuple
  (** The type for tuple types of type ['a]. *)
end

val tuple :
  ?name:applied_name -> ?doc:string -> ?meta:'p Meta.t ->
  'ctor -> ('p, 'ctor, 'p t) Product.cons
(** [tuple ctor] starts a tuple type whose values are constructed with [ctor]
    to be satured with {!Type.Gist.comp} fields and finished with {!finish}. *)

val t2 :
  ?name:applied_name -> ?doc:string -> ?meta:('a * 'b) Meta.t ->
  'a t -> 'b t -> ('a * 'b) t
(** [t2 c0 c1] represents pairs of type [c0 * c1]. *)

val t3 :
  ?name:applied_name -> ?doc:string -> ?meta:('a * 'b * 'c) Meta.t ->
  'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** [t3 c0 c1 c2] represents triplets of type [c0 * c1 * c2]. *)

val t4 :
  ?name:applied_name -> ?doc:string -> ?meta:('a * 'b * 'c * 'd) Meta.t ->
  'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
(** [t4 c0 c1 c2 c3] represents quadruplets of type [c0 * c1 * c2 * c3]. *)

(** {2:records Records}

    Record gist values represent record types.
    See {{!page-cookbook.blueprints_records}examples}. *)

(** Records for gist processors. *)
module Record : sig
  type 'a t = 'a Typegist__repr.record
  (** The type for record types of type ['a]. *)
end

val record :
  ?doc:string -> ?meta:'r Meta.t -> applied_name -> 'ctor ->
  ('r, 'ctor, 'r t) Product.cons
(** [record name ctor] starts a record type named [named] whose
    values are constructed with [ctor] to be satured with
    {!Type.Gist.field}s and finished with {!finish}. *)

(** {2:variants Variants}

    Variant gist values represent variant types. A couple of standard
    variants are distinguished and have direct combinators to construct them.
    See {{!page-cookbook.blueprints_variants}examples}. *)

(** Variants for gist processors. *)
module Variant : sig

  (** {1:variant_cases Variant cases} *)

  (** Variant cases. *)
  module Case : sig
    type name = Typegist__repr.variant_case_name
    (** The type for case constructor names. These must be unqualified. For
        example ["Left"] for [Either.Left], or ["`Left"] for [`Left]. *)

    type 'v t = 'v Typegist__repr.variant_case
    (** The type for representing a variant case of a variant of type ['v]. *)

    val make : ?doc:string -> name -> 'v Product.t -> 'v t
    (** [make name p] is a case with given constructor name and product.

        @raise Invalid_argument if [name] is [""]. *)

    val name : 'v t -> name
    (** [name c] is the constructor name of [c]. *)

    val doc : 'v t -> string
    (** [doc c] is the doc string of [c]. *)

    val product : 'v t -> 'v Product.t
    (** [product c] is the product of [c]. *)

    val is_inline_record : 'v t -> bool
    (** [is_inline_record c] is [true] if and only if [c] is a variant case
        with an inline record (checked by determining if the {!product}'s last
        field is named). *)
  end

  (** {1:variants Variants} *)

  type 'v t = 'v Typegist__repr.variant
  (** The type for representing variant types of type ['v]. *)

  val make : 'v Case.t list -> case_index:('v -> int) -> 'v t
  (** [make cases ~case_index] is a variant type with cases described
      by [cases] and whose values are attributed a case in the [cases] list
      with the zero-based [case_index] function. *)

  val cases : 'v t -> 'v Case.t iarray
  (** [cases vt] are the cases of [vt]. *)

  val case_index : 'v t -> 'v -> int
  (** [case_index vt] is the function that given a value of the
      variant type attributes it a case in the array [cases vt]. *)

  val case : 'v t -> 'v -> 'v Case.t
  (** [case vt v] is [Iarray.get (cases vt) (case_index vt)]. *)

  val is_polymorphic_variant : 'v t -> bool
  (** [is_polymorphic_variant vt] is [true] if and only if [vt]
      is a polymorphic variant (checked by determining if the first
      case's {!Case.val-name} starts with a backtick). *)
end

(** Variant likes for gist processors.

    A couple of standard variant types are distinguished to allow gist
    processors to process them specially. If you are uninterested in the
    specialization use {!Variant_like.to_variant} to treat them uniformly. *)
module Variant_like : sig
  type 'v t = 'v Typegist__repr.variant_like =
  | Option : 'a gist -> 'a option t
  | Either : 'a gist * 'b gist -> ('a, 'b) Either.t t
  | Result : 'a gist * 'b gist -> ('a, 'b) result t
  | List : 'a gist -> 'a list t
  | Variant : 'v Variant.t -> 'v t (** *)
  (** The type for representing variant like types of type ['v] *)

  val to_variant : 'v t -> 'v Variant.t
  (** [to_variant vl] is the variant like [vl] as a variant type. *)
end

val option :
  ?name:applied_name -> ?doc:string -> ?meta:'a option Meta.t ->
  'a t -> 'a option t
(** [option g] represents an ['a option] type for the type
    represented by [g]. *)

val either :
  ?name:applied_name -> ?doc:string -> ?meta:('a, 'b) Either.t Meta.t ->
  'a t -> 'b t -> ('a, 'b) Either.t t
(** [either l r] represents an [('l, 'r) Either.t] type for the types
    represented by [l] and [g]. *)

val result :
  ?name:applied_name -> ?doc:string -> ?meta:('a, 'b) result Meta.t ->
  'a t -> 'b t -> ('a, 'b) result t
(** [result ok error] represents a [('ok, 'error) result] type for the types
    represented by [ok] and [error]. *)

val list :
  ?name:applied_name -> ?doc:string -> ?meta:'a list Meta.t -> 'a t ->
  'a list t
(** [list elt] represents a ['elt list] type for the type represented by
    [elt]. *)

(** {3:generic_variants Generic variants}

    Generic variants (and polymorphic variants) are described by a list of
    case types and a function that indicates which case to use for a value of
    the type. See {{!page-cookbook.blueprints_variants}examples}. *)

val case :
  ?doc:string -> Variant.Case.name -> 'ctor ->
  ('v, 'ctor, 'v Variant.Case.t) Product.cons
(** [case name ctor] starts a variant case type with constructor name [name]
    whose values are constructed with [ctor] to be saturated with either
    {!Type.Gist.comp} or {!Type.Gist.field} (for inline records) and finished
    with {!finish}. *)

val variant :
  ?doc:string -> ?meta:'v Meta.t -> ?name:applied_name ->
  'v Variant.Case.t list -> case_index:('v -> int) -> 'v t
(** [variant ~name cases case_index] is a variant type with cases described
    by [cases] and whose values are attributed a case in the [cases] list with
    the zero-based [case_index] function. [name] should only be
    possibly omitted if you are describing a polymorphic variant. *)

val variant_of_enum :
  ?doc:string -> ?meta:'v Meta.t -> ?name:applied_name ->
  (Variant.Case.name * 'v) list -> 'v t
(** [variant_of_enum ~name cases] is a variant from the given 0-ary case
    enumeration. [name] should only be possibly omitted if you are describing
    a polymorphic variant. *)

(** {2:array_likes Array likes}

    Array like gist values represent linear array types. A couple of standard
    array types are distinguished and have direct combinators to construct
    them. See {{!page-cookbook.blueprints_array_likes}examples}. *)

(** Array likes for gist processors.

    A couple of standard array types are distinguished to allow gist
    processors to process them specially. If you are uninterested in the
    specialization use {!Array_like.to_module} to treat them uniformly. *)
module Array_like : sig

  (** {1:array_modules Array modules} *)

  (** Array interface. *)
  module type S = Typegist__repr.ARRAY

  type ('elt, 'array) module' = ('elt, 'array) Typegist__repr.array_module
  (** The type for array modules. *)

  (** {1:array_likes Array likes} *)

  type bytes_encoding = Typegist__repr.bytes_encoding =
  | Binary (** Arbitrary byte sequence *)
  | Utf_8 (** Valid UTF-8 byte sequence *)
  (** The type for specifying the two standard interpretations of
      OCaml [string] and [bytes] values. *)

  type ('elt, 'array) t = ('elt, 'array) Typegist__repr.array_like =
  | Bytes : bytes_encoding -> (char, bytes) t
  | String : bytes_encoding -> (char, string) t
  | Array : 'elt gist -> ('elt, 'elt array) t
  | Iarray : 'elt gist -> ('elt, 'elt iarray) t
  | Bigarray1 :
      'elt gist * ('elt, 'b) Bigarray.kind * 'c Bigarray.layout ->
      ('elt, ('elt, 'b, 'c) Bigarray.Array1.t) t
  | Module : 'elt gist * ('elt, 'array) module' -> ('elt, 'array) t (** *)
  (** The type for representing array like types of type ['array] with
      elements of type ['elt]. *)

  val elt : ('elt, 'array) t -> 'elt gist
  (** [elt a] is the representation of the elements of [a]. *)

  val to_module : ('elt, 'array) t -> ('elt, 'array) module'
  (** [to_module a] is an array module for [a]. *)
end

val bytes : bytes t
(** [bytes] represents mutable the type for sequence of bytes. *)

val utf_8_bytes : bytes t
(** [utf_8_bytes] represents the type for mutable, UTF-8 encoded, text
    strings. *)

val binary_string : string t
(** [binary_string] represents the type for immutable sequences of bytes. *)

val utf_8_string : string t
(** [string_as_utf_8] represent the type for immutable, UTF-8 encoded, text
    strings. *)

val array :
  ?name:string -> ?doc:string -> ?meta:'elt array Meta.t -> 'elt t ->
  'elt array t
(** [array elt] represents the type for arrays with elements of type [elt]. *)

val iarray :
  ?name:string -> ?doc:string -> ?meta:'elt iarray Meta.t -> 'elt t ->
  'elt iarray t
(** [array elt] represents the type for immutable arrays with elements of
    type [elt]. *)

val bigarray1 :
  ?name:string -> ?doc:string ->
  ?meta:('elt, 'b, 'c) Bigarray.Array1.t Meta.t -> 'elt t ->
  ('elt, 'b) Bigarray.kind -> 'c Bigarray.layout ->
  ('elt, 'b, 'c) Bigarray.Array1.t t
(** [bigarray1 elt kind layout] represents the type of linear bigarrays stored
    with [kind] according to [layout] and accessed with [elt] elements.

    Other bigarrays dimensions are supported as views over linear bigarrays.
    See {!bigarray2}, {!bigarray3} and {!bigarraygen}. *)

val bigbytes :
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t t
(** [bigbytes] is [bigarray1 ~name:"bigbytes" int Int8_unsigned C_layout]. *)

val floatarray : floatarray t
(** [floatarray] is an {!array_module} for [floatarray]s. *)

val dynarray :
  ?name:string -> ?doc:string -> ?meta:'elt Dynarray.t Meta.t -> 'elt t ->
  'elt Dynarray.t t
(** [dynarray elt] is an {!array_module} for an [Dynarray.t] type
    with elements of type [elt]. *)

val weak :
  ?name:string -> ?doc:string -> ?meta:'elt Weak.t Meta.t -> 'elt t ->
  'elt Weak.t t
(** [weak elt] is an {!array_module} for a [Weak.t] type with
    elements of type [elt]. *)

val array_module :
  ?name:string -> ?doc:string -> ?meta:'array Meta.t -> 'elt t ->
  ('elt, 'array) Array_like.module' -> 'array t
(** [array_module elt array] represents an array type [array] with
    elements of type [elt]. *)

(** {2:map_likes Map likes}

    Map like gist values represent key-value maps. This is either
    mutable map types like hashtables or immutable key-value maps.
    See {{!page-cookbook.blueprints_map_likes}examples}. *)

(** Map likes for gist processors.  *)
module Map_like : sig

  (** {1:hashtbl_modules Hash table modules} *)

  (** Generic hashbtl interface. *)
  module type HASHTBL = Typegist__repr.HASHTBL

  type ('k, 'v, 'hashtbl) hashtbl_module =
    ('k, 'v, 'hashtbl) Typegist__repr.hashtbl_module
  (** The type for hash table modules. *)

  (** {1:map_modules Map modules} *)

  (** Generic map interface. *)
  module type MAP = Typegist__repr.MAP

  type ('k, 'v, 'map) map_module = ('k, 'v, 'map) Typegist__repr.map_module
  (** The type for map modules. *)

  (** {1:map_likes Map likes} *)

  type ('k, 'v, 'map) t = ('k, 'v, 'map) Typegist__repr.map_like =
  | Hashtbl_module :
      'k gist * 'v gist * ('k, 'v, 'hashtbl) hashtbl_module ->
      ('k, 'v, 'hashtbl) t
  | Map_module :
      'k gist * 'v gist * ('k, 'v, 'map) map_module -> ('k, 'v, 'map) t
  (** *)
  (** The type for representing map like types of type ['map] with keys of
      type ['k] and values of type ['v]. *)

  val key : ('k, 'v, 'map) t -> 'k gist
  (** [key m] is the representation of the keys of [m]. *)

  val value : ('k, 'v, 'map) t -> 'v gist
  (** [value m] is the representation of the values of [m]. *)
end

val hashtbl :
  ?name:applied_name -> ?doc:string -> ?meta:('k, 'v) Hashtbl.t Meta.t ->
  'k t -> 'v t -> ('k, 'v) Hashtbl.t t
(** [hashtbl k v] represents the [Hashtbl.t] type with type of
    keys [k] and type of values of type [v]. *)

val map :
  qualified_name -> (module Map : Map.S) -> ?name:applied_name -> ?doc:string ->
  ?meta:'v Map.t Meta.t -> Map.key t -> 'v t  -> 'v Map.t t
(** [map "Map.t" (module Map) k v] represents the [Map.t] type with type of
    keys [k] and type of values of type [v]. *)

val hashtbl_module :
  ?name:applied_name -> ?doc:string -> ?meta:'hashtbl Meta.t ->
  'k t -> 'v t -> ('k, 'v, 'hashtbl) Map_like.hashtbl_module -> 'hashtbl t
(** [hashbl_module k v h] represents a hashtbl of type [h] with keys of type
    [k] and values of type [v]. *)

val map_module :
  ?name:applied_name -> ?doc:string -> ?meta:'map Meta.t ->
  'k t -> 'v t -> ('k, 'v, 'map) Map_like.map_module -> 'map t
(** [map_module k v m] represents a map of type [m] with keys of type [k] and
    values of type [v]. *)

(** {2:cell_likes Cell likes}

    Cell like gist values represent cell types. A couple of standard
    cell types are distinguished and have direct combinators to construct
    them. See {{!page-cookbook.blueprints_cell_likes}examples}. *)

(** Cell likes for gist processors. *)
module Cell_like : sig

  (** {1:cell_modules Cell modules} *)

  (** Cell interface. *)
  module type S = Typegist__repr.CELL

  type ('a, 'cell) module' = ('a, 'cell) Typegist__repr.cell_module
  (** The type for cell modules. *)

  (** {1:cell_likes Cell likes} *)

  type ('a, 'cell) t = ('a, 'cell) Typegist__repr.cell_like =
  | Lazy : 'a gist -> ('a, 'a lazy_t) t
  | Ref : 'a gist -> ('a, 'a ref) t
  | Atomic : 'a gist -> ('a, 'a Atomic.t) t
  | Module : 'a gist * ('a, 'cell) module' -> ('a, 'cell) t (** *)
  (** The type for representing cell like types with contents of type ['a]. *)

  val contents : ('a, 'cell) t -> 'a gist
  (** [contents c] is the representation of the contents of [c]. *)

  val to_module : ('a, 'cell) t -> ('a, 'cell) module'
  (** [to_module c] is a cell module for [c]. *)
end

val lazy' :
  ?name:applied_name -> ?doc:string -> ?meta:'a lazy_t Meta.t ->
  'a t -> 'a lazy_t t
(** [lazy' g] represents a lazy cell type on values of type [g]. *)

val ref :
  ?name:applied_name -> ?doc:string -> ?meta:'a ref Meta.t ->
  'a t -> 'a ref t
(** [ref g] represents a reference cell type on values of type [g]. *)

val atomic :
  ?name:applied_name -> ?doc:string -> ?meta:'a Atomic.t Meta.t ->
  'a t -> 'a Atomic.t t
(** [atomic g] represents an atomic cell type on values of type [g]. *)

val cell_module :
  ?name:string -> ?doc:string -> ?meta:'cell Meta.t -> 'a t ->
  ('a, 'cell) Cell_like.module' -> 'cell t
(** [cell_module contents cell] represents a cell type [cell] with contents
    type [contents]. *)

(** {2:functions Functions}

    Function gist values represent function types.
    See {{!page-cookbook.blueprints_functions}examples}. *)

(** Functions for gist processors. *)
module Func : sig

  (** {1:functions Functions} *)

  type ('a, 'b) t = ('a, 'b) Typegist__repr.func
  (** The type for representing function types from type ['a] to ['b]. *)

  val make : domain:'a gist -> range:'b gist -> ('a, 'b) t
  (** [make ~domain ~range] is a function type from [domain] to [range]. *)

  val domain : ('a, 'b) t -> 'a gist
  (** [domain f] is representation of the domain of [f]. *)

  val range : ('a, 'b) t -> 'b gist
  (** [range f] is representation of the range of [f]. *)
end

val func :
  ?name:string -> ?doc:string -> ?meta:('a -> 'b) Meta.t ->
  'a t -> 'b t -> ('a -> 'b) t
(** [func a b] represents a function from domain of type [a] to a range of
    type [b]. *)

val ( @-> ) : 'a t -> 'b t -> ('a -> 'b) t
(** [a @-> b] is [func a b]. *)

(** {2:abstract_types Abstract types}

    Abstract gist values represent abstract types by lists of public versioned
    representations. See {{!page-cookbook.blueprints_abstract_types}
    examples}. *)

(** Abstract type representations. *)
module Abstract : sig

  (** {1:abtract_types Abstract types} *)

  (** Versioned representations. *)
  module Version : sig

    type name = Typegist__repr.abstract_version_name
    (** The type for version names. This assumes an UTF-8 encoding. *)

    type 'a t = 'a Typegist__repr.abstract_version
    (** The type for a versioned type representation for type ['a]. *)

    val make : name:name -> 'a gist -> 'a t
    (** [make ~name g] is a public version named [name] described by the type
        [g]. *)

    val name : 'a t -> name
    (** [name v] is the version name of [v]. *)

    val gist : 'a t -> 'a gist
    (** [gist v] is the type representation of [v]. *)
  end

  type 'a t = 'a Typegist__repr.abstract
  (** The type for representing an abstract type of type ['a]. *)

  val make : ?version_index:('a -> int) -> 'a Version.t list -> 'a t
  (** [make versions] is an abstract type with public representations
      [versions].The zero-based [version_index] function attributes versions
      from the [versions] list to values (default to [Fun.const
      (List.length versions - 1)]). *)

  val is_opaque : 'a t -> bool
  (** [is_opaque a] is [true] if and only if no representations are exposed
      by [a]. This means that [Iarray.length (versions a) = 0]. *)

  val versions : 'a t -> 'a Version.t iarray
  (** [versions a] is the list of public representations of [a] (if any).

      {b Warning.} In general versions may be partial views over values
      of type [v]. So generic processors should not assume that any version
      can deal with a given [v], [version a v] will though. *)

  val version_index : 'a t -> 'a -> int
  (** [version_index a v] is the index of the version attributed to [v] in
      the array [versions a].

      @raises Invalid_argument if [is_opaque a] is [true]. *)

  val version : 'v t -> 'v -> 'v Version.t
  (** [version a v] is [Iarray.get (versions a) (version_index a v)].

      @raises Invalid_argument if [is_opaque a] is [true]. *)

  val find_version : Version.name -> 'a t -> 'a Version.t option
  (** [find_version n a] is the version named [n] of [a] if any. *)

  (** {1:gist_processors Gist processor helpers} *)

  val check_non_opaque : 'a gist -> 'a t -> unit
   (** [check_non_opaque g a] assumes [a] is [g]'s expressions
       and raises [Invalid_argument _] with an error message that
       mentions [g]'s name if [a] is opaque. *)

  val check_compatible_version_index : 'a gist -> 'a t -> 'a -> 'a -> int
  (** [compatible_value g a i0 i1] raises [Invalid_argument _] with
      an error message that mentions [g]'s name if
      [version_index a v0 <> version_index a i1] or returns the version
      index otherwise. This is to check that a binary operation on two values
      [v0] and [v1] is using the same version. *)

  val get_gist : 'a gist -> 'a t -> 'a -> 'a gist
  (** [get_gist g a v] gets the gist for value [v]. This uses
      {!check_non_opaque} before using {!version} and {!Version.gist}. *)

  val get_binop_gist : 'a gist -> 'a t -> 'a -> 'a -> 'a gist
  (** [get_binop_gist g a v0 v1] gets a gist for a binary operation.
      This uses both {!check_non_opaque} and
      {!check_compatible_version_index} to return the gist of the version
      that [v0] and [v1] share. *)
end

val abstract :
  ?doc:string -> ?meta:'a Meta.t -> ?version_index:('a -> int) ->
  applied_name -> 'a Abstract.Version.t list -> 'a t
(** [abstract name versions] represents the abstract type [name] by the
    public representations [versions]. If [versions] is the empty
    list, the abstract type remains fully opaque.

    The zero-based [version_index] function attribute versions
    from the list [versions] to the values of the type. It defaults to
    [Fun.const (List.length versions - 1)] which assumes all values are
    expressed in the last (latest) public representation of [versions]. *)

(** {2:views Views}

    A view gist value represent a type by the type of another
    gist value. See {{!page-cookbook.blueprints_views}examples}. *)

(** Views for gist processors. *)
module View : sig
  type ('a, 'b) t = ('a, 'b) Typegist__repr.view
  (** The type for types represented by another type. *)

  val make : inject:('a -> 'b) -> project:('b -> 'a) -> 'b gist -> ('a, 'b) t
  (** [make ~inject ~project g] is a view with given parameters. *)

  val inject : ('a, 'b) t -> ('a -> 'b)
  (** [inject v] is the injection function of [v]. *)

  val project : ('a, 'b) t -> ('b -> 'a)
  (** [project v] is the projection function of [v]. *)

  val gist : ('a, 'b) t -> 'b gist
  (** [gist v] is the gist for the range of [v]. *)
end

val view :
  ?name:applied_name -> ?doc:string -> ?meta:'a Meta.t ->
  inject:('a -> 'b) -> project:('b -> 'a) -> 'b t -> 'a t
(** [view inject project g] represents a type by viewing as values the type
    [g] with [inject] and back with [project]. *)

(** {2:recursion Recursion}

    Recursive gist values tie the knot for representing recursive types.
    See {{!page-cookbook.blueprints_recursive_types}examples}. *)

val rec' : 'a t lazy_t -> 'a t
(** [rec' g] is the recursive type defined by [g]. *)

(** {1:stdlib Other [Stdlib] types}

    These gists are for standard library types which are not directly
    expressed in the representation. They are provided here so that they are
    given a unique identity or polymorphic definition that libraries and
    processors can agree on. Some types may be missing do not hesitate to
    open an issue about it. *)

val complex : Complex.t t
(** [complex] represents the {!Complex.t} type by a record gist. *)

val bigarray2 :
  ?doc:string -> ?meta:('elt, 'b, 'c) Bigarray.Array2.t Meta.t -> 'elt t ->
  ('elt, 'b) Bigarray.kind -> 'c Bigarray.layout ->
  ('elt, 'b, 'c) Bigarray.Array2.t t
(** [bigarray2 elt kind layout] represents the type of 2D bigarrays by viewing
    them as their dimension sizes tupled with a reshaped {!bigarray1}. *)

val bigarray3 :
  ?doc:string -> ?meta:('elt, 'b, 'c) Bigarray.Array3.t Meta.t -> 'elt t ->
  ('elt, 'b) Bigarray.kind -> 'c Bigarray.layout ->
  ('elt, 'b, 'c) Bigarray.Array3.t t
(** [bigarray3 elt kind layout] represents the type of 3D bigarrays by viewing
    them as their dimensions sizes tupled with a reshaped {!bigarray1}. *)

val bigarraygen :
  ?doc:string -> ?meta:('elt, 'b, 'c) Bigarray.Genarray.t Meta.t -> 'elt t ->
  ('elt, 'b) Bigarray.kind -> 'c Bigarray.layout ->
  ('elt, 'b, 'c) Bigarray.Genarray.t t
(** [bigarraygen elt kind layout] represents the type of generic bigarrays
    by viewing them as their dimensions sizes tupled with a reshaped
    {!bigarray1}. *)

val digest : Digest.t t
(** [digest] represents the {!Digest.t} type by a {!binary_string} under
    a new identity. *)

val digest_blake128 : Digest.BLAKE128.t t
(** [digest_blake128] represents the {!Digest.BLAKE128.t} type by a
    {!binary_string} under a new {{!id}identity}. *)

val digest_blake256 : Digest.BLAKE256.t t
(** [digest_blake256] represents the {!Digest.BLAKE256.t} type by a
    {!binary_string} under a new identity. *)

val digest_blake512 : Digest.BLAKE512.t t
(** [digest_blake512] represents the {!Digest.BLAKE512.t} type by a
    {!binary_string} under a new identity. *)

val digest_md5 : Digest.MD5.t t
(** [digest_md5] represents the {!Digest.MD5.t} type by a
    {!binary_string} under a new identity. *)

val float_fp_class : Float.fpclass t
(** [float_fp_class] represents the {!Float.fpclass} as a variant gist. *)

val gc_stat : Gc.stat t
(** [gc_stat] represents the {!Gc.stat} type by a record gist. *)

val gc_control : Gc.control t
(** [gc_stat] represents the {!Gc.control} type by a record gist. *)

val pqueue_max :
  applied_name -> (module Pqueue : Pqueue.Max) -> ?doc:string ->
  ?meta:Pqueue.t Meta.t -> Pqueue.elt t -> Pqueue.t t
(** [pqueue_min name (module Pqueue) elt] represents the given [Pqueue.t]
    type by viewing it as type of list of type [elt]. Injecting into the
    list does not consume the queue elements. *)

val pqueue_max_poly :
  applied_name -> (module Pqueue : Pqueue.MaxPoly) -> ?doc:string ->
  ?meta:'a Pqueue.t Meta.t -> 'a Pqueue.elt t -> 'a Pqueue.t t
(** [pqueue_min_poly name (module Pqueue) elt] represents the given [Pqueue.t]
    type by viewing it as type of list of type [elt]. Injecting into the
    list does not consume the queue elements. *)

val pqueue_min :
  applied_name -> (module Pqueue : Pqueue.Min) -> ?doc:string ->
  ?meta:Pqueue.t Meta.t -> Pqueue.elt t -> Pqueue.t t
(** [pqueue_min name (module Pqueue) elt] represents the given [Pqueue.t]
    type by viewing it as type of list of type [elt]. Injecting into the
    list does not consume the queue elements. *)

val pqueue_min_poly :
  applied_name -> (module Pqueue : Pqueue.MinPoly) -> ?doc:string ->
  ?meta:'a Pqueue.t Meta.t -> 'a Pqueue.elt t -> 'a Pqueue.t t
(** [pqueue_min_poly name (module Pqueue) elt] represents the given [Pqueue.t]
    type by viewing it as type of list of type [elt]. Injecting into the
    list does not consume the queue elements. *)

val queue : ?doc:string -> ?meta:'a Queue.t Meta.t -> 'a t -> 'a Queue.t t
(** [queue elt] represents a ['elt ]{!Queue.t} type for the type represented
    by [elt] by viewing the queue as a list. Injecting into the list
    does not consume the queue elements. *)

val random_state : Random.State.t t
(** [random_state] represents the {!Random.State.t} type as a view over its
    serialization to binary strings (see {!Random.State.to_binary_string}). *)

val set :
  applied_name -> (module Set : Set.S) -> ?doc:string -> ?meta:Set.t Meta.t ->
  Set.elt t -> Set.t t
(** [set name (module Set) elt] represents the given [Set.t] type by
     viewing it as type of list of type [elt]. *)

val stack : ?doc:string -> ?meta:'a Stack.t Meta.t -> 'a t -> 'a Stack.t t
(** [stack elt] represents a ['elt ]{!Stack.t} type for the type represented
    by [elt] by viewing the queue as a list. Injecting into the list
    does not consume the stack elements. *)

val sys_signal : Sys.signal t
(** [sys_signal] represents the {!Sys.signal} type by an {!int} scalar
    under a new identity. *)
