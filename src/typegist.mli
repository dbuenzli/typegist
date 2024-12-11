(*---------------------------------------------------------------------------
   Copyright (c) 2023 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Extended {!Stdlib.Type} and {!Stdlib.Fun} modules. Open to use it.

    Open the module to use it. This adds these modules

    {!modules: Type.Gist Fun.Generic}

    to the {!Stdlib.Type} and {!Stdlib.Func} modules. Before OCaml 5.1
    this also defines the {!Type} module.

        The [typegist] library:

    Open the module to use it, this only redefines the [Type] and [Fun] modules
    in your scope.
*)

(** Type introspection. *)
module Type : sig

  type (_, _) eq = Equal: ('a, 'a) eq (** *)
  (** The purpose of [eq] is to represent type equalities that may not otherwise
      be known by the type checker (e.g. because they may depend on dynamic
      data).

      A value of type [(a, b) eq] represents the fact that types [a] and [b]
      are equal.

      If one has a value [eq : (a, b) eq] that proves types [a] and [b] are
      equal, one can use it to convert a value of type [a] to a value of type
      [b] by pattern matching on [Equal]:
      {[
        let cast (type a) (type b) (Equal : (a, b) Type.eq) (a : a) : b = a
      ]}

      At runtime, this function simply returns its second argument unchanged.

      Available in OCaml 5.1. *)

(** Type identifiers (available in OCaml 5.1).

    A type identifier is a value that denotes a type. Given two type
    identifiers, they can be tested for {{!Id.provably_equal}equality} to
    prove they denote the same type. Note that:

    - Unequal identifiers do not imply unequal types: a given type can be
      denoted by more than one identifier.
    - Type identifiers can be marshalled, but they get a new, distinct,
      identity on unmarshalling, so the equalities are lost. *)
  module Id : sig

    (** {1:ids Type identifiers} *)

    type !'a t
    (** The type for identifiers for type ['a]. *)

    val make : unit -> 'a t
    (** [make ()] is a new type identifier. *)

    val uid : 'a t -> int
    (** [uid id] is a runtime unique identifier for [id]. *)

    val provably_equal : 'a t -> 'b t -> ('a, 'b) eq option
    (** [provably_equal i0 i1] is [Some Equal] if identifier [i0] is equal
        to [i1] and [None] otherwise. *)
  end

  (** Type gists.

      Type gists reflect the essence of your types as OCaml values of
      a given {{!Gist.type-t}representation type}.

      Type gists are useful to type the interface of your values at
      the boundaries of your program or to devise generic type indexed
      functions.

      See the {{!page-quick}quick start}. The {{!page-cookbook}cookbook}
      has simple description examples. Generic functions
      can be found in {!Fun.Generic} and this is
      a {{!page-cookbook.writing_generic}generic function template}
      to write your own. *)
  module Gist : sig

    (** {1:intf Interfaces}

        Interfaces allow to directly inject types that satisfy a given
        interface in the representation. *)

    (** {2:array_interface Arrays} *)

    (** Array interface. *)
    module type ARRAY = sig
      type t
      (** The type for arrays. *)

      type elt
      (** The type of elements of the array. *)

      val get : t -> int -> elt
      (** [get a i] is the [i]th zero-based element of [a]. *)

      val set : t -> int -> elt -> unit
      (** [set a i v] sets the [i]th zero-based element of [a] to [v]. *)

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

      val type_name : string
      (** [type_name] is a type name for the array. *)
    end

    type ('elt, 'arr) array_module =
      (module ARRAY with type t = 'arr and type elt = 'elt)
    (** The type for representing array types of type ['a] with elements
        of type ['elt]. *)

    (** {2:map_interface Maps} *)

    (** Map interface. *)
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
      (** [fold f m init] folds over the bindings of [m] with [f]
          starting with [init]. *)

      val equal : t -> t -> bool
      (** [equal eq m0 m1] tests whether [m0] and [m1] contain
          equal keys and associate them with equal values. *)

      val compare : t -> t -> int
      (** [compare cmp m0 m1] totally order [m0] and [m1] with
          [cmp] used to compare the value of equal keys. *)

      val type_name : string
      (** [type_name] is a type name for the map. *)
    end

    type ('k, 'v, 'm) map_module =
      (module MAP with type t = 'm and type key = 'k and type value = 'v)
    (** The type for representing map types of type ['m] mapping keys of type
        ['k] to values of type ['v]. *)

    (** {1:type_representation Type representation} *)

    (** Gist metadata.

        Metadata, found throughout the description, allows to
        associate arbitrary type-dependent key-value data to the
        description.  Processors can create and expose new keys to
        allow descriptions to influence their outputs.

        Since the key value can depend on the type of the type you
        describe the interface is slightly unusual compared
        to a classic heterogeneous dictionary. A key is the result of
        a functor instantiated with the data type of the value. For example:
        {[
          type 'a fmt = Format.formatter -> 'a -> unit
          module Meta_fmt = Type.Gist.Meta.Key (struct type 'a t = 'f fmt end)
          module Meta_skip = Type.Gist.Meta.Key (struct type 'a t = bool end)
        ]}
        you can then use the module's {{!Meta.KEY}key interface} to test
        key membership, add and remove data for the key. For example:
        {[
          let meta : int Meta.t =
            Type.Gist.Meta.empty |> Meta_fmt.add Format.pp_print_int

          let meta : string Meta.t =
            Type.Gist.Meta.empty |> Meta_fmt.add Format.pp_print_string
        ]}*)
    module Meta : sig
      type 'a t
      (** The type for metadata for types of type ['a]. Key values
          can depend on ['a]. *)

      val make : doc:string -> 'a t
      (** [make ~doc] is [Doc.add doc empty]. *)

      val empty : 'a t
      (** [empty] is the empty metadata. *)

      val is_empty : 'a t -> bool
      (** [is_empty m] is [iff] [m] has no bindings. *)

     (** Type signature to describe the type dependent value
         of a type. *)
      module type VALUE = sig
        type 'a t
        (** The type for the key value that depends on ['a]. *)
      end

      (** The type for key modules.

          A key module is a module that handles a specific key. *)
      module type KEY = sig
        type 'a meta := 'a t

        type 'a value
        (** The type for the key's value. *)

        val mem : 'a meta -> bool
        (** [mem m] is [true] iff [m] has a binding for the key. *)

        val add : 'a value -> 'a meta -> 'a meta
        (** [add v m] is [m] with the key bound to [v] *)

        val find : 'a meta -> 'a value option
        (** [find m] is the binding for the key (if any). *)

        val remove : 'a meta -> 'a meta
        (** [remove m] is [m] with the binding for the key removed (if
            bound). *)
      end

      (** [Key (V)] is a new key with values of type ['a V.t]. *)
      module Key (V : VALUE) : KEY with type 'a value = 'a V.t

      (** {1:std_keys Standard keys. *)

      (** [Doc] is a key for a doc string. *)
      module Doc : KEY with type 'a value = string

      (** [Ignore] is a key for specifying to ignore the description. *)
      module Ignore : KEY with type 'a value = bool
    end

    type 'a scalar =
    | Unit : unit Meta.t -> unit scalar
    | Bool : bool Meta.t -> bool scalar
    | Char : char Meta.t -> char scalar
    | Uchar : Uchar.t Meta.t -> Uchar.t scalar
    | Int : int Meta.t -> int scalar
    | Int32 : int32 Meta.t -> int32 scalar
    | Int64 : int64 Meta.t -> int64 scalar
    | Nativeint : nativeint Meta.t -> nativeint scalar
    | Float : float Meta.t -> float scalar (** *)
    (** The type for representing scalar types of type ['a].
        See {{!scalar_ops}scalars}. *)

    type ('elt, 'arr) arraylike =
    | String : string Meta.t * char t -> (char, string) arraylike
    | Bytes : bytes Meta.t * char t -> (char, bytes) arraylike
    | Array : 'elt array Meta.t * 'elt t -> ('elt, 'elt array) arraylike
    | Bigarray1 :
        ('elt, 'b, 'c) Bigarray.Array1.t Meta.t *
        ('elt, 'b) Bigarray.kind * 'c Bigarray.layout * 'elt t ->
        ('elt, ('elt, 'b, 'c) Bigarray.Array1.t) arraylike
    | Array_module :
        'arr Meta.t * ('elt, 'arr) array_module * 'elt t ->
        ('elt, 'arr) arraylike
    (** *)
    (** The type for representing array types of type ['a] with
        elements of type ['elt]. See {{!arraylike_ops}arraylike}. *)

    and ('k, 'v, 'm) maplike =
    | Hashtbl :
        ('k, 'v) Hashtbl.t Meta.t * 'k t * 'v t ->
        ('k, 'v, ('k, 'v) Hashtbl.t) maplike
    | Map_module :
        'm Meta.t * ('k, 'v, 'm) map_module * 'k t * 'v t ->
        ('k, 'v, 'm) maplike
    (** *)
    (** The type for representing map types of type ['m] mapping keys of
        type ['k] to values of type ['v].
        See {{!maplike_ops}maplike}. *)

    and 'p product
    (** The type for representing product types of type ['p].
        See {{!product_ops}products}. *)

    and 'r record = 'r product
    (** The type for representing record types of type ['r].
        See {{!record_ops}records}. *)

    and 'v variant
    (** The type for representing variants of type ['v].
        See {{!variant_ops}variants}. *)

    and 's sum =
    | Option : 'a option Meta.t * 'a t -> 'a option sum
    | Either : ('a, 'b) Either.t Meta.t * 'a t * 'b t -> ('a, 'b) Either.t sum
    | Result : ('a, 'b) result Meta.t * 'a t * 'b t -> ('a, 'b) result sum
    | List : 'a list Meta.t * 'a t -> 'a list sum
    | Variant : 'v variant -> 'v sum (** *)
    (** The type for representing sum types of type ['s]. See
        {{!sum_ops}sums}. *)

    and ('a, 'b) func
    (** The type for representing functions types ['a -> 'b].
        See {{!function_ops}functions}. *)

    and 'a abstract
    (** The type for representing abstract types of type ['a].
        See {{!abstract_ops}abstract types}. *)

    and 'a t =
    | Scalar : 'a scalar -> 'a t
    | Arraylike : ('elt, 'arr) arraylike -> 'arr t
    | Maplike : ('k, 'v, 'm) maplike -> 'm t
    | Product : 'p product -> 'p t
    | Record : 'r product -> 'r t
    | Sum : 's sum -> 's t
    | Func : ('a, 'b) func -> ('a -> 'b) t
    | Abstract : 'a abstract -> 'a t
    | Lazy : 'a lazy_t Meta.t * 'a t -> 'a lazy_t t
    | Ref : 'a ref Meta.t * 'a t -> 'a ref t
    | Rec : 'a t lazy_t -> 'a t (** Recursion *)
    (** The type for type gists. *)

    (** {1:constructors Constructors and operations} *)

    val todo : ?type_name:string -> unit -> 'a t
    (** [todo ~type_name ()] is a stub gist. Generic functions
        will raise [Invalid_argument] when they hit the stub. *)

    val ref : ?meta:'a ref Meta.t -> 'a t -> 'a ref t
    (** [ref g] is [Ref g]. *)

    val lazy' : ?meta:'a lazy_t Meta.t -> 'a t -> 'a lazy_t t
    (** [lazy' ~meta g] is [Lazy (meta, g)]. *)

    val rec' : 'a t lazy_t -> 'a t
    (** [rec' lg] is [Rec lg]. *)

    (** {2:scalar_ops Scalars} *)

    (** Operating on scalar types. *)
    module Scalar : sig

      type 'a t = 'a scalar
      (** The type for scalars of type ['a].  *)

      val meta : 'a scalar -> 'a Meta.t
      (** [meta s] is the meta of [s]. *)

      val zero : 'a scalar -> 'a
      (** [zero s] is a zero value for [s]. *)

      val type_name : 'a scalar -> string
      (** [type_name s] is the OCaml type name of the scalar of [s]. *)

      val with_meta : 'a Meta.t -> 'a scalar -> 'a scalar
      (** [with_meta meta s] is [s] with meta [meta]. *)

      val equal : 'a scalar -> 'a -> 'a -> bool
      (** [equal s v0 v1] is [true] iff [v0] and [v1] are the same
          value as per the Stdlib's corresponding [M.equal] function. *)

      val compare : 'a scalar -> 'a -> 'a -> int
      (** [equal s v0 v1] is [true] iff [v0] and [v1] are the same
          value as per the Stdlib's corresponding [M.compare] function. *)

      val pp : 'a scalar -> Format.formatter -> 'a -> unit
      (** [pp s] is a formatter for scalar [s] *)

      val to_string : 'a scalar -> 'a -> string
      (** [to_string s v] converts [v] to a string. This uses the
          Stdlib's corresponding [M.to_string] functions, except
          for:
          {ul
          {- [char], UTF-8 compatible characters are output as is,
             otherwise hex escaped. {b TODO.}
             What about simply interpreting the byte as a Unicode scalar
             value ? (that'd give a latin1 interpretation I think).}
          {- [Uchar.t], the UTF-8 encoding of the character is returned}} *)
    end

    val unit : unit t
    (** [unit] is [Scalar (Unit Meta.empty)]. *)

    val bool : bool t
    (** [bool] is [Scalar (Bool Meta.empty)]. *)

    val char : char t
    (** [char] is [Scalar (Char Meta.empty)]. *)

    val uchar : Uchar.t t
    (** [uchar] is [Scalar (Uchar Meta.empty)]. *)

    val int : int t
    (** [int] is [Scalar (Int Meta.empty)]. *)

    val int32 : int32 t
    (** [int32] is [Scalar (Int32 Meta.empty)]. *)

    val int64 : int64 t
    (** [int64] is [Scalar (Int64 Meta.empty)]. *)

    val nativeint : nativeint t
    (** [nativeint] is [Scalar (Nativeint Meta.empt)y]. *)

    val float : float t
    (** [float] is [Scalar (Float Meta.empty)]. *)

    (** {2:arraylike_ops Arraylike}

        The arraylike type gathers {{!type-array_module}generic linear arrays}
        and a few {{!type-arraylike}special cases} for linear array types
        of the standard library. The specialisation can be
        {{!Arraylike.to_array_module}converted} to generic linear arrays. *)

    (** Operating on arraylike types. *)
    module Arraylike : sig

      (** {1:arraylike Arraylikes} *)

      type 'a gist := 'a t

      type ('elt, 'arr) t = ('elt, 'arr) arraylike
      (** The type for representing arraylike types of type ['arr]
          with elements of type ['elt]. *)

      val meta : ('elt, 'arr) arraylike -> 'arr Meta.t
      (** [meta a] is the metadata of [a]. *)

      val type_name : ('elt, 'arr) arraylike -> string
      (** [type_name a] is the type name of [a]. *)

      val elt : ('elt, 'arr) t -> 'elt gist
      (** [elt a] is the representation of the elements of [a]. *)

      val with_meta :
        'arr Meta.t -> ('elt, 'arr) arraylike -> ('elt, 'arr) arraylike
      (** [with_meta meta a] is [a] with meta [meta]. *)

      (** {1:generalizing Generalizing to array modules} *)

      val to_array_module : ('elt, 'arr) arraylike -> ('elt, 'arr) array_module
      (** [to_array_module a] is an array module for [a].
          Note that if [a] is {!constructor-String}, the {!ARRAY.set}
          function raises [Invalid_argument] in the resulting module. *)
    end

    val string : string t
    (** [string] is [Arraylike (String (Meta.empty, char))]. *)

    val bytes : bytes t
    (** [bytes] is [Arraylike (Bytes (Meta.empty, char))]. *)

    val array : ?meta:'elt array Meta.t -> 'elt t -> 'elt array t
    (** [array] represents arrays with elements of given representation. *)

    val bigarray1 :
      ?meta:('elt, 'b, 'c) Bigarray.Array1.t Meta.t ->
      ('elt, 'b) Bigarray.kind -> 'c Bigarray.layout ->
      'elt t -> ('elt, 'b, 'c) Bigarray.Array1.t t
    (** [bigarray] represents bigarrays with elements of given
        representation. *)

    val array_module : ?meta:'arr Meta.t ->
      ('elt, 'arr) array_module -> 'elt t -> 'arr t
    (** [array_module] represents array modules with elements of the
        given representation. *)

    (** {2:maplike_ops Maplike}

        The maplike type gathers generic {{!MAP}functional key value
        maps} and a {{!type-maplike}special case} for hash tables. *)

    (** Operations on maplikes. *)
    module Maplike : sig

      (** {1:maplikes Maplikes} *)

      type 'a gist := 'a t

      type ('k, 'v, 'm) t = ('k, 'v, 'm) maplike
      (** The type for representing maplike types of type ['m]
          with keys of type ['k] and values of type ['v]. *)

      val meta : ('k, 'v, 'm) maplike -> 'm Meta.t
      (** [meta m] is the metadata of [m]. *)

      val type_name : ('k, 'v, 'm) maplike -> string
      (** [type_name m] is the type name of [m]. *)

      val key : ('k, 'v, 'm) maplike -> 'k gist
      (** [key m] is the representation of the keys of [m]. *)

      val value : ('k, 'v, 'm) maplike -> 'v gist
      (** [value m] is the representation of the values of [m]. *)

      val with_meta : 'm Meta.t -> ('k, 'v, 'm) maplike -> ('k, 'v, 'm) maplike
      (** [with_meta meta m] is [m] with meta [meta]. *)

      (** {1:map_modules Map modules from [Map.S] modules} *)

      (** The type for map values. *)
      module type VALUE = sig
        type t
        (** The type for map values. *)

        val equal : t -> t -> bool
        (** [equal] tests values for equality. *)

        val compare : t -> t -> int
        (** [compare] is a total order on values compatible with {!equal}. *)
      end

      module Map_module_of_map (V : VALUE) (M : Map.S) : MAP
        with type t = V.t M.t
         and type key = M.key
         and type value = V.t
      (** [Map_module_of_map (V) (M)] is a map module for standard
          library map [M] with value of type [V]. *)
    end

    val hashtbl :
      ?meta:('k, 'v) Hashtbl.t Meta.t -> 'k t -> 'v t ->
      ('k, 'v) Hashtbl.t t
    (** [hashtbl] represents hashtables with keys and values of
        given representations. *)

    val map_module :
      ?meta:'m Meta.t -> ('k, 'v, 'm) map_module -> 'k t -> 'v t -> 'm t
    (** [map_module m k v] represents map modules with keys and values
        of the given representations. See {{!Maplike.map_modules}here}
        create map modules from standard library maps. *)

    (** {2:product_ops Products}

        Products (tuples), records and variant cases are all products
        of types. The toplevel type representation distinguishes them in
        different cases but otherwise they share the same representation: a
        product of typed and possibly named fields.

        The {!type-field} type represents an individual field of a product. The
        {!type-fields} type represent the ordered sequence of fields and the
        way to construct the product from its fields with a constructor
        function. *)

    type ('p, 'f) field
    (** The type for representing the field of type ['f] of a product of
        type ['p]. See the {!Field} module. *)

    type ('p, _) fields =
    | Ctor : 'ctor -> ('p, 'ctor) fields
    | App : ('p, 'f -> 'a) fields * ('p, 'f) field -> ('p, 'a) fields (** *)
    (** The type for representing the fields of a product of type ['p] and
        its construction via a constructor function. See the {!Fields} module.

        When a fields value types with [('p, 'p) fields] we know how
        to project each field of product ['p] and how to construct a
        value from its fields. *)

    (** Operating on fields.

        Fields represent components of {{!module-Product}products} which are
        used to represent OCaml's tuples, records and variants
        cases. *)
    module Field : sig

      (** {1:fields Fields} *)

      type 'a gist := 'a t

      type ('p, 'f) t = ('p, 'f) field
      (** The type for representing the field of type ['f] of a product of
          type ['p]. *)

      val make :
        ?meta:('p, 'f) field Meta.t -> ?name:string ->
        ?inject:('p -> 'f -> 'p) -> ?set:('p -> 'f -> unit) ->
        ?default:'f -> 'f gist ->
        ('p -> 'f) -> ('p, 'f) field
      (** [v g project] is a field such that:
          {ul
          {- [g] is the type representation of the field.}
          {- [project] projects the field value from the product.}
          {- [meta] is the metadata (defaults to {!Meta.empty}).}
          {- [name] is the name for the field (defaults to [""]). For
             record fields this is the field name. For variant case this is
             the case name. For tuples this is the empty string or a
             name for a type abbreviation.}
          {- [inject] is an immutable update function to update the field
             of the prodcut (defaults to [None]).}
          {- [set] is a mutable update function to set the field of the
             product, for mutable record fields (defaults to [None]).}
          {- [default] is a default value for the dimension
             (defaults to [None]).}} *)

      val meta : ('p, 'f) field -> ('p, 'f) field Meta.t
      (** [meta f] is the meta of [f]. *)

      val name : ('p, 'f) field ->  string
      (** [name f] is the name of [f] (if any). *)

      val gist :  ('p, 'f) field -> 'f gist
      (** [gist f] is the gist of [f]. *)

      val project : ('p, 'f) field ->  ('p -> 'f)
      (** [project f] is the projection function of [f]. *)

      val inject :  ('p, 'f) field ->  ('p -> 'f -> 'p) option
      (** [inject f] is the optional immutable function of [f]. *)

      val set : ('p, 'f) field ->  ('p -> 'f -> unit) option
      (** [set f] is the optional mutable function of [f]. *)

      val default : ('p, 'f) field -> 'f option
      (** [default f] is a default value for the field. *)
    end

    (** Operating on sequences of fields. *)
    module Fields : sig

      (** {1:fields Fields} *)

      type ('p, 'a) t = ('p, 'a) fields
      (** The type for fields for a type of type ['p].

          The ['a] represents the partial application of a constructor to
          the fields. Once ['a] is equal to ['p] we have a
          {{!module-Product}product}. *)

      val is_empty : ('p, 'a) fields -> bool
      (** [is_empty fs] is [true] if [fs] has no fields. *)

      val is_singleton : ('p, 'a) fields -> bool
      (** [is_singleton fs] is [true] if [fs] has a single field. *)
    end

    val ctor : 'a -> ('p, 'a) fields
    (** [ctor f] is [Ctor f]. This lifts the constructor function [f] for the
        type ['p] in order to construct a list of fields with {!app} yielding a
        result of type ['p]. *)

    val app : ('p, 'f -> 'a) fields -> ('p, 'f) field -> ('p, 'a) fields
    (** [app f arg] is [App (f, app)] *)

    val ( * ) : ('p, 'f -> 'a) fields -> ('p, 'f) field -> ('p, 'a) fields
    (** [fs * f] is [App (fs, f)]. Fancy operator so that you can
        write, for example :
        {[
let pair_gist gfst gsnd =
  let pair x y = (x, y) in
  let fst = Type.Gist.(dim gfst fst) in
  let snd = Type.Gist.(dim gsnd snd) in
  Type.Gist.(product @@ ctor pair * fst * snd)
        ]} *)

    (** Operating on products. *)
    module Product : sig

      (** {1:products Products} *)

      type 'p t = 'p product
      (** The type for products. *)

      val make :
        ?meta:'p Meta.t -> ?name:string -> ('p, 'p) fields -> 'p product
      (** [make fields] is a product with:
          {ul
          {- [fields] the ordered sequence of fields of the product.}
          {- [meta] is the metadata (defaults to {!Meta.empty}).}
          {- [name] the name of the product (defaults to [""]). For records
             this is the type name. For variant cases this is the case name.
             For products, if non empty, this is the name of a type
             abbreviation.}} *)

      val meta : 'p product -> 'p Meta.t
      (** [meta p] is the metadata of [p]. *)

      val name : 'p product -> string
      (** [name p] is the name of [p] (if any). For records or products this
          is the type name for variant cases this is the case name. *)

      val fields : 'p product -> ('p, 'p) fields
      (** [fields p] are the fields of [p]. *)

      val is_empty : 'p product -> bool
      (** [is_empty p] is [true] if [p] has no fields. *)

      val is_singleton : 'p product -> bool
      (** [is_singleton p] is [true] if [p] has a single field. *)

      val with_meta : 'p Meta.t -> 'p product -> 'p product
      (** [with_meta meta p] is [p] with meta [meta]. *)
    end

    val dim :
      ?meta:('p, 'a) field Meta.t -> ?name:string ->
      ?inject:('p -> 'a -> 'p) -> ?default:'a -> 'a t -> ('p -> 'a) ->
      ('p, 'a) field
    (** [dim g project] defines a dimension of a product of type ['p].
        This is just {!Field.make}. *)

    val product :
      ?meta:'p Meta.t -> ?type_name:string -> ('p, 'p) fields -> 'p t
    (** [product dims] is a product with dimensions and construction
        represented by [dims]. This is {!Product.make} wrapped in a
        {!constructor-Product} gist value. *)

    val p2 :
      ?meta:('a * 'b) Meta.t -> ?type_name:string -> 'a t -> 'b t -> ('a * 'b) t
    (** [p2] represents pairs with given dimensions types. *)

    val p3 :
      ?meta:('a * 'b *'c) Meta.t -> ?type_name:string -> 'a t -> 'b t -> 'c t ->
      ('a * 'b * 'c) t
    (** [p3] represents triplets with given dimensions types. *)

    val p4 :
      ?meta:('a * 'b * 'c * 'd) Meta.t -> ?type_name:string -> 'a t -> 'b t ->
      'c t -> 'd t -> ('a * 'b * 'c * 'd) t
    (** [p4] represents quadruplets with given dimensions types. *)

   (** {2:record_ops Records}

       Records are represented by named {{!product_ops}products} of
       named fields. *)

    val field :
      ?meta:('r, 'f) field Meta.t -> ?inject:('r -> 'f -> 'r) ->
      ?set:('r -> 'f -> unit) -> ?default:'f -> string -> 'f t ->
      ('r -> 'f) -> ('r, 'f) field
    (** [field name g project ] defines a record field for a record ['r].
        This is just {!Field.make}. *)

    val record : ?meta:'r Meta.t -> string -> ('r, 'r) fields -> 'r t
    (** [record name fields] is a record named [name] with fields
        [field]. This is {!Product.make} wrapped in a {!constructor-Record}
        gist value. *)

(** {2:variant_ops Variants}

    Variants are described by a list of cases and function that
    indicate how to select the case for a value. A case is a
    {{!product_ops}product} named after the case name. *)

    (** Operating on variants. *)
    module Variant : sig

      (** {1:variant_cases Variant cases} *)

      type 'v case = 'v product
      (** The type for representing variant cases of a variant of
          type ['v]. *)

      (** Variant cases.

          Variant cases are {{!module-Product}products} whose name
          is the case name. *)
      module Case : sig

        (** {1:products Variant cases} *)

        type 'v t = 'v case
        (** The type for products. *)

        val make : ?meta:'v Meta.t -> string -> ('v, 'v) fields -> 'v case
        (** [make fields] is a product with fields [fields]. [name] is
            optional, it defaults to [""]. *)

        val meta : 'v case -> 'v Meta.t
        (** [meta c] is the metadata of [c]. *)

        val name : 'v case -> string
        (** [name c] is the case name of [c] (if any). *)

        val fields : 'v case -> ('v, 'v) fields
        (** [fields c] are the fields of [c]. *)

        val is_empty : 'v case -> bool
        (** [is_empty c] is [true] if the case has no arguments. *)

        val rec_field_count : 'v case -> int
        (** [rec_field_count c] is the number of recursive fields
            in [c]. *)
      end

      (** {1:variants Variants} *)

      type 'a gist := 'a t

      type 'v t = 'v variant
      (** The type for representing variants of type ['v]. *)

      val make :
        ?meta:'v Meta.t -> string -> ('v -> 'v case) ->
        'v case list -> 'v variant
      (** [v type_name project cases] is a variant with type name [type_name]
          with deconstructor [project] and case enumeration [cases]. *)

      val meta : 'v variant -> 'v Meta.t
      (** [meta v] is the metadata of [v]. *)

      val type_name : 'v variant -> string
      (** [type_name v] is the type name of [v]. *)

      val project : 'v variant -> 'v -> 'v case
      (** [project v] is the projection function (case selector) of [v]. *)

      val cases : 'v variant -> 'v case list
      (** [cases v] are the variant's cases. *)

      (** {1:std Stdlib variants as variants}

          The {!sum} type has specializations for these variants. You should
          like use that if appropriate in your descriptions,
          see {{!sum_ops}sums}. Consumers of the representation can always
          generalize with {!Sum.to_variant} which invokes these functions. *)

      val of_option : ?meta:'a option Meta.t -> 'a gist -> 'a option variant
      (** [of_option] represents options of the given representation type
          as a variant. *)

      val of_either :
        ?meta:('a, 'b) Either.t Meta.t -> 'a gist -> 'b gist ->
        ('a, 'b) Either.t variant
      (** [of_option] represents eithers of the given representation type
          as a variant. *)

      val of_result :
        ?meta:('a, 'b) result Meta.t -> 'a gist -> 'b gist ->
        ('a, 'b) result variant
      (** [of_result] represents results of the given representation types
          as a variant. *)

      val of_list : ?meta:'a list Meta.t -> 'a gist -> 'a list variant
      (** [of_list] represents lists of the given representation type
          as a variant. *)
    end

    val case : ?meta:'v Meta.t -> string -> ('v, 'v) fields -> 'v Variant.case
    (** [case name fields] is a variant case with product defined
        by [fields]. This is {!Variant.Case.make}. *)

    val variant :
      ?meta:'v Meta.t -> string -> ('v -> 'v Variant.case) ->
      'v Variant.case list -> 'v t
    (** [variant type_name project cases] is a variant decontructed
        by [project] and whose cases are enumerated in [cases].
        This is {!Variant.make} wrapped in a {!constructor-Variant}
        and {!constructor-Sum} gist value. *)

   (** {2:sum_ops Sums}

       The sum type gathers {{!variant_ops}generic variants} and a few
       {{!type-sum}special cases} for variants of the standard library.
       The specialisation can be {{!Sum.to_variant}converted} to a generic
       variant. *)

    (** Operating on sums. *)
    module Sum : sig

      type 's t = 's sum
      (** The type for sum types of type ['s] *)

      val meta : 's sum -> 's Meta.t
      (** [meta s] is the meta of [s]. *)

      val type_name : 's sum -> string
      (** [type_name s] is the type name of [s]. *)

      val to_variant : 's sum -> 's variant
      (** [to_variant s] is [s] as a variant type representation.
          This generalizes the specialized {{!type-sum}[sum] cases} to the
          generic representation of variants. *)

      val with_meta : 's Meta.t -> 's sum -> 's sum
      (** [with_meta meta p] is [p] with meta [meta]. *)
    end

    val option : ?meta:'a option Meta.t -> 'a t -> 'a option t
    (** [option] represents options of the given representation type as
        an {!constructor-Option} wrapped in a {!constructor-Sum} gist value. *)

    val either :
      ?meta:('a, 'b) Either.t Meta.t -> 'a t -> 'b t -> ('a, 'b) Either.t t
    (** [either] represents eithers of the given representation types as
        an {!constructor-Either} wrapped in a {!constructor-Sum} gist value. *)

    val result :
      ?meta:('a, 'b) result Meta.t -> 'a t -> 'b t -> ('a, 'b) result t
    (** [result] represents results of the given representation types as
        an {!constructor-Result} wrapped in a {!constructor-Sum} gist value. *)

    val list : ?meta:'a list Meta.t -> 'a t -> 'a list t
    (** [list] represents lists of the given representation type as
        {!constructor-List} wrapped in a {!constructor-Sum} gist value. *)

    (** {2:function_ops Functions} *)

    (** Operating on functions. *)
    module Func : sig

      (** {1:functions Functions} *)

      type 'a gist := 'a t

      type ('a, 'b) t = ('a, 'b) func
      (** The type for representing functions from type ['a] to ['b]. *)

      val make : ?meta:('a -> 'b) Meta.t -> 'a gist -> 'b gist -> ('a, 'b) func
      (** [make d r] is a function with domain represented by [d] and
          range represented by [r]. *)

      val meta : ('a, 'b) func -> ('a -> 'b) Meta.t
      (** [meta f] is the metadata of [f]. *)

      val domain : ('a, 'b) func -> 'a gist
      (** [domain f] is representation of the domain of [f]. *)

      val range : ('a, 'b) func -> 'b gist
      (** [range f] is representation of the range of [f]. *)

      val with_meta : ('a -> 'b) Meta.t -> ('a, 'b) func -> ('a, 'b) func
      (** [with_meta meta f] is [f] with meta [meta]. *)
    end

    val func : ?meta:('a -> 'b) Meta.t -> 'a t -> 'b t -> ('a -> 'b) t
    (** [func ~meta d r] represents a function from domain [d] to
        range [r]. This is {!Func.make} wrapped in {!constructor-Func}
        gist value. *)

    val ( @-> ) : 'a t -> 'b t -> ('a -> 'b) t
    (** [d @-> r] is [func d r]. *)

   (** {2:abstract_ops Abstract types}

       Abstract types are represented by lists of versioned public
       {{!Abstract.module-Repr}representations} with which they can
       be converted. *)

    (** Operating on abstract types. *)
    module Abstract : sig

      (** Public representations. *)
      module Repr : sig
        type 't gist := 't t

        type ('a, 'repr) t
        (** The type for representing abtract types of type ['a] with
            public values of type ['repr]. *)

        val make :
          ?meta:'repr Meta.t -> version:string -> 'repr gist ->
          ('a -> 'repr) -> ('repr -> 'a) -> ('a, 'repr) t
        (** [make ~version r inject project] is a public representation
            for abstract type ['t] with:
            {ul
            {- [version] is a version name for the public representation.
               Intepretation is left to users, usually testing with
               string equality should be sufficient.}
            {- [r] is the representation representing the abstract type.}
            {- [inject] injects abstract types in the public representation.}
            {- [project] projects the public representation into the abstract
               type.}
            {- [meta] metadata for the representation.}} *)

        val meta : ('a, 'repr) t -> 'repr Meta.t
        (** [meta r] is the metadata of [r]. *)

        val version : ('a, 'repr) t -> string
        (** [version r] is a version name for the representation. *)

        val gist : ('a, 'repr) t -> 'repr gist
        (** [gist r] is the public representation of [r]. *)

        val inject : ('a, 'repr) t -> 'a -> 'repr
        (** [inject r] injects the abstract type into the public
            type. *)

        val project : ('a, 'repr) t -> 'repr -> 'a
        (** [project r] projects the public representation in the abstract
            type. *)

        val with_meta : 'repr Meta.t -> ('a, 'repr) t -> ('a, 'repr) t
        (** [with_meta meta r] is [r] with meta [meta]. *)
      end

      type 't repr = Repr : ('t, 'rep) Repr.t -> 't repr
      (** The type for existential abtract representation. *)

      val repr :
        ?meta:'repr Meta.t -> version:string -> 'repr t ->
        ('a -> 'repr) -> ('repr -> 'a) -> 'a repr
      (** [repr ~version repr inject projet] is a public representation.
          This is {!Repr.make} wrapped in {!constructor-Repr}. *)

      (** {1:abstract_types Abstract types} *)

      type 'a t = 'a abstract
      (** The type for abstract types of type ['a]. *)

      val make : ?meta:'a Meta.t -> string -> reprs:'a repr list -> 'a abstract
      (** [v ~meta name reprs] represents an abstract type named
          [name] with public representations [reprs].

          The first representation of [reprs] is assumed to be the
          default one to use. Other representations can be used when
          interfacing with systems that support representation
          versioning. If [reprs] is empty, the type remains
          abstract. *)

      val meta : 'a abstract -> 'a Meta.t
      (** [meta a] is the metadata of [a]. *)

      val type_name : 'a abstract -> string
      (** [type_name a] is the type name of [a]. *)

      val reprs : 'a abstract -> 'a repr list
      (** [reprs a] are the public representations of [a]. The first
          representation (if any) is the one to favour. *)

      val with_meta : 'a Meta.t -> 'a abstract -> 'a abstract
      (** [with_meta meta a] is [a] with meta [meta]. *)
    end

    val abstract :
      ?meta:'a Meta.t -> string -> 'a Abstract.repr list -> 'a t
    (** [abstract ~meta name reprs] represents an abstract type named
        [type_name] with public representations [reprs]. This is
        {!Abstract.make} wrapped in an {!constructor-Abstract} gist value. *)

    (** {2:gist_ops Gists} *)

    val meta : 'a t -> 'a Meta.t
    (** [meta g] is [g]'s top meta. *)

    val with_meta : 'a Meta.t -> 'a t -> 'a t
    (** [with_meta m g] is [g] with meta [meta]. *)

    val type_name : 'a t -> string
    (** [type_name g] is [g]'s type name. *)

    val pp_type : Format.formatter -> 'a t -> unit
    (** [pp_type g] formats a pseudo OCaml type expression for [g]. *)
  end
end

(** Function manipulation and generic functions. *)
module Fun : sig

  (** Generic functions. *)
  module Generic :sig

    (** Generic function metadata keys.

        A few {!Type.Gist.Meta.t} keys to control the behaviour of generic
        functions. *)
    module Meta : sig

      (** Custom formatter key.

          The {!Fun.Generic.pp} function consults this key in the
          metadata of a gist [g] before printing the value described
          by the gist. If a formatter is found, the value is formatted
          using that formatter instead. This can also be used to indicate
          that you would not like to print a substructure by formatting
          nothing, the {!Fmt.ignore} function does that. *)
      module Fmt : sig
        type 'a t = Format.formatter -> 'a -> unit
        (** The type for custom value formatter of type ['a]. *)

        val ignore : 'a t
        (** [ignore] formats nothing. *)

        include Type.Gist.Meta.KEY with type 'a value := 'a t (** @inline *)
      end

      (** Custom equality key.

          The {!Fun.Generic.equal} function consults this key in the
          metadata of a gist [g] before determining equality between
          values described by the gist. If a function is found, the
          values are compared with this function instead.

          This can also be used to indicate that you would not like to compare
          a subtructure by always returning [true], the
          {!Equal.ignore} function does that. *)
      module Equal : sig
        type 'a t = 'a -> 'a -> bool
        (** The type for custom value equality of type ['a]. *)

        val ignore : 'a -> 'a -> bool
        (** [ignore v0 v1] is [true]. *)

        include Type.Gist.Meta.KEY with type 'a value := 'a t (** @inline *)
      end

      (** Custom comparison key.

          The {!Fun.Generic.compare} function consults this key in the
          metadata of a gist [g] before comparing values described
          by the gist. If a function is found, the values are compared
          with this function instead.

          This can also be used to indicate that you would not like to
          compare a substructure by always returning [0] (equal), the
          {!Compare.ignore} function does that. *)
      module Compare : sig
        type 'a t = 'a -> 'a -> int
        (** The type for custom value equality of type ['a]. *)

        val ignore : 'a -> 'a -> int
        (** [ignore v0 v1] is [0]. *)

        include Type.Gist.Meta.KEY with type 'a value := 'a t (** @inline *)
      end

      (** Custom random generator and random sizing.

          These keys influence the {!Fun.Generic.random} function. *)
      module Random : sig

      (** Custom random generator.

          The {!Fun.Generic.random} function consults this key in the
          metadata of a gist [g] before generating values described
          by the gist. If a function is found, the values are generated
          with this function instead.

          This can also be used to indicate that you would like a
          substructure to remain constant. The {!Gen.const} function
          does that. *)
        module Gen : sig

          type 'a t = size:int -> Random.State.t -> bound:int -> 'a
          (** The type for custom random generators. [bound] is a
              bounding factor that depends on [size] for recursive data
              structures when the bound reaches [0] you should no
              longer recurse. *)

          val const : 'a -> 'a t
          (** [const v] ignores the random generator and returns [v]. *)

          include Type.Gist.Meta.KEY with type 'a value := 'a t (** @inline *)
        end

        (** Sizing factor alteration.

            The {!Fun.Generic.random} function consults this key in
            the metadata of a gist [g] to determine the sizing factor
            for generating a value described by the gist. *)
        module Size : sig
          type 'a t = int
          include Type.Gist.Meta.KEY with type 'a value := 'a t (** @inline *)
        end
      end
    end

    val pp : 'a Type.Gist.t -> Format.formatter -> 'a -> unit
    (** [pp g ppf v] formats [v] as described by [g] on [ppf] with a pseudo
        OCaml syntax similar to what you would see in the toplevel.
        The function can be selectively overriden via the {!Meta.Fmt}
        metadata key. Fields whose {!Type.Gist.Meta.Ignore} is [true]
        are not printed. *)

    val equal : 'a Type.Gist.t -> 'a -> 'a -> bool
    (** [equal g v0 v1] determines the equality of values [v0] and [v1]
        as described by [g]. The process can be selectively overriden
        via the {!Meta.Equal} metadata key. Fields whose
        {!Type.Gist.Meta.Ignore} is [true] are ignored (are always equal).

        The function raises [Invalid_argument] on functional values and
        abstract types without representation; you can ignore them by using
        {!Meta.Equal.ignore} for their {!Meta.Equal} key. *)

    val compare : 'a Type.Gist.t -> 'a -> 'a -> int
    (** [compare g v0 v1] compares values [v0] and [v1] as described by [g].
        The function can be selectively overriden via {!Meta.Compare} metadata
        keys. Fields whose {!Type.Gist.Meta.Ignore} is [true] are ignored
        (are always equal).

        The function raises [Invalid_argument] on function values and
        abstract types without representation; you can ignore them
        by using {!Meta.Compare.ignore} for their {!Meta.Compare} key. *)

    val random :
      'a Type.Gist.t -> ?size:int -> ?state:Random.State.t -> unit -> 'a
    (** [random g ~state ()] generates a random value described by [g].
        [state] is the random state to use, defaults to
        {!Random.get_state}[ ()].

        FIXME do something with {!Type.Gist.Meta.Ignore}.

        [size] is an indicative strictly positive, size factor to let
        users control an upper bound on the size and complexity of
        generated values. It should be used by generators to
        proportionally bound the length of sequences or the number of
        nodes in trees. It can be altered by gists via the
        {!Meta.Random.Size} key.

        The function raises [Invalid_argument] on functional values
        (may be lifted in the future) and abstract types without
        representation; you can represent them
        by using a suitable {!Meta.Random.Gen.const} constant value for the
        {!Meta.Random.Gen} key. *)
  end

  include module type of Fun (** @closed *)
end
