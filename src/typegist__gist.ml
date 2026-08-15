(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Fmt = Typegist__repr.Fmt

(* Names *)

type name = Typegist__repr.name
type qualified_name = Typegist__repr.qualified_name
type applied_name = Typegist__repr.qualified_name

module Name = struct
  let unqualified = Typegist__repr.name_unqualified
  let module_path = Typegist__repr.name_module_path
  let type_args = Typegist__repr.name_type_args
  let pp_applied = Typegist__repr.pp_applied_name
  let make_applied args n = Fmt.str "%a" (pp_applied args) n
end

(* Metadata *)

module Meta = Typegist__meta

(* Gists *)

type 'a t = 'a Typegist__repr.t
type 'a gist = 'a t
type v = Typegist__repr.v = V : 'a t -> v

(* Properties *)

let name = Typegist__repr.name
let doc = Typegist__repr.doc
let id = Typegist__repr.id
let meta = Typegist__repr.meta
let expr = Typegist__repr.expr

module Expr = struct
  type 'a t = 'a Typegist__repr.expr =
  | Scalar : 'a Typegist__repr.scalar -> 'a t
  | Tuple : 't Typegist__repr.tuple -> 't t
  | Record : 'r Typegist__repr.record -> 'r t
  | Variant_like : 's Typegist__repr.variant_like -> 's t
  | Array_like : ('elt, 'array) Typegist__repr.array_like -> 'array t
  | Map_like : ('k, 'v, 'map) Typegist__repr.map_like -> 'map t
  | Cell_like : ('a, 'cell) Typegist__repr.cell_like -> 'cell t
  | Func : ('a, 'b) Typegist__repr.func -> ('a -> 'b) t
  | Abstract : 'a Typegist__repr.abstract -> 'a t
  | View : ('a, 'b) Typegist__repr.view -> 'a t
  | Rec : 'a gist lazy_t -> 'a t

  let fold_gists = Typegist__repr.expr_fold_gists
  let map_gists = Typegist__repr.expr_map_gists
  let pp = Typegist__repr.pp_expr
end

let gists g = Expr.fold_gists (fun acc g -> V g :: acc) [] (expr g)

(* Predicates *)

let is_recursive = Typegist__repr.is_recursive
let is_nameless = Typegist__repr.is_nameless
let has_gist = Typegist__repr.has_gist

(* Updating *)

let update = Typegist__repr.update
let of_gist = Typegist__repr.of_gist
let rebind = Typegist__repr.rebind
let replace = Typegist__repr.replace

(* Formatting *)

let pp_def = Typegist__repr.pp_def
let pp_type_arg = Typegist__repr.pp_type_arg

(* Representations *)

let todo ?doc ?meta name =
  let todo name = invalid_arg ("TODO: " ^ name ^ " Type.Gist.t") in
  let expr = Typegist__repr.Rec (lazy (todo name)) in
  Typegist__repr.make ?doc ?meta ~name expr

(* Scalars *)

module Scalar = struct
  type 'a t = 'a Typegist__repr.scalar =
  | Unit : unit t | Bool : bool t | Char : char t | Uchar : Uchar.t t
  | Int : int t | Int32 : int32 t | Int64 : int64 t | Nativeint : nativeint t
  | Float : float t

  let applied_name = Typegist__repr.scalar_applied_name

  (* Generic constants *)

  let zero : type a. a t -> a = function
  | Unit -> () | Bool -> false | Char -> '\x00'
  | Uchar -> Uchar.of_int 0x0000 | Int -> 0 | Int32 -> 0l
  | Int64 -> 0L | Nativeint -> 0n | Float -> 0.0

  let min_scalar : type a. a t -> a = function
  | Unit -> () | Bool -> false | Char -> '\x00'
  | Uchar -> Uchar.min | Int -> Int.min_int | Int32 -> Int32.min_int
  | Int64 -> Int64.min_int | Nativeint -> Nativeint.min_int
  | Float -> min_float

  let max_scalar : type a. a t -> a = function
  | Unit -> () | Bool -> true | Char -> '\xFF'
  | Uchar -> Uchar.max | Int -> Int.max_int | Int32 -> Int32.max_int
  | Int64 -> Int64.max_int | Nativeint -> Nativeint.max_int
  | Float -> min_float

  (* Generic predicates and comparisons *)

  let equal : type a. a t -> a -> a -> bool = function
  | Unit -> Unit.equal | Bool -> Bool.equal | Char -> Char.equal
  | Uchar -> Uchar.equal | Int -> Int.equal
  | Int32 -> Int32.equal | Int64 -> Int64.equal
  | Nativeint -> Nativeint.equal | Float -> Float.equal

  let compare : type a. a t -> a -> a -> int = function
  | Unit -> Unit.compare | Bool -> Bool.compare | Char -> Char.compare
  | Uchar -> Uchar.compare | Int -> Int.compare
  | Int32 -> Int32.compare | Int64 -> Int64.compare
  | Nativeint -> Nativeint.compare | Float -> Float.compare

  (* Generic conversions *)

  let pp_char ppf = function
  | '\'' -> Fmt.string ppf {|'\''|}
  | ' ' .. '~' as c -> Fmt.char ppf '\''; Fmt.char ppf c; Fmt.char ppf '\''
  | '\n' -> Fmt.string ppf {|'\n'|}
  | '\r' -> Fmt.string ppf {|'\r'|}
  | '\t' -> Fmt.string ppf {|'\t'|}
  | c ->
      Fmt.char ppf '\''; Fmt.pf ppf "\\x%02x" (Char.code c); Fmt.char ppf '\''

  let pp_uchar ppf u =
    let esc = match Uchar.to_int u with
    | u when 0x0000 <= u && u <= 0x001F -> true (* C0 control characters *)
    | u when 0x0080 <= u && u <= 0x009F -> true (* C1 control characters *)
    | 0x2028 -> true (* line separator *)
    | 0x2029 -> true (* paragraph separator *)
    | 0x200E -> true (* left-to-right mark *)
    | 0x200F -> true (* right-to-left mark *)
    | u -> false
    in
    if esc then Fmt.pf ppf "'\\u{%04X}'" (Uchar.to_int u) else
    let b = Bytes.create (Uchar.utf_8_byte_length u) in
    ignore (Bytes.set_utf_8_uchar b 0 u);
    Fmt.char ppf '\''; Format.pp_print_as ppf 1 (Bytes.unsafe_to_string b);
    Fmt.char ppf '\''

  let pp : type a. a t -> Format.formatter -> a -> unit =
  fun g -> match g with
  | Unit -> fun ppf () -> Format.pp_print_string ppf "()"
  | Bool -> Format.pp_print_bool
  | Char -> pp_char
  | Uchar -> pp_uchar
  | Int -> Format.pp_print_int
  | Int32 -> fun ppf v -> Format.fprintf ppf "%ld" v
  | Int64 -> fun ppf v -> Format.fprintf ppf "%Ld" v
  | Nativeint -> fun ppf v -> Format.fprintf ppf "%nd" v
  | Float -> fun ppf v -> Format.fprintf ppf "%g" v
end

let unit = Typegist__repr.make (Scalar Unit)
let bool = Typegist__repr.make (Scalar Bool)
let char = Typegist__repr.char
let uchar = Typegist__repr.make (Scalar Uchar)
let int = Typegist__repr.make (Scalar Int)
let int32 = Typegist__repr.make (Scalar Int32)
let int64 = Typegist__repr.make (Scalar Int64)
let nativeint = Typegist__repr.make (Scalar Nativeint)
let float = Typegist__repr.make (Scalar Float)

(* Products *)

module Field = struct
  type nonrec name = Typegist__repr.field_name
  type ('p, 'v) t = ('p, 'v) Typegist__repr.field
  let make = Typegist__repr.make_field
  let name = Typegist__repr.field_name
  let doc = Typegist__repr.field_doc
  let meta = Typegist__repr.field_meta
  let gist = Typegist__repr.field_gist
  let get = Typegist__repr.field_get
  let set = Typegist__repr.field_set
  let iset = Typegist__repr.field_iset
  let is_mutable = Typegist__repr.field_is_mutable
end

module Product = struct
  type 'p t = 'p Typegist__repr.product
  type ('p, 'ctor) fields = ('p, 'ctor) Typegist__repr.fields =
  | Ctor : 'ctor -> ('p, 'ctor) fields
  | App : ('p, 'v -> 'ctor) fields * ('p, 'v) Field.t -> ('p, 'ctor) fields

  let is_empty = Typegist__repr.product_is_empty
  let is_singleton = Typegist__repr.product_is_singleton
  let field_count = Typegist__repr.product_field_count
  let fold_fields = Typegist__repr.product_fold_fields

  (* Fields constructor *)

  type ('p, 'ctor, 'ret) cons =
  { fields : ('p, 'ctor) fields;
    finish : ('p, 'p) fields -> 'ret }

  let cons_ctor ctor finish = { fields = Ctor ctor; finish }
  let cons_app f cons = { cons with fields = App (cons.fields, f) }
  let cons_finish cons = cons.finish cons.fields
end

let field' = Product.cons_app
let finish = Product.cons_finish
let field ?doc ?meta ?set ?iset name gist project p =
  Product.cons_app (Field.make ?doc ?meta ?set ?iset ~name gist project) p

let comp ?doc ?meta ?iset gist project p =
  Product.cons_app (Field.make ?doc ?meta ?iset gist project) p

(* Tuples *)

module Tuple = struct
  type 'a t = 'a Typegist__repr.tuple
end

let tuple ?name ?doc ?meta ctor =
  let finish p = Typegist__repr.make ?name ?doc ?meta (Expr.Tuple p) in
  Product.cons_ctor ctor finish

let t2 ?name ?doc ?meta g0 g1 =
  tuple ?name ?doc ?meta (fun v0 v1 -> v0, v1)
  |> comp g0 fst
  |> comp g1 snd
  |> finish

let t3 ?name ?doc ?meta g0 g1 g2 =
  tuple ?name ?doc ?meta (fun v0 v1 v2 -> v0, v1, v2)
  |> comp g0 (fun (v, _, _) -> v)
  |> comp g1 (fun (_, v, _) -> v)
  |> comp g2 (fun (_, _, v) -> v)
  |> finish

let t4 ?name ?doc ?meta g0 g1 g2 g3 =
  tuple ?name ?doc ?meta (fun v0 v1 v2 v3 -> v0, v1, v2, v3)
  |> comp g0 (fun (v, _, _, _) -> v)
  |> comp g1 (fun (_, v, _, _) -> v)
  |> comp g2 (fun (_, _, v, _) -> v)
  |> comp g3 (fun (_, _, _, v) -> v)
  |> finish

(* Records *)

module Record = struct
  type 'a t = 'a Typegist__repr.record
end

let record ?doc ?meta name ctor =
  let finish p = Typegist__repr.make ~name ?doc ?meta (Expr.Record p) in
  Product.cons_ctor ctor finish

(* Variants *)

module Variant = struct
  module Case = struct
    type name = Typegist__repr.variant_case_name
    type 'v t = 'v Typegist__repr.variant_case
    let make = Typegist__repr.make_variant_case
    let name = Typegist__repr.variant_case_name
    let doc = Typegist__repr.variant_case_doc
    let product = Typegist__repr.variant_case_product
    let is_inline_record = Typegist__repr.variant_case_is_inline_record
  end
  type 'v t = 'v Typegist__repr.variant
  let make = Typegist__repr.make_variant
  let cases = Typegist__repr.variant_cases
  let case_index = Typegist__repr.variant_case_index
  let case vt v = Iarray.get (cases vt) (case_index vt v)
  let is_polymorphic_variant = Typegist__repr.variant_is_polymorphic_variant
end

let case ?doc name ctor =
  let finish fs = Typegist__repr.make_variant_case ?doc name fs in
  Product.cons_ctor ctor finish

module Variant_like = struct
  type 'v t = 'v Typegist__repr.variant_like =
  | Option : 'a gist -> 'a option t
  | Either : 'a gist * 'b gist -> ('a, 'b) Either.t t
  | Result : 'a gist * 'b gist -> ('a, 'b) result t
  | List : 'a gist -> 'a list t
  | Variant : 'v Variant.t -> 'v t

  let none = Product.Ctor None
  let option_variant a =
    let none = Variant.Case.make "None" none in
    let some = case "Some" Option.some |> comp a Option.get |> finish in
    let case_index = function None -> 0 | Some _ -> 1 in
    Variant.make [none; some] ~case_index

  let either_variant l r =
    let l = case "Left" Either.left |> comp l Either.get_left |> finish in
    let r = case "Right" Either.right |> comp r Either.get_right |> finish in
    let case_index = function Either.Left _ -> 0 | Either.Right _ -> 1 in
    Variant.make [l; r] ~case_index

  let result_variant a b =
    let ok = case "Ok" Result.ok |> comp a Result.get_ok |> finish in
    let err = case "Error" Result.error |> comp b Result.get_error |> finish in
    let case_index = function Ok _ -> 0 | Error _ -> 1 in
    Variant.make [ok; err] ~case_index

  let empty_list = Product.Ctor []
  let list_variant elt =
    let rec variant = lazy begin
      let self = Variant (Lazy.force variant) in
      let self_gist = Typegist__repr.make (Variant_like self) in
      let empty = Variant.Case.make "[]" empty_list in
      let cons =
        case "::" List.cons
        |> comp elt List.hd
        |> comp self_gist List.tl
        |> finish
      in
      let case_index = function [] -> 0 | _ :: _ -> 1 in
      Variant.make [empty; cons] ~case_index
    end
    in
    Lazy.force variant

  let to_variant : type v. v t -> v Variant.t = function
  | Option v -> option_variant v
  | Either (l, r) -> either_variant l r
  | Result (ok, error) -> result_variant ok error
  | List elt -> list_variant elt
  | Variant v -> v
end

let option ?name ?doc ?meta v =
  Typegist__repr.make ?name ?doc ?meta (Variant_like (Option v))

let either ?name ?doc ?meta l r =
  Typegist__repr.make ?name ?doc ?meta (Variant_like (Either (l, r)))

let result ?name ?doc ?meta v e =
  Typegist__repr.make ?name ?doc ?meta (Variant_like (Result (v, e)))

let list ?name ?doc ?meta elt =
  Typegist__repr.make ?name ?doc ?meta (Variant_like (List elt))

let variant ?doc ?meta ?name cases ~case_index =
  let variant = Variant.make cases ~case_index in
  Typegist__repr.make ?name ?doc ?meta (Variant_like (Variant variant))

let variant_of_enum ?doc ?meta ?name enum =
  let cases = List.map (fun (name, v) -> case name v |> finish) enum in
  let case_values = Iarray.of_list (List.map snd enum) in
  let case_index v = match Iarray.find_index (Repr.equal v) case_values with
  | None -> invalid_arg "Incomplete case enumeration" | Some i -> i
  in
  variant ?doc ?meta ?name cases ~case_index

(* Array like *)

module Array_like = struct

  (* Array modules *)

  module type S = Typegist__repr.ARRAY
  type ('elt, 'array) module' = ('elt, 'array) Typegist__repr.array_module

  module String_array_module = struct
    let immutable = true
    include String
    type t = string
    type elt = char
    let set _s _i _v = invalid_arg "strings are immutable"
    let equal _eq_char s0 s1 = String.equal s0 s1
    let compare _cmp_char s0 s1 = String.compare s0 s1
    let pp_applied_name ppf () = Fmt.string ppf "string"
  end

  module Bytes_array_module = struct
    let immutable = false
    include Bytes
    type t = bytes
    type elt = char
    let equal _eq_char s0 s1 = Bytes.equal s0 s1
    let compare _cmp_char s0 s1 = Bytes.compare s0 s1
    let pp_applied_name ppf () = Fmt.string ppf "bytes"
  end

  let array_array_module : type elt. elt gist -> (elt, elt array) module' =
  fun elt ->
    let module Array = struct
      let immutable = false
      include Array
      type t = elt array
      type nonrec elt = elt
      let pp_applied_name ppf () = Name.pp_applied [V elt] ppf "array"
    end
    in
    (module Array)

  let iarray_array_module : type elt. elt gist -> (elt, elt iarray) module' =
  fun elt ->
    let module Array = struct
      let immutable = true
      include Iarray
      type t = elt iarray
      type nonrec elt = elt
      let set _a _i _v = invalid_arg "iarrays are immutable"
      let pp_applied_name ppf () = Name.pp_applied [V elt] ppf "iarray"
    end
    in
    (module Array)


  let pp_bigarray_type elt kind layout ppf typ =
    Fmt.pf ppf "@[<h>(%a,@ %a,@ %a)@ %s@]"
      (pp_type_arg ~single:false) elt
      Typegist__repr.pp_bigarray_kind kind
      Typegist__repr.pp_bigarray_layout layout
      typ

  let bigarray1_array_module :
  type elt a b.
    elt gist -> (elt, a) Bigarray.kind -> b Bigarray.layout ->
    (elt, (elt, a, b) Bigarray.Array1.t) module' =
  fun elt kind layout ->
    let module Array = struct
      let immutable = false
      type t = (elt, a, b) Bigarray.Array1.t
      type nonrec elt = elt
      let get = Bigarray.Array1.get
      let set = Bigarray.Array1.set
      let length = Bigarray.Array1.dim
      let init = Bigarray.Array1.init kind layout
      let iter f a =
        for i = 0 to Bigarray.Array1.dim a - 1
        do f (Bigarray.Array1.unsafe_get a i) done

      let fold_left f init a =
        let acc = Stdlib.ref init in
        for i = 0 to Bigarray.Array1.dim a - 1
        do acc := (f !acc (Bigarray.Array1.unsafe_get a i)) done;
        !acc

      let equal eq a0 a1 =
        let len0 = length a0 and len1 = length a1 in
        if len0 <> len1 then false else
        try
          for i = 0 to len0 - 1
          do if eq (get a0 i) (get a1 i) then () else raise_notrace Exit done;
          true
        with Exit -> false

      let compare cmp a0 a1 =
        let len0 = length a0 and len1 = length a1 in
        let c = Int.compare len0 len1 in
        if c <> 0 then c else
        let rec loop cmp i max a0 a1 =
          if i > max then 0 else
          let c = cmp (get a0 i) (get a1 i) in
          if c <> 0 then c else loop cmp (i + 1) max a0 a1
        in
        loop cmp 0 (len0 - 1) a0 a1

      let pp_applied_name ppf () =
        pp_bigarray_type elt kind layout ppf "Bigarray.Array1.t"
    end
    in
    (module Array)

  (* Array likes *)

  type bytes_encoding = Typegist__repr.bytes_encoding = Binary | Utf_8
  type ('elt, 'array) t = ('elt, 'array) Typegist__repr.array_like =
  | Bytes :  bytes_encoding -> (char, bytes) t
  | String : bytes_encoding -> (char, string) t
  | Array : 'elt gist -> ('elt, 'elt array) t
  | Iarray : 'elt gist -> ('elt, 'elt iarray) t
  | Bigarray1 :
      'elt gist * ('elt, 'b) Bigarray.kind * 'c Bigarray.layout ->
      ('elt, ('elt, 'b, 'c) Bigarray.Array1.t) t
  | Module : 'elt gist * ('elt, 'array) module' -> ('elt, 'array) t

  let elt = Typegist__repr.array_like_elt

  let to_module : type e a. (e, a) Typegist__repr.array_like -> (e, a) module' =
  fun a -> match a with
  | Module (_, m) -> m
  | String (_) -> (module String_array_module)
  | Bytes (_) -> (module Bytes_array_module)
  | Array elt -> array_array_module elt
  | Iarray elt -> iarray_array_module elt
  | Bigarray1 (elt, k, l) -> bigarray1_array_module elt k l
end

let bytes =
  Typegist__repr.make (Array_like (Bytes Binary))

let utf_8_bytes =
  Typegist__repr.make (Array_like (Bytes Utf_8))

let binary_string =
  Typegist__repr.make (Array_like (String Binary))

let utf_8_string =
  Typegist__repr.make (Array_like (String Utf_8))

let array ?name ?doc ?meta g =
  Typegist__repr.make ?name ?doc ?meta (Array_like (Array g))

let iarray ?name ?doc ?meta g =
  Typegist__repr.make ?name ?doc ?meta (Array_like (Iarray g))

let bigarray1 ?name ?doc ?meta g k l =
  Typegist__repr.make ?name ?doc ?meta (Array_like (Bigarray1 (g, k, l)))

let array_module ?name ?doc ?meta elt m =
  Typegist__repr.make ?name ?doc ?meta (Array_like (Module (elt, m)))

let bigbytes = bigarray1 ~name:"bigbytes" int Int8_unsigned C_layout
let floatarray =
  let module Array = struct
    let immutable = false
    include Float.Array
    type t = Float.Array.t
    type nonrec elt = float
    let pp_applied_name ppf () = Fmt.string ppf "floatarray"
  end
  in
  array_module float (module Array)

let dynarray :
type elt. ?name:string -> ?doc:string -> ?meta:elt Dynarray.t Meta.t ->
  elt t -> elt Dynarray.t t =
fun ?name ?doc ?meta elt ->
  let module Array = struct
    let immutable = false
    include Dynarray
    type t = elt Dynarray.t
    type nonrec elt = elt
    let pp_applied_name ppf () = Name.pp_applied [V elt] ppf "Dynarray.t"
  end
  in
  array_module ?name ?doc ?meta elt (module Array)

let weak :
type elt. ?name:string -> ?doc:string -> ?meta:elt Weak.t Meta.t ->
  elt gist -> elt Weak.t t =
fun ?name ?doc ?meta elt ->
  let weak_elt = option elt in
  let module Array = struct
    let immutable = false
    include Weak
    type t = elt Weak.t
    type nonrec elt = elt option
    let init n f =
      let a = create n in for i = 0 to n - 1 do set a i (f i) done; a

    let iter f a = for i = 0 to length a - 1 do f (get a i) done
    let fold_left f init a =
      let acc = Stdlib.ref init in
      for i = 0 to length a - 1
      do acc := (f !acc (get a i)) done;
      !acc

    let equal eq a0 a1 =
      let len0 = length a0 and len1 = length a1 in
      if len0 <> len1 then false else
      try
        for i = 0 to len0 - 1
        do if eq (get a0 i) (get a1 i) then () else raise_notrace Exit done;
        true
      with Exit -> false

    let compare cmp a0 a1 =
      let len0 = length a0 and len1 = length a1 in
      let c = Int.compare len0 len1 in
      if c <> 0 then c else
      let rec loop cmp i max a0 a1 =
        if i > max then 0 else
        let c = cmp (get a0 i) (get a1 i) in
        if c <> 0 then c else loop cmp (i + 1) max a0 a1
      in
      loop cmp 0 (len0 - 1) a0 a1

    let pp_applied_name ppf () = Name.pp_applied [V elt] ppf "Weak.t"
  end
  in
  array_module ?name ?doc ?meta weak_elt (module Array)

(* Map like *)

module Map_like = struct

  (* Hash table modules *)

  module type HASHTBL = Typegist__repr.HASHTBL
  type ('k, 'v, 'h) hashtbl_module = ('k, 'v, 'h) Typegist__repr.hashtbl_module

  (* Map modules *)

  module type MAP = Typegist__repr.MAP
  type ('k, 'v, 'map) map_module = ('k, 'v, 'map) Typegist__repr.map_module

  (* Map like *)

  type ('k, 'v, 'm) t = ('k, 'v, 'm) Typegist__repr.map_like =
  | Hashtbl_module :
      'k gist * 'v gist * ('k, 'v, 'h) hashtbl_module -> ('k, 'v, 'h) t
  | Map_module :
      'k gist * 'v gist * ('k, 'v, 'm) map_module -> ('k, 'v, 'm) t

  let key = Typegist__repr.map_like_key
  let value = Typegist__repr.map_like_value
end

let hashtbl :
type k v.
  ?name:applied_name -> ?doc:string -> ?meta:(k, v) Hashtbl.t Meta.t ->
  k t -> v t -> (k, v) Hashtbl.t t =
fun ?name ?doc ?meta k v ->
  let module Hashtbl = struct
    include Hashtbl
    type nonrec t = (k, v) t
    type key = k
    type value = v
    let create n = create n
    let equal eq_v h0 h1 =
      if Hashtbl.length h0 <> Hashtbl.length h1 then false else
      let rec loop = function
      | Seq.Nil -> true
      | Seq.Cons (k, keys) ->
          let l0 = Hashtbl.find_all h0 k in
          let l1 = Hashtbl.find_all h1 k in
          if List.equal eq_v l0 l1 then loop (keys ()) else false
      in
      loop ((Hashtbl.to_seq_keys h0) ())

    (* This is not so great but the API gives no ordering guarantees (likely
       because of the hash randomization) Also it's highly unclear what users
       expect from a notion of comparison on hashtables. *)

    let compare cmp_k cmp_v h0 h1 =
      let bcmp (k0, _) (k1, _) = cmp_k k0 k1 in (* we keep insert order *)
      let kv0 = List.stable_sort bcmp (List.of_seq (Hashtbl.to_seq h0)) in
      let kv1 = List.stable_sort bcmp (List.of_seq (Hashtbl.to_seq h1)) in
      let cmp_kv (k0, v0) (k1, v1) =
        let cmp = cmp_k k0 k1 in if cmp <> 0 then cmp else cmp_v v0 v1
      in
      List.compare cmp_kv kv0 kv1

    let pp_applied_name ppf () = Name.pp_applied [V k; V v] ppf "Hashtbl.t"
  end
  in
  let hashtbl_module = Map_like.Hashtbl_module (k, v, (module Hashtbl)) in
  Typegist__repr.make ?name ?doc ?meta (Map_like hashtbl_module)

let map :
type v.
  qualified_name -> (module Map : Map.S) -> ?name:string -> ?doc:string ->
  ?meta:v Map.t Meta.t -> Map.key t -> v t  -> v Map.t t =
fun mod_name (module M : Map.S) ?name ?doc ?meta k v ->
  let module Map = struct
    include M
    type nonrec t = v t
    type value = v
    let pp_applied_name ppf () = Name.pp_applied [V v] ppf mod_name
  end
  in
  let map_module = Map_like.Map_module (k, v, (module Map)) in
  Typegist__repr.make ?name ?doc ?meta (Map_like map_module)

let hashtbl_module ?name ?doc ?meta k v h =
  Typegist__repr.make ?name ?doc ?meta (Map_like (Hashtbl_module (k, v, h)))

let map_module ?name ?doc ?meta k v m =
  Typegist__repr.make ?name ?doc ?meta (Map_like (Map_module (k, v, m)))

(* Cells *)

module Cell_like = struct

  (* Cell modules *)

  module type S = Typegist__repr.CELL
  type ('a, 'cell) module' = ('a, 'cell) Typegist__repr.cell_module

  let lazy_cell_module : type a. a gist -> (a, a lazy_t) module' =
  fun contents ->
    let module Cell = struct
      let immutable = true
      type t = a lazy_t
      type contents = a
      let make = Lazy.from_val
      let get c = Lazy.force c
      let set c v = invalid_arg "Lazy values are immutable"
      let pp_applied_name ppf () = Name.pp_applied [V contents] ppf "lazy_t"
    end
    in
    (module Cell)

  let ref_cell_module : type a. a gist -> (a, a ref) module' =
  fun contents ->
    let module Cell = struct
      let immutable = false
      type t = a ref
      type contents = a
      let make v = ref v
      let get c = !c
      let set c v = c := v
      let pp_applied_name ppf () = Name.pp_applied [V contents] ppf "ref"
    end
    in
    (module Cell)

  let atomic_cell_module : type a. a gist -> (a, a Atomic.t) module' =
  fun contents ->
    let module Cell = struct
      let immutable = false
      include Atomic
      type t = a Atomic.t
      type contents = a
      let pp_applied_name ppf () = Name.pp_applied [V contents] ppf "Atomic.t"
    end
    in
    (module Cell)

  (* Cell likes *)

  type ('a, 'cell) t = ('a, 'cell) Typegist__repr.cell_like =
  | Lazy : 'a gist -> ('a, 'a lazy_t) t
  | Ref : 'a gist -> ('a, 'a ref) t
  | Atomic : 'a gist -> ('a, 'a Atomic.t) t
  | Module : 'a gist * ('a, 'cell) module' -> ('a, 'cell) t

  let contents = Typegist__repr.cell_like_contents

  let to_module : type a c. (a, c) Typegist__repr.cell_like -> (a, c) module' =
  fun a -> match a with
  | Module (_, m) -> m
  | Lazy g -> lazy_cell_module g
  | Ref g -> ref_cell_module g
  | Atomic g -> atomic_cell_module g
end

let lazy' ?name ?doc ?meta g =
  Typegist__repr.make ?name ?doc ?meta (Cell_like (Lazy g))

let ref ?name ?doc ?meta g =
  Typegist__repr.make ?name ?doc ?meta (Cell_like (Ref g))

let atomic ?name ?doc ?meta g =
  Typegist__repr.make ?name ?doc ?meta (Cell_like (Atomic g))

let cell_module ?name ?doc ?meta g m =
  Typegist__repr.make ?name ?doc ?meta (Cell_like (Module (g, m)))

(* Functions *)

module Func = struct
  type ('a, 'b) t = ('a, 'b) Typegist__repr.func
  let make = Typegist__repr.make_func
  let domain = Typegist__repr.func_domain
  let range = Typegist__repr.func_range
end

let func ?name ?doc ?meta domain range =
  Typegist__repr.make ?name ?doc ?meta (Func (Func.make ~domain ~range))

let ( @-> ) d r = func d r

(* Abstract types *)

module Abstract = struct
  module Version = struct
    type name = Typegist__repr.abstract_version_name
    type 'a t = 'a Typegist__repr.abstract_version
    type version = Typegist__repr.abstract_version_name
    let make = Typegist__repr.make_abstract_version
    let name = Typegist__repr.abstract_version_name
    let gist = Typegist__repr.abstract_version_gist
  end
  type 'a t = 'a Typegist__repr.abstract
  let make ?version_index versions =
    Typegist__repr.make_abstract ?version_index versions

  let is_opaque = Typegist__repr.abstract_is_opaque
  let versions = Typegist__repr.abstract_versions
  let version_index = Typegist__repr.abstract_version_index
  let version a v = Iarray.get (versions a) (version_index a v)
  let find_version n a =
    Iarray.find_opt (fun v -> Version.name v = n) (versions a)

  let check_non_opaque g a =
    if is_opaque a
    then invalid_arg (Fmt.str "%s: abstract type is opaque" (name g))

  let check_compatible_version_index g a v0 v1 =
    let v0 = version_index a v0 in
    let v1 = version_index a v1 in
    if v0 = v1 then v0 else
    invalid_arg (Fmt.str "%s: incompatible value versions: %s <> %s"
      (name g)
      (Version.name (Iarray.get (versions a) v0))
      (Version.name (Iarray.get (versions a) v1)))

  let get_gist g a v = check_non_opaque g a; Version.gist (version a v)
  let get_binop_gist g a v0 v1 =
    check_non_opaque g a;
    let idx = check_compatible_version_index g a v0 v1 in
    let version = Iarray.get (versions a) idx in
    Version.gist version
end

let abstract ?doc ?meta ?version_index name versions =
  let abstract = Abstract.make ?version_index versions in
  Typegist__repr.make ~name ?doc ?meta (Abstract abstract)

(* Views *)

module View = struct
  type ('a, 'b) t = ('a, 'b) Typegist__repr.view
  let make = Typegist__repr.make_view
  let inject = Typegist__repr.view_inject
  let project = Typegist__repr.view_project
  let gist = Typegist__repr.view_gist
end

let view ?name ?doc ?meta ~inject ~project g =
  let v = View.make ~inject ~project g in
  Typegist__repr.make ?name ?doc ?meta (View v)

(* Recursion *)

let rec' lg = Typegist__repr.make (Rec lg)

(* Stdlib types *)

let complex =
  record "Complex.t" (fun re im -> Complex.{ re; im })
  |> field "re" float (fun c -> c.Complex.re)
  |> field "im" float (fun c -> c.Complex.im)
  |> finish


let genbigarray' elt kind layout = t2 (bigarray1 elt kind layout) (iarray int)

let bigarray2 ?doc ?meta elt kind layout =
  let inject b =
    let dim1 = Bigarray.Array2.dim1 b in
    let dim2 = Bigarray.Array2.dim2 b in
    let g = Bigarray.genarray_of_array2 b in
    let a = Bigarray.reshape_1 g (dim1 * dim2) in
    (dim1, dim2), a
  in
  let project ((dim1, dim2), a) =
    Bigarray.reshape_2 (Bigarray.genarray_of_array1 a) dim1 dim2
  in
  let name =
    let pp_arr = Array_like.pp_bigarray_type elt kind layout in
    Fmt.str "%a" pp_arr "Bigarray.Array2.t"
  in
  view ?doc ?meta ~name ~inject ~project @@
  t2 (t2 int int) (bigarray1 elt kind layout)

let bigarray3 ?doc ?meta elt kind layout =
  let inject b =
    let dim1 = Bigarray.Array3.dim1 b in
    let dim2 = Bigarray.Array3.dim2 b in
    let dim3 = Bigarray.Array3.dim3 b in
    let g = Bigarray.genarray_of_array3 b in
    let a = Bigarray.reshape_1 g (dim1 * dim2 * dim3) in
    (dim1, dim2, dim3), a
  in
  let project ((dim1, dim2, dim3), a) =
    Bigarray.reshape_3 (Bigarray.genarray_of_array1 a) dim1 dim2 dim3
  in
  let name =
    let pp_arr = Array_like.pp_bigarray_type elt kind layout in
    Fmt.str "%a" pp_arr "Bigarray.Array2.t"
  in
  view ?doc ?meta ~name ~inject ~project @@
  t2 (t3 int int int) (bigarray1 elt kind layout)

let bigarraygen ?doc ?meta elt kind layout =
  let inject b =
    let dims = Bigarray.Genarray.dims b in
    let count = Array.fold_left ( * ) 1 dims in
    let a = Bigarray.reshape_1 b count in
    Iarray.of_array dims, a
  in
  let project (dims, a) =
    let dims = Iarray.to_array dims in
    Bigarray.reshape (Bigarray.genarray_of_array1 a) dims
  in
  let name =
    let pp_arr = Array_like.pp_bigarray_type elt kind layout in
    Fmt.str "%a" pp_arr "Bigarray.Array2.t"
  in
  view ?doc ?meta ~name ~inject ~project @@
  t2 (iarray int) (bigarray1 elt kind layout)

let digest = of_gist ~name:"Digest.t" binary_string
let digest_blake128 = of_gist ~name:"Digest.Blake128.t" binary_string
let digest_blake256 = of_gist ~name:"Digest.Blake256.t" binary_string
let digest_blake512 = of_gist ~name:"Digest.Blake512.t" binary_string
let digest_md5 = of_gist ~name:"Digest.MD5.t" binary_string

let float_fp_class =
  let fp_normal = case "FP_normal" FP_normal |> finish in
  let fp_subnormal = case "FP_subnormal" FP_subnormal |> finish in
  let fp_zero = case "FP_zero" FP_subnormal |> finish in
  let fp_infinite = case "FP_infinite" FP_infinite |> finish in
  let fp_nan = case "FP_nan" FP_nan |> finish in
  let case_index = function
  | FP_normal -> 0 | FP_subnormal -> 1 | FP_zero -> 2  | FP_infinite -> 3
  | FP_nan -> 4
  in
  let cases = [fp_normal; fp_subnormal; fp_zero; fp_infinite; fp_nan] in
  variant ~name:"Float.fp_class" cases ~case_index

let gc_stat =
  let make minor_words promoted_words major_words minor_collections
      major_collections heap_words heap_chunks live_words live_blocks
      free_words free_blocks largest_free fragments compactions
      top_heap_words stack_size forced_major_collections
      live_stacks_words =
    { Gc.minor_words; promoted_words; major_words; minor_collections;
      major_collections; heap_words; heap_chunks; live_words;
      live_blocks; free_words; free_blocks; largest_free; fragments;
      compactions; top_heap_words; stack_size;
      forced_major_collections; live_stacks_words}
  in
  record "Gc.stat" make
  |> field "minor_words" float (fun s -> s.Gc.minor_words)
  |> field "promoted_words" float (fun s -> s.Gc.promoted_words)
  |> field "major_words" float (fun s -> s.Gc.major_words)
  |> field "minor_collections" int (fun s -> s.Gc.minor_collections)
  |> field "major_collections" int (fun s -> s.Gc.major_collections)
  |> field "heap_words" int (fun s -> s.Gc.heap_words)
  |> field "heap_chunks" int (fun s -> s.Gc.heap_chunks)
  |> field "live_words" int (fun s -> s.Gc.live_words)
  |> field "live_blocks" int (fun s -> s.Gc.live_blocks)
  |> field "free_words" int (fun s -> s.Gc.free_words)
  |> field "free_blocks" int (fun s -> s.Gc.free_blocks)
  |> field "largest_free" int (fun s -> s.Gc.largest_free)
  |> field "fragments" int (fun s -> s.Gc.fragments)
  |> field "compactions" int (fun s -> s.Gc.compactions)
  |> field "top_heap_words" int (fun s -> s.Gc.top_heap_words)
  |> field "stack_size" int (fun s -> s.Gc.stack_size)
  |> field "forced_major_collections" int
    (fun s -> s.Gc.forced_major_collections)
  |> field "live_stacks_words" int (fun s -> s.Gc.live_stacks_words)
  |> finish

let gc_control =
  let make minor_heap_size major_heap_increment space_overhead verbose
      max_overhead stack_limit allocation_policy window_size
      custom_major_ratio custom_minor_ratio custom_minor_max_size
    =
    { Gc.minor_heap_size; major_heap_increment; space_overhead;
      verbose; max_overhead; stack_limit; allocation_policy; window_size;
      custom_major_ratio; custom_minor_ratio; custom_minor_max_size;}
  in
  record "Gc.control" make
  |> field "minor_heap_size" int (fun c -> c.Gc.minor_heap_size)
  |> field "major_heap_increment" int (fun c -> c.Gc. major_heap_increment)
  |> field "space_overhead" int (fun c -> c.Gc.space_overhead)
  |> field "verbose" int (fun c -> c.Gc. verbose)
  |> field "max_overhead" int (fun c -> c.Gc.max_overhead)
  |> field "stack_limit" int (fun c -> c.Gc.stack_limit)
  |> field "allocation_policy" int (fun c -> c.Gc.allocation_policy)
  |> field "window_size" int (fun c -> c.Gc.window_size)
  |> field "custom_major_ratio" int (fun c -> c.Gc.custom_major_ratio)
  |> field "custom_minor_ratio" int (fun c -> c.Gc.custom_minor_ratio)
  |> field "custom_minor_max_size" int (fun c -> c.Gc.custom_minor_max_size)
  |> finish

(* For Pqueues we only get [fold_unordered], so on injection we take
   elements out and put them back in. *)

let pqueue_max name (module P : Pqueue.Max) ?doc ?meta elt =
  let rec to_list acc q = match P.pop_max q with
  | None -> P.add_iter q List.iter acc; List.rev acc
  | Some v -> to_list (v :: acc) q
  in
  let inject = fun p -> to_list [] p in
  let project = fun l -> P.of_list l in
  view ?doc ?meta ~name ~inject ~project (list elt)

let pqueue_max_poly name (module P : Pqueue.MaxPoly) ?doc ?meta elt =
  let rec to_list acc q = match P.pop_max q with
  | None -> P.add_iter q List.iter acc; List.rev acc
  | Some v -> to_list (v :: acc) q
  in
  let inject = fun p -> to_list [] p in
  let project = fun l -> P.of_list l in
  view ?doc ?meta ~name ~inject ~project (list elt)

let pqueue_min name (module P : Pqueue.Min) ?doc ?meta elt =
  let rec to_list acc q = match P.pop_min q with
  | None -> P.add_iter q List.iter acc; List.rev acc
  | Some v -> to_list (v :: acc) q
  in
  let inject = fun p -> to_list [] p in
  let project = fun l -> P.of_list l in
  view ?doc ?meta ~name ~inject ~project (list elt)

let pqueue_min_poly name (module P : Pqueue.MinPoly) ?doc ?meta elt =
  let rec to_list acc q = match P.pop_min q with
  | None -> P.add_iter q List.iter acc; List.rev acc
  | Some v -> to_list (v :: acc) q
  in
  let inject = fun p -> to_list [] p in
  let project = fun l -> P.of_list l in
  view ?doc ?meta ~name ~inject ~project (list elt)

let queue ?doc ?meta elt =
  let inject q = List.rev (Queue.fold (Fun.flip List.cons) [] q) in
  let project els =
    let q = Queue.create () in
    List.iter (Fun.flip Queue.add q) els; q
  in
  let name = Name.make_applied [V elt] "Queue.t" in
  view ?doc ?meta ~name ~inject ~project (list elt)

let random_state =
  let inject s = Random.State.to_binary_string s in
  let project s = Random.State.of_binary_string s in
  view ~name:"Random.State.t" ~inject ~project binary_string

let set name (module S : Set.S) ?doc ?meta elt =
  let inject = fun s -> S.to_list s in
  let project = fun l -> S.of_list l in
  view ?doc ?meta ~name ~inject ~project (list elt)

let stack ?doc ?meta elt =
  let inject q = List.rev (Stack.fold (Fun.flip List.cons) [] q) in
  let project els =
    let q = Stack.create () in
    List.iter (Fun.flip Stack.push q) els; q
  in
  let name = Name.make_applied [V elt] "Stack.t" in
  view ?doc ?meta ~name ~inject ~project (list elt)

let sys_signal = of_gist ~name:"Sys.signal" int
