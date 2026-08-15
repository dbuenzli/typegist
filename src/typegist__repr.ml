(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Fmt = struct
  type 'a t = Format.formatter -> 'a -> unit
  let str = Format.asprintf
  let pf = Format.fprintf
  let string = Format.pp_print_string
  let char = Format.pp_print_char
  let iter = Format.pp_print_iter
  let list = Format.pp_print_list
  let sp = Format.pp_print_space
end

(* Names *)

type name = string
type qualified_name = string
type applied_name = string (* XXX should we make that structured ? *)
type variant_case_name = name
type field_name = name
type abstract_version_name = string

let name_unqualified n =
  let dot_or_sp c = Char.equal '.' c || Char.equal ' ' c in
  match String.find_last_index dot_or_sp n with
  | None -> n | Some i -> String.drop_first (i + 1) n

let name_module_path n = match String.find_last_index (Char.equal '.') n with
| None -> ""
| Some i ->
    let last = i - 1 in
    let first = match String.find_last_index (Char.equal ' ') n with
    | None -> 0 | Some i -> i + 1
    in
    String.sub n first (last - first + 1)

let name_type_args n =
  let rpar_or_sp c = Char.equal ')' c || Char.equal ' ' c in
  match String.find_last_index rpar_or_sp n with
  | None -> ""
  | Some i ->
      let last = if n.[i] = ')' then i else i - 1 in
      String.sub n 0 (last + 1)

(* Interfaces *)

module type ARRAY = sig
  val immutable : bool
  type t
  type elt
  val get : t -> int -> elt
  val set : t -> int -> elt -> unit
  val length : t -> int
  val init : int -> (int -> elt) -> t
  val iter : (elt -> unit) -> t -> unit
  val fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val equal : (elt -> elt -> bool) -> t -> t -> bool
  val compare : (elt -> elt -> int) -> t -> t -> int
  val pp_applied_name : Format.formatter -> unit -> unit
end

module type HASHTBL = sig
  type t
  type key
  type value
  val create : int -> t
  val mem : t -> key -> bool
  val add : t -> key -> value -> unit
  val remove : t -> key -> unit
  val find_opt : t -> key -> value option
  val fold : (key -> value -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val equal : (value -> value -> bool) -> t -> t -> bool
  val compare : (key -> key -> int) -> (value -> value -> int) -> t -> t -> int
  val pp_applied_name : Format.formatter -> unit -> unit
end

module type MAP = sig
  type t
  type key
  type value
  val empty : t
  val mem : key -> t -> bool
  val add : key -> value -> t -> t
  val remove : key -> t -> t
  val find_opt : key -> t -> value option
  val fold : (key -> value -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val equal : (value -> value -> bool) -> t -> t -> bool
  val compare : (value -> value -> int) -> t -> t -> int
  val pp_applied_name : Format.formatter -> unit -> unit
end

module type CELL = sig
  val immutable : bool
  type t
  type contents
  val make : contents -> t
  val get : t -> contents
  val set : t -> contents -> unit
  val pp_applied_name : Format.formatter -> unit -> unit
end

(* Module types *)

type ('elt, 'array) array_module =
  (module ARRAY with type t = 'array and type elt = 'elt)

type ('k, 'v, 'hashtbl) hashtbl_module =
  (module HASHTBL with type key = 'k and type value = 'v and type t = 'hashtbl)

type ('k, 'v, 'map) map_module =
  (module MAP with type key = 'k and type value = 'v and type t = 'map)

type ('a, 'cell) cell_module =
  (module CELL with type contents = 'a and type t = 'cell)

(* Gists *)

type bytes_encoding = Binary | Utf_8

type 'a scalar =
| Unit : unit scalar
| Bool : bool scalar
| Char : char scalar
| Uchar : Uchar.t scalar
| Int : int scalar
| Int32 : int32 scalar
| Int64 : int64 scalar
| Nativeint : nativeint scalar
| Float : float scalar

type ('p, 'v) field =
{ name : field_name;
  doc : string;
  meta : ('p, 'v) Typegist__meta.t2;
  gist : 'v t;
  get : ('p -> 'v);
  set : ('p -> 'v -> unit) option;
  iset : ('p -> 'v -> 'p) option; }

and ('t, _) fields =
| Ctor : 'ctor -> ('t, 'ctor) fields
| App : ('t, 'v -> 'ctor) fields * ('t, 'v) field -> ('t, 'ctor) fields

and 'p product = ('p, 'p) fields
and 't tuple = 't product
and 'r record = 'r product
and 'v variant_case =
  { name : variant_case_name;
    doc : string;
    product : ('v, 'v) fields }

and 'v variant = { cases : 'v variant_case iarray; case_index : 'v -> int; }
and 'v variant_like =
| Option : 'a t -> 'a option variant_like
| Either : 'a t * 'b t -> ('a, 'b) Either.t variant_like
| Result : 'a t * 'b t -> ('a, 'b) result variant_like
| List : 'a t -> 'a list variant_like
| Variant : 'v variant -> 'v variant_like

and ('elt, 'array) array_like =
| Bytes :  bytes_encoding -> (char, bytes) array_like
| String : bytes_encoding -> (char, string) array_like
| Array : 'elt t -> ('elt, 'elt array) array_like
| Iarray : 'elt t -> ('elt, 'elt iarray) array_like
| Bigarray1 :
    'elt t * ('elt, 'b) Bigarray.kind * 'c Bigarray.layout ->
    ('elt, ('elt, 'b, 'c) Bigarray.Array1.t) array_like
| Module : 'elt t * ('elt, 'array) array_module -> ('elt, 'array) array_like

and ('k, 'v, 'map) map_like =
| Hashtbl_module :
    'k t * 'v t * ('k, 'v, 'h) hashtbl_module -> ('k, 'v, 'h) map_like
| Map_module :
    'k t * 'v t * ('k, 'v, 'map) map_module -> ('k, 'v, 'map) map_like

and ('a, 'cell) cell_like =
| Lazy : 'a t -> ('a, 'a lazy_t) cell_like
| Ref : 'a t -> ('a, 'a ref) cell_like
| Atomic : 'a t -> ('a, 'a Atomic.t) cell_like
| Module : 'a t * ('a, 'cell) cell_module -> ('a, 'cell) cell_like

and ('a, 'b) func = { domain : 'a t; range : 'b t }
and 'a abstract_version = { name : abstract_version_name; gist : 'a t }
and 'a abstract =
  { versions : 'a abstract_version iarray; version_index : 'a -> int }

and ('a, 'b) view = { inject : 'a -> 'b; project : 'b -> 'a; gist : 'b t; }
and 'a expr =
| Scalar : 'a scalar -> 'a expr
| Tuple : 't tuple -> 't expr
| Record : 'r record -> 'r expr
| Variant_like : 's variant_like -> 's expr
| Array_like : ('elt, 'array) array_like -> 'array expr
| Map_like : ('k, 'v, 'm) map_like -> 'm expr
| Cell_like : ('a, 'cell) cell_like -> 'cell expr
| Func : ('a, 'b) func -> ('a -> 'b) expr
| Abstract : 'a abstract -> 'a expr
| View : ('a, 'b) view -> 'a expr
| Rec : 'a t lazy_t -> 'a expr

and 'a t =
{ name : applied_name;
  doc : string;
  id : 'a Type.Id.t;
  meta : 'a Typegist__meta.t;
  expr : 'a expr }

type v = V : 'a t -> v

let make ?(name = "") ?(doc = "") ?id ?(meta = Typegist__meta.empty) expr =
  let id = match id with None -> Type.Id.make () | Some id -> id in
  { name; doc; id; meta; expr }

let name (g : _ t) = g.name
let doc (g : _ t) = g.doc
let id (g : _ t) = g.id
let id_int (g : _ t) = Type.Id.uid g.id
let meta (g : _ t) = g.meta
let expr (g : _ t) = g.expr
let is_nameless (g : _ t) = String.is_empty g.name

(* Scalars *)

let scalar_applied_name : type a. a scalar -> applied_name = function
| Unit -> "unit" | Bool -> "bool" | Char -> "char"
| Uchar -> "Uchar.t" | Int -> "int" | Int32 -> "int32"
| Int64 -> "int64" | Nativeint -> "nativeint" | Float -> "float"

let char = make (Scalar Char)

(* Fields *)

let make_field
    ?(name = "") ?(doc = "") ?(meta = Typegist__meta.empty) ?set ?iset gist get
  =
  { name; doc; meta; gist; get; set; iset }

let field_meta (f : (_,_) field) = f.meta
let field_name (f : (_,_) field) = f.name
let field_doc (f : (_,_) field) = f.doc
let field_gist (f : (_,_) field) = f.gist
let field_get (f : (_,_) field) = f.get
let field_set (f : (_,_) field) = f.set
let field_iset (f : (_,_) field) = f.iset
let field_is_mutable (f : (_,_) field) = Option.is_some f.set

(* Products *)

let product_is_empty = function Ctor _ -> true | _ -> false
let product_is_singleton = function App (Ctor _, _) -> true | _ -> false
let rec product_fold_fields :
type p a. ('v. 'acc -> (p, 'v) field -> 'acc) -> 'acc -> (p, a) fields -> 'acc =
fun f acc fs -> match fs with
| Ctor _ -> acc
| App (fs, field) -> f (product_fold_fields f acc fs) field

let product_field_count p = product_fold_fields (fun acc _ -> acc + 1) 0 p

(* Variants *)

let err_invalid_case_name n = Fmt.str "%S: not a valid variant case name" n
let is_valid_case_name n = not (String.is_empty n) (* more could be done *)
let is_polymorphic_variant_case_name n = n.[0] = '`'

let make_variant_case ?(doc = "") name product =
  if is_valid_case_name name then {name; doc; product} else
  invalid_arg (err_invalid_case_name name)

let variant_case_name (c : _ variant_case) = c.name
let variant_case_doc (c : _ variant_case) = c.doc
let variant_case_product (c : _ variant_case) = c.product
let variant_case_is_inline_record (c : _ variant_case) = match c.product with
| Ctor _ -> false | App (_, f) -> not (String.is_empty (field_name f))

let make_variant cases ~case_index = {cases = Iarray.of_list cases; case_index}
let variant_cases (v : _ variant) = v.cases
let variant_case_index (v : _ variant) = v.case_index
let variant_is_polymorphic_variant (v : _ variant) =
  if Iarray.length v.cases = 0 then false else
  is_polymorphic_variant_case_name (Iarray.get v.cases 0).name

(* Array likes *)

let array_like_elt : type elt array. (elt, array) array_like -> elt t =
function
| Bytes _ -> char | String _ -> char | Array elt -> elt | Iarray elt -> elt
| Bigarray1 (elt, _, _) -> elt | Module (elt, _) -> elt

(* Map likes *)

let map_like_key : type k v m. (k, v, m) map_like -> k t = function
| Hashtbl_module (k, _, _) | Map_module (k, _, _) -> k

let map_like_value : type k v m. (k, v, m) map_like -> v t = function
| Hashtbl_module (_, v, _) | Map_module (_, v, _) -> v

(* Cell likes *)

let cell_like_contents : type a cell. (a, cell) cell_like -> a t = function
| Lazy g -> g | Ref g -> g | Atomic g -> g | Module (g, _) -> g

(* Functions *)

let make_func ~domain ~range = { domain; range }
let func_domain (f : (_, _) func) = f.domain
let func_range (f : (_, _) func) = f.range

(* Abstract types *)

let make_abstract_version ~name gist = { name; gist }
let abstract_version_name (v : _ abstract_version) = v.name
let abstract_version_gist (v : _ abstract_version) = v.gist
let make_abstract ?version_index versions =
  let versions = Iarray.of_list versions in
  let version_index = match version_index with
  | None -> Fun.const (Iarray.length versions - 1) | Some vidx -> vidx
  in
  { versions; version_index }

let abstract_is_opaque a = Iarray.length a.versions = 0
let abstract_versions a = a.versions
let abstract_version_index a =
  if abstract_is_opaque a then invalid_arg "abstract type is opaque" else
  a.version_index

(* Views *)

let make_view ~inject ~project gist = { inject; project; gist }
let view_inject (v : (_,_) view) = v.inject
let view_project (v : (_,_) view) = v.project
let view_gist (v : (_,_) view) = v.gist

(* Formatting *)

let pp_bigarray_layout (type a) ppf (l : a Bigarray.layout) = match l with
| Bigarray.C_layout -> Fmt.string ppf "Bigarray.c_layout"
| Bigarray.Fortran_layout -> Fmt.string ppf "Bigarray.fortran_layout"

let pp_bigarray_kind (type a b) ppf (k : (a, b) Bigarray.kind) = match k with
| Float32 -> Fmt.string ppf "Bigarray.float32_elt"
| Float64 -> Fmt.string ppf "Bigarray.float64_elt"
| Int8_signed -> Fmt.string ppf "Bigarray.int8_signed_elt"
| Int8_unsigned -> Fmt.string ppf "Bigarray.int8_unsigned_elt"
| Int16_signed -> Fmt.string ppf "Bigarray.int16_signed_elt"
| Int16_unsigned -> Fmt.string ppf "Bigarray.int16_unsigned_elt"
| Int32 -> Fmt.string ppf "Bigarray.int32_elt"
| Int64 -> Fmt.string ppf "Bigarray.int64_elt"
| Int -> Fmt.string ppf "Bigarray.int_elt"
| Nativeint -> Fmt.string ppf "Bigarray.nativeint_elt"
| Complex32 -> Fmt.string ppf "Bigarray.complext32_elt"
| Complex64 -> Fmt.string ppf "Bigarray.complext64_elt"
| Char -> Fmt.string ppf "Bigarray.int8_unsigned_elt"
| Float16 -> Fmt.string ppf "Bigarray.float16_elt"

let rec needs_parens : type a. a expr -> bool = function
| Scalar s -> false
| Tuple t -> true
| Record t -> false
| Variant_like t -> false
| Array_like (String _ | Bytes _) -> false
| Array_like _ -> true
| Map_like _ -> true
| Cell_like _ -> true
| Func _ -> true
| Abstract _ -> false
| View v -> needs_parens (expr (view_gist v))
| Rec (lazy g) -> needs_parens (expr g)

let rec pp_type_arg : type a. single:bool -> a t Fmt.t =
fun ~single ppf g -> if single then pp_gist ppf g else pp_gist_no_parens ppf g

and pp_gist : type a. a t Fmt.t =
fun ppf g ->
if not (is_nameless g) then Fmt.string ppf (name g) else
  let needs_parens = needs_parens (expr g) in
  if needs_parens
  then Fmt.pf ppf "@[<1>(%a)@]" pp_expr (expr g)
  else Fmt.pf ppf "@[%a@]" pp_expr (expr g)

and pp_gist_no_parens : type a. a t Fmt.t =
fun ppf g ->
  if not (is_nameless g) then Fmt.string ppf (name g) else
  Fmt.pf ppf "@[%a@]" pp_expr (expr g)

and pp_applied_name args ppf n = match args with
| [] -> Fmt.string ppf n
| [V g] -> Fmt.pf ppf "@[<h>%a %s@]" (pp_type_arg ~single:true) g n
| args ->
    let pp_sep ppf () = Fmt.string ppf ", " in
    let pp_arg ppf (V g) = pp_type_arg ~single:false ppf g in
    Fmt.pf ppf "@[<h1>(%a) %s@]" (Fmt.list ~pp_sep pp_arg) args n

and pp_tuple : type t ctor. (t, ctor) fields Fmt.t =
fun ppf fs -> match fs with
| Ctor _ -> ()
| App (Ctor _, f) -> pp_gist ppf (field_gist f)
| App (App _ as fs, f) ->
    pp_tuple ppf fs; Fmt.pf ppf " *@ %a" pp_gist (field_gist f)

and pp_record_field : type r v. (r, v) field Fmt.t =
fun ppf f ->
  let mut = if field_is_mutable f then "mutable " else "" in
  Fmt.pf ppf "@[%s%s :@ @[%a@]@]"
    mut (field_name f) pp_gist_no_parens (field_gist f)

and pp_record_fields : type r ctor. (r, ctor) fields Fmt.t =
fun ppf fs -> match fs with
| Ctor _ -> ()
| App (Ctor _, f) -> pp_record_field ppf f
| App (fs, f) ->
    pp_record_fields ppf fs; Fmt.pf ppf ";@,"; pp_record_field ppf f

and pp_record : type r. r record Fmt.t =
fun ppf fs -> Fmt.pf ppf "@[<v2>{ %a@] }" pp_record_fields fs

and pp_variant_case : type v. v variant_case Fmt.t =
fun ppf c ->
  let pp_product =
    if variant_case_is_inline_record c then pp_record else pp_tuple
  in
  let p = variant_case_product c in
  let is_empty = product_is_empty p in
  let name = variant_case_name c in
  if is_empty
  then Fmt.pf ppf "%s" name
  else Fmt.pf ppf "@[%s of@ @[%a@]@]" name pp_product p

and pp_variant : type v. v variant Fmt.t =
fun ppf v ->
  let is_poly = variant_is_polymorphic_variant v in
  let pp_cases ppf v =
    let pp_sep ppf () = Fmt.pf ppf "@ |@ " in
    Fmt.iter ~pp_sep Iarray.iter pp_variant_case ppf (variant_cases v);
  in
  if is_poly
  then Fmt.pf ppf "@[[ %a ]@]" pp_cases v
  else Fmt.pf ppf "@[| %a@]" pp_cases v

and pp_variant_like : type v. v variant_like Fmt.t =
fun ppf v -> match v with
| Variant v -> pp_variant ppf v
| Option g -> pp_applied_name [V g] ppf "option"
| Either (l, r) -> pp_applied_name [V l; V r] ppf "Either.t"
| Result (ok, error) -> pp_applied_name [V ok; V error] ppf "result"
| List elt -> pp_applied_name [V elt] ppf "list"

and pp_array_like : type elt array. (elt, array) array_like Fmt.t =
fun ppf a -> match a with
| String _ -> Fmt.string ppf "string"
| Bytes _ -> Fmt.string ppf "bytes"
| Array elt -> pp_applied_name [V elt] ppf "array"
| Iarray elt -> pp_applied_name [V elt] ppf "iarray"
| Bigarray1 (elt, k, l) ->
    Fmt.pf ppf "@[@[<1>(%a,@ %a,@ %a)@]@ Bigarray.Array1.t@]"
      pp_gist_no_parens elt pp_bigarray_kind k pp_bigarray_layout l
| Module (elt, (module Array)) -> Array.pp_applied_name ppf ()

and pp_map_like : type k v map. (k, v, map) map_like Fmt.t =
fun ppf m -> match m with
| Hashtbl_module (_, _, (module Hashtbl)) -> Hashtbl.pp_applied_name ppf ()
| Map_module (_, _, (module Map)) -> Map.pp_applied_name ppf ()

and pp_cell_like : type a cell. (a, cell) cell_like Fmt.t =
fun ppf c -> match c with
| Lazy g -> pp_applied_name [V g] ppf "lazy_t"
| Ref g -> pp_applied_name [V g] ppf "ref"
| Atomic g -> pp_applied_name [V g] ppf "Atomic.t"
| Module (g, (module Cell)) -> Cell.pp_applied_name ppf ()

and pp_func : type a b. (a, b) func Fmt.t =
fun ppf func ->
  Fmt.pf ppf "@[%a ->@ %a@]"
    pp_gist (func_domain func) pp_gist (func_range func)

and pp_rec : type a. a t Fmt.t = fun ppf g ->
  if is_nameless g then Fmt.string ppf "<rec>" else
  Fmt.string ppf (name_unqualified (name g))

and pp_expr : type a. a expr Fmt.t = fun ppf e -> match e with
| Scalar s -> Fmt.string ppf (scalar_applied_name s)
| Tuple t -> pp_tuple ppf t
| Record r -> pp_record ppf r
| Variant_like v -> pp_variant_like ppf v
| Array_like a -> pp_array_like ppf a
| Map_like m -> pp_map_like ppf m
| Cell_like c -> pp_cell_like ppf c
| Func f -> pp_func ppf f
| Abstract a -> Fmt.string ppf "<abstr>"
| View v -> pp_expr ppf (expr (view_gist v))
| Rec (lazy g) -> pp_rec ppf g

let pp_def ppf g = match name g with
| "" -> Fmt.pf ppf "@[%a@]" pp_expr (expr g)
| n -> Fmt.pf ppf "@[type %s =@ @[%a@]@]" n pp_expr (expr g)


(* expr_map_gists *)

let rec gist_map_gists : type b. ('a. 'a t -> 'a t) -> b t -> b t =
fun f g -> let g' = f g in { g' with expr = expr_map_gists f (expr g') }

and fields_map_gists :
type p c. ('a. 'a t -> 'a t) -> (p, c) fields ->  (p, c) fields =
fun f fs -> match fs with
| Ctor _ as ctor -> ctor
| App (fs, field) ->
    let fs = fields_map_gists f fs in
    App (fs, { field with gist = gist_map_gists f field.gist })

and expr_map_gists : type b. ('a. 'a t -> 'a t) -> b expr -> b expr =
fun f e -> match e with
| Scalar _ as expr -> expr
| Tuple fs -> Tuple (fields_map_gists f fs)
| Record fs -> Record (fields_map_gists f fs)
| Variant_like v ->
    begin match v with
    | Option g ->
        Variant_like (Option (gist_map_gists f g))
    | Either (el, er) ->
        Variant_like (Either (gist_map_gists f el, gist_map_gists f er))
    | Result (v, e) ->
        Variant_like (Result (gist_map_gists f v, gist_map_gists f e))
    | List g ->
        Variant_like (List (gist_map_gists f g))
    | Variant v ->
        let case_map_gists case =
          { case with product = fields_map_gists f case.product }
        in
        let v = { v with cases = Iarray.map case_map_gists v.cases } in
        Variant_like (Variant v)
    end
| Array_like a as expr ->
    begin match a with
    | String _ | Bytes _ -> expr
    | Array g -> Array_like (Array (gist_map_gists f g))
    | Iarray g -> Array_like (Iarray (gist_map_gists f g))
    | Bigarray1 (g, k, l) -> Array_like (Bigarray1 (gist_map_gists f g, k, l))
    | Module (g, m) -> Array_like (Module (gist_map_gists f g, m))
    end
| Map_like m ->
    begin match m with
    | Hashtbl_module (k, v, m) ->
        Map_like (Hashtbl_module (gist_map_gists f k, gist_map_gists f v, m))
    | Map_module (k, v, m) ->
        Map_like (Map_module (gist_map_gists f k, gist_map_gists f v, m))
    end
| Cell_like c ->
    begin match c with
    | Lazy g -> Cell_like (Lazy (gist_map_gists f g))
    | Ref g -> Cell_like (Ref (gist_map_gists f g))
    | Atomic g -> Cell_like (Atomic (gist_map_gists f g))
    | Module (g, m) -> Cell_like (Module (gist_map_gists f g, m))
    end
| Func func ->
    Func { domain = gist_map_gists f func.domain;
           range = gist_map_gists f func.range }
| Abstract a ->
    let version_map_gists (version : _ abstract_version) =
      { version with gist = gist_map_gists f version.gist }
    in
    let versions = Iarray.map version_map_gists a.versions in
    Abstract { a with versions }
| View v -> View { v with gist = gist_map_gists f v.gist }
| Rec (lazy g) -> Rec (lazy (gist_map_gists f g))

let rebind r g =
  let rebind : type a b. a t -> b t -> b t = fun r g ->
    match Type.Id.provably_equal (id r) (id g) with
    | Some Type.Equal -> r | None -> g
  in
  gist_map_gists (fun g -> rebind r g) g

let replace g ~by g' =
  let replace : type a b. a t -> by:a t -> b t -> b t = fun g ~by g' ->
    match Type.Id.provably_equal (id g) (id g') with
    | Some Type.Equal -> by | None -> g'
  in
  gist_map_gists (fun g' -> replace g ~by g') g'

(* expr_fold_gists *)

module Iset = Set.Make (Int)

let rec gist_fold_gists :
type acc b. ('a. acc -> 'a t -> acc) -> Iset.t * acc -> b t -> Iset.t * acc =
fun f (seen, acc as ret) g ->
  let gid = id_int g in
  if Iset.mem gid seen then ret else
  expr_fold_gists f (Iset.add gid seen, f acc g) (expr g)

and fields_fold_gists :
type acc p c.
  ('a. acc -> 'a t -> acc) -> Iset.t * acc -> (p, c) fields -> Iset.t * acc =
fun f acc fs -> match fs with
| Ctor _ -> acc
| App (fs, field) -> gist_fold_gists f (fields_fold_gists f acc fs) field.gist

and expr_fold_gists :
type acc b. ('a. acc -> 'a t -> acc) -> Iset.t * acc -> b expr -> Iset.t * acc =
fun f acc expr -> match expr with
| Scalar _ -> acc
| Tuple fs -> fields_fold_gists f acc fs
| Record fs -> fields_fold_gists f acc fs
| Variant_like v ->
    begin match v with
    | Option g -> gist_fold_gists f acc g
    | Either (lg, rg) -> gist_fold_gists f (gist_fold_gists f acc lg) rg
    | Result (og, eg) -> gist_fold_gists f (gist_fold_gists f acc og) eg
    | List elt -> gist_fold_gists f acc elt
    | Variant v ->
        let fold_case acc (c : _ variant_case) =
          fields_fold_gists f acc c.product
        in
        Iarray.fold_left fold_case acc v.cases
    end
| Array_like a -> gist_fold_gists f acc (array_like_elt a)
| Map_like m ->
    gist_fold_gists f (gist_fold_gists f acc (map_like_key m))
      (map_like_value m)
| Cell_like c -> gist_fold_gists f acc (cell_like_contents c)
| Func func -> gist_fold_gists f (gist_fold_gists f acc func.domain) func.range
| Abstract a ->
    let fold_version acc (v : _ abstract_version) =
      gist_fold_gists f acc v.gist
    in
    Iarray.fold_left fold_version acc a.versions
| View v -> gist_fold_gists f acc v.gist
| Rec (lazy g) -> gist_fold_gists f acc g

let expr_fold_gists :
type acc b. ('a. acc -> 'a t -> acc) -> acc -> b expr -> acc =
fun f acc e -> snd (expr_fold_gists f (Iset.empty, acc) e)

let has_gist sub g =
  try
    let subid = id_int sub in
    let f () g = if Int.equal subid (id_int g) then raise Exit else () in
    expr_fold_gists f () (expr g); false
  with
  | Exit -> true

let is_recursive g = has_gist g g

let update ?name ?doc ?meta ?expr (g : 'a t) =
  let name = Option.value ~default:g.name name in
  let doc = Option.value ~default:g.doc doc in
  let meta = Option.value ~default:g.meta meta in
  let expr = Option.value ~default:g.expr expr in
  let g' = { g with name; doc; meta; expr } in
  (* Make sure recursive occurences are updated *)
  rebind g' g'

let of_gist ?name ?doc ?meta ?expr (g : 'a t) =
  let name = Option.value ~default:g.name name in
  let doc = Option.value ~default:g.doc doc in
  let meta = Option.value ~default:g.meta meta in
  let expr = Option.value ~default:g.expr expr in
  make ~name ~doc ~meta expr
