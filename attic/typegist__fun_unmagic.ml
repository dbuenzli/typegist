(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a unmagic = Obj.t -> ('a, string) result

module Unmagic = struct
  type 'a t = 'a unmagic
  module V = struct type nonrec ('a, 'b) t = 'b unmagic end
  include Typegist__gist.Meta.Key (V)
end

let is_float_gist : type a. a Typegist__gist.t -> bool =
fun g -> match (Typegist__gist.expr g) with
| Scalar Float -> true | _ -> false

let ret obj = Ok (Obj.obj obj)

let rec unmagic_fields :
type p a. (p, a) Typegist__gist.Product.fields -> p unmagic =
fun fs obj -> match fs with
| Ctor _ -> ret obj
| App (fs, f) -> failwith "TODO"

and unmagic_scalar : type s b. s Typegist__gist.Scalar.t -> s unmagic =
fun s obj -> failwith "TODO"

and unmagic_tuple : type t b. t Typegist__gist.Tuple.t -> t unmagic =
fun t obj ->
  if Obj.tag obj = Obj.double_array_tag then failwith "TODO" else
  if Obj.tag obj = 0 then failwith "TODO" else
  Error "Unmatched tuple value"

and unmagic_record : type r b. r Typegist__gist.Record.t -> r unmagic =
fun r obj ->
  if Obj.tag obj = Obj.double_array_tag then failwith "TODO" else
  if Obj.tag obj = 0 then failwith "TODO" else
  Error "Unmatched record value"

and unmagic_variant_like :
type v. v Typegist__gist.Variant_like.t -> v unmagic =
fun v obj -> failwith "TODO"

and unmagic_array_like :
type elt a b. (elt, a) Typegist__gist.Array_like.t -> a unmagic =
fun a obj -> match a with
| Bytes _ | String _ ->
    if Obj.tag obj = Obj.string_tag
    then ret obj
    else Error "Unmatched string value"
| Module (g, _) | Array g | Iarray g ->
    if Obj.tag obj = Obj.double_array_tag && is_float_gist g then ret obj else
    if Obj.tag obj = 0 then
      let rec loop obj i max =
        if i > max then ret obj else
        let v = Obj.field obj i in
        match unmagic g v with
        | Ok _ -> loop obj (i + 1) max
        | Error e -> Error ("Unmatched array element: " ^ e)
      in
      loop obj 0 (Obj.size obj - 1)
    else
    Error "Unmatched array value"
| Bigarray1 (_g, _k, _l) ->
    Error "Don't know how to unmagic bigarrays"

and unmagic_map_like :
type k v map b. (k, v, map) Typegist__gist.Map_like.t -> map unmagic =
fun m obj -> match m with
| Hashtbl_module (k, v, (module Hashtbl)) -> failwith "TODO"
| Map_module (k, v, (module Map)) -> failwith "TODO"

and unmagic_cell_like :
type a cell. (a, cell) Typegist__gist.Cell_like.t -> cell unmagic =
fun c obj -> match c with
| Lazy g -> failwith "TODO"
| Ref g -> failwith "TODO"
| Atomic g -> failwith "TODO"
| Module (g, (module Cell)) -> failwith "TODO"

and unmagic_func :
type a b. (a, b) Typegist__gist.Func.t -> (a -> b) unmagic =
fun f obj -> Error "Don't know how to unmagic functional value"

and unmagic_abstract : type a. a Typegist__gist.Abstract.t -> a unmagic =
fun a obj ->
  if Typegist__gist.Abstract.is_opaque a
  then Error "Cannot unmagic opaque abstract value"
  else
  let versions = Typegist__gist.Abstract.versions a in
  (* Returns the first one that unmagics, starting with the latest version *)
  let rec loop versions i obj =
    if i < 0 then Error "Could not unmagic abstract value" else
    let version = Iarray.get versions i in
    match unmagic (Typegist__gist.Abstract.Version.gist version) obj with
    | Ok _ as v -> v
    | Error _ -> loop versions (i - 1) obj
  in
  loop versions (Iarray.length versions - 1) obj

and unmagic_view : type a b. (a, b) Typegist__gist.View.t -> a unmagic =
fun w obj -> match unmagic (Typegist__gist.View.gist w) obj with
| Error _ as e -> e
| Ok v -> Ok (Typegist__gist.View.project w v)

and unmagic : type a. a Typegist__gist.t -> a unmagic =
fun g obj ->
  let meta = Typegist__gist.meta g in
  match Unmagic.find meta with
  | Some unmagic -> unmagic obj
  | None ->
      match Typegist__gist.expr g with
      | Scalar s -> unmagic_scalar s obj
      | Tuple p -> unmagic_tuple p obj
      | Record r -> unmagic_record r obj
      | Variant_like variant -> unmagic_variant_like variant obj
      | Array_like a -> unmagic_array_like a obj
      | Map_like m -> unmagic_map_like m obj
      | Cell_like c -> unmagic_cell_like c obj
      | Func f -> unmagic_func f obj
      | Abstract a -> unmagic_abstract a obj
      | View w -> unmagic_view w obj
      | Rec (lazy g) -> unmagic g obj

let unmagic g v = unmagic g (Obj.repr v)
let safer_unmarshal ?(first = 0) g s = unmagic g (Marshal.from_string s first)
