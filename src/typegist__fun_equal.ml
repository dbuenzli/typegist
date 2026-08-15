(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let invalid_argf fmt =
  Format.ksprintf invalid_arg ("Fun.Generic.equal: " ^^ fmt)

type 'a eq = 'a -> 'a -> bool

module Equal = struct
  type 'a t = 'a eq
  let ignore _ _ = true
  module V = struct type nonrec ('a, 'b) t = 'b t end
  include Typegist__gist.Meta.Key (V)
end

let rec equal_fields : type p a. (p, a) Typegist__gist.Product.fields -> p eq =
fun fs v0 v1 -> match fs with
| Ctor _ -> true
| App (fs, f) ->
    equal_fields fs v0 v1 &&
    let v0 = Typegist__gist.Field.get f v0 in
    let v1 = Typegist__gist.Field.get f v1 in
    let meta = Typegist__gist.Field.meta f in
    match Equal.find meta with
    | Some equal -> equal v0 v1
    | None ->
        match Typegist__gist.Meta.Ignore.find meta with
        | Some true -> true
        | _ -> equal (Typegist__gist.Field.gist f) v0 v1

and equal_variant_like : type v. v Typegist__gist.Variant_like.t -> v eq =
fun variant v0 v1 -> match variant with
| Option a -> Option.equal (equal a) v0 v1
| Either (l, r) -> Either.equal ~left:(equal l) ~right:(equal r) v0 v1
| Result (o, e) -> Result.equal ~ok:(equal o) ~error:(equal e) v0 v1
| List a -> List.equal (equal a) v0 v1
| Variant v ->
    let v0_case_idx = Typegist__gist.Variant.case_index v v0 in
    let v1_case_idx = Typegist__gist.Variant.case_index v v1 in
    if not (Int.equal v0_case_idx v1_case_idx) then false else
    let case = Iarray.get (Typegist__gist.Variant.cases v) v0_case_idx in
    equal_fields (Typegist__gist.Variant.Case.product case) v0 v1

and equal_array_like :
type elt array. (elt, array) Typegist__gist.Array_like.t -> array eq =
fun a v0 v1 -> match a with
| String _ -> String.equal v0 v1
| Bytes _ -> Bytes.equal v0 v1
| Array g -> Array.equal (equal g) v0 v1
| Iarray g -> Iarray.equal (equal g) v0 v1
| (Bigarray1 _ | Module _) ->
    let (module Array) = Typegist__gist.Array_like.to_module a in
    let elt = Typegist__gist.Array_like.elt a in
    let eq = equal elt in
    Array.equal eq v0 v1

and equal_map_like : type k v m. (k, v, m) Typegist__gist.Map_like.t -> m eq =
fun m v0 v1 -> match m with
| Hashtbl_module (k, v, (module Hashtbl)) -> Hashtbl.equal (equal v) v0 v1
| Map_module (k, v, (module Map)) -> Map.equal (equal v) v0 v1

and equal_cell_like :
type a cell. (a, cell) Typegist__gist.Cell_like.t -> cell eq =
fun c v0 v1 -> match c with
| Lazy g -> equal g (Lazy.force v0) (Lazy.force v1)
| Ref g -> equal g !v0 !v1
| Atomic g -> equal g (Atomic.get v0) (Atomic.get v1)
| Module (g, (module Cell)) -> equal g (Cell.get v0) (Cell.get v1)

and equal_func : type d r. (d, r) Typegist__gist.Func.t -> (d -> r) eq =
fun f _ _ -> invalid_argf "functional value"

and equal_abstract :
type a. a Typegist__gist.t -> a Typegist__gist.Abstract.t -> a eq =
fun g a v0 v1 ->
  equal (Typegist__gist.Abstract.get_binop_gist g a v0 v1) v0 v1

and equal_view : type a b. (a, b) Typegist__gist.View.t -> a eq =
fun w v0 v1 ->
  let inject = Typegist__gist.View.inject w in
  equal (Typegist__gist.View.gist w) (inject v0) (inject v1)

and equal : type a. a Typegist__gist.t -> a eq = fun g v0 v1 ->
  let meta = Typegist__gist.meta g in
  match Equal.find meta with
  | Some equal -> equal v0 v1
  | None ->
      match Typegist__gist.Meta.Ignore.find meta with
      | Some true -> true
      | _ ->
          match Typegist__gist.expr g with
          | Scalar s -> Typegist__gist.Scalar.equal s v0 v1
          | Tuple p -> equal_fields p v0 v1
          | Record r -> equal_fields r v0 v1
          | Variant_like vl -> equal_variant_like vl v0 v1
          | Array_like a -> equal_array_like a v0 v1
          | Map_like m -> equal_map_like m v0 v1
          | Cell_like c -> equal_cell_like c v0 v1
          | Func f -> equal_func f v0 v1
          | Abstract a -> equal_abstract g a v0 v1
          | View w -> equal_view w v0 v1
          | Rec (lazy g) -> equal g v0 v1
