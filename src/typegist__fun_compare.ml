(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let invalid_argf fmt =
  Format.ksprintf invalid_arg ("Fun.Generic.compare: " ^^ fmt)

type 'a cmp = 'a -> 'a -> int

module Compare = struct
  type 'a t = 'a cmp
  let ignore _ _ = 0
  module V = struct type nonrec ('a, 'b) t = 'b t end
  include Typegist__gist.Meta.Key (V)
end

let rec compare_fields :
type p a. (p, a) Typegist__gist.Product.fields -> p cmp =
fun fs v0 v1 -> match fs with
| Ctor _ -> 0
| App (fs, f) ->
    let cmp = compare_fields fs v0 v1 in
    if cmp <> 0 then cmp else
    let v0 = Typegist__gist.Field.get f v0 in
    let v1 = Typegist__gist.Field.get f v1 in
    let meta = Typegist__gist.Field.meta f in
    match Compare.find meta with
    | Some compare -> compare v0 v1
    | None ->
        match Typegist__gist.Meta.Ignore.find meta with
        | Some true -> 0
        | _ -> compare (Typegist__gist.Field.gist f) v0 v1

and compare_variant_like : type s. s Typegist__gist.Variant_like.t -> s cmp =
fun variant v0 v1 -> match variant with
| Option a -> Option.compare (compare a) v0 v1
| Either (l, r) -> Either.compare ~left:(compare l) ~right:(compare r) v0 v1
| Result (o, e) -> Result.compare ~ok:(compare o) ~error:(compare e) v0 v1
| List a -> List.compare (compare a) v0 v1
| Variant v ->
    let v0_case_idx = Typegist__gist.Variant.case_index v v0 in
    let v1_case_idx = Typegist__gist.Variant.case_index v v1 in
    let cmp = Int.compare v0_case_idx v1_case_idx in
    if cmp <> 0 then cmp else
    let case = Iarray.get (Typegist__gist.Variant.cases v) v0_case_idx in
    compare_fields (Typegist__gist.Variant.Case.product case) v0 v1

and compare_array_like :
type elt arr. (elt, arr) Typegist__gist.Array_like.t -> arr cmp =
fun a v0 v1 -> match a with
| String _ -> String.compare v0 v1
| Bytes _ -> Bytes.compare v0 v1
| Array g -> Array.compare (compare g) v0 v1
| Iarray g -> Iarray.compare (compare g) v0 v1
| (Bigarray1 _ | Module _) ->
    let (module Array) = Typegist__gist.Array_like.to_module a in
    let elt = Typegist__gist.Array_like.elt a in
    let compare = compare elt in
    Array.compare compare v0 v1

and compare_map_like :
type k v m. (k, v, m) Typegist__gist.Map_like.t -> m cmp =
fun m v0 v1 -> match m with
| Hashtbl_module (k, v, (module H)) -> H.compare (compare k) (compare v) v0 v1
| Map_module (k, v, (module Map)) -> Map.compare (compare v) v0 v1

and compare_cell_like :
type a cell. (a, cell) Typegist__gist.Cell_like.t -> cell cmp =
fun c v0 v1 -> match c with
| Lazy g -> compare g (Lazy.force v0) (Lazy.force v1)
| Ref g -> compare g !v0 !v1
| Atomic g -> compare g (Atomic.get v0) (Atomic.get v1)
| Module (g, (module Cell)) -> compare g (Cell.get v0) (Cell.get v1)

and compare_func : type d r. (d, r) Typegist__gist.Func.t -> (d -> r) cmp =
fun f _ _ -> invalid_argf "functional value"

and compare_abstract :
type a. a Typegist__gist.t -> a Typegist__gist.Abstract.t -> a cmp =
fun g a v0 v1 ->
  compare (Typegist__gist.Abstract.get_binop_gist g a v0 v1) v0 v1

and compare_view : type a b. (a, b) Typegist__gist.View.t -> a cmp =
fun w v0 v1 ->
  let inject = Typegist__gist.View.inject w in
  compare (Typegist__gist.View.gist w) (inject v0) (inject v1)

and compare : type a. a Typegist__gist.t -> a cmp = fun g v0 v1 ->
  let meta = Typegist__gist.meta g in
  match Compare.find meta with
  | Some compare -> compare v0 v1
  | None ->
      match Typegist__gist.Meta.Ignore.find meta with
      | Some true -> 0
      | _ ->
          match Typegist__gist.expr g with
          | Scalar s -> Typegist__gist.Scalar.compare s v0 v1
          | Tuple p -> compare_fields p v0 v1
          | Record r -> compare_fields r v0 v1
          | Variant_like vl -> compare_variant_like vl v0 v1
          | Array_like a -> compare_array_like a v0 v1
          | Map_like m -> compare_map_like m v0 v1
          | Cell_like c -> compare_cell_like c v0 v1
          | Func f -> compare_func f v0 v1
          | Abstract a -> compare_abstract g a v0 v1
          | View w -> compare_view w v0 v1
          | Rec (lazy g) -> compare g v0 v1
