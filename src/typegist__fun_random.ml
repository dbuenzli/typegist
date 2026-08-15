(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let invalid_argf fmt =
  Format.ksprintf invalid_arg ("Fun.Generic.random: " ^^ fmt)

type 'a gen = size:int -> Random.State.t -> bound:int -> 'a

module Gen = struct
  type 'a t = 'a gen
  let const v = fun ~size:_ _rstate ~bound:_ -> v
  module V = struct type nonrec ('a, 'b) t = 'b t end
  include Typegist__gist.Meta.Key (V)
end

module Size = struct
  type t = int
  module V = struct type ('a, 'b) t = int end
  include Typegist__gist.Meta.Key (V)
end

let case_is_recursive g c =
  let check_field () f =
    let field_gist = Typegist__gist.Field.gist f in
    if Typegist__gist.has_gist g field_gist then raise Exit else ()
  in
  let product = Typegist__gist.Variant.Case.product c in
  try Typegist__gist.Product.fold_fields check_field () product; false with
  | Exit -> true

let uchar_surrogate_count = 0xDFFF - 0xD800 + 1
let uchar_count = Uchar.to_int Uchar.max + 1 - uchar_surrogate_count
let random_uchar st =
  let u = Random.State.int st uchar_count in
  let u = if u < 0xD800 then u else u + uchar_surrogate_count in
  Uchar.unsafe_of_int u

let random_iarray_elt st a =
  Iarray.get a (Random.State.int st (Iarray.length a))

let rbits = 30 (* number of bits returned by Random.State.bits. *)
let rbits_calls = Int.cdiv Sys.int_size rbits
let intbits st = (* Missing in stdlib: Random.State.intbits *)
  let r = ref 0 in
  for i = 1 to rbits_calls do
    let bits = Random.State.bits st in r := (!r lsl rbits) lor bits
  done;
  !r

let sized_nat ~size st = Random.State.int st (size + 1)

let random_utf_8_bytes ~size st ~bound =
  let chars = Iarray.init (sized_nat ~size st) (fun i -> random_uchar st) in
  let add_uchar_len acc u = acc + Uchar.utf_8_byte_length u in
  let bytes_len = Iarray.fold_left add_uchar_len 0 chars in
  let b = Bytes.create bytes_len in
  let i = ref 0 in
  for k = 0 to Iarray.length chars - 1
  do i := !i + Bytes.set_utf_8_uchar b !i (Iarray.get chars 0) done;
  b

let random_utf_8_string ~size st ~bound =
  Bytes.unsafe_to_string (random_utf_8_bytes ~size st ~bound)

let frequency st a =
  let freq = fst in
  let rec find_item a i acc item =
    let el = Iarray.get a i in
    let acc = acc + freq el in
    if item <= acc then snd el else find_item a (i + 1) acc item
  in
  let sum = Iarray.fold_left (fun acc el -> acc + (freq el)) 0 a in
  if sum = 0 then None else Some (find_item a 0 (-1) (Random.State.int st sum))

let rec gen_fields : type p a. (p, a) Typegist__gist.Product.fields -> a gen =
fun fs ~size st ~bound -> match fs with
| Ctor ctor -> ctor
| App (fs, f) ->
    let ctor = gen_fields fs ~size st ~bound in
    let v =
      let meta = Typegist__gist.Field.meta f in
      let size = Option.value ~default:size (Size.find meta) in
      match Gen.find meta with
      | Some gen -> gen ~size st ~bound
      | None -> gen (Typegist__gist.Field.gist f) ~size st ~bound
    in
    ctor v

and gen_scalar : type a. a Typegist__gist.Scalar.t -> a gen =
fun s ~size st ~bound -> match s with
| Unit -> ()
| Bool -> Random.State.bool st
| Char -> Char.chr (Random.State.int st 256)
| Uchar -> random_uchar st
| Int -> intbits st
| Int32 -> Random.State.bits32 st
| Int64 -> Random.State.bits64 st
| Nativeint -> Random.State.nativebits st
| Float -> Int64.to_float (Random.State.bits64 st)

and gen_variant_like :
type v. v Typegist__gist.t -> v Typegist__gist.Variant_like.t -> v gen =
fun g variant ~size st ~bound -> match variant with
| List elt -> List.init (sized_nat ~size st) (fun _ -> gen elt ~size st ~bound)
| variant ->
    let variant = Typegist__gist.Variant_like.to_variant variant in
    let cases = Typegist__gist.Variant.cases variant in
    let case_frequency c =
      (* XXX the frequency scheme here should perhaps be made smarter *)
      let freq = if case_is_recursive g c && bound = 0 then 0 else 1 in
      freq, c
    in
    let case = match frequency st (Iarray.map case_frequency cases) with
    | None -> random_iarray_elt st cases
    | Some case -> case
    in
    let product = Typegist__gist.Variant.Case.product case in
    gen_fields product ~size st ~bound

and gen_array_like :
type elt array. (elt, array) Typegist__gist.Array_like.t -> array gen =
fun array ~size st ~bound -> match array with
| String Utf_8 -> random_utf_8_string ~size st ~bound
| Bytes Utf_8 -> random_utf_8_bytes ~size st ~bound
| array ->
    let (module Array) = Typegist__gist.Array_like.to_module array in
    let elt = Typegist__gist.Array_like.elt array in
    Array.init (sized_nat ~size st) (fun _ -> gen elt ~size st ~bound)

and gen_map_like : type k v m. (k, v, m) Typegist__gist.Map_like.t -> m gen =
fun map ~size st ~bound ->
  let binding_count = sized_nat ~size st in
  let init, add, k, v = match map with
  | Hashtbl_module (k, v, (module Hashtbl)) ->
      let add k v h = Hashtbl.add h k v; h in
      Hashtbl.create binding_count, add, k, v
  | Map_module (k, v, (module Map)) ->
      Map.empty, Map.add, k, v
  in
  let gen_key = gen (Typegist__gist.Map_like.key map) in
  let gen_value = gen (Typegist__gist.Map_like.value map) in
  let rec loop count acc =
    if binding_count <= 0 then acc else
    let k = gen_key ~size st ~bound in
    let v = gen_value ~size st ~bound in
    loop (binding_count - 1) (add k v acc)
  in
  loop binding_count init

and gen_cell_like :
type a cell. (a, cell) Typegist__gist.Cell_like.t -> cell gen =
fun cell ~size st ~bound -> match cell with
| Lazy g -> lazy (gen g ~size st ~bound)
| Ref g -> ref (gen g ~size st ~bound)
| Atomic g -> Atomic.make (gen g ~size st ~bound)
| Module (g, (module Cell)) -> Cell.make (gen g ~size st ~bound)

and gen_func : type a b. (a, b) Typegist__gist.Func.t -> (a -> b) gen =
fun func ~size st ->
(* XXX we could do something here but see §3.3 of the quickcheck
   paper. random splitting which couldC help. *)
  invalid_argf "functional value"

and gen_abstract :
type a. a Typegist__gist.t -> a Typegist__gist.Abstract.t -> a gen =
fun g a ~size st ~bound ->
  Typegist__gist.Abstract.check_non_opaque g a;
  let versions = Typegist__gist.Abstract.versions a in
  let version = random_iarray_elt st versions in
  let g = Typegist__gist.Abstract.Version.gist version in
  gen g ~size st ~bound

and gen_view : type a b. (a, b) Typegist__gist.View.t -> a gen =
fun view ~size st ~bound ->
  let v = gen (Typegist__gist.View.gist view) ~size st ~bound in
  Typegist__gist.View.project view v

and gen : type a. a Typegist__gist.t -> a gen = fun g ~size st ~bound ->
  let meta = Typegist__gist.meta g in
  let size = Option.value ~default:size (Size.find meta) in
  match Gen.find meta with
  | Some gen -> gen ~size st ~bound
  | None ->
      match Typegist__gist.expr g with
      | Scalar s -> gen_scalar s ~size st ~bound
      | Tuple t -> gen_fields t ~size st ~bound
      | Record r -> gen_fields r ~size st ~bound
      | Variant_like v -> gen_variant_like g v ~size st ~bound
      | Array_like a -> gen_array_like a ~size st ~bound
      | Map_like m -> gen_map_like m ~size st ~bound
      | Cell_like c -> gen_cell_like c ~size st ~bound
      | Func f -> gen_func f ~size st ~bound
      | Abstract a -> gen_abstract g a ~size st ~bound
      | View w -> gen_view w ~size st ~bound
      | Rec (lazy g) -> gen g ~size st ~bound:(bound - 1)

let random g ?(size = 23) ?(state = Random.State.make_self_init ()) () =
  gen g ~size state ~bound:size

module Random = struct
  module Gen = Gen
  module Size = Size
end
