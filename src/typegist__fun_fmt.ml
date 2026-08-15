(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a fmt = Format.formatter -> 'a -> unit

module Fmt = struct
  type 'a t = 'a fmt
  let ignore _ppf _v = ()
  module V = struct type nonrec ('a, 'b) t = 'b t end
  include Typegist__gist.Meta.Key (V)
end

let pf = Format.fprintf
let pp_char = Format.pp_print_char
let pp_string = Format.pp_print_string
let pp_list = Format.pp_print_list
let pp_comma ppf () = pf ppf ",@ "
let pp_semi ppf () = pf ppf ";@ "
let pp_semi_cut ppf () = pf ppf ";@,"
let pp_iter = Format.pp_print_iter
let pp_iter_map ~kind iter pp_k pp_v ppf m =
  let is_first = ref true in
  let pp_binding ppf k v =
    (if !is_first then is_first := false else pp_semi ppf ());
    pf ppf "@[<1>(@[%a@],@, @[%a@])@]" pp_k k pp_v v
  in
  let pp_map ppf m = iter (pp_binding ppf) m in
  pf ppf "@[<1><%a: [@[%a@]]>@]" kind () pp_map m

let pp_iter_array ~kind iter pp_elt ppf a =
  pf ppf "@[<2>[%a|%a|]@]" kind () (pp_iter iter ~pp_sep:pp_semi pp_elt) a

let pp_text_string ppf s =
  let esc ppf u = pf ppf "\\u{%04X}" u in
  let rec loop ppf i max =
    if i > max then () else
    let dec = String.get_utf_8_uchar s i in
    let len = Uchar.utf_decode_length dec in
    let next = i + len in
    begin match Uchar.to_int (Uchar.utf_decode_uchar dec) with
    | 0x0022 -> pp_char ppf '\\'; pp_char ppf '\"'
    | 0x005C -> pp_string ppf "\\\\"
    | 0x000A -> pp_string ppf "\\n"
    | 0x000D -> pp_string ppf "\\r"
    | u when 0x0000 <= u && u <= 0x001F -> esc ppf u (* C0 control characters *)
    | u when 0x0080 <= u && u <= 0x009F -> esc ppf u (* C1 control characters *)
    | 0x2028 as u -> esc ppf u (* line separator *)
    | 0x2029 as u -> esc ppf u (* paragraph separator *)
    | 0xFFFD as u -> esc ppf u (* Uchar.rep *)
    | u -> for k = 0 to len - 1 do pp_char ppf (s.[i + k]) done
    end;
    loop ppf next max
  in
  pp_char ppf '\"'; loop ppf 0 (String.length s - 1); pp_char ppf '\"'

let pp_binary_string ppf s =
  let pp_sep ppf () = () in
  let pp_v ppf c = pf ppf "\\x%02x" (Char.code c) in
  pp_char ppf '\"'; pp_iter ~pp_sep String.iter pp_v ppf s; pp_char ppf '\"'

let pp_text_bytes ppf b = pp_text_string ppf (Bytes.unsafe_to_string b)
let pp_bytes ppf b = pp_binary_string ppf (Bytes.unsafe_to_string b)

let rec pp_fields :
type p a. ('q 'b. sep:bool -> ('q, 'b) Typegist__gist.Field.t -> 'q fmt) ->
  (p, a) Typegist__gist.Product.fields -> p fmt =
fun pp_field fs ppf v -> match fs with
| Ctor _ -> ()
| App (Ctor _, f) -> pp_field ~sep:false f ppf v
| App (fs, f) -> pp_fields pp_field fs ppf v; pp_field ~sep:true f ppf v

and pp_tuple_field :
type p a. sep:bool -> (p, a) Typegist__gist.Field.t -> p fmt =
fun ~sep f ppf p ->
  let meta = Typegist__gist.Field.meta f in
  let v = Typegist__gist.Field.get f p in
  match Fmt.find meta with
  | Some pp -> pp ppf v (* the pp should do everything for [ignore] to work *)
  | None ->
      match Typegist__gist.Meta.Ignore.find meta with
      | Some true -> ()
      | Some false | None ->
          let g = Typegist__gist.Field.gist f in
          (if sep then pp_comma ppf ()); pp g ppf v

and pp_tuple : type t. t Typegist__gist.Tuple.t -> t fmt =
fun t ppf v -> match t with
| Ctor _ -> ()
| App (Ctor _, comp) -> pp_tuple_field ~sep:false comp ppf v
| fs -> pf ppf "@[<1>(%a)@]" (pp_fields pp_tuple_field fs) v

and pp_record_field :
type p a. sep:bool -> (p, a) Typegist__gist.Field.t -> p fmt =
fun ~sep f ppf p ->
  let meta = Typegist__gist.Field.meta f in
  let v = Typegist__gist.Field.get f p in
  match Fmt.find meta with
  | Some pp -> pp ppf v (* the pp should do everything for [ignore] to work *)
  | None ->
      match Typegist__gist.Meta.Ignore.find meta with
      | Some true -> ()
      | Some false | None ->
          let g = Typegist__gist.Field.gist f in
          (if sep then pp_semi_cut ppf ());
          pf ppf "@[%s =@ @[%a@]@]" (Typegist__gist.Field.name f) (pp g) v

and pp_record : type r. r Typegist__gist.Record.t -> r fmt = fun r ppf v ->
  pf ppf "@[<v2>{ %a@] }" (pp_fields pp_record_field r) v

and pp_variant_like : type a. a Typegist__gist.Variant_like.t -> a fmt =
fun variant ppf v -> match variant with
| List a -> pf ppf "@[<1>[%a]@]" (pp_list ~pp_sep:pp_semi (pp a)) v
| variant ->
    let variant = Typegist__gist.Variant_like.to_variant variant in
    let case = Typegist__gist.Variant.case variant v in
    let name = Typegist__gist.Variant.Case.name case in
    let product = Typegist__gist.Variant.Case.product case in
    if Typegist__gist.Product.is_empty product then pp_string ppf name else
    if Typegist__gist.Variant.Case.is_inline_record case
    then pf ppf "%s %a" name (pp_record product) v
    else pf ppf "%s %a" name (pp_tuple product) v

and pp_array_like :
type elt a. (elt, a) Typegist__gist.Array_like.t -> a fmt =
fun a ppf v -> match a with
| Bytes Binary -> pp_bytes ppf v
| Bytes Utf_8 -> pp_text_bytes ppf v
| String Binary -> pp_binary_string ppf v
| String Utf_8 -> pp_text_string ppf v
| Array g -> pp_iter_array ~kind:Fmt.ignore Array.iter (pp g) ppf v
| Iarray g -> pp_iter_array ~kind:Fmt.ignore Iarray.iter (pp g) ppf v
| (Bigarray1 _ | Module _) ->
    let (module Array) = Typegist__gist.Array_like.to_module a in
    let kind = Array.pp_applied_name in
    let elt = Typegist__gist.Array_like.elt a in
    pp_iter_array ~kind Array.iter (pp elt) ppf v

and pp_map_like : type k v m. (k, v, m) Typegist__gist.Map_like.t -> m fmt =
fun m ppf v -> match m with
| Hashtbl_module (gk, gv, (module Hashtbl)) ->
    let iter f h = Hashtbl.fold (fun k v () -> f k v) h () in
    let kind = Hashtbl.pp_applied_name in
    pp_iter_map ~kind iter (pp gk) (pp gv) ppf v
| Map_module (gk, gv, (module Map)) ->
    let iter f m = Map.fold (fun k v () -> f k v) m () in
    let kind = Map.pp_applied_name in
    pp_iter_map ~kind iter (pp gk) (pp gv) ppf v

and pp_cell_like :
type a cell. (a, cell) Typegist__gist.Cell_like.t -> cell fmt =
fun c ppf v -> match c with
| Lazy g -> pf ppf "@[<1>(lazy@ @[%a@])@]" (pp g) (Lazy.force v)
| Ref g -> pf ppf "@[<1>{contents =@ @[%a@]@]" (pp g) (!v)
| Atomic g -> pf ppf "@[<1>Atomic.{contents =@ @[%a@]@]" (pp g) (Atomic.get v)
| Module (g, (module Cell)) ->
    pf ppf "@[<1>(%a %a)@]" Cell.pp_applied_name () (pp g) (Cell.get v)

and pp_func : type a b. (a, b) Typegist__gist.Func.t fmt = fun ppf g ->
  pp_string ppf "<fun>"

and pp_abstract : type a. a Typegist__gist.Abstract.t -> a fmt = fun a ppf v ->
  if Typegist__gist.Abstract.is_opaque a then pp_string ppf "<abstr>" else
  let version = Typegist__gist.Abstract.version a v in
  let g = Typegist__gist.Abstract.Version.gist version in
  let version = Typegist__gist.Abstract.Version.name version in
  pf ppf "@[<1><abstr:%s@ @[%a@]>@]" version (pp g) v

and pp_view : type a b. (a, b) Typegist__gist.View.t -> a fmt =
fun view ppf v ->
  pp (Typegist__gist.View.gist view) ppf (Typegist__gist.View.inject view v)

and pp : type a. a Typegist__gist.t -> a fmt = fun g ppf v ->
  let meta = Typegist__gist.meta g in
  match Fmt.find (Typegist__gist.meta g) with
  | Some pp -> pp ppf v
  | None ->
      match Typegist__gist.Meta.Ignore.find meta with
      | Some true -> ()
      | _ ->
          match Typegist__gist.expr g with
          | Scalar s -> Typegist__gist.Scalar.pp s ppf v
          | Tuple p -> pp_tuple p ppf v
          | Record r -> pp_record r ppf v
          | Variant_like s -> pp_variant_like s ppf v
          | Array_like a -> pp_array_like a ppf v
          | Map_like m -> pp_map_like m ppf v
          | Cell_like c -> pp_cell_like c ppf v
          | Func func -> pp_func ppf func
          | Abstract a -> pp_abstract a ppf v
          | View w -> pp_view w ppf v
          | Rec (lazy g) -> pp g ppf v
