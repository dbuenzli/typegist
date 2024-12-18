(*---------------------------------------------------------------------------
   Copyright (c) 2023 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Typegist

let test_pair () =
  Test.test "pairs" @@ fun () ->
  let pair_gist gfst gsnd =
    Type.Gist.product (fun x y -> (x, y))
    |> Type.Gist.dim gfst fst
    |> Type.Gist.dim gsnd snd
    |> Type.Gist.finish_product
  in
  let pair = pair_gist Type.Gist.int Type.Gist.string_as_utf_8 in
  Test.log " type: @[%a@]" Type.Gist.pp_type pair;
  Test.log "value: @[%a@]" (Fun.Generic.pp pair) (3, "hey")

let test_func () =
  Test.test "function representation" @@ fun () ->
  let func = Type.Gist.(string_as_utf_8 @-> int) in
  let ret_pair = Type.Gist.(int @-> int @-> p2 int int) in
  let pair x y = x, y in
  Test.log "   value String.length: @[%a@]" (Fun.Generic.pp func) String.length;
  Test.log "value pair constructor: @[%a@]" (Fun.Generic.pp ret_pair) pair

let test_btree () =
  Test.test "binary tree" @@ fun () ->
  let module Btree : sig
    type 'a t = Empty | Node of 'a t * 'a * 'a t
    val type_gist : 'a Type.Gist.t -> 'a t Type.Gist.t
    val pp : 'a Type.Gist.t -> Format.formatter -> 'a t -> unit
  end = struct
    type 'a t = Empty | Node of 'a t * 'a * 'a t
    let type_gist gel =
      let rec g = lazy begin
        let g = Type.Gist.rec' g in
        let empty_case = Type.Gist.case0 "Btree.Empty" Empty in
        let node_case =
          Type.Gist.case "Btree.Node" (fun l v r -> Node (l, v, r))
          |> Type.Gist.dim g (function Node (l,_,_) -> l | _ -> assert false)
          |> Type.Gist.dim gel (function Node (_,v,_) -> v | _ -> assert false)
          |> Type.Gist.dim g (function Node (_,_,r) -> r | _ -> assert false)
          |> Type.Gist.finish_case
        in
        Type.Gist.variant "Btree.t"
          (function Empty -> empty_case | Node _ -> node_case)
          [empty_case; node_case]
      end
      in
      Lazy.force g

    let pp elt = Fun.Generic.pp (type_gist elt)
  end
  in
  let int_tree = Btree.type_gist Type.Gist.int in
  let t = Btree.Node (Node (Empty, 2, Empty), 1, Empty) in
  Test.log " type: @[%a@]" Type.Gist.pp_type int_tree;
  Test.log "value: @[%a@]" (Btree.pp Type.Gist.int) t

let test_abstract_person () =
  Test.test "Person" @@ fun () ->
  let module Person : sig
    type t
    val make : string -> string -> string option -> t
    val name : t -> string
    val email : t -> string
    val phone : t -> string option
    val type_gist : t Type.Gist.t
    val pp : Format.formatter -> t -> unit
  end = struct
    type t = { name : string; email : string; phone : string option }
    let make name email phone = { name; email; phone }
    let name p = p.name
    let email p = p.email
    let phone p = p.phone

    let name_field =
      Type.Gist.Field.make ~name:"name" Type.Gist.string_as_utf_8 name

    let email_field =
      Type.Gist.Field.make ~name:"email" Type.Gist.string_as_utf_8 email

    let phone_field =
      let phone_gist = Type.Gist.(option string_as_utf_8) in
      Type.Gist.Field.make ~name:"phone" phone_gist phone

    let type_gist =
      let repr_v0 = (* First version had no phone numbers *)
        let to_v1 name email = { name; email; phone = None } in
        let g =
          Type.Gist.record "person" to_v1
          |> Type.Gist.field' name_field
          |> Type.Gist.field' email_field
          |> Type.Gist.finish_record
        in
        Type.Gist.Abstract.repr ~version:"v0" g Fun.id Fun.id
      in
      let repr_v1 = (* Added phone numbers *)
        let g =
          Type.Gist.record "person" make
          |> Type.Gist.field' name_field
          |> Type.Gist.field' email_field
          |> Type.Gist.field' phone_field
          |> Type.Gist.finish_record
        in
        Type.Gist.Abstract.repr ~version:"v1" g Fun.id Fun.id
      in
      Type.Gist.abstract "person" [repr_v1; repr_v0]

    let pp = Fun.Generic.pp type_gist
  end
  in
  let p =
    Person.make "Bactrian" "bactrian@example.com" (Some "+XX XXX XX XX")
  in
  Test.log " type: @[%a@]" Type.Gist.pp_type Person.type_gist;
  Test.log "value: @[%a@]" Person.pp p

let test_maplike () =
  Test.test "maplike" @@ fun () ->
  let module String_map = Map.Make (String) in
  let imap = String_map.(empty |> add "fst" 1 |> add "snd" 2) in
  let module M = Type.Gist.Maplike.Map_module_of_map (Int) (String_map) in
  let g = Type.Gist.(map_module (module M) string_as_utf_8 int) in
  Test.log " type: @[%a@]" Type.Gist.pp_type g;
  Test.log "value: @[%a@]" (Fun.Generic.pp g) imap

let test_rec_meta () =
  let module M = struct
    type nat = Z | Succ of nat
    let type_gist =
      let rec g = lazy begin
        let g = Type.Gist.rec' g in
        let z = Type.Gist.case0 "M.Z" Z in
        let succ =
          Type.Gist.case "M.Succ" (fun n -> Succ n)
          |> Type.Gist.dim g (function Succ v -> v | _ -> assert false)
          |> Type.Gist.finish_case
        in
        let project = function Z -> z | Succ _ -> succ in
        Type.Gist.variant "M.nat" project [z; succ]
      end
      in
      Lazy.force g
  end
  in
  ()

let test_rec () =
  Test.test "mutually recursive" @@ fun () ->
  let module M = struct
    type one = One | To_two of two
    and two = Two | To_one of one

    let type_gist_one, type_gist_two =
      let rec gone = lazy begin
        let gtwo = Type.Gist.rec' gtwo in
        let one_case = Type.Gist.case0 "M.One" One in
        let to_two_case =
          Type.Gist.case "M.To_two" (fun t -> To_two t)
          |> Type.Gist.dim gtwo (function To_two v -> v | _ -> assert false)
          |> Type.Gist.finish_case
        in
        let one_proj = function One -> one_case | To_two _ -> to_two_case in
        Type.Gist.variant "M.one" one_proj [one_case; to_two_case]
      end
      and gtwo = lazy begin
        let gone = Type.Gist.rec' gone in
        let two_case = Type.Gist.case0 "Two" Two in
        let to_one_case =
          Type.Gist.case "To_one" (fun t -> To_one t)
          |> Type.Gist.dim gone (function To_one v -> v | _ -> assert false)
          |> Type.Gist.finish_case
        in
        let two_proj = function Two -> two_case | To_one _ -> to_one_case in
        Type.Gist.variant "M.two" two_proj [two_case; to_one_case]
      end
      in
      (Lazy.force gone), (Lazy.force gtwo)
  end
  in
  let v = M.To_two (M.To_one (M.To_two Two)) in
  Test.log "type one: @[%a@]" Type.Gist.pp_type M.type_gist_one;
  Test.log "type two: @[%a@]" Type.Gist.pp_type M.type_gist_two;
  Test.log "   value: @[%a@]" (Fun.Generic.pp M.type_gist_one) v

let test_custom_fmt () =
  Test.test "custom formatter" @@ fun () ->
  let us = [|Uchar.of_int 0x1F42B; Uchar.of_int 0x41|] in
  let uchar_debug ppf u = Format.fprintf ppf "U+%04X" (Uchar.to_int u) in
  let uchar_debug =
    let m = Type.Gist.Meta.empty |> Fun.Generic.Meta.Fmt.add uchar_debug in
    Type.Gist.Scalar (Uchar m)
  in
  let uchars = Type.Gist.(array uchar) in
  let uchars_debug = Type.Gist.(array uchar_debug) in
  Test.log "normal: @[%a@]" (Fun.Generic.pp uchars) us;
  Test.log "custom: @[%a@]" (Fun.Generic.pp uchars_debug) us

let test_gadt () =
  Test.test "existential GADT" @@ fun () ->
  let module M = struct
    type 'a t =
    | Int : int t
    | Float : float t
    | Pair : 'a t * 'b t -> ('a * 'b) t

    type v = V : 'a t -> v (* We need to hide the witness. *)

    let type_gist : v Type.Gist.t =
      let rec g = lazy begin
        let g = Type.Gist.rec' g in
        let int_case = Type.Gist.case0 "Int" (V Int) in
        let float_case = Type.Gist.case0 "Float" (V Float) in
        let pair_case =
          Type.Gist.case "Pair" (fun (V a) (V b) -> V (Pair (a, b)))
          |> Type.Gist.dim g (function V Pair (a, _) -> V a | _ -> assert false)
          |> Type.Gist.dim g (function V Pair (_, b) -> V b | _ -> assert false)
          |> Type.Gist.finish_case
        in
        let t_proj : type a. v -> v Type.Gist.Variant.case = function
        | V Int -> int_case | V Float -> float_case | V (Pair _) -> pair_case
        in
        let cases = [int_case; float_case; pair_case] in
        Type.Gist.variant "_ M.t" t_proj cases
      end
      in
      Lazy.force g
  end
  in
  let v = M.V (Pair (Pair (Int, Float), Float)) in
  Test.log " type: @[%a@]" Type.Gist.pp_type M.type_gist;
  Test.log " type: @[%a@]" Type.Gist.pp_type M.type_gist;
  Test.log "value: @[%a@]" (Fun.Generic.pp M.type_gist) v;
  ()

let test_key () =
  Test.test "meta keys" @@ fun () ->
  let module Min = Type.Gist.Meta.Key (struct type 'a t = 'a end) in
  let meta = Min.add 3 Type.Gist.Meta.empty in
  assert (Min.find meta = Some 3)

let main () =
  Test.main @@ fun () ->
  test_pair ();
  test_func ();
  test_btree ();
  test_abstract_person ();
  test_maplike ();
  test_rec ();
  test_custom_fmt ();
  test_gadt ();
  test_key ();
  ()

let () = if !Sys.interactive then () else exit (main ())
