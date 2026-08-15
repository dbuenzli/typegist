(*---------------------------------------------------------------------------
   Copyright (c) 2023 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Typegist

let typedef g = Format.asprintf "@[%a@]" Type.Gist.pp_def g
let gpp g v = Format.asprintf "@[%a@]" (Fun.Generic.pp g) v
let names g = (* Tests fold a bit *)
  let names acc g =
    let n = Type.Gist.name g in
    if not (String.is_empty n) then n :: acc else acc
  in
  let names = Type.Gist.Expr.fold_gists names [] (Type.Gist.expr g) in
  String.concat " " names

let test_meta =
  Test.test "Type.Gist.Meta" @@ fun () ->
  let module V = Type.Gist.Meta.Key (struct type ('a, 'b) t = 'b end) in
  let meta =
    Type.Gist.Meta.empty
    |> V.add 3
    |> Type.Gist.Meta.Min.add 0
    |> Type.Gist.Meta.Max.add 4
  in
  let meta' = Type.Gist.Meta.Max.remove meta in
  Snap.(option T.int) (V.find meta) @> __POS_OF__ (Some 3);
  Snap.(option T.int) (Type.Gist.Meta.Min.find meta) @> __POS_OF__ (Some 0);
  Snap.(option T.int) (Type.Gist.Meta.Max.find meta) @> __POS_OF__ (Some 4);
  Snap.(option T.int) (V.find meta') @> __POS_OF__ (Some 3);
  Snap.(option T.int) (Type.Gist.Meta.Min.find meta') @> __POS_OF__ (Some 0);
  Snap.(option T.int) (Type.Gist.Meta.Max.find meta') @> __POS_OF__ None;
  ()

let test_gist_rebind =
  Test.test "Type.Gist.rebind" @@ fun () ->
  let module M = struct
    type t = { sep : Uchar.t; }
    let make sep = { sep }
    let sep v = v.sep
    let gist =
      Type.Gist.record "M.t" make
      |> Type.Gist.field "sep" Type.Gist.uchar sep
      |> Type.Gist.finish
  end
  in
  (* Makes a new uchar formatter for Fun.Generic.pp *)
  let uchar =
    let pp_uchar ppf u = Format.fprintf ppf "U+%04X" (Uchar.to_int u) in
    let meta = Type.Gist.meta Type.Gist.uchar in
    let meta = Fun.Generic.Fmt.add pp_uchar meta in
    Type.Gist.update ~meta Type.Gist.uchar
  in
  let str g v = Format.asprintf "%a" (Fun.Generic.pp g) v in
  let v = M.make (Uchar.of_char ' ') in
  (* Using the default definition for Type.Gist.uchar *)
  Snap.string (str M.gist v) @> __POS_OF__
    "{ sep = ' ' }";
  (* Rebind the definition of Type.Gist.uchar in M.t with our new formatter *)
  Snap.string (str (Type.Gist.rebind uchar M.gist) v) @> __POS_OF__
    "{ sep = U+0020 }";
  Test.bool (Type.Gist.is_recursive M.gist) false;
  ()

let test_pp_scalars =
  Test.test "Type.Gist.pp_def on scalars" @@ fun () ->
  Snap.string (typedef Type.Gist.unit) @> __POS_OF__ "unit";
  Snap.string (typedef Type.Gist.bool) @> __POS_OF__ "bool";
  Snap.string (typedef Type.Gist.char) @> __POS_OF__ "char";
  Snap.string (typedef Type.Gist.uchar) @> __POS_OF__ "Uchar.t";
  Snap.string (typedef Type.Gist.int) @> __POS_OF__ "int";
  Snap.string (typedef Type.Gist.int32) @> __POS_OF__ "int32";
  Snap.string (typedef Type.Gist.int64) @> __POS_OF__ "int64";
  Snap.string (typedef Type.Gist.nativeint) @> __POS_OF__ "nativeint";
  Snap.string (typedef Type.Gist.float) @> __POS_OF__ "float";
  Test.bool (Type.Gist.is_recursive Type.Gist.unit) false;
  ()

let test_pp_tuple =
  Test.test "Type.Gist.pp_def on tuples" @@ fun () ->
  let ipair = Type.Gist.(t2 int int) in
  let pt = Type.Gist.(t2 ~name:"Pt.t" int int) in
  let pt_pair = Type.Gist.(t2 ~name:"Pt.pair" pt pt) in
  Snap.string (typedef ipair) @> __POS_OF__
    "int * int";
  Snap.string (typedef pt) @> __POS_OF__
    "type Pt.t = int * int";
  Snap.string (typedef pt_pair) @> __POS_OF__
    "type Pt.pair = Pt.t * Pt.t";
  Test.bool (Type.Gist.is_recursive pt) false;
  Test.bool (Type.Gist.is_recursive pt_pair) false;
  ()

let test_tuple =
  Test.test "Type.Gist.tuple" @@ fun () ->
  let type point = float * float * float in
  let point_gist =
    Type.Gist.tuple ~name:"M.point" (fun x y z -> x, y, z)
    |> Type.Gist.comp Type.Gist.float (fun (x, _, _) -> x)
    |> Type.Gist.comp Type.Gist.float (fun (_, y, _) -> y)
    |> Type.Gist.comp Type.Gist.float (fun (_, _, z) -> z)
    |> Type.Gist.finish
  in
  Snap.string (typedef point_gist) @> __POS_OF__
    "type M.point = float * float * float";
  let type span = string * (int * int) in
  let span_gist = Type.Gist.(t2 ~name:"M.span" binary_string (t2 int int)) in
  Snap.string (typedef span_gist) @> __POS_OF__
    "type M.span = string * (int * int)";
  ()

module Person = struct
  type t = { name : string; age : int option }
  let make name age = { name; age }
  let name p = p.name
  let age p = p.age
  let gist =
    Type.Gist.record "Person.t" make
    |> Type.Gist.field "name" Type.Gist.utf_8_string name
    |> Type.Gist.field "age" Type.Gist.(option int) age
    |> Type.Gist.finish
end

let test_record =
  Test.test "Type.Gist.record" @@ fun () ->
  Snap.string (typedef Person.gist) @> __POS_OF__
    "type Person.t = { name : string;\n\
    \                  age : int option }";
  Test.bool (Type.Gist.is_recursive Person.gist) false;
  ()

let test_record_mutable_field =
  Test.test "Type.Gist.record with mutable fields" @@ fun () ->
  let type t = { mutable name : string } in
  let gist =
    let make name = { name } in
    let name p = p.name in
    let set_name p s = p.name <- s in
    Type.Gist.record "M.t" make
    |> Type.Gist.field "name" Type.Gist.utf_8_string name ~set:set_name
    |> Type.Gist.finish
  in
  Snap.string (typedef gist) @> __POS_OF__
    "type M.t = { mutable name : string }";
  Test.bool (Type.Gist.is_recursive gist) false;
  ()

let test_option =
  Test.test "Test.Gist.option" @@ fun () ->
  let type age = int option in
  let age_gist = Type.Gist.(option ~name:"age" int) in
  Snap.string (typedef age_gist) @> __POS_OF__
    "type age = int option";
  let gist = Type.Gist.(option ~name:"person_option" Person.gist) in
  Snap.string (typedef gist) @> __POS_OF__
    "type person_option = Person.t option";
  Test.bool (Type.Gist.is_recursive gist) false;
  ()

let test_either =
  Test.test "Test.Gist.either" @@ fun () ->
  let gist = Type.Gist.(either ~name:"query_result" Person.gist int) in
  Snap.string (typedef gist) @> __POS_OF__
    "type query_result = (Person.t, int) Either.t";
  Test.bool (Type.Gist.is_recursive gist) false;
  ()

let test_result =
  Test.test "Test.Gist.either" @@ fun () ->
  let gist = Type.Gist.(result ~name:"query" Person.gist utf_8_string) in
  Snap.string (typedef gist) @> __POS_OF__
    "type query = (Person.t, string) result";
  Test.bool (Type.Gist.is_recursive gist) false;
  ()

let test_list =
  Test.test "Test.Gist.list" @@ fun () ->
  let type ipair_list = (int * int) list in
  let gist = Type.Gist.(list ~name:"ipair_list" (t2 int int)) in
  Snap.string (typedef gist) @> __POS_OF__
    "type ipair_list = (int * int) list";
  let type users = Person.t list in
  let gist = Type.Gist.(list ~name:"users" Person.gist) in
  Snap.string (typedef gist) @> __POS_OF__
    "type users = Person.t list";
  Test.bool (Type.Gist.is_recursive gist) false;
  ()

let test_enum =
  Test.test "Type.Gist.enum" @@ fun () ->
  let type status = Todo | Done | Cancelled in
  let status_enum = ["Todo", Todo; "Done", Done; "Cancelled", Cancelled] in
  let status_gist = Type.Gist.variant_of_enum ~name:"M.status" status_enum in
  Snap.string (typedef status_gist) @> __POS_OF__
    "type M.status = | Todo | Done | Cancelled";
  (* Without [name] *)
  let type state = { status : [`Todo | `Done | `Cancelled ] } in
  let status_enum = ["`Todo",`Todo; "`Done", `Done; "`Cancelled",`Cancelled] in
  let status_gist = Type.Gist.variant_of_enum status_enum in
  let gist =
    Type.Gist.record "state" (fun status -> { status })
    |> Type.Gist.field "status" status_gist (fun s -> s.status)
    |> Type.Gist.finish
  in
  Snap.string (typedef gist) @> __POS_OF__
    "type state = { status : [ `Todo | `Done | `Cancelled ] }";
  Test.bool (Type.Gist.is_recursive gist) false;
  ()

let test_variant =
  Test.test "Type.Gist.variant" @@ fun () ->
  let type operation = Create | Delete of { id : int } in
  let operation_gist =
    let create = Type.Gist.case "Create" Create |> Type.Gist.finish in
    let delete =
      let make id = Delete { id } in
      let id = function Delete { id } -> id | _ -> assert false in
      Type.Gist.case "Delete" make
      |> Type.Gist.field "id" Type.Gist.int id
      |> Type.Gist.finish
    in
    let case_index = function Create -> 0 | Delete _ -> 1 in
    Type.Gist.variant ~name:"M.operation" [create; delete] ~case_index
  in
  Snap.string (typedef operation_gist) @> __POS_OF__
    "type M.operation = | Create | Delete of { id : int }";
  Test.bool (Type.Gist.is_recursive operation_gist) false;
  ()

let test_variant_tuple_distinction =
  Test.test "Type.Gist.variant tuple distinction" @@ fun () ->
  (* Pairs as pairs *)
  let type pair = Pair of (int * int) in
  let gist =
    let pair =
      let make v = Pair v in
      let get = function Pair v -> v in
      Type.Gist.case "Pair" make
      |> Type.Gist.comp Type.Gist.(t2 int int) get
      |> Type.Gist.finish
    in
    Type.Gist.variant ~name:"M.pair" [pair] ~case_index:(Fun.const 0)
   in
  Snap.string (typedef gist) @> __POS_OF__
    "type M.pair = | Pair of (int * int)";
  (* Pairs as constructors *)
  let type pair = Pair of int * int in
  let gist =
    let pair =
      let make x y  = Pair (x, y) in
      let x = function Pair (x, _) -> x in
      let y = function Pair (_, y) -> y in
      Type.Gist.case "Pair" make
      |> Type.Gist.comp Type.Gist.int x
      |> Type.Gist.comp Type.Gist.int y
      |> Type.Gist.finish
    in
    Type.Gist.variant ~name:"M.pair" [pair] ~case_index:(Fun.const 0)
  in
  Snap.string (typedef gist) @> __POS_OF__
    "type M.pair = | Pair of int * int";
  Test.bool (Type.Gist.is_recursive gist) false;
  ()

let test_recursive_variant =
  Test.test "Type.Gist.{variant,rec'}" @@ fun () ->
  let type nat = Zero | Succ of nat in
  let nat_gist =
    let rec g = lazy begin
      let nat = Type.Gist.rec' g in
      let zero = Type.Gist.case "Zero" Zero |> Type.Gist.finish in
      let succ =
        let make n = Succ n in
        let get = function Succ n -> n | _ -> assert false in
        Type.Gist.case "Succ" make
        |> Type.Gist.comp nat get
        |> Type.Gist.finish
      in
      let case_index = function Zero -> 0 | Succ _ -> 1 in
      Type.Gist.variant ~name:"M.nat" [zero; succ] ~case_index
    end
    in
    Lazy.force g
  in
  Snap.string (typedef nat_gist) @> __POS_OF__
    "type M.nat = | Zero | Succ of nat";
  Snap.string (names nat_gist) @> __POS_OF__
    "M.nat";
  Test.bool (Type.Gist.is_recursive nat_gist) true;
  (* Check that recursive types have their meta updated in recursive
     occurences. *)
  let nat_gist =
    let meta = Type.Gist.meta nat_gist in
    let meta = Type.Gist.Meta.Min.add Zero meta in
    Type.Gist.update ~meta nat_gist
  in
  let expr_has_min =
    let has_min acc g = acc || Type.Gist.Meta.Min.mem (Type.Gist.meta g) in
    Type.Gist.Expr.fold_gists has_min false (Type.Gist.expr nat_gist)
  in
  Test.bool expr_has_min true ~__POS__;
  ()

let test_gadt =
  Test.test "Type.Gist.variant GADT witness hiding" @@ fun () ->
  let type 'a number =
  | Int : int -> int number
  | Float : float -> float number
  in
  let type e_number = E : 'a number -> e_number in
  let number_gist =
    let int =
      let make i = E (Int i) in
      let get = function E (Int i) -> i | _ -> assert false in
      Type.Gist.case "Int" make
      |> Type.Gist.comp Type.Gist.int get
      |> Type.Gist.finish
    in
    let float =
      let make f = E (Float f) in
      let get = function E (Float f) -> f | _ -> assert false in
      Type.Gist.case "Float" make
      |> Type.Gist.comp Type.Gist.float get
      |> Type.Gist.finish
    in
    let case_index = function E (Int _) -> 0 | E (Float _) -> 1 in
    Type.Gist.variant ~name:"_ number" [int; float] ~case_index
  in
  Snap.string (typedef number_gist) @> __POS_OF__
    "type _ number = | Int of int | Float of float";
  Test.bool (Type.Gist.is_recursive number_gist) false;
  ()

let test_string =
  Test.test "Type.Gist.utf_8_string new identity" @@ fun () ->
  let type username = string in
  let username_gist =
    let pp_bold ppf s = Format.fprintf ppf "@<0>\x1B[1m%s@<0>\x1B[0m" s in
    let meta = Type.Gist.Meta.empty |> Fun.Generic.Fmt.add pp_bold in
    Type.Gist.of_gist ~name:"M.username" ~meta Type.Gist.utf_8_string
  in
  Snap.string (typedef username_gist) @> __POS_OF__
    "type M.username = string";
  Test.bool (Type.Gist.is_recursive username_gist) false;
  ()

let test_array =
  Test.test "Type.Gist.array" @@ fun () ->
  let type samples = int array in
  let samples_gist = Type.Gist.array ~name:"samples" Type.Gist.int in
  Snap.string (typedef samples_gist) @> __POS_OF__
    "type samples = int array";
  let gist = Type.Gist.array Type.Gist.(t2 int int) in
  Snap.string (typedef gist) @> __POS_OF__
    "(int * int) array";
  Test.bool (Type.Gist.is_recursive gist) false;
  ()

let test_other_arrays =
  Test.test "Type.Gist.{bigbytes,floatarray,dynarray,weak}" @@ fun () ->
  Snap.string (typedef Type.Gist.bigbytes) @> __POS_OF__
    "type bigbytes =\n\
     (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t";
  Snap.string (typedef Type.Gist.floatarray) @> __POS_OF__
    "floatarray";
  Snap.string (typedef Type.Gist.(dynarray int)) @> __POS_OF__
    "int Dynarray.t";
  Snap.string (typedef Type.Gist.(weak int)) @> __POS_OF__
    "int Weak.t";
  ()

let test_map =
  Test.test "Type.Gist.map" @@ fun () ->
  let module String_map = Map.Make (String) in
  let imap = String_map.(empty |> add "fst" 1 |> add "snd" 2) in
  let imap_gist =
    Type.Gist.(map "String_map.t" (module String_map) utf_8_string int)
  in
  Snap.string (typedef imap_gist) @> __POS_OF__
    "int String_map.t";
  Snap.string (gpp imap_gist imap) @> __POS_OF__
    "<int String_map.t: [(\"fst\", 1); (\"snd\", 2)]>";
  Test.bool (Type.Gist.is_recursive imap_gist) false;
  ()

let test_hashtbl =
  Test.test "Type.Gist.hashtbl" @@ fun () ->
  let h = Hashtbl.create 3 in
  let () = Hashtbl.add h "fst" 1; Hashtbl.add h "snd" 2 in
  let hgist = Type.Gist.(hashtbl utf_8_string int) in
  Snap.string (typedef hgist) @> __POS_OF__
    "(string, int) Hashtbl.t";
  Snap.string (gpp hgist h) @> __POS_OF__
    {|<(string, int) Hashtbl.t: [("snd", 2); ("fst", 1)]>|};
  Test.bool (Type.Gist.is_recursive hgist) false;
  ()

let test_atomic =
  Test.test "Type.Gist.atomic" @@ fun () ->
  let type epoch = int Atomic.t in
  let epoch_gist = Type.Gist.atomic ~name:"M.epoch" Type.Gist.int in
  Snap.string (typedef epoch_gist) @> __POS_OF__
    "type M.epoch = int Atomic.t";
  Test.bool (Type.Gist.is_recursive epoch_gist) false;
  ()

let test_func =
  Test.test "Type.Gist.func" @@ fun () ->
  let type predicate = char -> bool in
  let predicate_gist = Type.Gist.(func ~name:"predicate" char bool) in
  Snap.string (typedef predicate_gist) @> __POS_OF__
    "type predicate = char -> bool";
  let gist = Type.Gist.(int @-> binary_string @-> bool) in
  Snap.string (typedef gist) @> __POS_OF__
    "int -> (string -> bool)";
  Test.bool (Type.Gist.is_recursive gist) false;
  ()

let test_abstract =
  Test.test "Type.Gist.abstract" @@ fun () ->
  let module _ = struct
    type person = string
    let person_gist_v1 = Type.Gist.(of_gist ~name:"M.person" utf_8_string)
    let person_gist =
      let v1 = Type.Gist.Abstract.Version.make ~name:"v1" person_gist_v1 in
      Type.Gist.abstract "M.person" [v1]
    ;;
    let () = Snap.string (typedef person_gist) @> __POS_OF__
      "type M.person = <abstr>";
  end
  in
  let module _ = struct
    type person = string * string
    let person_gist_v2 =
      Type.Gist.(t2 ~name:"M.person" utf_8_string utf_8_string)

    let person_gist_v1 =
      let inject (last, first) = String.concat " " [last; first] in
      let project n = match String.split_first ~sep:" " n with
      | None -> ("", n) | Some (last, first) -> last, first
      in
      Type.Gist.view ~inject ~project @@
      Type.Gist.(of_gist ~name:"M.person" utf_8_string)

    let person_gist =
      let v1 = Type.Gist.Abstract.Version.make ~name:"v1" person_gist_v1 in
      let v2 = Type.Gist.Abstract.Version.make ~name:"v2" person_gist_v2 in
      Type.Gist.abstract "M.person" [v1; v2]

    let () =
      Snap.string (typedef person_gist) @> __POS_OF__
        "type M.person = <abstr>";
      let person = "Doe", "Jane" in
      Snap.string (gpp person_gist person) @> __POS_OF__
        {|<abstr:v2 ("Doe", "Jane")>|};
  end
  in
  let module _ = struct
    type person_v1 = string
    type person_v2 = string * string
    type person = V1 of person_v1 | V2 of person_v2

    let person_gist_v1 =
      let inject = function V1 v1 -> v1 | _ -> assert false in
      let project v1 = V1 v1 in
      Type.Gist.view ~inject ~project @@
      Type.Gist.(of_gist ~name:"M.person" utf_8_string)

    let person_gist_v2 =
      let inject = function V2 v2 -> v2 | _ -> assert false in
      let project v2 = V2 v2 in
      Type.Gist.view ~inject ~project @@
      Type.Gist.(t2 ~name:"M.person" utf_8_string utf_8_string)

    let person_gist =
      let v1 = Type.Gist.Abstract.Version.make ~name:"v1" person_gist_v1 in
      let v2 = Type.Gist.Abstract.Version.make ~name:"v2" person_gist_v2 in
      let version_index = function V1 _ -> 0 | V2 _ -> 1 in
      Type.Gist.abstract "M.person" [v1; v2] ~version_index

    let () = Snap.string (typedef person_gist) @> __POS_OF__
      "type M.person = <abstr>";
  end
  in
  ()

let test_queue =
  Test.test "Type.Gist.queue" @@ fun () ->
  let int_queue_gist = Type.Gist.(queue int) in
  Snap.string (typedef int_queue_gist) @> __POS_OF__
    "type int Queue.t = int list";
  ()

let test_tree =
  Test.test "binary tree" @@ fun () ->
  let type 'a tree = Empty | Node of 'a tree * 'a * 'a tree in
  let tree_gist : 'a. 'a Type.Gist.t -> 'a tree Type.Gist.t = fun elt ->
    let rec tree = lazy begin
      let tree = Type.Gist.rec' tree in
      let empty = Type.Gist.case "Empty" Empty |> Type.Gist.finish in
      let node =
        let make l v r = Node (l, v, r) in
        let left  = function Node (l, _, _) -> l | _ -> assert false in
        let value = function Node (_, v, _) -> v | _ -> assert false in
        let right = function Node (_, _, r) -> r | _ -> assert false in
        Type.Gist.case "Node" make
        |> Type.Gist.comp tree left
        |> Type.Gist.comp elt value
        |> Type.Gist.comp tree right
        |> Type.Gist.finish
      in
      let case_index = function Empty -> 0 | Node _ -> 1 in
      let name = Type.Gist.Name.make_applied [V elt] "M.tree" in
      Type.Gist.variant ~name [empty; node] ~case_index
    end
    in
    Lazy.force tree
  in
  let int_tree_gist = tree_gist Type.Gist.int in
  let int_pair_tree_gist = tree_gist Type.Gist.(t2 int int) in
  Snap.string (typedef int_tree_gist) @> __POS_OF__
    "type int M.tree = | Empty | Node of tree * int * tree";
  Snap.string (typedef int_pair_tree_gist) @> __POS_OF__
    "type (int * int) M.tree = | Empty | Node of tree * (int * int) * tree";
  let tree = Node (Node (Empty, 2, Empty), 1, Empty) in
  Snap.string (gpp int_tree_gist tree) @> __POS_OF__
    "Node (Node (Empty, 2, Empty), 1, Empty)";
  Test.bool (Type.Gist.is_recursive int_tree_gist) true;
  let int =
    let pp_hex ppf d = Format.fprintf ppf "0x%x" d in
    let meta = Type.Gist.Meta.empty |> Fun.Generic.Fmt.add pp_hex in
    Type.Gist.update ~meta Type.Gist.int
  in
  let int_tree_gist = Type.Gist.rebind int int_tree_gist in
  Snap.string (gpp int_tree_gist tree) @> __POS_OF__
    "Node (Node (Empty, 0x2, Empty), 0x1, Empty)";
  ()

let test_mutually_rec =
  Test.test "Type.Gist.rec' on mutually recursive types" @@ fun () ->
  let type even = Zero | Succ_odd of odd and odd = Succ_even of even in
  let even_gist, odd_gist =
    let rec even_gist = lazy begin
      let odd_gist = Type.Gist.rec' odd_gist in
      let zero = Type.Gist.case "Zero" Zero |> Type.Gist.finish in
      let succ_odd =
        let make odd = Succ_odd odd in
        let get = function Succ_odd odd -> odd | _ -> assert false in
        Type.Gist.case "Succ_odd" make
        |> Type.Gist.comp odd_gist get
      |> Type.Gist.finish
      in
      let case_index = function Zero -> 0 | Succ_odd _ -> 1 in
      Type.Gist.variant ~name:"M.even" [zero; succ_odd] ~case_index
    end
    and odd_gist = lazy begin
      let even_gist = Type.Gist.rec' even_gist in
      let succ_even =
        let make even = Succ_even even in
        let get = function Succ_even even -> even in
        Type.Gist.case "Succ_even" make
        |> Type.Gist.comp even_gist get
        |> Type.Gist.finish
      in
      Type.Gist.variant ~name:"M.odd" [succ_even] ~case_index:(Fun.const 0)
    end
    in
    (Lazy.force even_gist), (Lazy.force odd_gist)
  in
  Snap.string (typedef even_gist) @> __POS_OF__
    "type M.even = | Zero | Succ_odd of odd";
  Snap.string (typedef odd_gist) @> __POS_OF__
    "type M.odd = | Succ_even of even";
  Snap.string (names even_gist) @> __POS_OF__
    "M.even M.odd";
  Snap.string (names odd_gist) @> __POS_OF__
    "M.odd M.even";
  let two = Succ_odd (Succ_even Zero) in
  let three = Succ_even (Succ_odd (Succ_even Zero)) in
  Snap.string (gpp even_gist two) @> __POS_OF__
    "Succ_odd Succ_even Zero";
  Snap.string (gpp odd_gist three) @> __POS_OF__
    "Succ_even Succ_odd Succ_even Zero";
  (* Have a look at rebinding over a mutual recursive definition. *)
  let odd_gist =
    let pp_odd ppf _even = Format.pp_print_string ppf "STOP" in
    let meta = Type.Gist.meta odd_gist in
    let meta = Fun.Generic.Fmt.add pp_odd meta in
    Type.Gist.update ~meta odd_gist
  in
  let even_gist = Type.Gist.rebind odd_gist even_gist in
  Snap.string (gpp even_gist two) @> __POS_OF__
    "Succ_odd STOP";
  Snap.string (gpp odd_gist three) @> __POS_OF__
    "STOP";
  Test.bool (Type.Gist.is_recursive even_gist) true;
  Test.bool (Type.Gist.is_recursive odd_gist) true;
  ()

let test_set =
  Test.test "Type.Gist.set" @@ fun () ->
  let module String_set = Set.Make (String) in
  let set_gist =
    Type.Gist.set "String_set.t" (module String_set) Type.Gist.binary_string
  in
  Snap.string (typedef set_gist) @> __POS_OF__
    "type String_set.t = string list";
  Test.bool (Type.Gist.is_recursive set_gist) false;
  ()

let test_custom_fmt =
  Test.test "Fun.Generic.Fmt.add" @@ fun () ->
  let us = [|Uchar.of_int 0x1F42B; Uchar.of_int 0x41|] in
  let uchar_debug ppf u = Format.fprintf ppf "U+%04X" (Uchar.to_int u) in
  let uchar_debug =
    let meta = Type.Gist.Meta.empty |> Fun.Generic.Fmt.add uchar_debug in
    Type.Gist.update ~meta Type.Gist.uchar
  in
  let uchars = Type.Gist.(array uchar) in
  let uchars_debug = Type.Gist.(array uchar_debug) in
  Snap.string (gpp uchars us) @> __POS_OF__ "[|'🐫'; 'A'|]";
  Snap.string (gpp uchars_debug us) @> __POS_OF__ "[|U+1F42B; U+0041|]";
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
