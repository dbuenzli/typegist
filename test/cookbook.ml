(*---------------------------------------------------------------------------
   Copyright (c) 2024 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Typegist

(* Troubleshoot *)

(* Rebind *)

let uchar =
  let pp_uchar ppf u = Format.fprintf ppf "U+%04X" (Uchar.to_int u) in
  let meta = Type.Gist.meta Type.Gist.uchar in
  let meta = Fun.Generic.Fmt.add pp_uchar meta in
  Type.Gist.update ~meta Type.Gist.uchar

let pp g ppf v = Fun.Generic.pp (Type.Gist.rebind uchar g) v

(* Gist blueprints *)

(* Tuples *)

type point = float * float * float

let point_gist =
  Type.Gist.tuple ~name:"M.point" (fun x y z -> x, y, z)
  |> Type.Gist.comp Type.Gist.float (fun (x, _, _) -> x)
  |> Type.Gist.comp Type.Gist.float (fun (_, y, _) -> y)
  |> Type.Gist.comp Type.Gist.float (fun (_, _, z) -> z)
  |> Type.Gist.finish

let point_gist = Type.Gist.(t3 ~name:"M.point" float float float)

type sub = string * (int * int)
let sub_gist = Type.Gist.(t2 ~name:"M.sub" binary_string (t2 int int))

(* Records *)

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

(* Standard variants *)

type age = int option
let age = Type.Gist.option ~name:"M.age" Type.Gist.int

(* Enumerations *)

type status = Todo | Done | Cancelled
let status_enum = ["Todo", Todo; "Done", Done; "Cancelled", Cancelled]
let status_gist = Type.Gist.variant_of_enum ~name:"M.status" status_enum

(* Generic *)

type operation = Create | Delete of { id : int }
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

(* Generic with recursion *)

type nat = Zero | Succ of nat
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

(* GADT witness hiding *)

type 'a number =
| Int : int -> int number
| Float : float -> float number

type e_number = E : 'a number -> e_number

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

(* Strings *)

type username = string
let username_gist =
  let pp_bold ppf s = Format.fprintf ppf "@<0>\x1B[1m%s@<0>\x1B[0m" s in
  let meta = Type.Gist.Meta.empty |> Fun.Generic.Fmt.add pp_bold in
  Type.Gist.of_gist ~name:"username" ~meta Type.Gist.utf_8_string

(* Arrays *)

type samples = int array
let samples_gist = Type.Gist.array ~name:"samples" Type.Gist.int

(* Map likes *)

module String_map = Map.Make (String)
let int_string_map_gist =
  Type.Gist.(map "String_map.t" (module String_map) utf_8_string int)

(* Cell likes *)

type epoch = int Atomic.t
let epoch_gist = Type.Gist.atomic ~name:"M.epoch" Type.Gist.int

(* Functions *)

type predicate = char -> bool
let predicate_gist = Type.Gist.(func ~name:"predicate" char bool)

(* Abstract types *)

module _ = struct
  type person = string
  let person_gist_v1 = Type.Gist.(of_gist ~name:"M.person" utf_8_string)
  let person_gist =
    let v1 = Type.Gist.Abstract.Version.make ~name:"v1" person_gist_v1 in
    Type.Gist.abstract "M.person" [v1]
end

module _ = struct

type person = string * string
let person_gist_v2 = Type.Gist.(t2 ~name:"M.person" utf_8_string utf_8_string)

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

end

module _ = struct

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
end

(* Views *)

let queue_gist elt =
  let inject q = List.rev (Queue.fold (Fun.flip List.cons) [] q) in
  let project els =
    let q = Queue.create () in
    List.iter (Fun.flip Queue.add q) els; q
  in
  let name = Type.Gist.Name.make_applied [V elt] "Queue.t" in
  Type.Gist.view ~name ~inject ~project (Type.Gist.list elt)

(* Recursive types *)

type even = Zero | Succ_odd of odd
and odd = Succ_even of even
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

(* Polymorphic types *)

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree
let tree_gist : 'a Type.Gist.t -> 'a tree Type.Gist.t = fun elt ->
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

(* Overriding a generic function *)

type private_info = string
let private_info_gist =
  let pp ppf s = Format.pp_print_string ppf "<redacted for privacy>" in
  let name = "M.private_info" in
  let meta = Type.Gist.meta Type.Gist.utf_8_string in
  let meta = Fun.Generic.Fmt.add pp meta in
  Type.Gist.of_gist ~name ~meta Type.Gist.utf_8_string

(* Writing a generic function *)

type 'a op = 'a -> 'a -> unit

module Op = struct
  type 'a t = 'a op
  let ignore : 'a op = fun _ _ -> () (* if you have a way to express that *)
  module V = struct type nonrec ('a, 'b) t = 'b t end
  include Typegist__gist.Meta.Key (V)
end

let rec op_fields : type p a. (p, a) Type.Gist.Product.fields -> p op =
fun fs v0 v1 -> match fs with
| Ctor _ -> ()
| App (fs, f) ->
    op_fields fs v0 v1;
    let v0 = Type.Gist.Field.get f v0 in
    let v1 = Type.Gist.Field.get f v1 in
    let meta = Type.Gist.Field.meta f in
    match Op.find meta with
    | Some op -> op v0 v1
    | None ->
        match Type.Gist.Meta.Ignore.find meta with
        | Some true -> ()
        | _ -> op (Type.Gist.Field.gist f) v0 v1

and op_scalar : type s. s Type.Gist.Scalar.t -> s op =
fun s v0 v1 -> failwith "TODO"

and op_tuple : type t. t Type.Gist.Tuple.t -> t op =
fun t v0 v1 -> failwith "TODO"

and op_record : type r. r Type.Gist.Record.t -> r op =
fun r v0 v1 -> failwith "TODO"

and op_variant_like : type v. v Type.Gist.Variant_like.t -> v op =
fun v v0 v1 -> failwith "TODO"

and op_array_like :
type elt array. (elt, array) Type.Gist.Array_like.t -> array op =
fun a v0 v1 -> failwith "TODO"

and op_map_like :
type k v map. (k, v, map) Type.Gist.Map_like.t -> map op =
fun m v0 v1 -> failwith "TODO"

and op_cell_like : type a cell. (a, cell) Type.Gist.Cell_like.t -> cell op =
fun c v0 v1 -> failwith "TODO"

and op_func : type a b. (a, b) Type.Gist.Func.t -> (a -> b) op =
fun f v0 v1 -> failwith "TODO"

and op_abstract : type a. a Type.Gist.Abstract.t -> a op =
fun a v0 v1 -> failwith "TODO"

and op_view : type a b. (a, b) Type.Gist.View.t -> a op =
fun w v0 v1 -> failwith "TODO"

and op : type a. a Type.Gist.t -> a op =
fun g v0 v1 ->
  let meta = Type.Gist.meta g in
  match Op.find meta with
  | Some op -> op v0 v1
  | None ->
      match Type.Gist.Meta.Ignore.find meta with
      | Some true -> ()
      | _ ->
          match Type.Gist.expr g with
          | Scalar s -> op_scalar s v0 v1
          | Tuple t -> op_tuple t v0 v1
          | Record r -> op_record r v0 v1
          | Variant_like v -> op_variant_like v v0 v1
          | Array_like a -> op_array_like a v0 v1
          | Map_like m -> op_map_like m v0 v1
          | Cell_like c -> op_cell_like c v0 v1
          | Func f -> op_func f v0 v1
          | Abstract a -> op_abstract a v0 v1
          | View w -> op_view w v0 v1
          | Rec (lazy g) -> op g v0 v1
