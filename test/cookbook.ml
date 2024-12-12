(*---------------------------------------------------------------------------
   Copyright (c) 2024 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Examples from the cookbook *)

(* Records *)

module Person = struct
  type t = { name : string; age : int option }
  let make name age = { name; age }
  let name p = p.name
  let age p = p.age

  open Typegist

  let gist =
    let name = Type.Gist.(field "name" string_as_utf_8) name in
    let age = Type.Gist.(field "age" (option int)) age in
    Type.Gist.(record "person" @@ ctor make * name * age)

  let pp = Fun.Generic.pp gist
end

(* Variants *)

module Status = struct
  type t = Todo | Done | Cancelled

  open Typegist

  let gist =
    let todo = Type.Gist.(case "Todo" @@ ctor Todo) in
    let done' = Type.Gist.(case "Done" @@ ctor Done) in
    let cancelled = Type.Gist.(case "Done" @@ ctor Cancelled) in
    let project = function
    | Todo -> todo | Done -> done' | Cancelled -> cancelled
    in
    let cases = [todo; done'; cancelled] in
    Type.Gist.variant "Status.t" project cases

  let pp = Fun.Generic.pp gist
end
