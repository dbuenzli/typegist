(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Typegist

module Status = struct
  type t = Todo | Done | Cancelled
  let enum = ["Todo", Todo; "Done", Done; "Cancelled", Cancelled ]
  let gist = Type.Gist.variant_of_enum ~name:"Status.t" enum
  let pp = Fun.Generic.pp gist
end

module Item = struct
  type t = { task : string; status : Status.t; tags : string list }
  let make task status tags = { task; status; tags }
  let task i = i.task
  let status i = i.status
  let tags i = i.tags
  let gist =
    let v1 =
      Type.Gist.record "Item.t" make
      |> Type.Gist.field "task" Type.Gist.utf_8_string task
      |> Type.Gist.field "status" Status.gist status
      |> Type.Gist.field "tags" Type.Gist.(list utf_8_string) tags
      |> Type.Gist.finish
    in
    let v1 = Type.Gist.Abstract.Version.make ~name:"v1" v1 in
    Type.Gist.abstract "Item.t" [v1]

  let pp = Fun.Generic.pp gist
end

let () = Format.printf "%a@." Item.pp (Fun.Generic.random Item.gist ~size:4 ())
