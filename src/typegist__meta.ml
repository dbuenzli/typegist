(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Abstracting type constructors with two parameter.
   See Yallop and White Lightweight Higher-Kinded Polymorphism
   https://doi.org/10.1007/978-3-319-07151-0_8 *)
module Higher = struct
  (*  Snippet from https://github.com/yallop/higher
      Copyright (c) 2013 Leo White and Jeremy Yallop
      SPDX-License-Identifier: MIT *)
  type ('a, 'f) app
  module Newtype2 (T : sig type ('a, 'b) t end) = struct
    type ('a, 'b) s = ('a, 'b) T.t
    type t
    external inj : 'a -> 'b = "%identity"
    external prj : 'a -> 'b = "%identity"
  end
end

module M = Map.Make (Int)
type 't mkey = 't Type.Id.t
type ('a, 'b, 't) value = ('a, ('b, 't) Higher.app) Higher.app
type ('a, 'b) binding = B : 't mkey * ('a, 'b, 't) value -> ('a, 'b) binding
type ('a, 'b) t2 = ('a, 'b) binding M.t
type 'a t = ('a, 'a) binding M.t

module type VALUE = sig type ('a, 'b) t end
module type KEY = sig
  type ('a, 'b) meta := ('a, 'b) t2
  type ('a, 'b) value
  val mem : ('a, 'b) meta -> bool
  val add : ('a, 'b) value -> ('a, 'b) meta -> ('a, 'b) meta
  val find : ('a, 'b) meta -> ('a, 'b) value option
  val remove : ('a, 'b) meta -> ('a, 'b) meta
end


let empty = M.empty
let is_empty = M.is_empty
let mem (module K : KEY) m = K.mem m
let add (module K : KEY) v m = K.add v m
let find (module K : KEY) m = K.find m
let remove (module K : KEY) m = K.remove m

module Key (V : VALUE) = struct
  type ('a, 'b) meta = ('a, 'b) t2
  module H = Higher.Newtype2 (V)
  type ('a, 'b) value = ('a, 'b) H.s
  type t = H.t
  let key = Type.Id.make ()
  let mem m = M.mem (Type.Id.uid key) m
  let add v m = M.add (Type.Id.uid key) (B (key, H.inj v)) m
  let remove m = M.remove (Type.Id.uid key) m
  let find m =
    let find : type v a b. v mkey -> (a, b) meta -> (a, b) value option =
    fun key m -> match M.find_opt (Type.Id.uid key) m with
    | None -> None
    | Some B (k', v) ->
        match Type.Id.provably_equal key k' with
        | Some Equal -> Some (H.prj v) | None -> assert false
    in
    find key m
end

type key = (module KEY)

module Default = Key (struct type ('a, 'b) t = 'b end)
let default = (module Default : KEY)

module Deprecated = Key (struct type ('a, 'b) t = string end)
module Ignore = Key (struct type ('a, 'b) t = bool end)
module Immediate = Key (struct type ('a, 'b) t = bool end)
module Immediate64 = Key (struct type ('a, 'b) t = bool end)
module Min = Key (struct type ('a, 'b) t = 'b end)
module Max = Key (struct type ('a, 'b) t = 'b end)
module Unboxed = Key (struct type ('a, 'b) t = bool end)
