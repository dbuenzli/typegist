(*---------------------------------------------------------------------------
   Copyright (c) 2026 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Fmt = Typegist__fun_fmt.Fmt
let pp = Typegist__fun_fmt.pp

module Equal = Typegist__fun_equal.Equal
let equal = Typegist__fun_equal.equal

module Compare = Typegist__fun_compare.Compare
let compare = Typegist__fun_compare.compare

module Random = Typegist__fun_random.Random
let random = Typegist__fun_random.random

(* Left out for the first release see the [attic]
module Unmagic = Typegist__fun_unmagic.Unmagic
let unmagic = Typegist__fun_unmagic.unmagic
let safer_unmarshal = Typegist__fun_unmagic.safer_unmarshal *)
