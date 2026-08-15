(*---------------------------------------------------------------------------
   Copyright (c) 2023 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Type = struct
  include Type
  module Gist = Typegist__gist
end

module Fun = struct
  include Stdlib.Fun
  module Generic = Typegist__fun_generic
end
