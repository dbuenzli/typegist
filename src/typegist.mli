(*---------------------------------------------------------------------------
   Copyright (c) 2023 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Extended {!Stdlib.Type} and {!Stdlib.Fun} modules. Open to use it.

    Open the module to use it. This adds these modules

    {!modules:
    Type.Gist
    Fun.Generic}

    to the {!Stdlib.Type} and {!Stdlib.Fun} modules. *)

(** Type introspection. *)
module Type : sig
  include module type of Type (** @closed *)

  (** {1:gists Type gists} *)

  module Gist = Typegist__gist
end

(** Function manipulation and generic functions. *)
module Fun : sig
  include module type of Fun (** @closed *)

  module Generic = Typegist__fun_generic
end
