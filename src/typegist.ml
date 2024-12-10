(*---------------------------------------------------------------------------
   Copyright (c) 2023 The typegist programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Type = struct

  (* As per Type module of OCaml 5.1 *)

  type (_, _) eq = Equal : ('a, 'a) eq
  module Id = struct
    type _ id = ..
    module type ID = sig type t type _ id += Id : t id end
    type 'a t = (module ID with type t = 'a)

    let make (type a) () : a t =
      (module struct type t = a type _ id += Id : t id end)

    let provably_equal
        (type a b) ((module A) : a t) ((module B) : b t) : (a, b) eq option
      =
      match A.Id with B.Id -> Some Equal | _ -> None

    let uid (type a) ((module A) : a t) =
      Obj.Extension_constructor.id (Obj.Extension_constructor.of_val A.Id)
  end

  (* Type gists *)

  module Gist = struct

    (* Metadata *)

    module Meta = struct
      (* Abstracting type constructors with one parameter. So that
         heterogeneous values in ['a Meta.t] can depend on ['a].
         See Yallop and White Lightweight Higher-Kinded Polymorphism
         https://doi.org/10.1007/978-3-319-07151-0_8 *)
      module Higher = struct
        (*  Snippet from https://github.com/yallop/higher
            MIT licensed Copyright (c) 2013 Leo White and Jeremy Yallop *)
        type ('a, 'f) app
        module Newtype1 (T : sig type 'a t end) = struct
          type 'a s = 'a T.t
          type t
          external inj : 'a -> 'b = "%identity"
          external prj : 'a -> 'b = "%identity"
        end
      end

      module M = Map.Make (Int)
      type 't key = 't Id.t
      type ('a, 't) value = ('a, 't) Higher.app
      type 'a binding = B : 't key * ('a, 't) value -> 'a binding
      type 'a t = 'a binding M.t
      let empty = M.empty
      let is_empty = M.is_empty

      module type VALUE = sig type 'a t end
      module type KEY = sig
        type 'a meta := 'a t
        type 'a value
        val mem : 'a meta -> bool
        val add : 'a value -> 'a meta -> 'a meta
        val find : 'a meta -> 'a value option
        val remove : 'a meta -> 'a meta
      end
      module Key (V : VALUE) = struct
        type 'a meta = 'a t
        module H = Higher.Newtype1 (V)
        type 'a value = 'a H.s
        type t = H.t
        let key = Id.make ()
        let mem m = M.mem (Id.uid key) m
        let add v m = M.add (Id.uid key) (B (key, H.inj v)) m
        let remove m = M.remove (Id.uid key) m
        let find m =
          let find : type v a. v key -> a meta -> a value option =
          fun key m -> match M.find_opt (Id.uid key) m with
          | None -> None
          | Some B (k', v) ->
              match Id.provably_equal key k' with
              | None -> assert false | Some Equal -> Some (H.prj v)
          in
          find key m
      end
      module Doc = Key (struct type 'a t = string end)

      let make ~doc = Doc.add doc M.empty
    end

    (* Interfaces *)

    module type ARRAY = sig
      type t
      type elt
      val get : t -> int -> elt
      val set : t -> int -> elt -> unit
      val length : t -> int
      val init : int -> (int -> elt) -> t
      val iter : (elt -> unit) -> t -> unit
      val fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a
      val type_name : string
    end

    type ('elt, 'arr) array_module =
      (module ARRAY with type t = 'arr and type elt = 'elt)

    module type MAP = sig
      type t
      type key
      type value
      val empty : t
      val mem : key -> t -> bool
      val add : key -> value -> t -> t
      val remove : key -> t -> t
      val find_opt : key -> t -> value option
      val fold : (key -> value -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val type_name : string
    end

    type ('k, 'v, 'm) map_module =
      (module MAP with type t = 'm and type key = 'k and type value = 'v)

    (* Type representation *)

    type 'a scalar =
    | Unit : unit Meta.t -> unit scalar
    | Bool : bool Meta.t -> bool scalar
    | Char : char Meta.t -> char scalar
    | Uchar : Uchar.t Meta.t -> Uchar.t scalar
    | Int : int Meta.t -> int scalar
    | Int32 : int32 Meta.t -> int32 scalar
    | Int64 : int64 Meta.t -> int64 scalar
    | Nativeint : nativeint Meta.t -> nativeint scalar
    | Float : float Meta.t -> float scalar

    type ('a, 'elt) arraylike =
    | String : string Meta.t * char t -> (char, string) arraylike
    | Bytes : bytes Meta.t * char t -> (char, bytes) arraylike
    | Array : 'elt array Meta.t * 'elt t -> ('elt, 'elt array) arraylike
    | Bigarray1 :
        ('elt, 'b, 'c) Bigarray.Array1.t Meta.t *
        ('elt, 'b) Bigarray.kind * 'c Bigarray.layout * 'elt t ->
        ('elt, ('elt, 'b, 'c) Bigarray.Array1.t) arraylike
    | Array_module :
        'arr Meta.t * ('elt, 'arr) array_module * 'elt t ->
        ('elt, 'arr) arraylike

    and ('k, 'v, 'm) maplike =
    | Hashtbl :
        ('k, 'v) Hashtbl.t Meta.t * 'k t * 'v t ->
        ('k, 'v, ('k, 'v) Hashtbl.t) maplike
    | Map_module :
        'm Meta.t * ('k, 'v, 'm) map_module * 'k t * 'v t ->
        ('k, 'v, 'm) maplike

    and ('p, 'f) field =
      { meta : 'f Meta.t;
        name : string;
        gist : 'f t;
        project : ('p -> 'f);
        inject : ('p -> 'f -> 'p) option;
        set : ('p -> 'f -> unit) option;
        default : 'f option; }

    and ('t, _) fields =
    | Ctor : 'a -> ('t, 'a) fields
    | App : ('t, 'f -> 'a) fields * ('t, 'f) field -> ('t, 'a) fields

    and 'p product =
      { meta : 'p Meta.t;
        name : string;
        fields : ('p, 'p) fields }

    and 'r record = 'r product
    and 'v variant =
      { meta : 'v Meta.t;
        type_name : string;
        project : 'v -> 'v product;
        cases : 'v product list; }

    and 's sum =
    | Option : 'a option Meta.t * 'a t -> 'a option sum
    | Either : ('a, 'b) Either.t Meta.t * 'a t * 'b t -> ('a, 'b) Either.t sum
    | Result : ('a, 'b) result Meta.t * 'a t * 'b t -> ('a, 'b) result sum
    | List : 'a list Meta.t * 'a t -> 'a list sum
    | Variant : 'v variant -> 'v sum

    and ('a, 'b) func =
      { meta : ('a -> 'b) Meta.t;
        domain : 'a t;
        range : 'b t }

    and ('a, 'repr) abstract_repr =
      { meta : 'repr Meta.t;
        version : string;
        gist : 'repr t;
        inject : 'a -> 'repr;
        project : 'repr -> 'a; }

    and 'a abstract_repr_exists =
    | Repr : ('a, 'repr) abstract_repr -> 'a abstract_repr_exists

    and 'a abstract =
      { meta : 'a Meta.t;
        type_name : string;
        reprs : 'a abstract_repr_exists list; }

    and 'a t =
    | Scalar : 'a scalar -> 'a t
    | Arraylike : ('elt, 'arr) arraylike -> 'arr t
    | Maplike : ('k, 'v, 'm) maplike -> 'm t
    | Product : 'p product -> 'p t
    | Record : 'r record -> 'r t
    | Sum : 's sum -> 's t
    | Func : ('a, 'b) func -> ('a -> 'b) t
    | Abstract : 'a abstract -> 'a t
    | Lazy : 'a lazy_t Meta.t * 'a t -> 'a lazy_t t
    | Ref : 'a ref Meta.t * 'a t -> 'a ref t
    | Rec : 'a t lazy_t -> 'a t

    (* Constructors and helpers *)

    let todo ?(type_name = "<unknown>") () =
      Rec (lazy (invalid_arg ("TODO: " ^ type_name ^ " gist")))

    let rec' lg = Rec lg
    let ref ?(meta = Meta.empty) g = Ref (meta, g)
    let lazy' ?(meta = Meta.empty) g = Lazy (meta, g)

    (* Scalars *)

    module Scalar = struct
      type 'a t = 'a scalar
      let meta : type a. a t -> a Meta.t = function
      | Unit m -> m | Bool m -> m | Char m -> m  | Uchar m -> m | Int m -> m
      | Int32 m -> m | Int64 m -> m | Nativeint m -> m | Float m -> m

      let zero : type a. a t -> a = function
      | Unit _ -> () | Bool _ -> false | Char _ -> '\x00'
      | Uchar _ -> Uchar.of_int 0x0000 | Int _ -> 0 | Int32 _ -> 0l
      | Int64 _ -> 0L | Nativeint _ -> 0n | Float _ -> 0.0

      let type_name : type a. a t -> string = function
      | Unit _ -> "unit" | Bool _ -> "bool" | Char _ -> "char"
      | Uchar _ -> "Uchar.t" | Int _ -> "int" | Int32 _ -> "int32"
      | Int64 _ -> "int64" | Nativeint _ -> "nativeint" | Float _ -> "float"

      let equal : type a. a t -> a -> a -> bool = function
      | Unit _ -> Unit.equal | Bool _ -> Bool.equal | Char _ -> Char.equal
      | Uchar _ -> Uchar.equal | Int _ -> Int.equal
      | Int32 _ -> Int32.equal | Int64 _ -> Int64.equal
      | Nativeint _ -> Nativeint.equal | Float _ -> Float.equal

      let compare : type a. a t -> a -> a -> int = function
      | Unit _ -> Unit.compare | Bool _ -> Bool.compare | Char _ -> Char.compare
      | Uchar _ -> Uchar.compare | Int _ -> Int.compare
      | Int32 _ -> Int32.compare | Int64 _ -> Int64.compare
      | Nativeint _ -> Nativeint.compare | Float _ -> Float.compare

      let char_to_string c = (* TODO should we rather interpret as latin1 ? *)
        if Char.code c <= 0x7F then String.make 1 c else
        Printf.sprintf "\\x%02x" (Char.code c)

      let uchar_to_string u =
        let b = Bytes.create (Uchar.utf_8_byte_length u) in
        ignore (Bytes.set_utf_8_uchar b 0 u);
        Bytes.unsafe_to_string b

      let to_string : type a. a t -> a -> string = function
      | Unit _ -> Unit.to_string | Bool _ -> Bool.to_string
      | Char _ -> char_to_string | Uchar _ -> uchar_to_string
      | Int _ -> Int.to_string | Int32 _ -> Int32.to_string
      | Int64 _ -> Int64.to_string | Nativeint _ -> Nativeint.to_string
      | Float _ -> Float.to_string

      let pp : type a. a t -> Format.formatter -> a -> unit =
      fun g -> match g with
      | Unit _ -> fun ppf _ -> Format.pp_print_string ppf "()"
      | Bool _ -> Format.pp_print_bool
      | Char _ -> Format.pp_print_char
      | Uchar _ -> fun ppf v -> Format.fprintf ppf "@<1>%s" (uchar_to_string v)
      | Int _ -> Format.pp_print_int
      | Int32 _ -> fun ppf v -> Format.fprintf ppf "%ld" v
      | Int64 _ -> fun ppf v -> Format.fprintf ppf "%Ld" v
      | Nativeint _ -> fun ppf v -> Format.fprintf ppf "%nd" v
      | Float _ -> fun ppf v -> Format.fprintf ppf "%g" v

      let with_meta : type a. a Meta.t -> a t -> a t = fun m s -> match s with
      | Unit _ -> Unit m | Bool _ -> Bool m | Char _ -> Char m
      | Uchar _ -> Uchar m | Int _ -> Int m | Int32 _ -> Int32 m
      | Int64 _ -> Int64 m | Nativeint _ -> Nativeint m | Float _ -> Float m
    end

    let unit = Scalar (Unit Meta.empty)
    let bool = Scalar (Bool Meta.empty)
    let char = Scalar (Char Meta.empty)
    let uchar = Scalar (Uchar Meta.empty)
    let int = Scalar (Int Meta.empty)
    let int32 = Scalar (Int32 Meta.empty)
    let int64 = Scalar (Int64 Meta.empty)
    let nativeint = Scalar (Nativeint Meta.empty)
    let float = Scalar (Float Meta.empty)

    (* Arraylike *)

    module Arraylike = struct
      type 'a gist = 'a t
      type ('elt, 'arr) t = ('elt, 'arr) arraylike
      let meta : type elt arr. (elt, arr) arraylike -> arr Meta.t = function
      | String (m, _) -> m | Bytes (m, _) -> m | Array (m, _) -> m
      | Bigarray1 (m, _, _, _) -> m | Array_module (m, _, _) -> m

      let type_name : type elt arr. (elt, arr) arraylike -> string = function
      | String (_, _) -> "string" | Bytes (_, _) -> "bytes"
      | Array (_, _) -> "array" | Bigarray1 (_, _, _, _) -> "bigarray1"
      | Array_module (_, (module A), _) -> A.type_name

      let elt : type elt arr. (elt, arr) arraylike -> elt gist = function
      | String (_, elt) -> elt | Bytes (_, elt) -> elt
      | Array (_, elt) -> elt | Bigarray1 (_, _, _, elt) -> elt
      | Array_module (_, _, elt) -> elt

      let with_meta :
        type elt arr. arr Meta.t -> (elt, arr) arraylike ->
        (elt, arr) arraylike
      =
      fun m a -> match a with
      | String (_, elt) -> String (m, elt) | Bytes (_, elt) -> Bytes (m, elt)
      | Array (_, elt) -> Array (m, elt)
      | Bigarray1 (_, k, l, elt) -> Bigarray1 (m, k, l, elt)
      | Array_module (_, a, elt) -> Array_module (m, a, elt)

      (* Generic array modules for the specialisation. *)

      module String_array_module = struct
        include String
        type t = string
        type elt = char
        let set s i elt = invalid_arg "Strings are immutable"
        let type_name = "string"
      end

      module Bytes_array_module = struct
        include Bytes
        type t = bytes
        type elt = char
        let type_name = "bytes"
      end

      let array_array_module :
        type elt. elt gist -> (elt, elt array) array_module
        =
        fun elt ->
        let module Array_module = struct
          include Array
          type t = elt array
          type nonrec elt = elt
          let type_name = "array"
        end
        in
        (module Array_module)

      let ba_iter f a =
          for i = 0 to Bigarray.Array1.dim a - 1 do
            f (Bigarray.Array1.unsafe_get a i)
          done

      let ba_fold_left f init a =
        let acc = Stdlib.ref init in
        for i = 0 to Bigarray.Array1.dim a - 1 do
          acc := (f !acc (Bigarray.Array1.unsafe_get a i))
        done;
        !acc

      let bigarray1_array_module :
        type elt a b.
        (elt, a) Bigarray.kind -> b Bigarray.layout ->
        elt gist -> (elt, (elt, a, b) Bigarray.Array1.t) array_module
        =
        fun kind layout elt ->
        let module Array_module = struct
          type t = (elt, a, b) Bigarray.Array1.t
          type nonrec elt = elt
          let get = Bigarray.Array1.get
          let set = Bigarray.Array1.set
          let length = Bigarray.Array1.dim
          let init = Bigarray.Array1.init kind layout
          let iter = ba_iter
          let fold_left = ba_fold_left
          let type_name = "array"
        end
        in
        (module Array_module)

      let to_array_module :
        type elt arr. (elt, arr) arraylike -> (elt, arr) array_module =
        function
        | Array_module (_, m, _) -> m
        | String (_, _) -> (module String_array_module)
        | Bytes (_, _) -> (module Bytes_array_module)
        | Array (_, elt) -> array_array_module elt
        | Bigarray1 (_, k, l, elt) -> bigarray1_array_module k l elt
    end

    let string = Arraylike (String (Meta.empty, char))
    let bytes = Arraylike (Bytes (Meta.empty, char))
    let array ?(meta = Meta.empty) g = Arraylike (Array (meta, g))
    let bigarray1 ?(meta = Meta.empty) kind layout g =
      Arraylike (Bigarray1 (meta, kind, layout, g))

    let array_module ?(meta = Meta.empty) m g =
      Arraylike (Array_module (meta, m, g))

    (* Maplike *)

    module Maplike = struct
      type 'a gist = 'a t
      type ('k, 'v, 'm) t = ('k, 'v, 'm) maplike
      let meta : type k v m. (k, v, m) maplike -> m Meta.t = function
      | Hashtbl (m, _, _) -> m | Map_module (m, _, _, _) -> m

      let type_name : type k v m. (k, v, m) maplike -> string = function
      | Hashtbl (m, _, _) -> "Hashtbl.t"
      | Map_module (_, (module M), _, _) -> M.type_name

      let key : type k v m. (k, v, m) maplike -> k gist = function
      | Hashtbl (_, k, _) | Map_module (_, _, k, _) -> k

      let value : type k v m. (k, v, m) maplike -> v gist = function
      | Hashtbl (_, _, v) | Map_module (_, _, _, v) -> v

      let with_meta :
        type k v m. m Meta.t -> (k, v, m) maplike -> (k, v, m) maplike
      =
      fun m map -> match map with
      | Hashtbl (m, k,  v) -> Hashtbl (m, k, v)
      | Map_module (m, map, k, v) -> Map_module (m, map, k, v)

      module type VALUE = sig
        type t
        val equal : t -> t -> bool
        val compare : t -> t -> int
      end
      module Map_module_of_map (V : VALUE) (M : Map.S) : MAP
        with type t = V.t M.t
         and type key = M.key
         and type value = V.t =
      struct
        type t = V.t M.t
        type key = M.key
        type value = V.t
        let empty = M.empty
        let mem = M.mem
        let add = M.add
        let remove = M.remove
        let find_opt = M.find_opt
        let fold = M.fold
        let equal = M.equal V.equal
        let compare = M.compare V.compare
        let type_name = "Map.t"
      end
    end

    let hashtbl ?(meta = Meta.empty) k v = Maplike (Hashtbl (meta, k, v))
    let map_module ?(meta = Meta.empty) m k v =
      Maplike (Map_module (meta, m, k, v))

    (* Fields *)

    module Field = struct
      type ('t, 'f) t = ('t, 'f) field
      let make
          ?(meta = Meta.empty) ?(name = "") ?inject ?set ?default gist project
        =
        { meta; name; gist; project; inject; set; default }

      let meta f = f.meta
      let name f = f.name
      let gist f = f.gist
      let project f = f.project
      let inject f = f.inject
      let set f = f.set
      let default f = f.default
    end

    module Fields = struct
      type ('p, 'a) t = ('p, 'a) fields
      let is_empty = function Ctor _ -> true | _ -> false
      let is_singleton = function App (Ctor _, _) -> true | _ -> false
    end

    let ctor f = Ctor f
    let app fs a = App (fs, a)
    let ( * ) = app

    (* Products *)

    module Product = struct
      type 'p t = 'p product
      let make ?(meta = Meta.empty) ?(name = "") fields =
        { meta; name; fields }
      let meta (p : 'p t) = p.meta
      let name (p : 'p t) = p.name
      let fields (p : 'p t) = p.fields
      let is_empty (p : 'v t) = Fields.is_empty p.fields
      let is_singleton (p : 'v t) = Fields.is_singleton p.fields
      let with_meta meta (p : 'v t) = { p with meta }
    end

    let dim ?meta ?name ?inject ?default gist project =
      Field.make ?meta ?name ?inject ?default gist project

    let product ?meta ?type_name:name fields =
      Product (Product.make ?meta ?name fields)

    let p2 ?meta ?type_name g0 g1 =
      let p2 v0 v1 = v0, v1 in
      let d0 = dim g0 fst and d1 = dim g1 snd in
      product ?meta ?type_name @@ ctor p2 * d0 * d1

    let p3 ?meta ?type_name g0 g1 g2 =
      let p3 v0 v1 v2 = v0, v1, v2 in
      let d0 = dim g0 (fun (v, _, _) -> v) in
      let d1 = dim g1 (fun (_, v, _) -> v) in
      let d2 = dim g2 (fun (_, _, v) -> v) in
      product ?meta ?type_name @@ ctor p3 * d0 * d1 * d2

    let p4 ?meta ?type_name g0 g1 g2 g3 =
      let p4 v0 v1 v2 v3 = v0, v1, v2, v3 in
      let d0 = dim g0 (fun (v, _, _, _) -> v) in
      let d1 = dim g1 (fun (_, v, _, _) -> v) in
      let d2 = dim g2 (fun (_, _, v, _) -> v) in
      let d3 = dim g3 (fun (_, _, _, v) -> v) in
      product ?meta ?type_name @@ ctor p4 * d0 * d1 * d2 * d3

    (* Records *)

    let field ?(meta = Meta.empty) ?inject ?set ?default name gist project =
      { meta; name; gist; project; inject; set; default }

    let record ?meta name fields = Record (Product.make ?meta ~name fields)

    (* Variants *)

    module Variant = struct
      type 'v case = 'v product
      module Case = struct
        type 'v t = 'v case
        let make ?(meta = Meta.empty) name fields = { meta; name; fields }
        let meta (c : 'v t) = c.meta
        let name (c : 'v t) = c.name
        let fields (c : 'v t) = c.fields
        let is_empty = Product.is_empty
        let rec_field_count (c : 'v t) =
          let rec loop : type p a. int -> (p, a) fields -> int =
            fun acc fs -> match fs with
            | Ctor _ -> acc
            | App (c, f) -> loop (acc + match f.gist with Rec _ -> 1 | _ -> 0) c
          in
          loop 0 c.fields
      end

      type 'v t = 'v variant

      let make ?(meta = Meta.empty) type_name project cases =
        { meta; type_name; project; cases }

      let meta (v : 'v t) = v.meta
      let type_name (v : 'v t) = v.type_name
      let project (v : 'v t) = v.project
      let cases (v : 'v t) = v.cases
      let case_count (v : 'v t) = List.length v.cases
      let get (v : 'v t) i = List.nth v.cases i

      (* Generalizes the specific cases of sum to variant descriptions.
         TODO add the parameter names in the resulting type names ? *)

      let none_case = { meta = Meta.empty; name = "None"; fields = Ctor None }
      let of_option ?meta a =
        let some_ctor v = Some v in
        let some_proj = function Some v -> v | _ -> assert false in
        let some_case = Case.make "Some" (ctor some_ctor * dim a some_proj) in
        let option_proj = function None -> none_case | Some _ -> some_case in
        make ?meta "option" option_proj [none_case; some_case]

      let of_either ?meta l r =
        let left_ctor v = Either.Left v in
        let left_proj = function Either.Left v -> v | _ -> assert false in
        let left_dim = dim l left_proj in
        let left_case = Case.make "Either.Left" (ctor left_ctor * left_dim) in
        let right_ctor v = Either.Right v in
        let right_proj = function Either.Right v -> v | _ -> assert false in
        let right_dim = dim r right_proj in
        let right_case = Case.make "Either.Right"(ctor right_ctor * right_dim)in
        let either_proj = function
        | Either.Left _ -> left_case | Right _ -> right_case
        in
        make ?meta "Either.t" either_proj [left_case; right_case]

      let of_result ?meta a b =
        let ok_ctor v = Ok v in
        let ok_proj = function Ok v -> v | _ -> assert false in
        let ok_case = Case.make "Ok" (ctor ok_ctor * dim a ok_proj) in
        let error_ctor e = Error e in
        let error_proj = function Error v -> v | _ -> assert false in
        let error_case = Case.make "Error"(ctor error_ctor * dim b error_proj)in
        let result_proj = function Ok _ -> ok_case | Error _ -> error_case in
        make ?meta "result" result_proj [ok_case; error_case]

      let empty_case = { meta = Meta.empty; name = "[]"; fields = Ctor [] }
      let of_list = fun ?meta a ->
        let rec g = lazy begin
          let cons_ctor x xs = x :: xs in
          let cons_head = function x :: _ -> x | _ -> assert false in
          let cons_tail = function _ :: xs -> xs | _ -> assert false in
          let cons_hdim = dim a cons_head in
          let self = Rec (lazy (Sum (Variant (Lazy.force g)))) in
          let cons_tdim = dim self cons_tail in
          let cons_prod = ctor cons_ctor * cons_hdim * cons_tdim in
          let cons_case = Case.make "::" cons_prod in
          let list_proj = function [] -> empty_case | _ -> cons_case in
          make ?meta "list" list_proj [empty_case; cons_case]
        end
        in
        Lazy.force g
    end

    let case ?(meta = Meta.empty) name fields = { meta; name; fields }
    let variant ?(meta = Meta.empty) type_name project cases =
      Sum (Variant { meta; type_name; project; cases })

    (* Sums *)

    module Sum = struct
      type 's t = 's sum

      let meta : type s. s sum -> s Meta.t = function
      | Option (m, _) -> m
      | Either (m, _, _) -> m
      | Result (m, _, _) -> m
      | List (m, _) -> m
      | Variant v -> Variant.meta v

      let type_name : type s. s sum -> string = function
      | Option (_, _) -> "option" | Either (_, _, _) -> "Either.t"
      | Result (_, _, _) -> "result" | List (_, _) -> "list"
      | Variant v -> Variant.type_name v

      let to_variant : type s. s sum -> s variant = function
      | Option (meta, a) -> Variant.of_option ~meta a
      | Either (meta, l, r) -> Variant.of_either ~meta l r
      | Result (meta, a, b) -> Variant.of_result ~meta a b
      | List (meta, a) -> Variant.of_list ~meta a
      | Variant v -> v

      let with_meta : type s. s Meta.t -> s sum -> s sum = fun meta s ->
      match s with
      | Option (_, v) -> Option (meta, v)
      | Either (_, l, r) -> Either (meta, l, r)
      | Result (_, a, b) -> Result (meta, a, b)
      | List (_, a) -> List (meta, a)
      | Variant v -> Variant { v with meta }
    end

    let option ?(meta = Meta.empty) v = Sum (Option (meta, v))
    let either ?(meta = Meta.empty) l r = Sum (Either (meta, l, r))
    let result ?(meta = Meta.empty) v e = Sum (Result (meta, v, e))
    let list ?(meta = Meta.empty) v = Sum (List (meta, v))

    (* Functions *)

    module Func = struct
      type ('a, 'b) t = ('a, 'b) func
      let make ?(meta = Meta.empty) domain range = { meta; domain; range }
      let meta (f : ('a, 'b) t) = f.meta
      let domain f = f.domain
      let range f = f.range
      let with_meta meta (f : ('a, 'b) t) = { f with meta }
    end

    let func ?meta d r = Func (Func.make ?meta d r)
    let ( @-> ) d r = func d r

    (* Abstract *)

    module Abstract = struct
      module Repr = struct
        type ('a, 'repr) t = ('a, 'repr) abstract_repr
        let make ?(meta = Meta.empty) ~version gist inject project =
          { meta; version; gist; inject; project }

        let meta (r : ('a, 'repr) t) = r.meta
        let version r = r.version
        let gist (r : ('a, 'repr) t) = r.gist
        let inject (r : ('a, 'repr) t) = r.inject
        let project (r : ('a, 'repr) t) = r.project
        let with_meta meta (r : ('a, 'repr) t) = { r with meta}
      end

      type 'a repr = 'a abstract_repr_exists =
      | Repr : ('a, 'repr) Repr.t -> 'a repr

      let repr ?meta ~version gist inject project =
        Repr (Repr.make ?meta ~version gist inject project)

      type 'a t = 'a abstract
      let make ?(meta = Meta.empty) type_name ~reprs =
        { meta; type_name; reprs }

      let meta (a : 'a t) = a.meta
      let type_name (a : 'a t) = a.type_name
      let reprs (a : 'a t) = a.reprs
      let with_meta meta (a : 'a t) = { a with meta }
    end

    let abstract ?meta name reprs = Abstract (Abstract.make ?meta name ~reprs)

    (* Gists *)

    let rec meta : type a. a t -> a Meta.t = function
    | Scalar s -> Scalar.meta s | Arraylike a -> Arraylike.meta a
    | Maplike m -> Maplike.meta m | Product p -> Product.meta p
    | Record r -> Product.meta r | Sum s -> Sum.meta s | Func f -> Func.meta f
    | Abstract a -> Abstract.meta a
    | Lazy (m, l) -> m | Ref (m, r) -> m
    | Rec r -> meta (Lazy.force r)

    let rec with_meta : type a. a Meta.t -> a t -> a t = fun m g -> match g with
    | Scalar s -> Scalar (Scalar.with_meta m s)
    | Arraylike a -> Arraylike (Arraylike.with_meta m a)
    | Maplike map -> Maplike (Maplike.with_meta m map)
    | Product p -> Product (Product.with_meta m p)
    | Record r -> Record (Product.with_meta m r)
    | Sum s -> Sum (Sum.with_meta m s)
    | Func f -> Func (Func.with_meta m f)
    | Abstract a -> Abstract (Abstract.with_meta m a)
    | Lazy (_, l) -> Lazy (m, l) | Ref (m, r) -> Ref (m, r)
    | Rec r -> with_meta m (Lazy.force r)

    let rec type_name : type a. a t -> string = function
    | Scalar s -> Scalar.type_name s | Arraylike a -> Arraylike.type_name a
    | Maplike m -> Maplike.type_name m
    | Product p -> Product.name p
    | Record r -> Product.name r
    | Sum s -> Sum.type_name s
    | Func f -> "<fun>" (* XXX fixme *)
    | Abstract a -> Abstract.type_name a
    | Lazy (_, l) -> type_name l ^ "_lazy" | Ref (m, r) -> type_name r ^ "_ref"
    | Rec r -> type_name (Lazy.force r)

    type 'a fmt = Format.formatter -> 'a -> unit
    let pf = Format.fprintf
    let pp_string = Format.pp_print_string
    let pp_type ppf g =
      (* FIXME The ref naming scheme should likely be improved and spelled
         out in the docs. *)
      let rec arraylike : type a elt. ref:bool -> (a, elt) arraylike fmt =
        fun ~ref ppf -> function
        | String (_, _) -> pp_string ppf "string"
        | Bytes (_, _) -> pp_string ppf "bytes"
        | Array (_, elt) -> pf ppf "@[@[%a@] array@]" (pp ~ref:true) elt
        | Bigarray1 (_, _, _, elt) ->
            pf ppf "@[@[%a@] Bigarray.Array1.t@]" (pp ~ref:true) elt
        | Array_module (_, (module A), elt) ->
            pf ppf "@[@[%a@] %s@]" (pp ~ref:true) elt A.type_name

      and maplike : type m k v. ref:bool -> (m, k, v) maplike fmt =
        fun ~ref ppf -> function
        | Hashtbl (m, k, v) ->
            let pp_k = pp ~ref:true and pp_v = pp ~ref:true in
            pf ppf "@[@[<1>(%a,@ %a)@] Hasthbl.t@]" pp_k k pp_v v
        | Map_module (_, (module M), k, v) ->
            let pp_k = pp ~ref:true and pp_v = pp ~ref:true in
            pf ppf "@[@[<1>(%a,@ %a)@] %s@]" pp_k k pp_v v M.type_name

      and product : type p. ref:bool -> paren:bool -> p product fmt =
        fun ~ref ~paren ppf p ->
        let field ppf f = pf ppf "@[%a@]" (pp ~ref:true) (Field.gist f) in
        let rec fields : type p a. (p, a) fields fmt = fun ppf fs ->
          match fs with
          | Ctor _ -> () | App (Ctor _, f) -> field ppf f
          | App (fs, f) -> fields ppf fs; pf ppf " *@ %a" field f
        in
        if ref && Product.name p <> "" then pf ppf "%s" (Product.name p) else
        if paren
        then pf ppf "@[<1>(%a)@]" fields (Product.fields p)
        else pf ppf "@[<1>%a@]" fields (Product.fields p)

      and record : type r. ref:bool -> r record fmt = fun ~ref ppf r ->
        let field ppf f =
          pf ppf "@[%s :@ %a@]" (Field.name f) (pp ~ref:true) (Field.gist f)
        in
        let rec fields : type p a. (p, a) fields fmt = fun ppf fs ->
          match fs with
          | Ctor _ -> () | App (Ctor _, f) -> field ppf f
          | App (fs, f) -> fields ppf fs; pf ppf ";@,%a" field f
        in
        if ref && Product.name r <> "" then pf ppf "%s" (Product.name r) else
        pf ppf "@[<v2>{ %a@] }" fields (Product.fields r)

      and sum : type a. ref:bool -> a sum fmt = fun ~ref ppf s -> match s with
      | Option (m, a) -> pf ppf "%a option" (pp ~ref:true) a
      | Either (m, a, b) ->
          let pp_a = pp ~ref:true and pp_b = pp ~ref:true in
          pf ppf "@[@[<1>(%a,@ %a)@] Either.t@]" pp_a a pp_b b
      | Result (m, a, b) ->
          let pp_a = pp ~ref:true and pp_b = pp ~ref:true in
          pf ppf "@[@[<1>(%a,@ %a)@] result@]" pp_a a pp_b b
      | List (m, a) ->
          pf ppf "%a list" (pp ~ref:true) a
      | Variant v ->
          if ref && Variant.type_name v <> ""
          then pf ppf "%s" (Variant.type_name v) else
          let pp_case ppf c = match Variant.Case.is_empty c with
          | true -> pf ppf "@[<2>| %s@]" (Variant.Case.name c)
          | false ->
              let pp_prod = product ~ref:false ~paren:false in
              pf ppf "@[<2>| %s of %a@]" (Variant.Case.name c) pp_prod c
          in
          pf ppf "@[<v>%a@]" (Format.pp_print_list pp_case) (Variant.cases v)

      and func : type a b. ref:bool -> (a, b) func fmt = fun ~ref ppf f ->
        let d = Func.domain f and r = Func.range f in
        pf ppf "@[%a ->@ %a@]" (pp ~ref:true) d (pp ~ref:true) r

      and pp : type a. ref:bool -> a t fmt = fun ~ref ppf g -> match g with
      | Scalar s -> Format.pp_print_string ppf (Scalar.type_name s)
      | Arraylike a -> arraylike ~ref ppf a
      | Maplike m -> maplike ~ref ppf m
      | Product p -> product ~ref ~paren:true ppf p
      | Record r -> record ~ref ppf r
      | Sum s -> sum ~ref ppf s
      | Func f -> func ~ref ppf f
      | Abstract a -> pp_string ppf (Abstract.type_name a)
      | Lazy (_, l) -> pf ppf "@[@[%a@] lazy_t@]" (pp ~ref:true) l
      | Ref (_, r) -> pf ppf "@[@[%a@] ref@]" (pp ~ref:true) r
      | Rec r -> pp ppf ~ref:true (Lazy.force r)
      in
      pp ~ref:false ppf g
  end
end

module Fun = struct
  include Stdlib.Fun

  module Generic = struct
    module Meta = struct
      module Fmt = struct
        module V = struct type 'a t = Format.formatter -> 'a -> unit end
        include V
        let ignore _ _ = ()
        include (Type.Gist.Meta.Key (V) : Type.Gist.Meta.KEY (* no mli ! *)
                 with type 'a value = 'a t)
      end
      module Equal = struct
        module V = struct type 'a t = 'a -> 'a -> bool end
        include V
        let ignore _ _ = true
        include (Type.Gist.Meta.Key (V) : Type.Gist.Meta.KEY (* no mli ! *)
                 with type 'a value = 'a t)
      end
      module Compare = struct
        module V = struct type 'a t = 'a -> 'a -> int end
        include V
        let ignore _ _ = 0
        include (Type.Gist.Meta.Key (V) : Type.Gist.Meta.KEY (* no mli ! *)
                 with type 'a value = 'a t)
      end

      module Random = struct
        module Gen = struct
          module V = struct
            type 'a t = size:int -> Random.State.t -> bound:int -> 'a
          end
          include V
          let const v = fun ~size:_ _ ~bound:_ -> v
          include (Type.Gist.Meta.Key (V) : Type.Gist.Meta.KEY (* no mli ! *)
                   with type 'a value = 'a t)
        end
        module Size = struct
          module V = struct type 'a t = int end
          include V
          include (Type.Gist.Meta.Key (V) : Type.Gist.Meta.KEY (* no mli ! *)
                   with type 'a value = 'a t)
        end
      end
    end

    module Gfmt = struct
      type 'a fmt = Format.formatter -> 'a -> unit
      let pf = Format.fprintf
      let pp_string = Format.pp_print_string
      let pp_list = Format.pp_print_list
      let pp_comma ppf () = pf ppf ",@ "
      let pp_semi ppf () = pf ppf ";@ "
      let pp_semi_cut ppf () = pf ppf ";@,"
      let pp_iter ?(pp_sep = Format.pp_print_cut) iter pp_v ppf v =
        let is_first = ref true in
        let pp_v v =
          (if !is_first then is_first := false else pp_sep ppf ()); pp_v ppf v
        in
        iter pp_v v

      let pp_array ~kind iter pp_elt ppf a =
        pf ppf "@[<2>[%s|%a|]@]" kind (pp_iter iter ~pp_sep:pp_semi pp_elt) a

      let pp_map ~kind iter pp_k pp_v ppf m =
        let is_first = ref true in
        let pp_binding ppf k v =
          (if !is_first then is_first := false else pp_semi ppf ());
          pf ppf "@[@[%a@] @<1>%s@ @[%a@]@]" pp_k k "\u{2192}" pp_v v
        in
        let pp_map ppf m = iter (pp_binding ppf) m in
        pf ppf "@[<2><%s: %a>@]" kind pp_map m

      let rec pp_arraylike : type elt a. (elt, a) Type.Gist.arraylike -> a fmt =
      fun a ppf v -> match a with
      | Bytes (_, _) -> pf ppf "%S" (Bytes.to_string v)
      | String (_, _) -> pf ppf "%S" v
      | Array (_, g) -> pp_array ~kind:"" Array.iter (pp g) ppf v
      | Bigarray1 (_, _, _, g) ->
          pp_array ~kind:"ba" Type.Gist.Arraylike.ba_iter (pp g) ppf v
      | Array_module (_, (module A), g) ->
          pp_array ~kind:A.type_name A.iter (pp g) ppf v

      and pp_maplike : type k v m. (k, v, m) Type.Gist.maplike -> m fmt =
      fun m ppf v -> match m with
      | Hashtbl (_, gk, gv) ->
          pp_map ~kind:"Hashtbl.t" Hashtbl.iter (pp gk) (pp gv) ppf v
      | Map_module (_, (module M), gk, gv) ->
          let iter f m = M.fold (fun k v () -> f k v) m () in
          pp_map ~kind:M.type_name iter (pp gk) (pp gv) ppf v

      and pp_product : type p. p Type.Gist.product -> p fmt = fun p ppf v ->
        let pp_dim ~sep dim ppf p =
          match Meta.Fmt.find (Type.Gist.Field.meta dim) with
          | Some pp when pp == Meta.Fmt.ignore -> () (* avoid sep *)
          | Some pp ->
              let v = Type.Gist.Field.project dim p in
              (if sep then pp_comma ppf ()); pp ppf v
          | None ->
              let g = Type.Gist.Field.gist dim in
              let v = Type.Gist.Field.project dim p in
              (if sep then pp_comma ppf ()); pp g ppf v
        in
        let rec pp_dims : type p a. (p, a) Type.Gist.fields -> p fmt =
          fun dims ppf v -> match dims with
          | Ctor _ -> ()
          | App (Ctor _, dim) -> pp_dim ~sep:false dim ppf v
          | App (dims, dim) -> pp_dims dims ppf v; pp_dim ~sep:true dim ppf v
        in
        match Type.Gist.Product.fields p with
        | Ctor _ -> ()
        | App (Ctor _, dim) -> pp_dim ~sep:false dim ppf v
        | _ -> pf ppf "@[<1>(%a)@]" (pp_dims (Type.Gist.Product.fields p)) v

      and pp_record : type r a. r Type.Gist.record -> r fmt = fun r ppf v ->
        let pp_field ~sep f ppf v =
          match Meta.Fmt.find (Type.Gist.Field.meta f) with
          | Some pp when pp == Meta.Fmt.ignore -> () (* avoid sep *)
          | Some pp ->
              let v = Type.Gist.Field.project f v in
              (if sep then pp_semi_cut ppf ()); pp ppf v
          | None ->
              let g = Type.Gist.Field.gist f in
              let v = Type.Gist.Field.project f v in
              (if sep then pp_semi_cut ppf ());
              pf ppf "@[%s =@ @[%a@]@]" (Type.Gist.Field.name f) (pp g) v
        in
        let rec pp_fields : type p a. (p, a) Type.Gist.fields -> p fmt =
          fun fs ppf v -> match fs with
          | Ctor _ -> ()
          | App (Ctor _, f) -> pp_field ~sep:false f ppf v
          | App (fs, f) -> pp_fields fs ppf v; pp_field ~sep:true f ppf v
        in
        pf ppf "@[<v2>{ %a@] }" (pp_fields (Type.Gist.Product.fields r)) v

      and pp_abstract : type a. a Type.Gist.abstract -> a fmt = fun a ppf v ->
        match Type.Gist.Abstract.reprs a with
        | [] -> pf ppf "<abstr>"
        | (Repr r) :: _ ->
            let v = Type.Gist.Abstract.Repr.inject r v in
            let g = Type.Gist.Abstract.Repr.gist r in
            pf ppf "@[<1><abstr:@ @[%a@]>@]" (pp g) v

      and pp_sum : type a. a Type.Gist.sum -> a fmt =
      fun s ppf v -> match s with
      | List (_, a) -> pf ppf "@[<1>[%a]@]" (pp_list ~pp_sep:pp_semi (pp a)) v
      | sum ->
          let variant = Type.Gist.Sum.to_variant sum in
          let case = Type.Gist.Variant.project variant v in
          let name = Type.Gist.Variant.Case.name case in
          if Type.Gist.Variant.Case.is_empty case
          then pp_string ppf name
          else pf ppf "%s %a" name (pp_product case) v

      and pp_func : type a b. (a -> b) Type.Gist.t fmt = fun ppf g ->
        pf ppf "<fun : %a>" Type.Gist.pp_type g

      and pp : type a. a Type.Gist.t -> a fmt = fun g ppf v ->
        (* N.B. Shifting the lookup in each case could be more efficient. *)
        match Meta.Fmt.find (Type.Gist.meta g) with
        | Some pp -> pp ppf v
        | None ->
            match g with
            | Scalar s -> Type.Gist.Scalar.pp s ppf v
            | Arraylike a -> pp_arraylike a ppf v
            | Maplike m -> pp_maplike m ppf v
            | Product p -> pp_product p ppf v
            | Record r -> pp_record r ppf v
            | Sum s -> pp_sum s ppf v
            | Func _ as g -> pp_func ppf g
            | Abstract a -> pp_abstract a ppf v
            | Lazy (_, g) -> pp g ppf (Lazy.force v)
            | Ref (_, g) -> pp g ppf (!v)
            | Rec g -> pp (Lazy.force g) ppf v
    end

    module Gequal = struct
      type 'a eq = 'a -> 'a -> bool

      let invalid_arg fmt =
        Format.ksprintf invalid_arg ("Fun.Generic.equal: " ^^ fmt)

      let equal_hashtbl eq_v h0 h1 = (* Stdlib could have something… *)
        if Hashtbl.length h0 <> Hashtbl.length h1 then false else
        let rec loop = function
        | Seq.Nil -> true
        | Seq.Cons (k, keys) ->
            let l0 = Hashtbl.find_all h0 k in
            let l1 = Hashtbl.find_all h1 k in
            if List.equal eq_v l0 l1 then loop (keys ()) else false
        in
        loop ((Hashtbl.to_seq_keys h0) ())

      let rec equal_arraylike :
        type elt arr. (elt, arr) Type.Gist.arraylike -> arr eq = fun a v0 v1 ->
        match a with
        | String (_, elt) when not (Meta.Equal.mem (Type.Gist.meta elt)) ->
            String.equal v0 v1
        | Bytes (_, elt) when not (Meta.Equal.mem (Type.Gist.meta elt)) ->
            Bytes.equal v0 v1
        | a ->
            (* FIXME there's room for improvement and clarifications
               here, e.g. for using the bare bigarrays comparison *)
            let (module A) = Type.Gist.Arraylike.to_array_module a in
            let elt = Type.Gist.Arraylike.elt a in
            let eq = equal elt in
            let v0_len = A.length v0 in
            let v1_len = A.length v1 in
            if v0_len <> v1_len then false else
            try
              for i = 0 to v0_len - 1 do
                if eq (A.get v0 i) (A.get v1 i) then () else raise_notrace Exit
              done;
              true
            with Exit -> false

      and equal_maplike :
        type k v m. (k, v, m) Type.Gist.maplike -> m eq = fun m v0 v1 ->
        match m with
        | Hashtbl (_, k, v) -> equal_hashtbl (equal v) v0 v1
        | Map_module (_, (module M), k, v) -> M.equal v0 v1

      and equal_product : type p. p Type.Gist.product -> p eq = fun p v0 v1 ->
        let rec loop : type p a. (p, a) Type.Gist.fields -> p eq =
          fun fs v0 v1 -> match fs with
          | Ctor _ -> true
          | App (fs, f) ->
              loop fs v0 v1 &&
              let v0 = Type.Gist.Field.project f v0 in
              let v1 = Type.Gist.Field.project f v1 in
              match Meta.Equal.find (Type.Gist.Field.meta f) with
              | None -> equal (Type.Gist.Field.gist f) v0 v1
              | Some equal -> equal v0 v1
        in
        loop (Type.Gist.Product.fields p) v0 v1

      and equal_sum : type s. s Type.Gist.sum -> s eq = fun s v0 v1 ->
        match s with
        | Option (_, a) -> Option.equal (equal a) v0 v1
        | Either (_,l,r) -> Either.equal ~left:(equal l) ~right:(equal r) v0 v1
        | Result (_, o, e) -> Result.equal ~ok:(equal o) ~error:(equal e) v0 v1
        | List (_, a) -> List.equal (equal a) v0 v1
        | Variant v ->
            let v0_case = Type.Gist.Variant.project v v0 in
            let v1_case = Type.Gist.Variant.project v v1 in
            if v0_case != v1_case then false else
            equal_product v0_case v0 v1

      and equal_func : type d r. (d, r) Type.Gist.func -> (d -> r) eq =
        fun f _ _ -> invalid_arg "functional value"

      and equal_abstract : type a. a Type.Gist.abstract -> a eq = fun a v0 v1 ->
        match Type.Gist.Abstract.reprs a with
        | [] ->
            let n = Type.Gist.Abstract.type_name a in
            invalid_arg "%s: abstract type exposes no representation" n
        | (Repr r) :: _ ->
            let v0 = Type.Gist.Abstract.Repr.inject r v0 in
            let v1 = Type.Gist.Abstract.Repr.inject r v1 in
            let g = Type.Gist.Abstract.Repr.gist r in
            equal g v0 v1

      and equal : type a. a Type.Gist.t -> a eq = fun g v0 v1 ->
        (* N.B. Shifting the lookup in each case could be more efficient. *)
        match Meta.Equal.find (Type.Gist.meta g) with
        | Some eq -> eq v0 v1
        | None ->
            match g with
            | Scalar s -> Type.Gist.Scalar.equal s v0 v1
            | Arraylike a -> equal_arraylike a v0 v1
            | Maplike m -> equal_maplike m v0 v1
            | Product p -> equal_product p v0 v1
            | Record r -> equal_product r v0 v1
            | Sum s -> equal_sum s v0 v1
            | Func f -> equal_func f v0 v1
            | Abstract a -> equal_abstract a v0 v1
            | Lazy (_, g) -> equal g (Lazy.force v0) (Lazy.force v1)
            | Ref (_, g) -> equal g !v0 !v1
            | Rec g -> equal (Lazy.force g) v0 v1
    end

    module Gcompare = struct
      type 'a cmp = 'a -> 'a -> int

      let invalid_arg fmt =
        Format.ksprintf invalid_arg ("Fun.Generic.compare: " ^^ fmt)

      let compare_hashtbl cmp_k cmp_v h0 h1 =
        (* Stdlib could have something… This is not so great but the
           API gives no ordering guarantees (likely because of the
           hash randomization) Also it's highly unclear what users
           expect from a notion of equality on hashtables. *)
        let bcmp (k0, _) (k1, _) = cmp_k k0 k1 in (* we keep insert order *)
        let kv0 = List.stable_sort bcmp (List.of_seq (Hashtbl.to_seq h0)) in
        let kv1 = List.stable_sort bcmp (List.of_seq (Hashtbl.to_seq h1)) in
        let cmp_kv (k0, v0) (k1, v1) =
          let cmp = cmp_k k0 k1 in if cmp <> 0 then cmp else cmp_v v0 v1
        in
        List.compare cmp_kv kv0 kv1

      let rec compare_arraylike :
        type elt arr. (elt, arr) Type.Gist.arraylike -> arr cmp = fun a v0 v1 ->
        match a with
        | String (_, elt) when not (Meta.Equal.mem (Type.Gist.meta elt)) ->
            String.compare v0 v1
        | Bytes (_, elt) when not (Meta.Equal.mem (Type.Gist.meta elt)) ->
            Bytes.compare v0 v1
        | a ->
            (* FIXME there's room for improvement and clarifications
               here, e.g. for using the bare bigarrays comparison *)
            let (module A) = Type.Gist.Arraylike.to_array_module a in
            let max = Int.min (A.length v0) (A.length v1) - 1 in
            let rec loop compare i max v0 v1 =
              if i > max then Int.compare (A.length v0) (A.length v1) else
              let cmp = compare (A.get v0 i) (A.get v1 i) in
              if cmp <> 0 then cmp else loop compare (i + 1) max v0 v1
            in
            loop (compare (Type.Gist.Arraylike.elt a)) 0 max v0 v1

      and compare_maplike :
        type k v m. (k, v, m) Type.Gist.maplike -> m cmp = fun m v0 v1 ->
        match m with
        | Hashtbl (_, k, v) -> compare_hashtbl (compare k) (compare v) v0 v1
        | Map_module (_, (module M), k, v) -> M.compare v0 v1

      and compare_product : type p. p Type.Gist.product -> p cmp =
        fun p v0 v1 ->
        let rec loop : type p a. (p, a) Type.Gist.fields -> p cmp =
          fun fs v0 v1 -> match fs with
          | Ctor _ -> 0
          | App (fs, f) ->
              let cmp = loop fs v0 v1 in
              if cmp <> 0 then cmp else
              let v0 = Type.Gist.Field.project f v0 in
              let v1 = Type.Gist.Field.project f v1 in
              match Meta.Compare.find (Type.Gist.Field.meta f) with
              | None -> compare (Type.Gist.Field.gist f) v0 v1
              | Some compare -> compare v0 v1
        in
        loop (Type.Gist.Product.fields p) v0 v1

      and compare_sum : type s. s Type.Gist.sum -> s cmp = fun s v0 v1 ->
        match s with
        | Option (_, a) ->
            Option.compare (compare a) v0 v1
        | Either (_,l,r) ->
            Either.compare ~left:(compare l) ~right:(compare r) v0 v1
        | Result (_, o, e) ->
            Result.compare ~ok:(compare o) ~error:(compare e) v0 v1
        | List (_, a) ->
            List.compare (compare a) v0 v1
        | Variant v ->
            let v0_case = Type.Gist.Variant.project v v0 in
            let v1_case = Type.Gist.Variant.project v v1 in
            if v0_case == v1_case then compare_product v0_case v0 v1 else
            (* FIXME maybe we need a better representation of variants here
               or at least a better internal data structure which associates
               a tag with each variant. Can also be useful for binary codecs. *)
            let rec loop v0c v1c = function
            | [] ->
                let n = Type.Gist.Variant.type_name v in
                invalid_arg "%s: inconsistent variant definition" n
            | c :: cs ->
                if c == v0c then -1 else
                if c == v1c then 1 else loop v0c v1c cs
            in
            loop v0_case v1_case (Type.Gist.Variant.cases v)

      and compare_func : type d r. (d, r) Type.Gist.func -> (d -> r) cmp =
        fun f _ _ -> invalid_arg "functional value"

      and compare_abstract : type a. a Type.Gist.abstract -> a cmp =
        fun a v0 v1 -> match Type.Gist.Abstract.reprs a with
        | [] ->
            let n = Type.Gist.Abstract.type_name a in
            invalid_arg "%s: abstract type exposes no representation" n
        | (Repr r) :: _ ->
            let v0 = Type.Gist.Abstract.Repr.inject r v0 in
            let v1 = Type.Gist.Abstract.Repr.inject r v1 in
            let g = Type.Gist.Abstract.Repr.gist r in
            compare g v0 v1

      and compare : type a. a Type.Gist.t -> a cmp = fun g v0 v1 ->
        (* N.B. Shifting the lookup in each case could be more efficient. *)
        match Meta.Compare.find (Type.Gist.meta g) with
        | Some compare -> compare v0 v1
        | None ->
            match g with
            | Scalar s -> Type.Gist.Scalar.compare s v0 v1
            | Arraylike a -> compare_arraylike a v0 v1
            | Maplike m -> compare_maplike m v0 v1
            | Product p -> compare_product p v0 v1
            | Record r -> compare_product r v0 v1
            | Sum s -> compare_sum s v0 v1
            | Func f -> compare_func f v0 v1
            | Abstract a -> compare_abstract a v0 v1
            | Lazy (_, g) -> compare g (Lazy.force v0) (Lazy.force v1)
            | Ref (_, g) -> compare g !v0 !v1
            | Rec g -> compare (Lazy.force g) v0 v1
    end

    module Grandom = struct
      type 'a rand = size:int -> Random.State.t -> bound:int -> 'a

      let invalid_arg fmt =
        Format.ksprintf invalid_arg ("Fun.Generic.random: " ^^ fmt)

      let sized_nat ~size st = Random.State.int st (size + 1)

      let uchar_surrogate_count = 0xDFFF - 0xD800 + 1
      let uchar_count = Uchar.to_int Uchar.max + 1 - uchar_surrogate_count
      let random_uchar st =
        let u = Random.State.int st uchar_count in
        let u = if u < 0xD800 then u else u + uchar_surrogate_count in
        Uchar.unsafe_of_int u

      let div_round_up x y = (x + y - 1) / y
      let rbits = 30 (* number of bits returned by Random.State.bits. *)
      let rbits_calls = div_round_up Sys.int_size rbits
      let random_int st =
        let r = ref 0 in
        for i = 1 to rbits_calls do
          let bits = Random.State.bits st in r := (!r lsl rbits) lor bits
        done;
        !r

      let rec rand_scalar :
        type a. a Type.Gist.scalar -> a rand = fun s ~size st ~bound ->
        match s with
        | Unit _ -> ()
        | Bool _ -> Random.State.bool st
        | Char _ -> Char.chr (Random.State.int st 256)
        | Uchar _ -> random_uchar st
        | Int _ -> random_int st
        | Int32 _ -> Random.State.bits32 st
        | Int64 _ -> Random.State.bits64 st
        | Nativeint _ -> Random.State.nativebits st
        | Float _ -> Int64.to_float (Random.State.bits64 st)

      and rand_arraylike :
        type elt arr. (elt, arr) Type.Gist.arraylike -> arr rand =
        fun a ~size st ~bound ->
        let (module A) = Type.Gist.Arraylike.to_array_module a in
        let elt = Type.Gist.Arraylike.elt a in
        A.init (sized_nat ~size st) (fun _ -> rand elt ~size st ~bound)

      and rand_maplike :
        type k v m. (k, v, m) Type.Gist.maplike -> m rand =
        fun m ~size st ~bound ->
        let bcount = sized_nat ~size st in
        let (empty, add, k, v) :
          m * (k -> v -> m -> m) * k Type.Gist.t * v Type.Gist.t =
          match m with
          | Hashtbl (_, k, v) ->
              let add k v h = Hashtbl.replace h k v; h in
              Hashtbl.create bcount, add, k, v
          | Map_module (_, (module M), k, v) ->
              M.empty, M.add, k, v
        in
        let rkey = rand (Type.Gist.Maplike.key m) in
        let rvalue = rand (Type.Gist.Maplike.value m) in
        let rec loop count m =
          if count <= 0 then m else
          let k = rkey ~size st ~bound and v = rvalue ~size st ~bound in
          loop (count - 1) (add k v m)
        in
        loop bcount empty

      and rand_product : type p. p Type.Gist.product -> p rand =
        fun p ~size st ~bound ->
        let rec rand_fields : type p a. (p, a) Type.Gist.fields -> a =
          function
          | Ctor f -> f
          | App (fs, field) ->
              let f = rand_fields fs in
              let v = rand (Type.Gist.Field.gist field) ~size st ~bound in
              f v
        in
        rand_fields (Type.Gist.Product.fields p)

      and rand_variant_case : type p. p Type.Gist.Variant.case -> p rand =
        fun c ~size st ~bound ->
        let rec rand_fields : type p a. (p, a) Type.Gist.fields -> a =
          function
          | Ctor f -> f
          | App (fs, field) ->
              let f = rand_fields fs in
              let v = match Type.Gist.Field.gist field with
              | Rec _ ->
                  rand (Type.Gist.Field.gist field) ~size st ~bound
              | _ ->
                  rand (Type.Gist.Field.gist field) ~size st ~bound:size
              in
              f v
        in
        rand_fields (Type.Gist.Variant.Case.fields c)

      and rand_sum : type s. s Type.Gist.sum -> s rand =
        fun s ~size st ~bound -> match s with
        | List (_, elt) ->
            List.init (sized_nat ~size st) (fun _ -> rand elt ~size st ~bound)
        | s ->
            let v = Type.Gist.Sum.to_variant s in
            let cases = Type.Gist.Variant.cases v in
            let rec case_distrib count acc = function
            | [] -> count, acc
            | c :: cs ->
                match Type.Gist.Variant.Case.rec_field_count c with
                | 0 -> case_distrib (count + 1) ((bound, c) :: acc) cs
                | n when bound <= 0 -> case_distrib count acc cs (* no rec *)
                | n ->
                    let bound = bound / n in
                    let c = bound, c in
                    (* more frequently, can we have a principle here rather
                       than a magic number ? *)
                    let acc = c :: c :: acc in
                    case_distrib (count + 2) acc cs
            in
            let count, case_distrib = case_distrib 0 [] cases in
            let i = Random.State.int st count in
            let bound, case = List.nth case_distrib i in
            rand_variant_case case ~size st ~bound

      and rand_func :
        type d r. (d, r) Type.Gist.func -> (d -> r) rand = fun f ~size st ->
        (* TODO we could do something here but see §3.3 of the quickcheck
           paper. Also on 5.0 we have splitting which could help. *)
        invalid_arg "functional value"

      and rand_abstract :
        type a. a Type.Gist.abstract -> a rand = fun a ~size st ~bound ->
        match Type.Gist.Abstract.reprs a with
        | [] ->
            let n = Type.Gist.Abstract.type_name a in
            invalid_arg "%s: abstract type exposes no representation" n
        | (Repr r) :: _ ->
            let g = Type.Gist.Abstract.Repr.gist r in
            let v = rand g ~size st ~bound in
            Type.Gist.Abstract.Repr.project r v

      and rand : type a. a Type.Gist.t -> a rand = fun g ~size st ~bound ->
        (* N.B. Shifting the lookup in each case could be more efficient.
           Also maybe we should merge Gen and Size into a single structure. *)
        let meta = Type.Gist.meta g in
        let size = Option.value ~default:size (Meta.Random.Size.find meta) in
        match Meta.Random.Gen.find (Type.Gist.meta g) with
        | Some rand -> rand ~size st ~bound
        | None ->
            match g with
            | Scalar s -> rand_scalar s ~size st ~bound
            | Arraylike a -> rand_arraylike a ~size st ~bound
            | Maplike m -> rand_maplike m ~size st ~bound
            | Product p -> rand_product p ~size st ~bound
            | Record r -> rand_product r ~size st ~bound
            | Sum s -> rand_sum s ~size st ~bound
            | Func f -> rand_func f ~size st ~bound
            | Abstract a -> rand_abstract a ~size st ~bound
            | Lazy (_, g) -> lazy (rand g ~size st ~bound)
            | Ref (_, g) -> ref (rand g ~size st ~bound)
            | Rec g -> rand (Lazy.force g) ~size st ~bound

      let random g ?(size = 23) ?state () =
        let st = match state with None -> Random.get_state () | Some s -> s in
        let v = rand g ~size st ~bound:size in
        Random.set_state st;
        v
    end

    let pp = Gfmt.pp
    let equal = Gequal.equal
    let compare = Gcompare.compare
    let random = Grandom.random
  end
end
