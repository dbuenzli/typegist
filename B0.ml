open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let typegist = B0_ocaml.libname "typegist"
let b0_std = B0_ocaml.libname "b0.std"

(* Libraries *)

let typegist_lib = B0_ocaml.lib typegist ~srcs: [`Dir ~/"src"]

(* Tests *)

let test ?(requires = []) = B0_ocaml.test ~requires:(typegist :: requires)
let test_typegist = test ~/"test/test_typegist.ml" ~requires:[b0_std]
let cookbook = test ~/"test/cookbook.ml" ~run:false

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The typegist programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/typegist"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/typegist/doc"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/typegist.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/typegist/issues"
    |> ~~ B0_meta.description_tags ["typerep"; "generic"; "org:erratique"]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|}; ]
    |> B0_meta.tag B0_opam.tag
  in
  B0_pack.make "default" ~doc:"The typegist package" ~meta ~locked:true @@
  B0_unit.list ()
