open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let typegist = B0_ocaml.libname "typegist"

(* Libraries *)

let typegist_lib =
  let srcs = Fpath.[ `Dir (v "src") ] in
  let requires = [] in
  B0_ocaml.lib typegist ~doc:"Typegist library" ~srcs ~requires

(* Tests *)

let test_src f = `File (Fpath.v (Fmt.str "test/%s" f))

let test =
  let srcs = [test_src "test.ml"] in
  let requires = [ typegist ] in
  let doc = "Test" in
  B0_ocaml.exe "test" ~doc ~srcs ~requires

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The typegist programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/typegist"
    |> add online_doc "https://erratique.ch/software/typegist/doc"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/typegist.git"
    |> add issues "https://github.com/dbuenzli/typegist/issues"
    |> add description_tags ["typerep"; "generic"; "org:erratique"; ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> tag B0_opam.tag
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|}; ]
  in
  B0_pack.v "default" ~doc:"typegist package" ~meta ~locked:true @@
  B0_unit.list ()
