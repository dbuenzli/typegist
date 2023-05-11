#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "typegist" @@ fun c ->
  Ok [ Pkg.mllib "src/typegist.mllib";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/quick.mld" ~dst:"odoc-pages/quick.mld" ]
