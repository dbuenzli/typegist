#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "typegist" @@ fun c ->
  Ok [ Pkg.mllib "src/typegist.mllib";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/quick.mld" ~dst:"odoc-pages/quick.mld";
       Pkg.doc "doc/notes.mld" ~dst:"odoc-pages/notes.mld";
       Pkg.doc "doc/cookbook.mld" ~dst:"odoc-pages/cookbook.mld";
     ]
