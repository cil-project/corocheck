open Ocamlbuild_plugin ;;
open Command ;;

let ocamlfind_query pkg =
  let cmd = Printf.sprintf "ocamlfind query %s" (Filename.quote pkg) in
  Ocamlbuild_pack.My_unix.run_and_open cmd input_line
;;

dispatch begin function
| After_rules ->
    ocaml_lib ~extern:true ~dir:(ocamlfind_query "ocamlgraph") "graph"
| _ -> ()
end ;;
