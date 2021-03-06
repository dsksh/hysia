open Jg_types

let compiled = ref false
let file = ref "template.tmpl"

let models = [
  ("list", Tlist [Tint 1; Tint 2; Tint 3]);
]

let nv = ref 4

let name = ref ""
  
let () =
  Arg.parse [
    ("-n", Arg.Int (fun n -> nv := n), "value of n.");
    ("-name", Arg.String (fun n -> name := n), "name.");
    (*("-", Arg.String (fun fn -> file := fn), "specify the template filename.");*)
  ] (fun fn -> file := fn) "";

  let result_string = 
    Jg_template.from_file !file ~use_compiled:false ~models:(("name",Tstr !name)::("n",Tint !nv)::models) in

  print_endline result_string
;;
