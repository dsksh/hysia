open Jg_types

let compiled = ref false
let file = ref "navigation.tmpl"

let models = [
  ("list", Tlist [Tint 1; Tint 2; Tint 3]);
]

let nv = ref 4
  
let () =
  Arg.parse [
    ("-n", Arg.Int (fun n -> nv := n), "set the value of n");
    ("-rotated", Arg.Unit (fun () -> file := "nav_rotated.tmpl"), "generate a pi/4 rotated Navigation model");
    ("-atm"), Arg.Unit (fun () -> file := "atm.tmpl"), "generate an ATM model"
  ] ignore "";

  let result_string = 
    Jg_template.from_file !file ~use_compiled:false ~models:(("n",Tint !nv)::models) in

  print_endline result_string
;;
