open Jg_types

let compiled = ref false
let file = ref "navigation.tmpl"

let models = [
  ("list", Tlist [Tint 1; Tint 2; Tint 3]);
]

let nv = ref 4
  
let () =
  Arg.parse [
    ("-n", Arg.Int (fun n -> nv := n), "# of vehicles");
    ("-hytech", Arg.Unit (fun () -> file := "navigation_hytech.tmpl"), "generate a HyTech model");
    ("-why", Arg.Unit (fun () -> file := "navigation_.tmpl"), "generate another HyTech model");
    ("-math", Arg.Unit (fun () -> file := "navigation_math.tmpl"), "generate Mathematica model");
  ] ignore "";

  let result_string = 
    Jg_template.from_file !file ~use_compiled:false ~models:(("n",Tint !nv)::models) in

  print_endline result_string
;;
